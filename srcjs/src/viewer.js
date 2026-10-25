import * as THREE from "three";
import { doubleClickSelection } from "./selection.js";
import { componentFor, matchesNameFilter, objectVisibilityReasons } from "./geometry-filter.js";
import { OrbitControls } from "three/addons/controls/OrbitControls.js";
import { float32, int32 } from "./binary.js";
import { objectColor, resultGradient } from "./colors.js";
import { icon, buttonIcon } from "./icons.js";
import { ViewerRibbon } from "./ribbon.js";
import { ViewNavigation } from "./navigation.js";
import { displayUnit, timeCaption } from "./presentation.js";
import { ResultQueryPanel } from "./result-query.js";
import { findSharedBoundaries, sharedBoundaryGroups } from "./shared-boundaries.js";
import { boundaryColor, isGlazing, GLASS_COLOR, GLASS_OPACITY } from "./appearance.js";

// Share familiar action symbols across the toolbar, panel, and playback strip.
const ACTION_ICONS = {
  Fit: "fit", "Fit model": "fit", "Fit selection": "isolate", Panel: "panel", Edges: "wireframe", "X-ray": "xray",
  Axes: "axes", Ground: "ground", Measure: "measure", "Show all": "reset",
  Snapshot: "snapshot", Isolate: "isolate", Hide: "hide", Clear: "clear",
  "Clear selection": "clear", "Clear filters": "filters", "Exit isolation": "undo", "Save current": "save",
  Delete: "delete", Previous: "previous", Next: "next", Play: "play"
};

const COMPONENTS = ["floor", "wall", "roof", "window", "door", "shading", "daylighting"];

// Create one DOM control with safe text content.
function element(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  if (tag === "button" && ACTION_ICONS[text]) {
    buttonIcon(node, ACTION_ICONS[text], text, ["Previous", "Next", "Play", "Clear"].includes(text));
  }
  return node;
}

// Create a select option without interpolating HTML.
function option(value, label = value) {
  const node = document.createElement("option");
  node.value = value;
  node.textContent = label;
  return node;
}

// Ignore missing values when deriving the numerical display range.
function finiteRange(values, fallback = [0, 1]) {
  let minimum = Infinity;
  let maximum = -Infinity;
  for (const value of values) {
    if (!Number.isFinite(value)) continue;
    minimum = Math.min(minimum, value);
    maximum = Math.max(maximum, value);
  }
  return Number.isFinite(minimum) ? [minimum, maximum] : fallback;
}

// Compute a finite quantile without allocating sorted copies for empty result sets.
function finiteQuantile(values, probability) {
  const finite = Array.from(values).filter(Number.isFinite).sort((a, b) => a - b);
  if (!finite.length) return Number.NaN;
  const position = Math.max(0, Math.min(1, probability)) * (finite.length - 1);
  const lower = Math.floor(position);
  const upper = Math.min(finite.length - 1, lower + 1);
  return finite[lower] + (finite[upper] - finite[lower]) * (position - lower);
}

// Return the categorical label represented by a render mode for legends and summaries.
function categoryValue(object, renderBy) {
  if (renderBy === "surface_type") return object.surface_type ?? "Undefined";
  if (renderBy === "boundary") return object.outside_boundary_condition ?? "Undefined";
  if (renderBy === "construction") return object.construction_name ?? "Undefined";
  if (renderBy === "zone") return object.zone_name ?? "No zone";
  if (renderBy === "space") return object.space_name ?? "No space";
  return "Surface";
}

// Format values consistently across legends, diagnostics, measurements, and selection details.
function formatNumber(value, digits = 5) {
  if (!Number.isFinite(Number(value))) return "Missing";
  return Number(value).toLocaleString(undefined, { maximumSignificantDigits: digits });
}

// Derive a stable day or month key from common EnergyPlus and ISO time labels.
function timeGroupKey(label, mode) {
  const text = String(label ?? "");
  const match = text.match(/(?:(\d{4})[-/])?(\d{1,2})[-/](\d{1,2})/);
  if (!match) return text;
  const year = match[1] ? `${match[1]}-` : "";
  const month = String(Number(match[2])).padStart(2, "0");
  const day = String(Number(match[3])).padStart(2, "0");
  return mode === "month" ? `${year}${month}` : `${year}${month}-${day}`;
}

// Render a domain-specific EnergyPlus scene while keeping all interaction state in the browser.
export class IdfViewer {
  // Own the scene, controls and WebGL resources for one widget instance.
  constructor(container) {
    this.container = container;
    this.container.innerHTML = "";
    this.root = element("div", "eplusr-viewer");
    this.canvasHost = element("div", "eplusr-viewer__canvas");
    this.root.appendChild(this.canvasHost);
    this.container.appendChild(this.root);

    this.scene = new THREE.Scene();
    this.scene.background = new THREE.Color("#f3f5f7");
    this.renderer = new THREE.WebGLRenderer({ antialias: true, alpha: false, preserveDrawingBuffer: true });
    this.renderer.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
    this.renderer.localClippingEnabled = true;
    this.renderer.domElement.setAttribute("role", "img");
    this.renderer.domElement.setAttribute("aria-label", "Interactive 3D EnergyPlus model. Use the toolbar, mouse, or keyboard shortcuts to inspect the geometry.");
    this.canvasHost.appendChild(this.renderer.domElement);

    this.perspectiveCamera = new THREE.PerspectiveCamera(38, 1, 0.01, 100000);
    this.perspectiveCamera.up.set(0, 0, 1);
    this.orthographicCamera = new THREE.OrthographicCamera(-1, 1, 1, -1, -100000, 100000);
    this.orthographicCamera.up.set(0, 0, 1);
    this.camera = this.perspectiveCamera;
    this.controls = new OrbitControls(this.camera, this.renderer.domElement);
    this.controls.enableDamping = true;
    this.controls.dampingFactor = 0.12;
    this.controls.screenSpacePanning = true;

    this.raycaster = new THREE.Raycaster();
    this.pointer = new THREE.Vector2();
    this.objects = [];
    // Empty typed buffers keep resize and panel observers safe before the first model payload arrives.
    this.positions = new Float32Array();
    this.faceObjects = new Int32Array();
    this.edgePositions = new Float32Array();
    this.edgeObjects = new Int32Array();
    this.pointPositions = new Float32Array();
    this.selected = new Set();
    this.hidden = new Set();
    this.isolated = null;
    this.isolationHistory = [];
    this.hovered = null;
    this.visibleObjects = new Set();
    this.componentVisibility = Object.fromEntries(COMPONENTS.map((name) => [name, true]));
    this.zoneFilter = "";
    this.spaceFilter = "";
    this.nameFilter = "";
    this.remoteFilters = {};
    this.selectionMode = "surface";
    this.renderBy = "surface_type";
    this.frame = 0;
    this.resultSet = [];
    this.resultVariableIndex = 0;
    this.results = null;
    this.resultAbortController = null;
    this.playbackTimer = null;
    this.playbackSpeed = 1;
    this.xray = false;
    this.windowStyle = "glass";
    this.glazingObjects = new Set();
    this.windowParents = new Map();
    this.showWireframe = true;
    this.showAxis = true;
    this.showGround = false;
    this.pointerDown = null;
    this.animationFrame = null;
    this.needsRender = true;
    this.clipPlane = new THREE.Plane(new THREE.Vector3(0, 0, -1), 0);
    this.clipEnabled = false;
    this.clipAxis = "z";
    this.clipCap = false;
    this.explodeMode = "none";
    this.explodeAmount = 0;
    this.measureMode = false;
    this.measurePoints = [];
    this.measurement = null;
    this.showDiagnostics = false;
    this.diagnosticOnly = false;
    this.diagnosticObjectSet = new Set();

    this.addLights();
    this.buildUi();
    this.bindEvents();
    this.animate();
  }

  // Use stable neutral lighting so categorical and result colors remain comparable across views.
  addLights() {
    this.scene.add(new THREE.HemisphereLight(0xffffff, 0x7d8796, 2.2));
    const light = new THREE.DirectionalLight(0xffffff, 2.5);
    light.position.set(-1, -1, 2);
    this.scene.add(light);
  }

  // Construct all controls in plain DOM so the widget works in RStudio, standalone HTML, and live R connections.
  buildUi() {
    this.toolbar = element("div", "eplusr-viewer__toolbar");
    this.toolbar.setAttribute("role", "toolbar");
    this.toolbar.setAttribute("aria-label", "3D model viewer controls");
    this.panel = element("aside", "eplusr-viewer__panel");
    this.panel.id = `${this.container.id || "eplusr-viewer"}-panel`;
    this.panel.setAttribute("aria-label", "Model inspection panel");
    this.tooltip = element("div", "eplusr-viewer__tooltip");
    this.legend = element("div", "eplusr-viewer__legend");
    this.legend.setAttribute("aria-label", "Color legend");
    this.workspace = element("div", "eplusr-viewer__workspace");
    this.scopeBar = element("div", "eplusr-viewer__scope");
    this.visibleCount = element("span", "eplusr-viewer__visible-count");
    this.visibleCount.setAttribute("role", "status");
    this.filterChips = element("div", "eplusr-viewer__filter-chips");
    this.filterChips.setAttribute("aria-label", "Active visibility filters");
    this.selectionCount = element("button", "eplusr-viewer__selection-count");
    this.selectionCount.type = "button";
    this.selectionCount.addEventListener("click", () => {
      this.setPanelTab("properties");
    });
    const clearFilters = element("button", "eplusr-viewer__clear-filters", "Clear filters");
    clearFilters.type = "button";
    clearFilters.addEventListener("click", () => this.clearFilters());
    this.clearFiltersButton = clearFilters;
    this.scopeBar.append(this.visibleCount, this.filterChips, clearFilters, this.selectionCount);
    this.workspace.append(this.canvasHost, this.panel);
    this.canvasHost.append(this.tooltip, this.legend);
    this.timeline = element("div", "eplusr-viewer__timeline");
    this.timeline.hidden = true;
    this.timeline.setAttribute("aria-label", "Result time controls");
    this.root.append(this.toolbar, this.scopeBar, this.workspace, this.timeline);

    const renderGroup = this.toolbarGroup("Rendering");
    this.renderSelect = element("select");
    this.renderSelect.setAttribute("aria-label", "Color surfaces by");
    [
      ["surface_type", "Surface type"], ["boundary", "Boundary"],
      ["construction", "Construction"], ["zone", "Zone"], ["space", "Space"],
      ["normal", "Normals"]
    ].forEach(([value, label]) => this.renderSelect.appendChild(option(value, label)));
    this.renderSelect.addEventListener("change", () => this.setRenderBy(this.renderSelect.value));
    renderGroup.append(icon("layers"), this.renderSelect);

    const cameraGroup = this.toolbarGroup("Camera");
    this.viewSelect = element("select");
    this.viewSelect.setAttribute("aria-label", "Camera view");
    [
      ["iso", "Isometric"], ["top", "Top"], ["bottom", "Bottom"],
      ["front", "Front"], ["back", "Back"], ["left", "Left"], ["right", "Right"]
    ].forEach(([value, label]) => this.viewSelect.appendChild(option(value, label)));
    const customView = option("custom", "Custom");
    customView.disabled = true;
    this.viewSelect.appendChild(customView);
    this.viewSelect.classList.add("eplusr-viewer__view-presets");
    this.viewSelect.addEventListener("change", () => this.setView(this.viewSelect.value));
    cameraGroup.append(icon("object"), this.viewSelect);

    this.projectionSelect = element("select", "eplusr-viewer__projection");
    this.projectionSelect.setAttribute("aria-label", "Projection");
    this.projectionSelect.append(option("perspective", "Perspective"), option("orthographic", "Orthographic"));
    this.projectionSelect.addEventListener("change", () => this.setProjection(this.projectionSelect.value === "orthographic"));
    this.fitModelButton = element("button", "eplusr-viewer__fit-model", "Fit model");
    this.fitModelButton.type = "button";
    this.fitModelButton.addEventListener("click", () => this.fitView());
    this.fitSelectionButton = element("button", "eplusr-viewer__fit-selection", "Fit selection");
    this.fitSelectionButton.type = "button";
    this.fitSelectionButton.disabled = true;
    this.fitSelectionButton.addEventListener("click", () => {
      if (this.selected.size) this.fitView(this.selected);
    });
    cameraGroup.append(this.projectionSelect, this.fitModelButton, this.fitSelectionButton);

    const displayGroup = this.toolbarGroup("Display");
    this.wireButton = this.toggleButton("Edges", true, (value) => {
      this.showWireframe = value;
      this.updateHelpers();
    }, displayGroup);
    this.xrayButton = this.toggleButton("X-ray", false, (value) => {
      this.xray = value;
      this.updateMaterials();
      this.updateColors();
    }, displayGroup);
    this.axisButton = this.toggleButton("Axes", true, (value) => {
      this.showAxis = value;
      this.updateHelpers();
    }, displayGroup);
    this.groundButton = this.toggleButton("Ground", false, (value) => {
      this.showGround = value;
      this.updateHelpers();
    }, displayGroup);

    const actionGroup = this.toolbarGroup("Actions");
    this.measureButton = this.toggleButton("Measure", false, (value) => this.toggleMeasureMode(value), actionGroup);
    this.measureButton.title = "Measure the distance between two picked points";
    const reset = element("button", "", "Show all");
    reset.type = "button";
    reset.addEventListener("click", () => this.showAll());
    const capture = element("button", "", "Snapshot");
    capture.type = "button";
    capture.addEventListener("click", () => this.downloadSnapshot());
    this.panelButton = element("button", "", "Panel");
    this.panelButton.type = "button";
    this.panelButton.setAttribute("aria-controls", this.panel.id);
    this.panelButton.setAttribute("aria-expanded", "true");
    this.panelButton.addEventListener("click", () => this.togglePanel());
    actionGroup.append(reset, capture, this.panelButton);

    this.buildTaskContainers();

    const searchSection = this.panelSection("Visibility filters", true);
    const searchRow = element("div", "eplusr-viewer__search");
    const searchLabel = element("label", "eplusr-viewer__field");
    searchLabel.appendChild(element("span", "eplusr-viewer__field-title", "Name contains"));
    this.nameFilterInput = element("input");
    this.nameFilterInput.type = "search";
    this.nameFilterInput.placeholder = "Part of an object name";
    this.nameFilterInput.setAttribute("aria-label", "Name contains");
    this.nameFilterInput.setAttribute("aria-describedby", this.panel.id + "-name-filter-status");
    this.searchList = element("datalist");
    this.searchList.id = this.panel.id + "-name-filter-options";
    this.nameFilterInput.setAttribute("list", this.searchList.id);
    searchLabel.append(this.nameFilterInput, this.searchList);
    this.clearNameFilterButton = element("button");
    this.clearNameFilterButton.type = "button";
    buttonIcon(this.clearNameFilterButton, "clear", "Clear name filter", true);
    this.clearNameFilterButton.addEventListener("click", () => {
      this.setNameFilter(""); this.nameFilterInput.focus();
    });
    searchRow.append(searchLabel, this.clearNameFilterButton);
    this.nameFilterStatus = element("div", "eplusr-viewer__status");
    this.nameFilterStatus.id = this.panel.id + "-name-filter-status";
    this.nameFilterStatus.setAttribute("role", "status");
    this.nameFilterStatus.setAttribute("aria-live", "polite");
    searchSection.body.append(searchRow, this.nameFilterStatus);
    this.nameFilterInput.addEventListener("input", () => this.setNameFilter(this.nameFilterInput.value));
    this.nameFilterInput.addEventListener("keydown", event => {
      if (event.key === "Enter") event.preventDefault();
      if (event.key === "Escape" && this.nameFilterInput.value) {
        event.preventDefault(); event.stopPropagation(); this.setNameFilter("");
      }
    });

    this.selectionModeSelect = this.field("Pick as", "select", searchSection.body);
    this.selectionModeSelect.append(option("surface", "Surfaces"), option("space", "Spaces"), option("zone", "Zones"));
    this.selectionModeSelect.addEventListener("change", () => {
      this.selectionMode = this.selectionModeSelect.value;
      this.updateViewportFeedback();
      this.root.dispatchEvent(new CustomEvent("eplusr-view"));
    });
    this.zoneSelect = this.field("Zone", "select", searchSection.body);
    this.zoneSelect.addEventListener("change", () => this.setZoneFilter(this.zoneSelect.value));
    this.spaceSelect = this.field("Space", "select", searchSection.body);
    this.spaceSelect.addEventListener("change", () => this.setSpaceFilter(this.spaceSelect.value));

    // Keep the narrow toolbar focused on camera, color, and opening the panel.
    const displaySection = this.panelSection("Display controls", true, "", "display");
    displaySection.body.append(displayGroup, this.measureButton, capture);
    searchSection.body.append(reset);
    const savedViewSection = this.panelSection("Saved views", false, "", "display");
    this.savedViewSelect = this.field("Camera preset", "select", savedViewSection.body);
    this.savedViewSelect.addEventListener("change", () => {
      if (this.savedViewSelect.value) this.restoreCameraView(this.savedViewSelect.value);
    });
    this.savedViewName = element("input");
    this.savedViewName.placeholder = "View name";
    this.savedViewName.setAttribute("aria-label", "Saved view name");
    const savedViewActions = element("div", "eplusr-viewer__button-row");
    const saveView = element("button", "", "Save current");
    saveView.type = "button";
    saveView.addEventListener("click", () => this.saveCameraView(this.savedViewName.value));
    const deleteView = element("button", "", "Delete");
    deleteView.type = "button";
    deleteView.addEventListener("click", () => this.deleteCameraView(this.savedViewSelect.value));
    savedViewActions.append(saveView, deleteView);
    savedViewSection.body.append(this.savedViewName, savedViewActions);
    this.savedViewStatus = element("p", "eplusr-viewer__status");
    this.savedViewStatus.setAttribute("role", "status");
    savedViewSection.body.append(this.savedViewStatus);
    this.populateSavedViews();

    const componentSection = this.panelSection("Components", true);
    const checks = element("div", "eplusr-viewer__checks");
    const labels = { floor: "Floors", wall: "Walls", roof: "Roofs / ceilings", window: "Windows", door: "Doors", shading: "Shading", daylighting: "Daylighting points" };
    this.componentInputs = {};
    COMPONENTS.forEach((component) => {
      const label = element("label");
      const input = element("input");
      input.type = "checkbox";
      input.checked = true;
      input.addEventListener("change", () => {
        // A browser edit replaces the effective type set, including a filter sent from R.
        this.remoteFilters.type = null;
        COMPONENTS.forEach((name) => { this.componentVisibility[name] = this.componentInputs[name].checked; });
        this.applyVisibilityChange();
      });
      label.append(input, document.createTextNode(labels[component]));
      checks.appendChild(label);
      this.componentInputs[component] = input;
    });
    componentSection.body.appendChild(checks);

    const appearanceSection = this.panelSection("Surface appearance", true, "", "display");
    this.windowStyleSelect = this.field("Window appearance", "select", appearanceSection.body);
    [["glass", "Transparent glass"], ["opaque", "Opaque colors"]].forEach(([value,label]) => this.windowStyleSelect.appendChild(option(value,label)));
    this.windowStyleSelect.value = this.windowStyle;
    this.windowStyleSelect.addEventListener("change", () => this.setWindowStyle(this.windowStyleSelect.value));
    appearanceSection.body.appendChild(element("p", "eplusr-viewer__status", "Glass is not color mapped. Choose Opaque colors to inspect window result colors."));

    const clippingSection = this.panelSection("Clipping and explode", false, "", "display");
    this.clipAxisSelect = this.field("Plane direction", "select", clippingSection.body);
    [["z", "Horizontal (Z)"], ["x", "X plane"], ["y", "Y plane"]]
      .forEach(([value, label]) => this.clipAxisSelect.appendChild(option(value, label)));
    this.clipAxisSelect.addEventListener("change", () => this.setClipAxis(this.clipAxisSelect.value));
    const clipping = element("div", "eplusr-viewer__field");
    const clipToggleLabel = element("label");
    this.clipToggle = element("input");
    this.clipToggle.type = "checkbox";
    this.clipToggle.addEventListener("change", () => {
      this.clipEnabled = this.clipToggle.checked;
      this.updateClipping();
    });
    clipToggleLabel.append(this.clipToggle, document.createTextNode(" Enable clipping plane"));
    this.clipSlider = element("input");
    this.clipSlider.type = "range";
    this.clipSlider.step = "any";
    this.clipSlider.setAttribute("aria-label", "Clipping plane position");
    this.clipSlider.addEventListener("input", () => this.updateClipping());
    this.clipValue = element("output", "eplusr-viewer__value");
    const clipCapLabel = element("label");
    this.clipCapToggle = element("input");
    this.clipCapToggle.type = "checkbox";
    this.clipCapToggle.addEventListener("change", () => {
      this.clipCap = this.clipCapToggle.checked;
      this.updateClipCap();
    });
    clipCapLabel.append(this.clipCapToggle, document.createTextNode(" Show section cap"));
    clipping.append(clipToggleLabel, this.clipSlider, this.clipValue, clipCapLabel);
    clippingSection.body.appendChild(clipping);

    this.explodeSelect = this.field("Explode by", "select", clippingSection.body);
    [["none", "None"], ["floor", "Floors"], ["zone", "Zones"]]
      .forEach(([value, label]) => this.explodeSelect.appendChild(option(value, label)));
    this.explodeSelect.addEventListener("change", () => this.setExplode(this.explodeSelect.value, Number(this.explodeSlider.value)));
    this.explodeSlider = element("input");
    this.explodeSlider.type = "range";
    this.explodeSlider.min = "0";
    this.explodeSlider.max = "2";
    this.explodeSlider.step = "0.05";
    this.explodeSlider.value = "0";
    this.explodeSlider.setAttribute("aria-label", "Explode distance");
    this.explodeSlider.addEventListener("input", () => this.setExplode(this.explodeSelect.value, Number(this.explodeSlider.value)));
    clippingSection.body.appendChild(this.explodeSlider);

    const resultPanel = this.panelSection("Color mapping", true, "eplusr-viewer__results", "results");
    this.resultSection = resultPanel.details;
    const variableField = element("label", "eplusr-viewer__field");
    variableField.appendChild(element("span", "", "Variable"));
    this.variableSelect = element("select");
    this.variableSelect.setAttribute("aria-label", "Result variable");
    this.variableSelect.addEventListener("change", () => {
      this.setResultVariable(Number(this.variableSelect.value)).catch((error) => this.reportResultError(error));
    });
    variableField.appendChild(this.variableSelect);

    const paletteField = element("label", "eplusr-viewer__field");
    paletteField.appendChild(element("span", "", "Color palette"));
    this.paletteSelect = element("select");
    this.paletteSelect.setAttribute("aria-label", "Color palette");
    [["viridis", "Viridis"], ["diverging", "Diverging"], ["grayscale", "Grayscale"], ["sequential", "Sequential"], ["rainbow", "Rainbow"]]
      .forEach(([value, label]) => this.paletteSelect.appendChild(option(value, label)));
    this.paletteSelect.addEventListener("change", () => this.setPalette(this.paletteSelect.value));
    paletteField.appendChild(this.paletteSelect);

    const rangeField = element("div", "eplusr-viewer__field");
    const rangeInputs = element("div", "eplusr-viewer__range");
    this.rangeMinimum = element("input");
    this.rangeMaximum = element("input");
    [this.rangeMinimum, this.rangeMaximum].forEach((input) => {
      input.type = "number";
      input.step = "any";
      input.addEventListener("change", () => this.setResultRange());
    });
    this.rangeMinimum.setAttribute("aria-label", "Minimum display value");
    this.rangeMaximum.setAttribute("aria-label", "Maximum display value");
    const minimumLabel = element("label", "eplusr-viewer__field");
    minimumLabel.append(element("span", "", "Min"), this.rangeMinimum);
    const maximumLabel = element("label", "eplusr-viewer__field");
    maximumLabel.append(element("span", "", "Max"), this.rangeMaximum);
    rangeInputs.append(minimumLabel, maximumLabel);
    this.rangeModeSelect = this.field("Range policy", "select", rangeField);
    this.rangeModeSelect.append(option("all", "All frames · fixed"), option("frame", "Current frame"), option("manual", "Manual"));
    this.rangeModeSelect.addEventListener("change", () => this.setRangeMode(this.rangeModeSelect.value));
    rangeField.appendChild(rangeInputs);
    const rangeActions = element("div", "eplusr-viewer__button-row");
    const autoRange = element("button", "", "Reset range");
    autoRange.type = "button";
    autoRange.addEventListener("click", () => this.resetResultRange());
    const symmetricRange = element("button", "", "Symmetric");
    symmetricRange.type = "button";
    symmetricRange.addEventListener("click", () => this.setSymmetricRange());
    this.percentileSelect = element("select");
    this.percentileSelect.setAttribute("aria-label", "Percentile clipping");
    [[1, "100%"], [0.99, "99%"], [0.95, "95%"], [0.9, "90%"]]
      .forEach(([value, label]) => this.percentileSelect.appendChild(option(value, label)));
    this.percentileSelect.addEventListener("change", () => this.setPercentileRange(Number(this.percentileSelect.value)));
    rangeActions.append(autoRange, symmetricRange);
    const percentileField = element("label", "eplusr-viewer__field");
    percentileField.append(element("span", "", "Percentile coverage"), this.percentileSelect);
    this.rangeStatus = element("div", "eplusr-viewer__status");
    this.rangeStatus.setAttribute("role", "status");
    rangeField.append(percentileField, this.rangeStatus);
    rangeField.appendChild(rangeActions);

    const missingField = element("label", "eplusr-viewer__field eplusr-viewer__color-field");
    missingField.appendChild(element("span", "", "Missing-value color"));
    this.missingColorInput = element("input");
    this.missingColorInput.type = "color";
    this.missingColorInput.value = "#b8bdc5";
    this.missingColorInput.setAttribute("aria-label", "Missing-value color");
    this.missingColorInput.addEventListener("input", () => this.setMissingColor(this.missingColorInput.value));
    missingField.appendChild(this.missingColorInput);

    const playback = element("div", "eplusr-viewer__playback");
    this.previousTimeButton = element("button", "", "Previous");
    this.previousTimeButton.type = "button";
    this.previousTimeButton.addEventListener("click", () => this.stepTime(-1));
    this.playButton = element("button", "", "Play");
    this.playButton.type = "button";
    this.playButton.addEventListener("click", () => this.playbackTimer ? this.pause() : this.play());
    this.nextTimeButton = element("button", "", "Next");
    this.nextTimeButton.type = "button";
    this.nextTimeButton.addEventListener("click", () => this.stepTime(1));
    this.speedSelect = element("select");
    this.speedSelect.setAttribute("aria-label", "Playback speed");
    [[0.5, "0.5×"], [1, "1×"], [2, "2×"], [5, "5×"], [10, "10×"]]
      .forEach(([value, label]) => this.speedSelect.appendChild(option(value, label)));
    this.speedSelect.value = String(this.playbackSpeed);
    this.speedSelect.addEventListener("change", () => {
      this.playbackSpeed = Number(this.speedSelect.value);
      if (this.playbackTimer) this.play();
      this.savePreferences();
    });
    playback.append(this.previousTimeButton, this.playButton, this.nextTimeButton, this.speedSelect);

    this.aggregationSelect = element("select");
    this.aggregationSelect.setAttribute("aria-label", "Time aggregation");
    [["frame", "Original frames"], ["day", "Daily mean"], ["month", "Monthly mean"]]
      .forEach(([value, label]) => this.aggregationSelect.appendChild(option(value, label)));
    this.aggregationSelect.addEventListener("change", () => {
      this.setAggregation(this.aggregationSelect.value).catch((error) => this.reportResultError(error));
    });

    this.timeJumpInput = element("input");
    this.timeJumpInput.type = "search";
    this.timeJumpInput.placeholder = "Jump to time";
    this.timeJumpInput.setAttribute("aria-label", "Jump to result time");
    this.timeJumpInput.addEventListener("keydown", (event) => {
      if (event.key === "Enter") this.jumpToTime(this.timeJumpInput.value);
    });

    this.timeField = element("div", "eplusr-viewer__field eplusr-viewer__time");
    this.timeLabel = element("span", "", "Time");
    this.timeSlider = element("input");
    this.timeSlider.type = "range";
    this.timeSlider.min = "0";
    this.timeSlider.step = "1";
    this.timeSlider.setAttribute("aria-label", "Result time frame");
    this.timeSlider.addEventListener("input", () => {
      this.setTime(Number(this.timeSlider.value)).catch((error) => this.reportResultError(error));
    });
    this.frameCounter = element("span", "eplusr-viewer__frame-count");
    const timelineHeading = element("div", "eplusr-viewer__timeline-heading");
    timelineHeading.append(variableField, this.timeLabel, this.frameCounter);
    this.timeField.append(playback, this.timeSlider, this.speedSelect);
    this.timeline.append(timelineHeading, this.timeField);
    this.resultStatus = element("div", "eplusr-viewer__status");
    this.resultStatus.setAttribute("role", "status");
    this.resultStatus.setAttribute("aria-live", "polite");
    const aggregationField = element("label", "eplusr-viewer__field");
    aggregationField.append(element("span", "", "Time aggregation"), this.aggregationSelect);
    const jumpField = element("label", "eplusr-viewer__field");
    jumpField.append(element("span", "", "Jump to time"), this.timeJumpInput);
    resultPanel.body.append(paletteField, rangeField, missingField, aggregationField, jumpField);
    this.resultEmpty = element("p", "eplusr-viewer__status", "Attach existing results from R to color surfaces and browse time frames.");
    this.tabBodies.results.appendChild(this.resultEmpty);
    this.timeline.appendChild(this.resultStatus);

    this.queryPanel = new ResultQueryPanel(this);
    const diagnosticPanel = this.panelSection("Geometry diagnostics", false, "eplusr-viewer__diagnostics");
    this.diagnosticSection = diagnosticPanel.details;
    this.diagnosticEmpty = element("p", "eplusr-viewer__status", "Geometry diagnostics are not attached to this widget.");
    diagnosticPanel.body.append(this.diagnosticEmpty);
    const showDiagnosticLabel = element("label");
    this.diagnosticToggle = element("input");
    this.diagnosticToggle.type = "checkbox";
    this.diagnosticToggle.addEventListener("change", () => {
      this.showDiagnostics = this.diagnosticToggle.checked;
      this.updateHelpers();
    });
    showDiagnosticLabel.append(this.diagnosticToggle, document.createTextNode(" Show nearby vertex candidates"));
    const onlyDiagnosticLabel = element("label");
    this.diagnosticOnlyToggle = element("input");
    this.diagnosticOnlyToggle.type = "checkbox";
    this.diagnosticOnlyToggle.addEventListener("change", () => {
      this.diagnosticOnly = this.diagnosticOnlyToggle.checked;
      this.applyVisibilityChange({ fit: true });
    });
    onlyDiagnosticLabel.append(this.diagnosticOnlyToggle, document.createTextNode(" Only show affected objects"));
    this.diagnosticTypeSelect = element("select");
    this.diagnosticTypeSelect.setAttribute("aria-label", "Diagnostic issue type");
    this.diagnosticTypeSelect.addEventListener("change", () => this.renderDiagnostics());
    this.diagnosticSummary = element("div", "eplusr-viewer__diagnostic-summary");
    this.diagnosticList = element("div", "eplusr-viewer__diagnostic-list");
    diagnosticPanel.body.append(showDiagnosticLabel, onlyDiagnosticLabel, this.diagnosticTypeSelect, this.diagnosticSummary, this.diagnosticList);

    const selectionSection = this.panelSection("Selection", true, "", "properties");
    this.selectionActions = element("div", "eplusr-viewer__button-row eplusr-viewer__selection-actions");
    const isolate = element("button", "", "Isolate");
    isolate.type = "button";
    isolate.addEventListener("click", () => this.isolateSelection());
    const hide = element("button", "", "Hide");
    hide.type = "button";
    hide.addEventListener("click", () => this.hideSelection());
    const clear = element("button", "", "Clear selection");
    clear.type = "button";
    clear.addEventListener("click", () => this.clearSelection());
    const exitIsolation = element("button", "", "Exit isolation");
    exitIsolation.type = "button";
    exitIsolation.addEventListener("click", () => this.exitIsolation());
    this.selectionActionButtons = { isolate, hide, clear, exitIsolation };
    this.selectionActions.append(isolate, hide, clear, exitIsolation);
    this.isolationStatus = element("div", "eplusr-viewer__isolation-status", "Model");
    this.isolationStatus.setAttribute("role", "status");
    this.isolationStatus.setAttribute("aria-live", "polite");
    this.selectionPanel = element("div", "eplusr-viewer__selection", "Click a surface to inspect its properties; double-click to select its Zone, then double-click again to select a surface.");
    this.selectionPanel.setAttribute("role", "status");
    this.selectionPanel.setAttribute("aria-live", "polite");
    this.measureStatus = element("div", "eplusr-viewer__measure-status", "Measurement is off.");
    selectionSection.body.append(this.isolationStatus, this.selectionActions, this.selectionPanel);
    this.buildViewportFeedback();
    this.navigation = new ViewNavigation(this);
    this.ribbon = new ViewerRibbon(this, {
      search: searchSection.details, components: componentSection.details,
      saved: savedViewSection.details, clipping: clippingSection.details,
      diagnostics: diagnosticPanel.details, selection: selectionSection.details,
      results: resultPanel.details
    }, { reset, capture });
    if (this.short) this.ribbon.collapse(true);

    // Observe the actual widget rather than the browser viewport (for RStudio and paired widgets).
    if (window.ResizeObserver) {
      this.layoutObserver = new ResizeObserver(() => this.resize());
      this.layoutObserver.observe(this.root);
      this.layoutObserver.observe(this.canvasHost);
      this.layoutObserver.observe(this.legend);
    }
  }

  // Collect related controls before the ribbon places them in command groups and task editors.
  buildTaskContainers() {
    this.tabBodies = Object.fromEntries(["objects", "display", "results", "properties"].map(name => [name, element("div")]));
  }

  // Open a requested inspection task without reserving a permanent canvas column.
  setPanelTab(name) {
    this.ribbon?.open(name);
  }

  // Group related toolbar controls so the layout can wrap by task instead of by individual button.
  toolbarGroup(label) {
    const group = element("div", "eplusr-viewer__toolbar-group");
    group.setAttribute("role", "group");
    group.setAttribute("aria-label", label);
    this.toolbar.appendChild(group);
    return group;
  }

  // Build a collapsible panel section while keeping its body available to plain DOM callers.
  panelSection(title, open = true, className = "", tab = "objects") {
    const details = element("details", `eplusr-viewer__section ${className}`.trim());
    details.open = open;
    details.appendChild(element("summary", "", title));
    const body = element("div", "eplusr-viewer__section-body");
    details.appendChild(body);
    this.tabBodies[tab].appendChild(details);
    return { details, body };
  }

  // Create an accessible pressed-state button inside the requested toolbar group.
  toggleButton(label, initial, callback, parent = this.toolbar) {
    const button = element("button", "", label);
    button.type = "button";
    button.setAttribute("aria-pressed", String(initial));
    button.addEventListener("click", () => {
      const next = button.getAttribute("aria-pressed") !== "true";
      button.setAttribute("aria-pressed", String(next));
      callback(next);
    });
    parent.appendChild(button);
    return button;
  }

  // Create a visible label around a native form control and append it to the requested panel body.
  field(label, control, parent = this.panel) {
    const wrapper = element("label", "eplusr-viewer__field");
    wrapper.appendChild(element("span", "", label));
    const input = element(control);
    input.setAttribute("aria-label", label);
    wrapper.appendChild(input);
    parent.appendChild(wrapper);
    return input;
  }

  // Decode immutable scene buffers once; later filtering only rebuilds the visible view.
  load(payload) {
    this.pause();
    if (this.resultAbortController) this.resultAbortController.abort();
    if (this.northArrow) {
      this.scene.remove(this.northArrow);
      this.northArrow = null;
    }
    if (this.northLabel) {
      this.scene.remove(this.northLabel);
      this.northLabel.material.map.dispose();
      this.northLabel.material.dispose();
      this.northLabel = null;
    }
    this.payload = payload;
    this.zoneFilter = "";
    this.spaceFilter = "";
    this.remoteFilters = {};
    this.objects = payload.objects ?? [];
    this.positions = float32(payload.geometry?.positions);
    this.faceObjects = int32(payload.geometry?.face_objects);
    this.edgePositions = float32(payload.geometry?.edge_positions);
    this.edgeObjects = int32(payload.geometry?.edge_objects);
    this.pointPositions = float32(payload.geometry?.point_positions);
    this.sharedBoundaryPairs = findSharedBoundaries(this.objects, this.positions, this.faceObjects, this.edgePositions, this.edgeObjects);
    this.glazingObjects = new Set(this.objects.flatMap((object,index) => isGlazing(object) ? [index] : []));
    const namedObjects = new Map(this.objects.map((object,index) => [object.name?.toLowerCase(), index]));
    this.windowParents = new Map([...this.glazingObjects].map(index => [index, namedObjects.get(this.objects[index].building_surface_name?.toLowerCase())]));
    this.computeObjectMetrics();
    this.setResults(payload.results, false);
    this.diagnostics = {
      ...(payload.diagnostics ?? {}),
      pointPositions: float32(payload.diagnostics?.point_positions),
      objectIndices: int32(payload.diagnostics?.object_indices),
      issues: payload.diagnostics?.issues ?? []
    };
    this.diagnosticObjectSet = new Set(this.diagnostics.objectIndices ?? []);
    this.showDiagnostics = Boolean(this.diagnostics.enabled && this.diagnostics.count);
    this.diagnosticOnly = false;
    this.renderBy = payload.options?.render_by ?? "surface_type";
    this.xray = Boolean(payload.options?.x_ray);
    this.showWireframe = payload.options?.wireframe !== false;
    this.showAxis = payload.options?.axis !== false;
    this.showGround = Boolean(payload.options?.ground);
    this.scene.background = new THREE.Color(payload.options?.background ?? "#f3f5f7");
    this.selected.clear();
    this.hidden.clear();
    this.isolated = null;
    this.isolationHistory = [];
    this.measurePoints = [];
    this.measurement = null;

    this.renderSelect.value = this.renderBy;
    if (this.resultSet.length && !Array.from(this.renderSelect.options).some((item) => item.value === "result")) {
      this.renderSelect.appendChild(option("result", "Results"));
    }
    this.renderSelect.value = this.renderBy;
    this.wireButton.setAttribute("aria-pressed", String(this.showWireframe));
    this.xrayButton.setAttribute("aria-pressed", String(this.xray));
    this.axisButton.setAttribute("aria-pressed", String(this.showAxis));
    this.groundButton.setAttribute("aria-pressed", String(this.showGround));
    this.populateFilters();
    this.loadPreferences();
    this.rebuildGeometry();
    this.configureResultControls();
    this.configureClipRange();
    const viewpoint = payload.options?.viewpoint ?? {};
    if (Number.isFinite(viewpoint.fov)) {
      this.perspectiveCamera.fov = viewpoint.fov;
      this.perspectiveCamera.updateProjectionMatrix();
    }
    this.viewSelect.value = viewpoint.look_at ?? "iso";
    this.setView(this.viewSelect.value);
    this.fitView();
    if (this.results?.mode === "provider") {
      this.setTime(0).catch((error) => this.reportResultError(error));
    }
    this.updateSelectionPanel();
    this.clearMeasurement();
    this.toggleMeasureMode(false);
    this.renderDiagnostics();
    this.ribbon.browser.build();
    this.resize();
  }

  // Replace numerical result buffers while preserving geometry, camera and selection.
  setResults(payload, update = true) {
    this.queryPanel.configure(payload?.query ?? null);
    this.pause();
    this.resultAbortController?.abort();
    const variablePayloads = payload?.variables ?? (payload ? [payload] : []);
    this.resultSet = variablePayloads.map((result) => {
      const values = float32(result.values);
      const times = Array.isArray(result.times) ? result.times : result.times == null ? [] : [result.times];
      return {
        ...result,
        times,
        values,
        sourceValues: values.slice(),
        sourceTimes: [...times],
        sourceRange: [...(result.range ?? finiteRange(values))],
        rangeMode: "all", coverage: 1, manualRange: null, loadedFrame: null,
        aggregation: "frame",
        frameGroups: times.map((_, index) => [index]),
        cache: new Map(),
        missingColor: "#b8bdc5"
      };
    });
    this.resultVariableIndex = Math.max(0, Math.min(
      Number(payload?.active ?? 0), Math.max(0, this.resultSet.length - 1)
    ));
    this.results = this.resultSet[this.resultVariableIndex] ?? null;
    if (!update) return;
    this.frame = 0;
    if (this.resultSet.length && !Array.from(this.renderSelect.options).some((item) => item.value === "result")) {
      this.renderSelect.appendChild(option("result", "Results"));
    }
    this.renderBy = this.resultSet.length ? "result" : "surface_type";
    this.renderSelect.value = this.renderBy;
    this.configureResultControls();
    this.updateColors();
    if (this.results?.mode === "provider") return this.setTime(0);
  }

  // Report semantic IDs and acknowledged UI state without serializing any mesh buffers.
  readState() {
    return {
      selected_ids: [...this.selected].map((index) => String(this.objects[index].id)),
      visible_ids: [...this.visibleObjects].map((index) => String(this.objects[index].id)),
      render_by: this.renderBy, selection_mode: this.selectionMode,
      zone: this.zoneFilter, space: this.spaceFilter, filters: this.effectiveFilters(),
      frame: this.frame, variable: this.resultVariableIndex,
      x_ray: this.xray, wireframe: this.showWireframe, window_style: this.windowStyle,
      axis: this.showAxis, ground: this.showGround,
      timeline_visible: !this.timeline.hidden, model_only: Boolean(this.ribbon?.clean),
      ribbon_tab: this.ribbon?.active ?? null,
      task_panel: this.ribbon?.activeTask ?? null, model_browser_visible: Boolean(this.ribbon?.browserVisible),
      tool: this.measureMode ? "measure" : "inspect",
      measurement: this.measurement?.distance ?? null,
      projection: this.camera === this.orthographicCamera ? "orthographic" : "perspective",
      view: this.viewSelect.value,
      result_query: this.queryPanel.applied ?? null,
      range_mode: this.results?.rangeMode ?? null, result_range: this.results?.range ?? null,
      camera: { position: this.camera.position.toArray(), target: this.controls.target.toArray() }
    };
  }

  // Expand a surface pick to its Space or Zone when that selection mode is active.
  selectionGroup(index) {
    const field = this.selectionMode === "zone" ? "zone_name" : this.selectionMode === "space" ? "space_name" : null;
    const value = field ? this.objects[index]?.[field] : null;
    if (!field || !value) return [index];
    return this.objects.flatMap((object, candidate) =>
      object[field] === value && this.isObjectVisible(candidate) ? [candidate] : []);
  }

  // Precompute object, floor, and Zone centers used by area summaries and explode transforms.
  computeObjectMetrics() {
    const sums = this.objects.map(() => new THREE.Vector3());
    const counts = new Uint32Array(this.objects.length);
    for (let face = 0; face < this.faceObjects.length; face += 1) {
      const objectIndex = this.faceObjects[face];
      const offset = face * 9;
      for (let vertex = 0; vertex < 3; vertex += 1) {
        sums[objectIndex].add(new THREE.Vector3(
          this.positions[offset + vertex * 3],
          this.positions[offset + vertex * 3 + 1],
          this.positions[offset + vertex * 3 + 2]
        ));
        counts[objectIndex] += 1;
      }
    }
    this.objectCenters = sums.map((sum, index) => counts[index] ? sum.multiplyScalar(1 / counts[index]) : new THREE.Vector3());
    this.modelCenter = this.objectCenters.reduce((sum, center, index) => counts[index] ? sum.add(center) : sum, new THREE.Vector3());
    const populated = counts.reduce((sum, count) => sum + (count ? 1 : 0), 0);
    if (populated) this.modelCenter.multiplyScalar(1 / populated);

    const levels = [...new Set(this.objectCenters.filter((_, index) => counts[index]).map((center) => Math.round(center.z * 100) / 100))].sort((a, b) => a - b);
    this.floorOffsets = this.objectCenters.map((center) => levels.indexOf(Math.round(center.z * 100) / 100) - (levels.length - 1) / 2);

    const zoneSums = new Map();
    const zoneCounts = new Map();
    this.objects.forEach((object, index) => {
      const zone = object.zone_name;
      if (!zone || !counts[index]) return;
      if (!zoneSums.has(zone)) zoneSums.set(zone, new THREE.Vector3());
      zoneSums.get(zone).add(this.objectCenters[index]);
      zoneCounts.set(zone, (zoneCounts.get(zone) ?? 0) + 1);
    });
    this.zoneDirections = new Map();
    zoneSums.forEach((sum, zone) => {
      const center = sum.multiplyScalar(1 / zoneCounts.get(zone));
      const direction = center.clone().sub(this.modelCenter);
      if (direction.lengthSq() < 1e-10) direction.set(1, 0, 0);
      direction.z = 0;
      this.zoneDirections.set(zone, direction.normalize());
    });
    this.modelRadius = Math.max(1, this.visibleBoxFromSource().getSize(new THREE.Vector3()).length() / 2);
  }

  // Return the untransformed model bounds for scale-dependent helper calculations.
  visibleBoxFromSource() {
    const box = new THREE.Box3();
    for (let index = 0; index < this.positions.length; index += 3) {
      box.expandByPoint(new THREE.Vector3(this.positions[index], this.positions[index + 1], this.positions[index + 2]));
    }
    if (box.isEmpty()) box.expandByPoint(new THREE.Vector3()).expandByScalar(1);
    return box;
  }

  // Compute the per-object translation used by floor and Zone explode modes.
  objectOffset(objectIndex) {
    if (!this.explodeAmount || this.explodeMode === "none") return new THREE.Vector3();
    const distance = this.modelRadius * 0.16 * this.explodeAmount;
    if (this.explodeMode === "floor") {
      return new THREE.Vector3(0, 0, (this.floorOffsets[objectIndex] ?? 0) * distance);
    }
    if (this.explodeMode === "zone") {
      return (this.zoneDirections.get(this.objects[objectIndex]?.zone_name) ?? new THREE.Vector3()).clone().multiplyScalar(distance);
    }
    return new THREE.Vector3();
  }

  // Apply explode mode changes to all visible buffers without changing object identifiers.
  setExplode(mode, amount) {
    // A changed display transform invalidates dimensions picked in the previous transform.
    this.clearMeasurement();
    this.explodeMode = mode;
    this.explodeAmount = Number.isFinite(amount) ? amount : 0;
    this.rebuildGeometry();
    this.configureClipRange();
    this.fitView();
  }

  // Populate all filter and search choices from the current model metadata.
  populateFilters() {
    const unique = (field) => [...new Set(this.objects.map((object) => object[field]).filter(Boolean))].sort();
    this.zoneSelect.replaceChildren(option("", "All zones"));
    unique("zone_name").forEach((value) => this.zoneSelect.appendChild(option(value)));
    this.populateSpaceFilter();
    const suggestions = [...new Set(this.objects.map(object => object.name).filter(Boolean))].sort();
    this.searchList.replaceChildren(...suggestions.slice(0, 5000).map((value) => option(value)));
    this.updateNameFilterStatus();
  }

  // Restrict Space choices to the selected Zone while preserving valid current selections.
  populateSpaceFilter() {
    const values = [...new Set(this.objects
      .filter((object) => this.remoteFilters.zone
        ? this.remoteFilters.zone.includes(String(object.zone_name ?? "").toLowerCase())
        : !this.zoneFilter || object.zone_name === this.zoneFilter)
      .map((object) => object.space_name)
      .filter(Boolean))].sort();
    if (this.spaceFilter && !values.includes(this.spaceFilter)) this.spaceFilter = "";
    this.spaceSelect.replaceChildren(option("", "All spaces"), ...values.map((value) => option(value)));
    this.spaceSelect.value = this.spaceFilter;
  }

  // Return the same normalized filter criteria for the UI, R state, and visibility rules.
  effectiveFilters() {
    return {
      ...this.remoteFilters,
      name: this.remoteFilters.name ?? (this.nameFilter.trim() ? [this.nameFilter.trim().toLowerCase()] : null),
      zone: this.remoteFilters.zone ?? (this.zoneFilter ? [this.zoneFilter.toLowerCase()] : null),
      space: this.remoteFilters.space ?? (this.spaceFilter ? [this.spaceFilter.toLowerCase()] : null),
      type: COMPONENTS.every((name) => this.componentVisibility[name]) ? null
        : COMPONENTS.filter((name) => this.componentVisibility[name])
    };
  }

  // Reconcile native controls and removable chips after either an R command or a browser edit.
  syncFilterUi() {
    if (!this.filterChips) return;
    const filters = this.effectiveFilters();
    const labelsFor = (key, values) => {
      const field = { zone: "zone_name", space: "space_name", surface: "name" }[key];
      return values.map((value) => this.objects.find((object) =>
        String(object[field] ?? "").toLowerCase() === value)?.[field] ?? value);
    };
    ["zone", "space"].forEach((key) => {
      const select = key === "zone" ? this.zoneSelect : this.spaceSelect;
      select.querySelectorAll('[data-filter-summary]').forEach((node) => node.remove());
      const values = filters[key];
      if (values?.length === 1) {
        const label = labelsFor(key, values)[0];
        if (![...select.options].some((item) => item.value === label)) select.appendChild(option(label));
        select.value = label;
      } else if (values) {
        const summary = option("__multiple__", values.length + " " + key + "s selected");
        summary.disabled = true;
        summary.dataset.filterSummary = "true";
        select.appendChild(summary);
        select.value = summary.value;
      } else select.value = "";
    });
    COMPONENTS.forEach((name) => { this.componentInputs[name].checked = this.componentVisibility[name]; });
    this.filterChips.replaceChildren();
    const chip = (text, clear) => {
      const button = element("button", "eplusr-viewer__filter-chip", text + " ×");
      button.type = "button";
      button.title = text;
      button.setAttribute("aria-label", "Remove filter: " + text);
      button.replaceChildren(element("span", "", text), icon("clear"));
      button.addEventListener("click", clear);
      this.filterChips.appendChild(button);
    };
    ["zone", "space", "surface"].forEach((key) => {
      const values = filters[key];
      if (!values) return;
      chip(key[0].toUpperCase() + key.slice(1) + ": " + (labelsFor(key, values).join(", ") || "None"), () => {
        this.remoteFilters[key] = null;
        if (key === "zone") this.zoneFilter = "";
        if (key === "space") this.spaceFilter = "";
        this.populateSpaceFilter();
        this.applyVisibilityChange({ fit: true });
      });
    });
    if (filters.name) chip("Name contains: " + (this.nameFilter || filters.name.join(", ")), () => this.setNameFilter(""));
    const name = this.remoteFilters.name?.[0] ?? this.nameFilter;
    if (this.nameFilterInput.value !== name) this.nameFilterInput.value = name;
    this.clearNameFilterButton.hidden = !filters.name;
    this.updateNameFilterStatus();
    if (filters.type) chip("Types: " + (filters.type.join(", ") || "None"), () => {
      this.remoteFilters.type = null;
      COMPONENTS.forEach((name) => { this.componentVisibility[name] = true; });
      this.applyVisibilityChange({ fit: true });
    });
    if (this.hidden.size) chip(this.hidden.size + " hidden", () => {
      this.hidden.clear(); this.applyVisibilityChange({ fit: true });
    });
    if (this.isolated) chip("Isolated: " + this.isolated.size, () => {
      this.isolated = null; this.isolationHistory = []; this.applyVisibilityChange({ fit: true });
    });
    if (this.diagnosticOnly) chip("Affected objects", () => {
      this.diagnosticOnly = false; this.diagnosticOnlyToggle.checked = false; this.applyVisibilityChange({ fit: true });
    });
    this.visibleCount.replaceChildren(icon("filters"), document.createTextNode(this.visibleObjects.size + " / " + this.objects.length + " visible"));
    buttonIcon(this.selectionCount, "isolate", this.selected.size + " selected");
    this.selectionCount.disabled = !this.selected.size;
    this.clearFiltersButton.hidden = !this.filterChips.children.length;
    this.ribbon?.browser.sync();
  }

  // Clear visibility criteria together while retaining the still-visible selected objects.
  clearFilters() {
    this.remoteFilters = {};
    this.nameFilter = "";
    this.zoneFilter = "";
    this.spaceFilter = "";
    this.hidden.clear();
    this.isolated = null;
    this.isolationHistory = [];
    this.diagnosticOnly = false;
    this.diagnosticOnlyToggle.checked = false;
    COMPONENTS.forEach((name) => { this.componentVisibility[name] = true; });
    this.populateSpaceFilter();
    this.applyVisibilityChange({ fit: true });
  }

  // Apply a Zone filter, cascade the Space choices, and reconcile selection with visibility.
  setZoneFilter(value) {
    this.remoteFilters.zone = null;
    this.remoteFilters.space = null;
    this.zoneFilter = value;
    this.populateSpaceFilter();
    this.applyVisibilityChange({ fit: true });
  }

  // Apply a Space filter and infer its Zone when the space belongs to exactly one zone.
  setSpaceFilter(value) {
    this.remoteFilters.space = null;
    this.spaceFilter = value;
    if (value) {
      const zones = [...new Set(this.objects.filter((object) => object.space_name === value).map((object) => object.zone_name).filter(Boolean))];
      if (zones.length === 1 && this.zoneFilter !== zones[0]) {
        this.remoteFilters.zone = null;
        this.zoneFilter = zones[0];
        this.zoneSelect.value = zones[0];
        this.populateSpaceFilter();
        this.spaceSelect.value = value;
      }
    }
    this.applyVisibilityChange({ fit: true });
  }

  // Rebuild visible buffers once and remove selections that can no longer be seen.
  applyVisibilityChange({ fit = false, preserveSelection = false } = {}) {
    this.rebuildGeometry();
    const previousCount = this.selected.size;
    if (!preserveSelection) {
      this.selected = new Set([...this.selected].filter((index) => this.isObjectVisible(index)));
    }
    if (fit) this.fitView();
    // Filtering may prune the selected IDs after rebuilding geometry. Repaint the
    // legend from the reconciled selection so its statistics cannot retain removed objects.
    this.updateColors();
    if (this.selected.size !== previousCount) this.emitSelection();
    else this.updateSelectionPanel();
  }

  // Update only the name criterion; keep the camera, remaining criteria and valid selection intact.
  setNameFilter(value) {
    this.nameFilter = value;
    this.remoteFilters.name = null;
    this.applyVisibilityChange();
    this.root.dispatchEvent(new CustomEvent("eplusr-view"));
  }

  // Distinguish model-wide name matches from the objects allowed by all active criteria.
  updateNameFilterStatus() {
    const queries = this.effectiveFilters().name;
    this.nameFilterStatus.hidden = !queries;
    if (!queries) return;
    const matches = this.objects.flatMap((object, index) => matchesNameFilter(object, queries) ? [index] : []);
    const visible = matches.filter(index => this.isObjectVisible(index));
    const reasons = [...new Set(matches.flatMap(index => this.visibilityReasons(index)))];
    this.nameFilterStatus.textContent = !matches.length ? "No object names match."
      : visible.length + " of " + matches.length + " name matches visible" +
        (visible.length < matches.length ? " · " + reasons.join(" · ") : "");
    this.nameFilterStatus.dataset.state = visible.length ? "matches" : "excluded";
  }

  // Keep the existing R panel command as an alias for the transient property inspector.
  togglePanel(forceHidden = null) {
    const open = this.ribbon.activeTask === "properties";
    if (forceHidden === true || (forceHidden === null && open)) this.ribbon.closeEditors();
    else if (!open) this.ribbon.open("properties");
  }

  // Update configure result controls for this viewer instance.
  configureResultControls() {
    this.rangeMinimum.removeAttribute("aria-invalid");
    this.rangeMaximum.removeAttribute("aria-invalid");
    this.rangeStatus.textContent = "";
    this.resultSection.style.display = this.resultSet.length ? "block" : "none";
    if (this.ribbon) this.ribbon.syncResults();
    else this.timeline.hidden = true;
    this.resultEmpty.hidden = Boolean(this.resultSet.length);
    this.variableSelect.replaceChildren();
    this.resultSet.forEach((result, index) => {
      this.variableSelect.appendChild(option(index, (result.label ?? `Variable ${index + 1}`) + (result.units ? ` (${displayUnit(result.units)})` : "")));
    });
    this.variableSelect.value = String(this.resultVariableIndex);
    if (this.timelineVariableLabel) this.timelineVariableLabel.textContent = this.variableSelect.selectedOptions[0]?.textContent ?? "Results";
    const frames = this.results?.times?.length ?? 0;
    this.timeField.style.display = "flex";
    this.timeSlider.disabled = frames < 2;
    this.frameCounter.textContent = frames ? `${this.frame + 1} / ${frames}` : "";
    this.timeSlider.max = String(Math.max(0, frames - 1));
    this.timeSlider.value = String(Math.min(this.frame, Math.max(0, frames - 1)));
    if (frames) this.timeLabel.textContent = timeCaption(this.results, this.frame);
    if (this.results) {
      this.paletteSelect.value = this.results.palette ?? "viridis";
      this.missingColorInput.value = this.results.missingColor ?? "#b8bdc5";
      this.aggregationSelect.value = this.results.aggregation ?? "frame";
      const range = this.results.range ?? finiteRange(this.results.values);
      this.rangeMinimum.value = String(range[0]);
      this.rangeMaximum.value = String(range[1]);
    }
    this.previousTimeButton.disabled = frames < 2;
    this.nextTimeButton.disabled = frames < 2;
    this.playButton.disabled = frames < 2;
    this.timeJumpInput.value = frames ? String(this.results.times[this.frame]) : "";
    this.resultStatus.textContent = this.results ? "" : "No simulation results attached.";
    this.speedSelect.disabled = frames < 2;
    this.syncRangeControls();
    const diagnosticCount = Number(this.diagnostics?.count ?? 0);
    this.diagnosticSection.style.display = "block";
    this.diagnosticEmpty.hidden = Boolean(this.diagnostics?.enabled);
    this.diagnosticToggle.disabled = !this.diagnostics?.enabled || diagnosticCount === 0;
    this.diagnosticTypeSelect.disabled = !this.diagnostics?.enabled || diagnosticCount === 0;
    this.diagnosticToggle.checked = this.showDiagnostics;
    this.diagnosticOnlyToggle.checked = this.diagnosticOnly;
    this.diagnosticOnlyToggle.disabled = diagnosticCount === 0;
    this.renderDiagnostics();
  }

  // Update configure clip range for this viewer instance.
  configureClipRange() {
    const box = this.visibleBox();
    const minimum = Number.isFinite(box.min[this.clipAxis]) ? box.min[this.clipAxis] : 0;
    const maximum = Number.isFinite(box.max[this.clipAxis]) ? box.max[this.clipAxis] : 1;
    this.clipSlider.min = String(minimum);
    this.clipSlider.max = String(maximum);
    this.clipSlider.value = String(maximum);
    this.clipValue.textContent = formatNumber(maximum);
    this.updateClipping();
  }

  // Normalize binary adjacency candidates and structured diagnostics into one issue list.
  diagnosticIssues() {
    if (Array.isArray(this.diagnostics?.issues) && this.diagnostics.issues.length) return this.diagnostics.issues;
    const issues = [];
    const pairs = this.diagnostics?.objectIndices ?? new Int32Array();
    const points = this.diagnostics?.pointPositions ?? new Float32Array();
    for (let index = 0; index + 1 < pairs.length; index += 2) {
      const pointIndex = index / 2 * 3;
      issues.push({
        type: "near_vertex",
        severity: "warning",
        label: "Nearby vertices may indicate an adjacency gap",
        object_indices: [pairs[index], pairs[index + 1]],
        point: Array.from(points.slice(pointIndex, pointIndex + 3))
      });
    }
    return issues;
  }

  // Render diagnostic filters and an actionable issue list linked back to model objects.
  renderDiagnostics() {
    if (!this.diagnosticTypeSelect) return;
    const issues = this.diagnosticIssues();
    const previous = this.diagnosticTypeSelect.value;
    const types = [...new Set(issues.map((issue) => issue.type))].sort();
    const labels = {
      near_vertex: "Adjacency gaps",
      nonplanar: "Non-planar surfaces",
      duplicate: "Duplicate surfaces",
      overlap: "Coplanar overlaps",
      reversed_normal: "Reversed normals",
      subsurface_outside: "Subsurfaces outside parents",
      self_intersection: "Self-intersections"
    };
    this.diagnosticTypeSelect.replaceChildren(option("", "All issue types"), ...types.map((type) => option(type, labels[type] ?? type)));
    if (types.includes(previous)) this.diagnosticTypeSelect.value = previous;
    const selectedType = this.diagnosticTypeSelect.value;
    const filtered = selectedType ? issues.filter((issue) => issue.type === selectedType) : issues;
    this.diagnosticObjectSet = new Set(filtered.flatMap((issue) => issue.object_indices ?? []));
    this.diagnosticSummary.hidden = !this.diagnostics?.enabled;
    this.diagnosticSummary.textContent = issues.length
      ? `${filtered.length} of ${issues.length} issues shown (tolerance ${this.diagnostics.tolerance} m).`
      : "No geometry issues found.";
    this.diagnosticList.replaceChildren();
    filtered.slice(0, 200).forEach((issue, index) => {
      const button = element("button", `eplusr-viewer__diagnostic-item eplusr-viewer__diagnostic-item--${issue.severity ?? "info"}`);
      button.type = "button";
      button.append(
        element("span", "eplusr-viewer__diagnostic-type", labels[issue.type] ?? issue.type),
        element("span", "", issue.label ?? "Geometry issue")
      );
      button.addEventListener("click", () => this.selectDiagnosticIssue(filtered[index]));
      this.diagnosticList.appendChild(button);
    });
    if (filtered.length > 200) this.diagnosticList.appendChild(element("div", "eplusr-viewer__status", `${filtered.length - 200} additional issues are not listed.`));
    this.diagnosticOnlyToggle.disabled = filtered.length === 0;
    this.rebuildDiagnostics();
    if (this.diagnosticOnly) this.applyVisibilityChange({ fit: true });
  }

  // Reveal and focus every object associated with a selected geometry diagnostic.
  selectDiagnosticIssue(issue) {
    const indices = (issue?.object_indices ?? []).filter((index) => this.objects[index]);
    if (!indices.length) return;
    this.remoteFilters = {};
    this.zoneFilter = "";
    this.spaceFilter = "";
    this.zoneSelect.value = "";
    this.populateSpaceFilter();
    this.hidden.clear();
    this.isolated = null;
    this.isolationHistory = [];
    this.selected = new Set(indices);
    this.rebuildGeometry();
    this.fitView(indices);
    this.emitSelection();
  }

  // Change the active clipping normal and recompute the plane extent for that axis.
  setClipAxis(axis) {
    this.clipAxis = axis;
    this.configureClipRange();
  }

  // Keep the tree's explanations identical to the geometry and picking visibility rules.
  visibilityReasons(objectIndex) {
    return objectVisibilityReasons(this.objects[objectIndex], objectIndex, {
      filters: this.effectiveFilters(), componentVisibility: this.componentVisibility,
      hidden: this.hidden, isolated: this.isolated, diagnosticOnly: this.diagnosticOnly,
      diagnosticObjectSet: this.diagnosticObjectSet
    });
  }

  // A model object is visible only when every active criterion allows it.
  isObjectVisible(objectIndex) {
    return this.visibilityReasons(objectIndex).length === 0;
  }

  // Rebuild only the visible non-indexed triangles, preserving a direct face-to-ID lookup.
  rebuildGeometry() {
    const offsets = this.objects.map((_, index) => this.objectOffset(index));
    this.currentOffsets = offsets;
    this.visibleFaceObjects = Array.from(this.faceObjects);
    this.visibleObjects = new Set();
    if (!this.mesh || this.mesh.geometry.getAttribute("position").array.length !== this.positions.length) {
      this.initializeGeometryObjects();
    }
    const positionAttribute = this.mesh.geometry.getAttribute("position");
    const visiblePositions = positionAttribute.array;
    for (let face = 0; face < this.faceObjects.length; face += 1) {
      const objectIndex = this.faceObjects[face];
      const offset = face * 9;
      if (!this.isObjectVisible(objectIndex)) {
        visiblePositions.fill(0, offset, offset + 9);
        continue;
      }
      const translation = offsets[objectIndex];
      for (let value = 0; value < 9; value += 1) {
        visiblePositions[offset + value] = this.positions[offset + value] + translation.getComponent(value % 3);
      }
      this.visibleObjects.add(objectIndex);
    }
    positionAttribute.needsUpdate = true;
    this.mesh.geometry.computeVertexNormals();

    const box = this.visibleBox();
    this.mesh.geometry.boundingBox = box;
    this.mesh.geometry.boundingSphere = box.getBoundingSphere(new THREE.Sphere());
    this.rebuildEdges();
    this.rebuildSelectionOutline();
    this.rebuildPoints();
    this.rebuildDiagnostics();
    this.updateColors();
    this.updateMaterials();
    this.updateHelpers();
    this.updateClipping();
    this.syncFilterUi();
    this.requestRender();
  }

  // Partition appearance without moving source vertices or changing face-to-object picking indices.
  updateGeometryGroups() {
    if (!this.mesh) return;
    this.glassIndices = this.windowStyle === "glass" && this.renderBy !== "normal" ? this.glazingObjects : new Set();
    const shared = sharedBoundaryGroups(this.faceObjects, this.sharedBoundaryPairs ?? new Int32Array(), this.visibleObjects, this.currentOffsets ?? [], this.glassIndices);
    this.activeSharedBoundaries = shared.active;
    this.mesh.geometry.clearGroups();
    for (const group of shared.groups) this.mesh.geometry.addGroup(group.start, group.count, group.materialIndex);
  }

  // Allocate stable Three.js objects once; later filtering only mutates typed visibility buffers.
  initializeGeometryObjects() {
    if (this.mesh) {
      this.scene.remove(this.mesh, this.backMesh);
      this.mesh.geometry.dispose();
      this.surfaceMaterial?.dispose();
      this.resultMaterial?.dispose();
      this.backMaterial.dispose();
      this.pairedSurfaceMaterial?.dispose();
      this.pairedResultMaterial?.dispose();
      this.hiddenBackMaterial?.dispose();
      this.glassMaterial?.dispose();
      this.pairedGlassMaterial?.dispose();
    }
    const geometry = new THREE.BufferGeometry();
    const position = new THREE.BufferAttribute(new Float32Array(this.positions.length), 3);
    position.setUsage(THREE.DynamicDrawUsage);
    const color = new THREE.BufferAttribute(new Float32Array(this.positions.length), 3);
    color.setUsage(THREE.DynamicDrawUsage);
    geometry.setAttribute("position", position);
    geometry.setAttribute("color", color);
    this.visibleVertexObjects = new Int32Array(this.faceObjects.length * 3);
    for (let face = 0; face < this.faceObjects.length; face += 1) {
      this.visibleVertexObjects.fill(this.faceObjects[face], face * 3, face * 3 + 3);
    }
    this.surfaceMaterial = new THREE.MeshStandardMaterial({
      vertexColors: true,
      side: THREE.DoubleSide,
      // Separate filled surfaces from their exact boundary lines in depth, without moving vertices.
      polygonOffset: true, polygonOffsetFactor: 1, polygonOffsetUnits: 1,
      roughness: 0.92,
      metalness: 0,
      transparent: this.xray,
      opacity: this.xray ? 0.34 : 1,
      depthWrite: !this.xray
    });
    this.resultMaterial = new THREE.MeshBasicMaterial({
      polygonOffset: true, polygonOffsetFactor: 1, polygonOffsetUnits: 1,
      vertexColors: true, side: THREE.DoubleSide, transparent: this.xray,
      opacity: this.xray ? 0.34 : 1, depthWrite: !this.xray
    });
    this.pairedSurfaceMaterial = this.surfaceMaterial.clone();
    this.pairedSurfaceMaterial.side = THREE.BackSide;
    this.pairedResultMaterial = this.resultMaterial.clone();
    this.pairedResultMaterial.side = THREE.BackSide;
    // Neutral glass remains independent of scientific result colors and never occludes the interior depth.
    this.glassMaterial = new THREE.MeshBasicMaterial({ color: GLASS_COLOR, side: THREE.DoubleSide,
      transparent: true, opacity: GLASS_OPACITY, depthWrite: false,
      polygonOffset: true, polygonOffsetFactor: 1, polygonOffsetUnits: 1 });
    this.pairedGlassMaterial = this.glassMaterial.clone();
    this.pairedGlassMaterial.side = THREE.BackSide;
    this.material = this.surfaceMaterial;
    this.pairedMaterial = this.pairedSurfaceMaterial;
    this.mesh = new THREE.Mesh(geometry, [this.material, this.pairedMaterial, this.glassMaterial, this.pairedGlassMaterial]);
    this.mesh.name = "EnergyPlus surfaces";
    this.backMaterial = new THREE.MeshBasicMaterial({ color: "#d34747", side: THREE.BackSide,
      polygonOffset: true, polygonOffsetFactor: 1, polygonOffsetUnits: 1 });
    // Normals inspection shows the facing normal once on a paired boundary.
    this.hiddenBackMaterial = new THREE.MeshBasicMaterial({ visible: false });
    this.backMesh = new THREE.Mesh(geometry, [this.backMaterial, this.hiddenBackMaterial]);
    this.scene.add(this.mesh, this.backMesh);

    if (this.edges) {
      this.scene.remove(this.edges);
      this.edges.geometry.dispose();
      this.edges.material.dispose();
    }
    const edgeGeometry = new THREE.BufferGeometry();
    const edgePosition = new THREE.BufferAttribute(new Float32Array(this.edgePositions.length), 3);
    edgePosition.setUsage(THREE.DynamicDrawUsage);
    edgeGeometry.setAttribute("position", edgePosition);
    const edgeColor = new THREE.BufferAttribute(new Float32Array(this.edgePositions.length), 3);
    edgeColor.setUsage(THREE.DynamicDrawUsage);
    edgeGeometry.setAttribute("color", edgeColor);
    this.edges = new THREE.LineSegments(edgeGeometry, new THREE.LineBasicMaterial({ vertexColors: true, color: "#ffffff" }));
    this.scene.add(this.edges);
  }

  // Update rebuild edges for this viewer instance.
  rebuildEdges() {
    const attribute = this.edges.geometry.getAttribute("position");
    const positions = attribute.array;
    for (let segment = 0; segment < this.edgeObjects.length; segment += 1) {
      const objectIndex = this.edgeObjects[segment];
      const offset = segment * 6;
      if (!this.isObjectVisible(objectIndex)) {
        positions.fill(0, offset, offset + 6);
        continue;
      }
      const translation = this.currentOffsets?.[objectIndex] ?? new THREE.Vector3();
      for (let value = 0; value < 6; value += 1) {
        positions[offset + value] = this.edgePositions[offset + value] + translation.getComponent(value % 3);
      }
    }
    attribute.needsUpdate = true;
    this.requestRender();
  }

  // Draw selected polygon boundaries above occluding geometry so search and multi-selection stay visible.
  rebuildSelectionOutline() {
    const positions = [];
    for (let segment = 0; segment < this.edgeObjects.length; segment += 1) {
      const objectIndex = this.edgeObjects[segment];
      if (!this.selected.has(objectIndex) || !this.isObjectVisible(objectIndex)) continue;
      const offset = segment * 6;
      const translation = this.currentOffsets?.[objectIndex] ?? new THREE.Vector3();
      for (let value = 0; value < 6; value += 1) {
        positions.push(this.edgePositions[offset + value] + translation.getComponent(value % 3));
      }
    }
    if (this.selectionOutline) {
      this.scene.remove(this.selectionOutline);
      this.selectionOutline.geometry.dispose();
      this.selectionOutline.material.dispose();
    }
    const geometry = new THREE.BufferGeometry();
    geometry.setAttribute("position", new THREE.Float32BufferAttribute(positions, 3));
    this.selectionOutline = new THREE.LineSegments(
      geometry,
      new THREE.LineBasicMaterial({ color: "#ff8a00", depthTest: false, transparent: true, opacity: 1 })
    );
    this.selectionOutline.renderOrder = 20;
    this.scene.add(this.selectionOutline);
    this.requestRender();
  }

  // Update rebuild points for this viewer instance.
  rebuildPoints() {
    if (!this.points) {
      const geometry = new THREE.BufferGeometry();
      geometry.setAttribute("position", new THREE.BufferAttribute(this.pointPositions, 3));
      this.points = new THREE.Points(geometry, new THREE.PointsMaterial({ color: "#e63946", size: 7, sizeAttenuation: false }));
      this.scene.add(this.points);
    }
    this.points.visible = this.componentVisibility.daylighting;
    this.requestRender();
  }

  // Render geometry-quality candidates as a separate overlay that never changes picking IDs.
  rebuildDiagnostics() {
    if (this.diagnosticPoints) {
      this.scene.remove(this.diagnosticPoints);
      this.diagnosticPoints.geometry.dispose();
      this.diagnosticPoints.material.dispose();
    }
    const selectedType = this.diagnosticTypeSelect?.value ?? "";
    const issuePoints = this.diagnosticIssues()
      .filter((issue) => !selectedType || issue.type === selectedType)
      .flatMap((issue) => {
        if (!Array.isArray(issue.point) || issue.point.length !== 3) return [];
        const objectIndex = issue.object_indices?.[0];
        const translation = Number.isInteger(objectIndex)
          ? (this.currentOffsets?.[objectIndex] ?? this.objectOffset(objectIndex))
          : new THREE.Vector3();
        return issue.point.map((value, axis) => value + translation.getComponent(axis));
      });
    const positions = issuePoints.length ? new Float32Array(issuePoints) : (this.diagnostics?.pointPositions ?? new Float32Array());
    const geometry = new THREE.BufferGeometry();
    geometry.setAttribute("position", new THREE.BufferAttribute(positions, 3));
    this.diagnosticPoints = new THREE.Points(
      geometry,
      new THREE.PointsMaterial({ color: "#dc2626", size: 10, sizeAttenuation: false, depthTest: false })
    );
    this.diagnosticPoints.renderOrder = 10;
    this.diagnosticPoints.visible = this.showDiagnostics;
    this.scene.add(this.diagnosticPoints);
  }

  // Update update colors for this viewer instance.
  updateColors() {
    if (!this.mesh) return;
    this.updateGeometryGroups();
    const colorAttribute = this.mesh.geometry.getAttribute("color");
    const objectColors = new Map();
    this.applyResultRange();
    const desiredMaterial = this.renderBy === "result" ? this.resultMaterial : this.surfaceMaterial;
    if (desiredMaterial !== this.material) {
      this.material = desiredMaterial;
      this.pairedMaterial = this.renderBy === "result" ? this.pairedResultMaterial : this.pairedSurfaceMaterial;
      this.mesh.material = [this.material, this.pairedMaterial, this.glassMaterial, this.pairedGlassMaterial];
      for (const material of this.mesh.material) material.clippingPlanes = this.clipEnabled ? [this.clipPlane] : [];
      this.updateMaterials();
    }
    const resultRange = this.results?.range ?? finiteRange(this.results?.values ?? []);
    for (let vertex = 0; vertex < this.visibleVertexObjects.length; vertex += 1) {
      const objectIndex = this.visibleVertexObjects[vertex];
      const resultValue = this.results ? this.currentResultValue(objectIndex) : Number.NaN;
      let color = objectColor(
        this.objects[objectIndex], this.renderBy, resultValue, resultRange,
        this.results?.palette, this.results?.missingColor
      );
      // Result fills encode data only. Interaction uses independent boundary overlays.
      if (this.renderBy !== "result") {
        if (this.selected.has(objectIndex)) color = new THREE.Color("#ff8a00");
        else if (this.hovered === objectIndex) color = new THREE.Color("#ffe066");
      }
      objectColors.set(objectIndex, color);
      colorAttribute.setXYZ(vertex, color.r, color.g, color.b);
    }
    colorAttribute.needsUpdate = true;
    this.updateEdgeColors(objectColors);
    this.backMesh.visible = this.renderBy === "normal";
    for (const [material, side] of [[this.material, this.renderBy === "normal" ? THREE.FrontSide : THREE.DoubleSide],
      [this.pairedMaterial, this.renderBy === "normal" ? THREE.FrontSide : THREE.BackSide]]) {
      if (material.side !== side) { material.side = side; material.needsUpdate = true; }
    }
    this.updateLegend();
    this.updateHoverOutline();
    this.refreshTooltip();
    this.requestRender();
  }

  // Keep outlines readable on dark results without altering the quantitative surface fills.
  updateEdgeColors(objectColors) {
    const attribute = this.edges?.geometry.getAttribute("color");
    if (!attribute) return;
    const background = this.scene.background;
    const colors = new Map();
    for (let segment = 0; segment < this.edgeObjects.length; segment += 1) {
      const index = this.edgeObjects[segment];
      if (!colors.has(index)) {
        const glass = this.glassIndices.has(index);
        const fill = glass ? this.glassMaterial.color : objectColors.get(index) ?? new THREE.Color("#b8bdc5");
        const adjacent = glass ? objectColors.get(this.windowParents.get(index)) : null;
        colors.set(index, boundaryColor(fill, background, glass ? this.glassMaterial.opacity : this.xray ? 0.34 : 1, adjacent));
      }
      const color = colors.get(index);
      attribute.setXYZ(segment * 2, color.r, color.g, color.b);
      attribute.setXYZ(segment * 2 + 1, color.r, color.g, color.b);
    }
    attribute.needsUpdate = true;
  }

  // Choose a distinct glazing appearance while keeping values, camera and selection intact.
  setWindowStyle(value) {
    if (!["glass", "opaque"].includes(value)) throw new Error("Window appearance must be glass or opaque.");
    this.windowStyle = value;
    this.windowStyleSelect.value = value;
    this.updateColors();
    this.updateClipping();
    this.root.dispatchEvent(new CustomEvent("eplusr-view"));
  }

  // Update update materials for this viewer instance.
  updateMaterials() {
    if (!this.material) return;
    for (const material of [this.material, this.pairedMaterial]) {
      material.transparent = this.xray;
      material.opacity = this.xray ? 0.34 : 1;
      material.depthWrite = !this.xray;
      material.needsUpdate = true;
    }
    this.requestRender();
  }

  // Update update helpers for this viewer instance.
  updateHelpers() {
    if (!this.mesh) return;
    if (!this.axisHelper) {
      const size = Math.max(1, this.mesh.geometry.boundingSphere?.radius ?? 1) * 0.5;
      this.axisHelper = new THREE.AxesHelper(size);
      this.scene.add(this.axisHelper);
    }
    this.axisHelper.visible = this.showAxis;
    this.edges.visible = this.showWireframe;
    this.points.visible = this.componentVisibility.daylighting;
    if (this.diagnosticPoints) this.diagnosticPoints.visible = this.showDiagnostics;

    if (!this.ground) {
      const box = this.mesh.geometry.boundingBox ?? new THREE.Box3();
      const size = new THREE.Vector3();
      const center = new THREE.Vector3();
      box.getSize(size);
      box.getCenter(center);
      const geometry = new THREE.PlaneGeometry(Math.max(size.x, 1) * 1.15, Math.max(size.y, 1) * 1.15);
      this.ground = new THREE.Mesh(geometry, new THREE.MeshBasicMaterial({ color: "#e3e5e7", side: THREE.DoubleSide }));
      this.ground.position.set(center.x, center.y, Math.min(0, box.min.z));
      this.scene.add(this.ground);
    }
    this.ground.visible = this.showGround;
    this.requestRender();
  }

  // Update update clipping for this viewer instance.
  updateClipping() {
    if (!this.material) return;
    const normals = {
      x: new THREE.Vector3(-1, 0, 0),
      y: new THREE.Vector3(0, -1, 0),
      z: new THREE.Vector3(0, 0, -1)
    };
    this.clipPlane.normal.copy(normals[this.clipAxis] ?? normals.z);
    this.clipPlane.constant = Number(this.clipSlider.value);
    this.clipValue.textContent = `${this.clipAxis.toUpperCase()}: ${formatNumber(this.clipPlane.constant)} m`;
    const planes = this.clipEnabled ? [this.clipPlane] : [];
    this.material.clippingPlanes = planes;
    this.pairedMaterial.clippingPlanes = planes;
    this.glassMaterial.clippingPlanes = planes;
    this.pairedGlassMaterial.clippingPlanes = planes;
    this.backMaterial.clippingPlanes = planes;
    this.edges.material.clippingPlanes = planes;
    if (this.selectionOutline) this.selectionOutline.material.clippingPlanes = planes;
    if (this.hoverOutline) this.hoverOutline.material.clippingPlanes = planes;
    this.material.needsUpdate = true;
    this.pairedMaterial.needsUpdate = true;
    this.glassMaterial.needsUpdate = true;
    this.pairedGlassMaterial.needsUpdate = true;
    this.updateClipCap();
    this.requestRender();
  }

  // Show a translucent section plane at the clipping boundary to make the cut location explicit.
  updateClipCap() {
    if (this.clipCapMesh) {
      this.scene.remove(this.clipCapMesh);
      this.clipCapMesh.geometry.dispose();
      this.clipCapMesh.material.dispose();
      this.clipCapMesh = null;
    }
    if (!this.clipEnabled || !this.clipCap) {
      this.requestRender();
      return;
    }
    const box = this.visibleBox();
    const size = box.getSize(new THREE.Vector3());
    const dimensions = this.clipAxis === "x" ? [size.y, size.z] : this.clipAxis === "y" ? [size.x, size.z] : [size.x, size.y];
    const geometry = new THREE.PlaneGeometry(Math.max(1, dimensions[0]) * 1.05, Math.max(1, dimensions[1]) * 1.05);
    const material = new THREE.MeshBasicMaterial({ color: "#f59e0b", transparent: true, opacity: 0.16, side: THREE.DoubleSide, depthWrite: false });
    this.clipCapMesh = new THREE.Mesh(geometry, material);
    const center = box.getCenter(new THREE.Vector3());
    this.clipCapMesh.position.copy(center);
    this.clipCapMesh.position[this.clipAxis] = Number(this.clipSlider.value);
    if (this.clipAxis === "x") this.clipCapMesh.rotation.y = Math.PI / 2;
    if (this.clipAxis === "y") this.clipCapMesh.rotation.x = Math.PI / 2;
    this.clipCapMesh.renderOrder = 12;
    this.scene.add(this.clipCapMesh);
    this.requestRender();
  }

  // Update update legend for this viewer instance.
  updateLegend() {
    this.legend.classList.toggle("eplusr-viewer__legend--result", this.renderBy === "result");
    if (this.renderBy === "result" && !this.results) {
      this.legend.style.display = "none";
      return;
    }
    this.legend.style.removeProperty("display");
    this.legend.classList.add("eplusr-viewer__legend--visible");
    this.legend.innerHTML = "";
    if (this.renderBy === "result") {
      const range = this.results.range ?? finiteRange(this.results.values);
      this.legend.appendChild(element("div", "eplusr-viewer__legend-title", `${this.results.label ?? "Result"}${this.results.units ? ` (${displayUnit(this.results.units)})` : ""}`));
      this.legend.appendChild(element("div", "eplusr-viewer__legend-time", String(this.results.times[this.frame] ?? "")));
      const gradient = element("div", "eplusr-viewer__legend-gradient");
      gradient.style.background = `linear-gradient(90deg, ${resultGradient(this.results.palette)})`;
      const labels = element("div", "eplusr-viewer__legend-labels");
      [0, 0.25, 0.5, 0.75, 1].forEach((amount) => {
        const tick = element("span", "eplusr-viewer__legend-tick", formatNumber(range[0] + amount * (range[1] - range[0]), 4));
        tick.title = String(range[0] + amount * (range[1] - range[0]));
        labels.appendChild(tick);
      });
      const policies = { all: "All frames · fixed", frame: "Current frame", manual: "Manual range" };
      this.legend.append(gradient, labels, element("div", "eplusr-viewer__legend-policy", policies[this.results.rangeMode] + (this.results.coverage < 1 ? " · " + Math.round(this.results.coverage * 100) + "%" : "")));
      const missing = element("div", "eplusr-viewer__legend-item");
      const missingSwatch = element("span", "eplusr-viewer__legend-swatch");
      missingSwatch.style.background = this.results.missingColor ?? "#b8bdc5";
      // Short IDE panes keep units and the missing-value key even when the long title is hidden.
      missing.append(element("span", "eplusr-viewer__legend-unit", displayUnit(this.results.units)), missingSwatch, element("span", "", "Missing"));
      this.legend.appendChild(missing);
      this.appendGlassLegend();
      if (this.selected.size === 1) {
        const value = this.currentResultValue([...this.selected][0]);
        if (Number.isFinite(value)) {
          const amount = range[1] === range[0] ? 0.5 : Math.max(0, Math.min(1, (value - range[0]) / (range[1] - range[0])));
          const marker = element("div", "eplusr-viewer__legend-marker", formatNumber(value));
          marker.style.left = `${amount * 100}%`;
          this.legend.appendChild(marker);
        }
      } else if (this.selected.size > 1) {
        const stats = this.selectionResultStats([...this.selected]);
        if (stats) this.legend.appendChild(element("div", "eplusr-viewer__legend-summary", `Selected: ${formatNumber(stats.minimum)} – ${formatNumber(stats.maximum)}, mean ${formatNumber(stats.mean)}`));
      }
      return;
    }

    const titles = {
      surface_type: "Surface type",
      boundary: "Boundary condition",
      construction: "Construction",
      zone: "Zone",
      space: "Space",
      normal: "Surface normal"
    };
    this.legend.appendChild(element("div", "eplusr-viewer__legend-title", titles[this.renderBy] ?? "Categories"));
    if (this.renderBy === "normal") {
      [["#f4f5f7", "Front face"], ["#d34747", "Back face"]].forEach(([color, label]) => {
        const row = element("div", "eplusr-viewer__legend-item");
        const swatch = element("span", "eplusr-viewer__legend-swatch");
        swatch.style.background = color;
        row.append(swatch, element("span", "", label));
        this.legend.appendChild(row);
      });
      return;
    }
    const categories = new Map();
    this.visibleObjects.forEach((index) => {
      const object = this.objects[index];
      if (this.glassIndices.has(index)) return;
      const label = categoryValue(object, this.renderBy);
      if (!categories.has(label)) categories.set(label, objectColor(object, this.renderBy, Number.NaN, [0, 1], "viridis"));
    });
    [...categories.entries()].sort(([a], [b]) => a.localeCompare(b)).slice(0, 20).forEach(([label, color]) => {
      const row = element("div", "eplusr-viewer__legend-item");
      const swatch = element("span", "eplusr-viewer__legend-swatch");
      swatch.style.background = `#${color.getHexString()}`;
      row.append(swatch, element("span", "", label));
      this.legend.appendChild(row);
    });
    this.appendGlassLegend();
    if (categories.size > 20) this.legend.appendChild(element("div", "eplusr-viewer__legend-summary", `…and ${categories.size - 20} more categories`));
  }

  // Identify glass separately so its composited tint is never read against the numerical color scale.
  appendGlassLegend() {
    if (!this.glassIndices || ![...this.glassIndices].some(index => this.visibleObjects.has(index))) return;
    const row = element("div", "eplusr-viewer__legend-item");
    const swatch = element("span", "eplusr-viewer__legend-swatch");
    swatch.style.background = "rgba(141,185,205,0.28)";
    row.append(swatch, element("span", "", this.renderBy === "result" ? "Glass · not color mapped" : "Transparent glass"));
    this.legend.appendChild(row);
  }

  // Update set render by for this viewer instance.
  setRenderBy(value) {
    if (value === "result" && !this.results) return;
    this.renderBy = value;
    this.renderSelect.value = value;
    this.updateColors();
  }

  // Switch variables without rebuilding geometry; provider variables fetch only their active frame.
  async setResultVariable(index) {
    if (!this.resultSet.length) return;
    this.pause();
    this.resultVariableIndex = Math.max(0, Math.min(this.resultSet.length - 1, Math.round(index)));
    this.results = this.resultSet[this.resultVariableIndex];
    this.frame = Math.min(this.frame, Math.max(0, this.results.times.length - 1));
    this.configureResultControls();
    this.setRenderBy("result");
    if (this.results.mode === "provider") await this.setTime(this.frame);
    else {
      this.updateColors();
      this.updateSelectionPanel();
    }
  }

  // Update set palette for this viewer instance.
  setPalette(palette) {
    if (!this.results) return;
    this.results.palette = palette;
    this.updateColors();
  }

  // Change the explicit color used for missing or non-finite result values.
  setMissingColor(color) {
    if (!this.results) return;
    this.results.missingColor = color;
    this.updateColors();
  }

  // Validate manual bounds before changing colors; invalid input keeps the previous display intact.
  setResultRange(minimum = Number(this.rangeMinimum.value), maximum = Number(this.rangeMaximum.value)) {
    if (!this.results) return;
    if (!Number.isFinite(minimum) || !Number.isFinite(maximum) || minimum >= maximum) {
      this.rangeStatus.textContent = "Min must be smaller than Max.";
      this.rangeMinimum.setAttribute("aria-invalid", "true");
      this.rangeMaximum.setAttribute("aria-invalid", "true");
      return;
    }
    this.results.manualRange = [minimum, maximum];
    this.results.rangeMode = "manual";
    this.results.coverage = 1;
    this.rangeStatus.textContent = "";
    this.rangeMinimum.removeAttribute("aria-invalid");
    this.rangeMaximum.removeAttribute("aria-invalid");
    this.updateColors();
  }

  // Store range policy per variable so changing time never erases a manual comparison range.
  setRangeMode(mode) {
    if (!this.results || !["all", "frame", "manual"].includes(mode)) return;
    this.results.rangeMode = mode;
    this.results.coverage = 1;
    if (mode === "manual" && !this.results.manualRange) {
      const [min, max] = this.results.range ?? this.results.sourceRange;
      this.results.manualRange = min === max ? [min - 0.5, max + 0.5] : [min, max];
    }
    this.rangeStatus.textContent = "";
    this.rangeMinimum.removeAttribute("aria-invalid");
    this.rangeMaximum.removeAttribute("aria-invalid");
    this.updateColors();
  }

  // Read the current frame only for frame policies, and dense values for embedded all-frame percentiles.
  rangeValues() {
    if (!this.results) return new Float32Array();
    if (this.results.mode === "provider") {
      return this.results.loadedFrame === this.frame ? this.results.values : new Float32Array();
    }
    if (this.results.rangeMode === "all") return this.results.values;
    const offset = this.frame * this.objects.length;
    return this.results.values.subarray(offset, offset + this.objects.length);
  }

  // Keep data colors, legend ticks, and range inputs on the same validated bounds.
  applyResultRange() {
    if (!this.results) return;
    const result = this.results;
    let range = result.rangeMode === "manual" ? result.manualRange
      : result.rangeMode === "all" ? result.sourceRange
      : finiteRange(this.rangeValues(), result.sourceRange);
    if (result.coverage < 1 && result.rangeMode !== "manual") {
      const values = this.rangeValues();
      const tail = (1 - result.coverage) / 2;
      const minimum = finiteQuantile(values, tail), maximum = finiteQuantile(values, 1 - tail);
      if (Number.isFinite(minimum) && Number.isFinite(maximum)) range = [minimum, maximum];
    }
    result.range = [...(range ?? result.sourceRange)];
    this.syncRangeControls();
  }

  // A provider supplies global extrema, not a global distribution; percentiles require its current frame.
  syncRangeControls() {
    if (!this.results || !this.rangeModeSelect) return;
    const result = this.results;
    this.rangeModeSelect.value = result.rangeMode;
    if (!this.rangeMinimum.hasAttribute("aria-invalid")) {
      this.rangeMinimum.value = String(result.range?.[0] ?? result.sourceRange[0]);
      this.rangeMaximum.value = String(result.range?.[1] ?? result.sourceRange[1]);
    }
    this.percentileSelect.value = String(result.coverage);
    this.percentileSelect.disabled = result.rangeMode === "manual" || (result.mode === "provider" && result.rangeMode === "all");
    this.percentileSelect.title = result.mode === "provider" && result.rangeMode === "all"
      ? "Choose Current frame to use percentile coverage with streamed results." : "Coverage of finite result values";
  }

  // Reset to the original all-frame bounds rather than silently changing a comparison to a single frame.
  resetResultRange() {
    this.setRangeMode("all");
  }

  // Coverage clips only displayed color bounds and never modifies the numerical result values.
  setPercentileRange(coverage) {
    if (!this.results || this.percentileSelect.disabled) return;
    this.results.coverage = Math.max(0, Math.min(1, Number(coverage)));
    this.updateColors();
  }

  // A symmetric range is an explicit manual range, retained across time and aggregation changes.
  setSymmetricRange() {
    if (!this.results) return;
    const range = this.results.range ?? this.results.sourceRange;
    const bound = Math.max(Math.abs(range[0]), Math.abs(range[1]));
    if (bound > 0) this.setResultRange(-bound, bound);
  }

  // Group source frames into original, daily, or monthly buckets using their time labels.
  buildFrameGroups(result, mode) {
    if (mode === "frame") return result.sourceTimes.map((label, index) => ({ label, indices: [index] }));
    const groups = new Map();
    result.sourceTimes.forEach((label, index) => {
      const key = timeGroupKey(label, mode);
      if (!groups.has(key)) groups.set(key, []);
      groups.get(key).push(index);
    });
    return [...groups.entries()].map(([label, indices]) => ({ label, indices }));
  }

  // Average embedded source frames by object without converting dense arrays to JSON structures.
  aggregateEmbedResult(result, groups) {
    const objectCount = Number(result.shape?.[1] ?? this.objects.length);
    const aggregated = new Float32Array(groups.length * objectCount);
    aggregated.fill(Number.NaN);
    groups.forEach((group, groupIndex) => {
      for (let objectIndex = 0; objectIndex < objectCount; objectIndex += 1) {
        let sum = 0;
        let count = 0;
        group.indices.forEach((frameIndex) => {
          const value = result.sourceValues[frameIndex * objectCount + objectIndex];
          if (!Number.isFinite(value)) return;
          sum += value;
          count += 1;
        });
        if (count) aggregated[groupIndex * objectCount + objectIndex] = sum / count;
      }
    });
    return aggregated;
  }

  // Switch time aggregation while preserving the original frame buffers for reversible changes.
  async setAggregation(mode) {
    if (!this.results) return;
    this.pause();
    const groups = this.buildFrameGroups(this.results, mode);
    this.results.aggregation = mode;
    this.results.frameGroups = groups.map((group) => group.indices);
    this.results.times = groups.map((group) => group.label);
    if (this.results.mode === "embed") {
      this.results.values = mode === "frame" ? this.results.sourceValues.slice() : this.aggregateEmbedResult(this.results, groups);
    } else {
      this.results.values = new Float32Array(this.objects.length);
      this.results.values.fill(Number.NaN);
    }
    this.frame = 0;
    this.configureResultControls();
    if (this.results.mode === "provider") await this.setTime(0);
    else {
      this.updateColors();
      this.updateSelectionPanel();
    }
  }

  // Move one frame in either direction while clamping at the available endpoints.
  stepTime(delta) {
    if (!this.results) return;
    this.pause();
    this.setTime(Math.max(0, Math.min(this.results.times.length - 1, this.frame + delta)))
      .catch((error) => this.reportResultError(error));
  }

  // Find a time label by exact or partial text and move the timeline to that frame.
  jumpToTime(query) {
    if (!this.results) return;
    const needle = query.trim().toLowerCase();
    const index = this.results.times.findIndex((label) => String(label).toLowerCase() === needle);
    const partial = index >= 0 ? index : this.results.times.findIndex((label) => String(label).toLowerCase().includes(needle));
    if (partial >= 0) {
      this.pause();
      this.setTime(partial).catch((error) => this.reportResultError(error));
      this.resultStatus.textContent = `Moved to ${this.results.times[partial]}.`;
    } else {
      this.resultStatus.textContent = `No time frame matches “${query}”.`;
    }
  }

  async setTime(frame) {
    if (!this.results) return;
    this.frame = Math.max(0, Math.min(this.results.times.length - 1, Math.round(frame)));
    this.timeSlider.value = String(this.frame);
    this.frameCounter.textContent = `${this.frame + 1} / ${this.results.times.length}`;
    this.timeSlider.setAttribute("aria-valuetext", String(this.results.times[this.frame]));
    this.timeLabel.textContent = timeCaption(this.results, this.frame);
    this.timeJumpInput.value = String(this.results.times[this.frame]);
    if (this.results.mode === "provider") {
      const requestedFrame = this.frame;
      const requestedResult = this.results;
      const frames = requestedResult.frameGroups?.[requestedFrame] ?? [requestedFrame];
      const cacheKey = `${requestedResult.aggregation}:${frames.join(",")}`;
      if (requestedResult.cache.has(cacheKey)) {
        requestedResult.values = requestedResult.cache.get(cacheKey);
        requestedResult.loadedFrame = requestedFrame;
        this.root.classList.remove("eplusr-viewer--error", "eplusr-viewer--loading");
        this.updateColors();
        this.updateSelectionPanel();
        this.resultStatus.textContent = `Loaded ${requestedResult.times[requestedFrame]}.`;
        this.root.dispatchEvent(new CustomEvent("eplusr-frame"));
        return;
      }
      if (this.resultAbortController) this.resultAbortController.abort();
      const controller = new AbortController();
      this.resultAbortController = controller;
      const separator = requestedResult.endpoint.includes("?") ? "&" : "?";
      this.root.classList.remove("eplusr-viewer--error");
      this.root.classList.add("eplusr-viewer--loading");
      this.resultStatus.textContent = `Loading ${requestedResult.times[requestedFrame]}…`;
      this.updateColors();
      this.updateSelectionPanel();
      try {
        const response = await fetch(
          `${requestedResult.endpoint}${separator}variable=${encodeURIComponent(requestedResult.variable)}&frames=${encodeURIComponent(frames.join(","))}`,
          { signal: controller.signal }
        );
        if (!response.ok) throw new Error(`Unable to load result frame ${requestedFrame} (${response.status}).`);
        const values = new Float32Array(await response.arrayBuffer());
        if (values.length !== this.objects.length) throw new Error("The result endpoint returned an unexpected number of values.");
        // Ignore an older response when the user moves the time slider quickly.
        if (requestedFrame !== this.frame || requestedResult !== this.results) return;
        requestedResult.values = values;
        requestedResult.loadedFrame = requestedFrame;
        requestedResult.cache.set(cacheKey, values);
        this.resultStatus.textContent = `Loaded ${requestedResult.times[requestedFrame]}.`;
      } catch (error) {
        if (error.name === "AbortError") return;
        this.pause();
        this.resultStatus.textContent = error.message;
        this.root.classList.add("eplusr-viewer--error");
        return;
      } finally {
        // An aborted stale request must not clear the loading state of its replacement.
        if (this.resultAbortController === controller) {
          this.resultAbortController = null;
          this.root.classList.remove("eplusr-viewer--loading");
        }
      }
    }
    this.updateColors();
    this.updateSelectionPanel();
    this.root.dispatchEvent(new CustomEvent("eplusr-frame"));
  }

  // Surface asynchronous result failures consistently for direct controls and provider requests.
  reportResultError(error) {
    if (error?.name === "AbortError") return;
    this.pause();
    this.root.classList.remove("eplusr-viewer--loading");
    this.root.classList.add("eplusr-viewer--error");
    if (this.resultStatus) this.resultStatus.textContent = error?.message ?? String(error);
  }

  // Update play for this viewer instance.
  play(speed = this.playbackSpeed) {
    if (!this.results || this.results.times.length < 2) return;
    this.pause();
    if (this.ribbon?.clean) this.ribbon.modelOnly(false);
    this.ribbon?.showTimeline(true);
    this.playbackSpeed = Number(speed) || 1;
    this.speedSelect.value = String(this.playbackSpeed);
    buttonIcon(this.playButton, "pause", "Pause", true);
    const interval = Math.max(50, 800 / this.playbackSpeed);
    this.playbackTimer = window.setInterval(() => {
      this.setTime((this.frame + 1) % this.results.times.length).catch((error) => {
        this.pause();
        this.resultStatus.textContent = error.message;
      });
    }, interval);
    this.savePreferences();
  }

  // Update pause for this viewer instance.
  pause() {
    if (this.playbackTimer) window.clearInterval(this.playbackTimer);
    this.playbackTimer = null;
    if (this.playButton) buttonIcon(this.playButton, "play", "Play", true);
  }

  // Update save preferences for this viewer instance.
  savePreferences() {
    try {
      window.localStorage.setItem("eplusr-idfviewer-settings-v1", JSON.stringify({
        playbackSpeed: this.playbackSpeed
      }));
    } catch (_) {
      // Storage can be disabled in embedded webviews; viewer behavior remains unchanged.
    }
  }

  // Update load preferences for this viewer instance.
  loadPreferences() {
    try {
      const saved = JSON.parse(window.localStorage.getItem("eplusr-idfviewer-settings-v1") ?? "{}");
      if (Number.isFinite(Number(saved.playbackSpeed))) this.playbackSpeed = Number(saved.playbackSpeed);
      this.speedSelect.value = String(this.playbackSpeed);
    } catch (_) {
      // Ignore malformed or inaccessible preference storage.
    }
  }

  // Read saved camera presets from local storage without making storage availability mandatory.
  cameraViews() {
    try {
      return JSON.parse(window.localStorage.getItem("eplusr-idfviewer-camera-views-v1") ?? "{}");
    } catch (_) {
      return {};
    }
  }

  // Refresh the saved-view selector after a preset is added or removed.
  populateSavedViews() {
    if (!this.savedViewSelect) return;
    const views = this.cameraViews();
    const selected = this.savedViewSelect.value;
    this.savedViewSelect.replaceChildren(option("", "Choose a saved view"), ...Object.keys(views).sort().map((name) => option(name)));
    if (views[selected]) this.savedViewSelect.value = selected;
  }

  // Persist the active camera type, transform, and orbit target under a user-facing name.
  saveCameraView(name) {
    const views = this.cameraViews();
    const label = name.trim() || `View ${Object.keys(views).length + 1}`;
    views[label] = {
      orthographic: this.camera === this.orthographicCamera,
      position: this.camera.position.toArray(),
      quaternion: this.camera.quaternion.toArray(),
      target: this.controls.target.toArray()
    };
    try {
      window.localStorage.setItem("eplusr-idfviewer-camera-views-v1", JSON.stringify(views));
      this.populateSavedViews();
      this.savedViewSelect.value = label;
      this.savedViewName.value = "";
      this.savedViewStatus.textContent = `Saved “${label}”.`;
    } catch (_) {
      this.savedViewStatus.textContent = "Camera presets cannot be saved in this browser. Your view and name are unchanged.";
    }
  }

  // Restore a saved camera transform without rebuilding the model scene.
  restoreCameraView(name) {
    const view = this.cameraViews()[name];
    if (!view) return;
    this.setProjection(Boolean(view.orthographic));
    this.camera.position.fromArray(view.position);
    this.camera.quaternion.fromArray(view.quaternion);
    this.controls.target.fromArray(view.target);
    this.controls.update();
    this.requestRender();
  }

  // Delete one saved camera preset and leave the current camera untouched.
  deleteCameraView(name) {
    if (!name) return;
    const views = this.cameraViews();
    delete views[name];
    this.savedViewStatus.textContent = "";
    try {
      window.localStorage.setItem("eplusr-idfviewer-camera-views-v1", JSON.stringify(views));
    } catch (_) {
      this.savedViewStatus.textContent = "Camera presets cannot be updated in this browser.";
    }
    this.populateSavedViews();
  }

  // Switch projections at the same target and apparent scale, preserving the user's viewing direction.
  setProjection(orthographic) {
    const next = orthographic ? this.orthographicCamera : this.perspectiveCamera;
    if (next === this.camera) return;
    this.stopNavigationMotion();
    const previous = this.camera;
    const offset = previous.position.clone().sub(this.controls.target);
    const tangent = Math.tan(THREE.MathUtils.degToRad(this.perspectiveCamera.fov / 2));
    if (orthographic) {
      this.orthoFitHeight = offset.length() * tangent;
      next.zoom = 1;
    } else {
      offset.setLength((this.orthoFitHeight ?? 1) / previous.zoom / tangent);
    }
    next.position.copy(this.controls.target).add(offset);
    next.quaternion.copy(previous.quaternion);
    next.up.copy(previous.up);
    this.camera = next;
    this.controls.object = next;
    this.projectionSelect.value = orthographic ? "orthographic" : "perspective";
    this.resize();
    this.controls.update();
    this.requestRender();
  }

  // Flush residual damping before a deliberate standard-view or fit operation.
  stopNavigationMotion() {
    const damping = this.controls.enableDamping;
    this.controls.enableDamping = false;
    this.controls.update();
    this.controls.enableDamping = damping;
  }

  // A manually orbited view is Custom; panning and zooming retain a standard orientation label.
  syncNavigation() {
    if (this.presetQuaternion && this.camera.quaternion.angleTo(this.presetQuaternion) > 0.01) {
      this.viewSelect.value = "custom";
    }
    this.projectionSelect.value = this.camera === this.orthographicCamera ? "orthographic" : "perspective";
    this.navigation?.update();
  }

  // Update set view for this viewer instance.
  setView(view) {
    if (view === "custom") return;
    this.stopNavigationMotion();
    const box = this.visibleBox();
    const center = box.getCenter(new THREE.Vector3());
    const size = box.getSize(new THREE.Vector3());
    const distance = Math.max(size.x, size.y, size.z, 1) * 1.8;
    const offset = {
      top: new THREE.Vector3(0, -distance * 1e-6, distance),
      bottom: new THREE.Vector3(0, -distance * 1e-6, -distance),
      front: new THREE.Vector3(0, -distance, 0),
      back: new THREE.Vector3(0, distance, 0),
      left: new THREE.Vector3(-distance, 0, 0),
      right: new THREE.Vector3(distance, 0, 0),
      iso: new THREE.Vector3(distance, -distance, distance)
    }[view] ?? new THREE.Vector3(distance, -distance, distance);
    this.camera.position.copy(center).add(offset);
    this.camera.up.set(0, 0, 1);
    this.controls.target.copy(center);
    this.camera.lookAt(center);
    this.controls.update();
    this.viewSelect.value = view;
    this.fitView();
    this.presetQuaternion = this.camera.quaternion.clone();
    this.viewSelect.value = view;
    this.navigation?.update();
    this.requestRender();
  }

  // Update visible box for this viewer instance.
  visibleBox(objectIndices = null) {
    const box = new THREE.Box3();
    const selected = objectIndices ? new Set(objectIndices) : null;
    for (let face = 0; face < this.faceObjects.length; face += 1) {
      const objectIndex = this.faceObjects[face];
      if (selected ? !selected.has(objectIndex) : !this.isObjectVisible(objectIndex)) continue;
      const offset = face * 9;
      const translation = this.currentOffsets?.[objectIndex] ?? this.objectOffset(objectIndex);
      for (let vertex = 0; vertex < 3; vertex += 1) {
        box.expandByPoint(new THREE.Vector3(
          this.positions[offset + vertex * 3] + translation.x,
          this.positions[offset + vertex * 3 + 1] + translation.y,
          this.positions[offset + vertex * 3 + 2] + translation.z
        ));
      }
    }
    if (box.isEmpty()) box.expandByPoint(new THREE.Vector3()).expandByScalar(1);
    return box;
  }

  // Fit geometry to the unobstructed canvas at its current aspect ratio and viewing direction.
  fitView(objectIndices = null) {
    this.stopNavigationMotion();
    const box = this.visibleBox(objectIndices);
    const center = box.getCenter(new THREE.Vector3());
    const radius = Math.max(0.5, box.getSize(new THREE.Vector3()).length() / 2);
    const direction = this.camera.position.clone().sub(this.controls.target).normalize();
    if (direction.lengthSq() < 0.5) direction.set(1, -1, 0.8).normalize();
    this.resize();
    const vertical = THREE.MathUtils.degToRad(this.perspectiveCamera.fov / 2);
    const horizontal = Math.atan(Math.tan(vertical) * this.perspectiveCamera.aspect);
    // Project the bounding-box corners into the chosen camera basis. A sphere wastes
    // most of a wide canvas for shallow buildings and long, narrow selections.
    const basis = new THREE.Matrix4().lookAt(direction, new THREE.Vector3(), this.camera.up);
    const right = new THREE.Vector3().setFromMatrixColumn(basis, 0);
    const up = new THREE.Vector3().setFromMatrixColumn(basis, 1);
    let distance = radius;
    let halfWidth = 0, halfHeight = 0;
    for (const x of [box.min.x, box.max.x]) {
      for (const y of [box.min.y, box.max.y]) {
        for (const z of [box.min.z, box.max.z]) {
          const corner = new THREE.Vector3(x, y, z).sub(center);
          const depth = corner.dot(direction);
          halfWidth = Math.max(halfWidth, Math.abs(corner.dot(right)));
          halfHeight = Math.max(halfHeight, Math.abs(corner.dot(up)));
          distance = Math.max(distance,
            depth + 1.14 * Math.abs(corner.dot(right)) / Math.tan(horizontal),
            depth + 1.14 * Math.abs(corner.dot(up)) / Math.tan(vertical));
        }
      }
    }
    this.controls.target.copy(center);
    this.camera.position.copy(center).addScaledVector(direction, distance);

    this.orthoFitHeight = Math.max(halfHeight, halfWidth / this.perspectiveCamera.aspect, 0.5) * 1.14;
    this.orthographicCamera.zoom = 1;
    this.resize();
    this.camera.lookAt(center);
    this.camera.updateProjectionMatrix();
    this.controls.update();
    this.requestRender();
  }

  // Restore every visibility control and clear transient search, selection, and isolation state.
  showAll() {
    this.remoteFilters = {};
    this.nameFilter = "";
    this.isolated = null;
    this.isolationHistory = [];
    this.hidden.clear();
    this.zoneFilter = "";
    this.spaceFilter = "";
    this.zoneSelect.value = "";
    this.populateSpaceFilter();
    this.spaceSelect.value = "";
    this.diagnosticOnly = false;
    this.diagnosticOnlyToggle.checked = false;
    COMPONENTS.forEach((component) => { this.componentVisibility[component] = true; });
    Object.values(this.componentInputs).forEach((input) => { input.checked = true; });
    this.selected.clear();
    this.updateNameFilterStatus();
    this.rebuildGeometry();
    this.fitView();
    this.emitSelection();
  }

  // Isolate the current selection while retaining a reversible isolation history.
  isolateSelection() {
    if (!this.selected.size) return;
    // Avoid adding a duplicate history level when the isolated set already matches the selection.
    if (this.isolated && this.isolated.size === this.selected.size &&
        [...this.selected].every((index) => this.isolated.has(index))) return;
    this.isolationHistory.push(this.isolated ? new Set(this.isolated) : null);
    this.isolated = new Set(this.selected);
    this.rebuildGeometry();
    this.fitView(this.selected);
    this.updateSelectionPanel();
  }

  // Restore the previous isolation level without resetting unrelated filters.
  exitIsolation() {
    if (!this.isolated) return;
    this.isolated = this.isolationHistory.length ? this.isolationHistory.pop() : null;
    this.applyVisibilityChange({ fit: true });
  }

  // Hide selected objects explicitly and clear them from the active selection.
  hideSelection() {
    if (!this.selected.size) return;
    this.selected.forEach((index) => this.hidden.add(index));
    this.selected.clear();
    this.applyVisibilityChange({ fit: true });
    this.emitSelection();
  }

  // Clear selection highlights and notify downstream DOM event consumers.
  clearSelection() {
    if (!this.selected.size) return;
    this.selected.clear();
    this.updateColors();
    this.emitSelection();
  }

  // Bind mouse and keyboard actions to this read-only view.
  bindEvents() {
    this.controls.addEventListener("change", () => { this.needsRender = true; this.syncNavigation(); });
    this.renderer.domElement.addEventListener("pointerdown", (event) => {
      this.pointerDown = { x: event.clientX, y: event.clientY };
    });
    this.renderer.domElement.addEventListener("pointermove", (event) => this.onPointerMove(event));
    this.renderer.domElement.addEventListener("pointerup", (event) => this.onPointerUp(event));
    this.renderer.domElement.addEventListener("click", (event) => this.onClick(event));
    this.renderer.domElement.addEventListener("dblclick", (event) => this.onDoubleClick(event));
    this.renderer.domElement.addEventListener("pointerleave", () => {
      this.hovered = null;
      this.tooltip.style.display = "none";
      this.updateColors();
    });
    this.root.addEventListener("keydown", (event) => {
      // Text entry and native selects own their keys; shortcuts apply to the canvas.
      if (typeof event.key !== "string") return;
      if (event.key === "Escape" && this.measureMode) {
        event.preventDefault();
        this.toggleMeasureMode(false);
        this.renderer.domElement.focus();
        return;
      }
      if (event.target.closest?.("input, select, textarea, [contenteditable]")) return;
      if (event.key === "Escape") {
        this.selected.clear();
        this.isolated = null;
        this.rebuildGeometry();
        this.emitSelection();
      }
      if (event.key.toLowerCase() === "f") this.fitView(this.selected.size ? this.selected : null);
      if (event.key.toLowerCase() === "i") this.isolateSelection();
      if (event.key.toLowerCase() === "m") {
        const enabled = !this.measureMode;
        this.measureButton.setAttribute("aria-pressed", String(enabled));
        this.toggleMeasureMode(enabled);
      }
      if (event.key === "?") {
        this.ribbon.open("help");
      }
    });
    this.root.tabIndex = 0;
    this.root.setAttribute("aria-keyshortcuts", "Escape F I M");
  }

  // Return both the picked object and world point so selection and measurement share one raycast.
  raycast(event) {
    if (!this.mesh || !this.visibleFaceObjects.length) return null;
    const rect = this.renderer.domElement.getBoundingClientRect();
    this.pointer.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    this.pointer.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
    this.raycaster.setFromCamera(this.pointer, this.camera);
    const intersection = this.raycaster.intersectObject(this.mesh, false).find((hit) =>
      !this.clipEnabled || this.clipPlane.distanceToPoint(hit.point) >= -1e-6);
    if (!intersection || intersection.faceIndex === undefined) return null;
    const objectIndex = this.visibleFaceObjects[intersection.faceIndex];
    return objectIndex === undefined ? null : { objectIndex, point: intersection.point.clone() };
  }

  // Keep the hover readout on the same model object and frame used for result colors.
  onPointerMove(event) {
    this.hoverPosition = { x: event.offsetX, y: event.offsetY };
    const objectIndex = this.raycast(event)?.objectIndex ?? null;
    if (objectIndex !== this.hovered) {
      this.hovered = objectIndex;
      this.updateColors();
    }
    this.refreshTooltip();
  }

  // Repaint the numeric probe when time, variable, range, selection, or visibility changes.
  refreshTooltip() {
    if (this.hovered === null || !this.isObjectVisible(this.hovered) || !this.hoverPosition) {
      this.tooltip.style.display = "none";
      return;
    }
    const object = this.objects[this.hovered];
    this.tooltip.replaceChildren(element("strong", "", object.name ?? object.class));
    this.tooltip.appendChild(element("div", "eplusr-viewer__value", [object.zone_name, object.space_name].filter(Boolean).join(" · ")));
    if (this.renderBy === "result" && this.results) {
      const value = this.currentResultValue(this.hovered);
      this.tooltip.append(
        element("div", "eplusr-viewer__probe-value", formatNumber(value) + (this.results.units ? " " + displayUnit(this.results.units) : "")),
        element("div", "eplusr-viewer__value", this.results.label ?? "Result"),
        element("div", "eplusr-viewer__value", timeCaption(this.results, this.frame))
      );
    }
    this.tooltip.style.display = "block";
    const { x, y } = this.hoverPosition;
    const left = Math.max(6, Math.min(x + 14, this.canvasHost.clientWidth - this.tooltip.offsetWidth - 6));
    const top = y + this.tooltip.offsetHeight + 20 < this.canvasHost.clientHeight ? y + 14 : Math.max(6, y - this.tooltip.offsetHeight - 14);
    this.tooltip.style.left = left + "px";
    this.tooltip.style.top = top + "px";
  }

  // Hover boundaries provide feedback without changing a quantitative surface fill.
  updateHoverOutline() {
    const index = this.hovered;
    const key = index + ":" + this.selected.has(index) + ":" + this.explodeAmount + ":" + this.explodeMode + ":" + this.visibleObjects.has(index);
    if (key === this.hoverOutlineKey) return;
    this.hoverOutlineKey = key;
    this.hoverOutline?.geometry.dispose();
    this.hoverOutline?.material.dispose();
    if (this.hoverOutline) this.scene.remove(this.hoverOutline);
    this.hoverOutline = null;
    if (index === null || this.selected.has(index) || !this.isObjectVisible(index)) return;
    const positions = [];
    const translation = this.currentOffsets?.[index] ?? new THREE.Vector3();
    for (let segment = 0; segment < this.edgeObjects.length; segment++) {
      if (this.edgeObjects[segment] !== index) continue;
      for (let coordinate = 0; coordinate < 6; coordinate++) {
        positions.push(this.edgePositions[segment * 6 + coordinate] + translation.getComponent(coordinate % 3));
      }
    }
    const geometry = new THREE.BufferGeometry();
    geometry.setAttribute("position", new THREE.Float32BufferAttribute(positions, 3));
    this.hoverOutline = new THREE.LineSegments(geometry, new THREE.LineBasicMaterial({ color: "#3874a8", depthTest: false }));
    this.hoverOutline.material.clippingPlanes = this.clipEnabled ? [this.clipPlane] : [];
    this.hoverOutline.renderOrder = 19;
    this.scene.add(this.hoverOutline);
  }

  // Validate primary pointer releases separately from native click-count semantics.
  onPointerUp(event) {
    this.pointerClickReady = false;
    if (!this.pointerDown) return;
    const distance = Math.hypot(event.clientX - this.pointerDown.x, event.clientY - this.pointerDown.y);
    this.pointerDown = null;
    if (event.button !== 0 || distance > 4) return;
    if (this.measureMode) {
      const hit = this.raycast(event);
      if (hit) this.addMeasurementPoint(hit.point);
      return;
    }
    this.pointerClickReady = true;
  }

  // Keep ordinary clicks immediate, but do not toggle additive selections twice during a double-click.
  onClick(event) {
    if (!this.pointerClickReady || this.measureMode || event.button !== 0 || event.detail > 1) return;
    this.selectionBeforeClick = new Set(this.selected);
    const objectIndex = this.raycast(event)?.objectIndex ?? null;
    if (objectIndex === null) {
      if (!event.shiftKey && !event.metaKey && !event.ctrlKey) this.selected.clear();
    } else if (event.shiftKey || event.metaKey || event.ctrlKey) {
      const group = this.selectionGroup(objectIndex);
      const remove = group.every((index) => this.selected.has(index));
      group.forEach((index) => remove ? this.selected.delete(index) : this.selected.add(index));
    } else {
      this.selected = new Set(this.selectionGroup(objectIndex));
    }
    this.updateColors();
    this.emitSelection();
  }

  // Enter the clicked Zone first, then its face, without changing geometry visibility or the camera.
  onDoubleClick(event) {
    if (!this.pointerClickReady || this.measureMode || event.button !== 0 || event.shiftKey || event.metaKey || event.ctrlKey) return;
    const hit = this.raycast(event);
    if (!hit) return;
    this.selected = new Set(doubleClickSelection(
      this.objects, this.visibleObjects, hit.objectIndex, this.selectionBeforeClick ?? new Set(), this.drilledZone
    ));
    this.drilledZone = this.objects[hit.objectIndex]?.zone_name ?? null;
    this.updateColors();
    this.emitSelection();
  }

  // Keep tool instructions and completed measurements available next to the viewport.
  buildViewportFeedback() {
    this.toolFeedback = element("div", "eplusr-viewer__tool-feedback");
    this.toolHint = element("div", "eplusr-viewer__tool-hint");
    this.measureStatus.setAttribute("role", "status");
    const actions = element("div", "eplusr-viewer__tool-actions");
    this.restartMeasureButton = element("button", "", "Restart");
    buttonIcon(this.restartMeasureButton, "reset", "Restart");
    this.restartMeasureButton.setAttribute("aria-label", "Restart measurement");
    this.restartMeasureButton.title = "Restart measurement";
    this.restartMeasureButton.type = "button";
    this.restartMeasureButton.addEventListener("click", () => this.clearMeasurement());
    this.exitMeasureButton = element("button");
    this.exitMeasureButton.type = "button";
    buttonIcon(this.exitMeasureButton, "clear", "Exit measurement", true);
    this.exitMeasureButton.addEventListener("click", () => this.toggleMeasureMode(false));
    actions.append(this.restartMeasureButton, this.exitMeasureButton);
    this.toolFeedback.append(this.measureStatus, this.toolHint, actions);
    this.measureLabel = element("div", "eplusr-viewer__measurement-label");
    this.measureLabel.hidden = true;
    this.canvasHost.append(this.toolFeedback, this.measureLabel);
    this.clearMeasureButton = element("button", "", "Clear measurement");
    this.clearMeasureButton.type = "button";
    buttonIcon(this.clearMeasureButton, "clear", "Clear measurement");
    this.clearMeasureButton.addEventListener("click", () => this.clearMeasurement());
    this.measureButton.parentNode.appendChild(this.clearMeasureButton);
  }

  // Exit cancels an incomplete pick while a completed dimension stays readable until cleared.
  toggleMeasureMode(enabled) {
    this.measureMode = Boolean(enabled);
    this.measureButton.setAttribute("aria-pressed", String(this.measureMode));
    if (!this.measureMode && this.measurePoints.length === 1) this.clearMeasurement();
    this.renderer.domElement.style.cursor = this.measureMode ? "crosshair" : "default";
    this.root.dispatchEvent(new CustomEvent("eplusr-view"));
    this.updateViewportFeedback();
    this.requestRender();
  }

  // Clear only measurement state, leaving model selection, camera, and result frames intact.
  clearMeasurement() {
    this.measurePoints = [];
    this.measurement = null;
    this.clearMeasurementGeometry();
    this.root.dispatchEvent(new CustomEvent("eplusr-view"));
    this.updateViewportFeedback();
  }

  // Measure the picked display coordinates, explicitly identifying exploded-view distances.
  addMeasurementPoint(point) {
    if (this.measurePoints.length >= 2) this.clearMeasurement();
    this.measurePoints.push(point.clone());
    if (this.measurePoints.length === 2) this.measurement = { distance: this.measurePoints[0].distanceTo(this.measurePoints[1]) };
    this.updateMeasurementGeometry();
    this.root.dispatchEvent(new CustomEvent("eplusr-view"));
    this.updateViewportFeedback();
  }

  // Position dimensions in screen space and keep tool and display restrictions visible across panel tabs.
  updateViewportFeedback() {
    if (!this.toolFeedback) return;
    const exploded = this.explodeMode !== "none" && this.explodeAmount > 0;
    let state = "Inspect · " + (this.selectionModeSelect.selectedOptions[0]?.textContent ?? "Surfaces");
    if (this.measureMode) state = this.measurePoints.length === 0 ? "Measure · Pick first point"
      : this.measurePoints.length === 1 ? "Measure · Pick second point"
      : (exploded ? "Display distance: " : "Distance: ") + formatNumber(this.measurement.distance) + " m";
    else if (this.clipEnabled) state += " · Clip " + this.clipAxis.toUpperCase() + " " + formatNumber(this.clipPlane.constant) + " m";
    else if (exploded) state += " · Exploded " + this.explodeMode;
    if (this.measureStatus.textContent !== state) this.measureStatus.textContent = state;
    this.toolFeedback.hidden = !(this.measureMode || this.clipEnabled || exploded || this.selectionMode !== "surface");
    this.toolHint.textContent = this.measureMode
      ? (this.measurePoints.length === 2 ? "Pick to measure again · Esc exits" : "Pick on model · Esc cancels")
      : "";
    this.toolFeedback.classList.toggle("eplusr-viewer__tool-feedback--active", this.measureMode);
    this.restartMeasureButton.hidden = !this.measureMode;
    this.restartMeasureButton.disabled = !this.measurePoints.length;
    this.exitMeasureButton.hidden = !this.measureMode;
    this.clearMeasureButton.disabled = !this.measurePoints.length;
    this.measureLabel.hidden = !this.measurement;
    if (this.measurement) {
      this.measureLabel.textContent = formatNumber(this.measurement.distance) + " m" + (exploded ? " · exploded view" : "");
      const middle = this.measurePoints[0].clone().add(this.measurePoints[1]).multiplyScalar(0.5).project(this.camera);
      this.measureLabel.hidden = Math.abs(middle.x) > 1 || Math.abs(middle.y) > 1 || middle.z < -1 || middle.z > 1;
      const x = (middle.x + 1) * this.canvasHost.clientWidth / 2;
      const y = (1 - middle.y) * this.canvasHost.clientHeight / 2;
      this.measureLabel.style.left = Math.max(8, Math.min(x, this.canvasHost.clientWidth - this.measureLabel.offsetWidth - 8)) + "px";
      this.measureLabel.style.top = Math.max(8, Math.min(y + 10, this.canvasHost.clientHeight - this.measureLabel.offsetHeight - 8)) + "px";
    }
  }

  // Replace measurement markers and lines without affecting model picking geometry.
  updateMeasurementGeometry() {
    this.clearMeasurementGeometry();
    const geometry = new THREE.BufferGeometry().setFromPoints(this.measurePoints);
    this.measurePointsObject = new THREE.Points(
      geometry,
      new THREE.PointsMaterial({ color: "#ef4444", size: 9, sizeAttenuation: false, depthTest: false })
    );
    this.measurePointsObject.renderOrder = 30;
    this.scene.add(this.measurePointsObject);
    if (this.measurePoints.length === 2) {
      const lineGeometry = new THREE.BufferGeometry().setFromPoints(this.measurePoints);
      this.measureLine = new THREE.Line(
        lineGeometry,
        new THREE.LineBasicMaterial({ color: "#ef4444", depthTest: false })
      );
      this.measureLine.renderOrder = 29;
      this.scene.add(this.measureLine);
    }
    this.requestRender();
  }

  // Dispose temporary measurement geometry whenever the mode or points reset.
  clearMeasurementGeometry() {
    ["measurePointsObject", "measureLine"].forEach((name) => {
      const object = this[name];
      if (!object) return;
      this.scene.remove(object);
      object.geometry.dispose();
      object.material.dispose();
      this[name] = null;
    });
    this.requestRender();
  }

  // Expose model metadata to the widget adapter after updating selection outlines.
  emitSelection() {
    const detail = [...this.selected].map((index) => ({ index, ...this.objects[index], value: this.currentResultValue(index) }));
    this.rebuildSelectionOutline();
    this.updateSelectionPanel();
    // Selection updates the contextual property editor without stealing the current ribbon task.
    this.container.dispatchEvent(new CustomEvent("eplusr-selection", { detail }));
  }

  // Return the selected frame value using its current object index.
  currentResultValue(index) {
    if (!this.results) return null;
    if (this.results.mode === "provider") return this.results.loadedFrame === this.frame ? this.results.values[index] : Number.NaN;
    return this.results.values[this.frame * this.objects.length + index];
  }

  // Sum original triangle areas for one or more model objects without relying on filtered buffers.
  objectArea(objectIndices) {
    const selected = new Set(objectIndices);
    let area = 0;
    const a = new THREE.Vector3();
    const b = new THREE.Vector3();
    const c = new THREE.Vector3();
    for (let face = 0; face < this.faceObjects.length; face += 1) {
      if (!selected.has(this.faceObjects[face])) continue;
      const offset = face * 9;
      a.set(this.positions[offset], this.positions[offset + 1], this.positions[offset + 2]);
      b.set(this.positions[offset + 3], this.positions[offset + 4], this.positions[offset + 5]);
      c.set(this.positions[offset + 6], this.positions[offset + 7], this.positions[offset + 8]);
      area += new THREE.Triangle(a, b, c).getArea();
    }
    return area;
  }

  // Compute numerical selection statistics for the active result frame.
  selectionResultStats(indices) {
    const values = indices.map((index) => this.currentResultValue(index)).filter(Number.isFinite);
    if (!values.length) return null;
    const range = finiteRange(values);
    return {
      minimum: range[0],
      maximum: range[1],
      mean: values.reduce((sum, value) => sum + value, 0) / values.length
    };
  }

  // Show properties and result values for the current selection.
  updateSelectionPanel() {
    this.syncFilterUi();
    this.fitSelectionButton.disabled = !this.selected.size;
    const buttons = this.selectionActionButtons;
    const hasSelection = this.selected.size > 0;
    const selectionIsIsolated = this.isolated && this.isolated.size === this.selected.size &&
      [...this.selected].every((index) => this.isolated.has(index));
    buttons.isolate.disabled = !hasSelection || selectionIsIsolated;
    buttons.hide.disabled = !hasSelection;
    buttons.clear.disabled = !hasSelection;
    buttons.exitIsolation.disabled = !this.isolated;
    this.isolationStatus.textContent = this.isolated
      ? `Model › Isolated selection (${this.isolated.size} objects) › Level ${Math.max(1, this.isolationHistory.length)}`
      : "Model";
    if (!this.selected.size) {
      this.selectionPanel.textContent = "Click a surface to inspect its properties; double-click to select its Zone, then double-click again to select a surface.";
      return;
    }
    const indices = [...this.selected];
    const multiple = indices.length > 1;
    const common = (field) => {
      const values = [...new Set(indices.map((index) => this.objects[index][field] || "Not assigned"))];
      return values.length === 1 ? values[0] : "Mixed (" + values.length + " values)";
    };
    const rows = multiple ? [
      ["Objects", indices.length],
      ["Zones", new Set(indices.map((index) => this.objects[index].zone_name).filter(Boolean)).size],
      ["Class", common("class")], ["Surface type", common("surface_type")],
      ["Zone", common("zone_name")], ["Space", common("space_name")],
      ["Construction", common("construction_name")], ["Boundary", common("outside_boundary_condition")]
    ] : this.objectPropertyRows(indices[0]);
    rows.push([multiple ? "Total face area" : "Face area", formatNumber(this.objectArea(indices)) + " m²"]);
    if (this.results) {
      const units = this.results.units ? " " + displayUnit(this.results.units) : "";
      const stats = this.selectionResultStats(indices);
      if (multiple) {
        rows.push(["Result values", indices.filter((index) => Number.isFinite(this.currentResultValue(index))).length + " / " + indices.length]);
        if (stats) rows.push(["Minimum", formatNumber(stats.minimum) + units],
          ["Object mean", formatNumber(stats.mean) + units], ["Maximum", formatNumber(stats.maximum) + units]);
      } else rows.push([this.results.label ?? "Result", formatNumber(this.currentResultValue(indices[0])) + units]);
    }
    const summary = this.propertyList(rows);
    const title = element("h4", "eplusr-viewer__selection-title", multiple ? indices.length + " objects selected" : this.objects[indices[0]].name);
    const areaNote = element("p", "eplusr-viewer__area-note", "Face area excludes openings. Shared boundaries are counted per IDF surface.");
    this.selectionPanel.replaceChildren(title, summary, areaNote);
    if (!multiple) return;
    const details = element("details", "eplusr-viewer__active-object");
    details.open = Boolean(this.activeDetailsOpen);
    details.appendChild(element("summary", "", "Inspect one selected object"));
    details.addEventListener("toggle", () => { this.activeDetailsOpen = details.open; });
    const select = this.field("Active object", "select", details);
    indices.forEach((index) => select.appendChild(option(index, this.objects[index].name ?? "Object " + (index + 1))));
    if (!this.selected.has(this.activeObjectIndex)) this.activeObjectIndex = indices[0];
    select.value = String(this.activeObjectIndex);
    const properties = element("div");
    const showObject = () => {
      const index = Number(select.value);
      this.activeObjectIndex = index;
      const objectRows = this.objectPropertyRows(index);
      if (this.results) objectRows.push([this.results.label ?? "Result", formatNumber(this.currentResultValue(index)) + (this.results.units ? " " + displayUnit(this.results.units) : "")]);
      properties.replaceChildren(this.propertyList(objectRows));
    };
    select.addEventListener("change", showObject);
    showObject();
    details.appendChild(properties);
    this.selectionPanel.appendChild(details);
  }

  // Build a safe metadata list shared by single-object and aggregate property views.
  propertyList(rows) {
    const list = element("dl");
    rows.filter(([, value]) => value !== null && value !== undefined && value !== "").forEach(([key, value]) => {
      list.append(element("dt", "", key), element("dd", "", String(value)));
    });
    return list;
  }

  // Keep individual model identity separate from common values across a multi-selection.
  objectPropertyRows(index) {
    const object = this.objects[index];
    return [["Name", object.name], ["Class", object.class], ["Surface type", object.surface_type],
      ["Zone", object.zone_name], ["Space", object.space_name],
      ["Construction", object.construction_name], ["Boundary", object.outside_boundary_condition]];
  }

  // Preview the actual camera export before the user saves it from the task pane.
  downloadSnapshot(filename = "eplusr-view.png") {
    this.renderer.render(this.scene, this.camera);
    const canvas = this.renderer.domElement;
    this.ribbon.previewSnapshot(canvas.toDataURL("image/png"), filename, canvas.width, canvas.height);
  }

  // Apply incremental view commands; geometry and the source IDF are never modified.
  applyCommand(command, args = {}) {
    if (command === "result_query_error") this.queryPanel.fail(args.message);
    if (command === "select_ids") {
      const ids = new Set((args.ids ?? []).map(String));
      this.selected = new Set(this.objects.flatMap((object, index) =>
        ids.has(String(object.id)) && this.isObjectVisible(index) ? [index] : []));
      this.updateColors();
      this.emitSelection();
    }
    if (command === "filter") {
      this.remoteFilters = Object.fromEntries(Object.entries(args).map(([key, values]) =>
        [key, values === null ? null : (Array.isArray(values) ? values : [values]).map((value) => String(value).toLowerCase())]));
      this.zoneFilter = "";
      this.spaceFilter = "";
      this.nameFilter = this.remoteFilters.name?.[0] ?? "";
      // Mirror a single R criterion in the native select; retain arrays for multi-value filters.
      const matchingName = (key, field) => this.remoteFilters[key]?.length === 1
        ? this.objects.find((object) => String(object[field] ?? "").toLowerCase() === this.remoteFilters[key][0])?.[field] ?? "" : "";
      this.zoneFilter = matchingName("zone", "zone_name");
      this.spaceFilter = matchingName("space", "space_name");
      COMPONENTS.forEach((name) => {
        this.componentVisibility[name] = !this.remoteFilters.type || this.remoteFilters.type.includes(name);
      });
      this.populateSpaceFilter();
      this.applyVisibilityChange({ fit: true });
    }
    if (command === "background") {
      this.scene.background = new THREE.Color(args.value);
      this.updateColors();
    }
    const toggles = { wireframe: ["showWireframe", "wireButton"], x_ray: ["xray", "xrayButton"], axis: ["showAxis", "axisButton"], ground: ["showGround", "groundButton"] };
    if (toggles[command]) {
      const [property, button] = toggles[command];
      this[property] = Boolean(args.on);
      this[button].setAttribute("aria-pressed", String(this[property]));
      this.rebuildGeometry();
    }
    if (command === "time") this.setTime(args.frame ?? 0).catch((error) => this.reportResultError(error));
    if (command === "variable") this.setResultVariable(args.index ?? 0).catch((error) => this.reportResultError(error));
    if (command === "render_by") this.setRenderBy(args.value);
    if (command === "window_style") this.setWindowStyle(args.value);
    if (command === "view") this.setView(args.value ?? "iso");
    if (command === "fit") this.fitView(this.selected.size ? this.selected : null);
    if (command === "fit_model") this.fitView();
    if (command === "fit_selection" && this.selected.size) this.fitView(this.selected);
    if (command === "projection") this.setProjection(args.value === "orthographic");
    if (command === "range_mode") this.setRangeMode(args.value);
    if (command === "show_all") this.showAll();
    if (command === "play") this.play(args.speed ?? this.playbackSpeed);
    if (command === "pause") this.pause();
    if (command === "palette") this.setPalette(args.value ?? "viridis");
    if (command === "range") this.setResultRange(Number(args.minimum), Number(args.maximum));
    if (command === "aggregation") this.setAggregation(args.value ?? "frame").catch((error) => this.reportResultError(error));
    if (command === "explode") {
      this.explodeSelect.value = args.value ?? "none";
      this.explodeSlider.value = String(args.amount ?? 0);
      this.setExplode(this.explodeSelect.value, Number(this.explodeSlider.value));
    }
    if (command === "clip") {
      if (args.axis) {
        this.clipAxisSelect.value = args.axis;
        this.setClipAxis(args.axis);
      }
      if (args.position !== undefined) this.clipSlider.value = String(args.position);
      if (args.on !== undefined) {
        this.clipEnabled = Boolean(args.on);
        this.clipToggle.checked = this.clipEnabled;
      }
      if (args.cap !== undefined) {
        this.clipCap = Boolean(args.cap);
        this.clipCapToggle.checked = this.clipCap;
      }
      this.updateClipping();
    }
    if (command === "measure") {
      const enabled = Boolean(args.on);
      this.measureButton.setAttribute("aria-pressed", String(enabled));
      this.toggleMeasureMode(enabled);
    }
    if (command === "hide_selected") this.hideSelection();
    if (command === "isolate") this.isolateSelection();
    if (command === "exit_isolation") this.exitIsolation();
    if (command === "clear_selection") this.clearSelection();
    if (command === "panel") this.togglePanel(args.hidden ?? null);
    if (command === "diagnostics") {
      if (args.on !== undefined) this.showDiagnostics = Boolean(args.on);
      if (args.only !== undefined) this.diagnosticOnly = Boolean(args.only);
      this.diagnosticToggle.checked = this.showDiagnostics;
      this.diagnosticOnlyToggle.checked = this.diagnosticOnly;
      this.rebuildGeometry();
    }
    if (command === "select") {
      this.selected = new Set(args.indices ?? []);
      this.updateColors();
      this.emitSelection();
    }
  }

  // Size the canvas from its layout slot, leaving inspection and time controls outside the model.
  resize() {
    const compact = this.root.clientWidth < 960;
    const short = this.root.clientHeight < 500;
    const enteringShort = short && !this.short;
    this.short = short;
    this.root.classList.toggle("eplusr-viewer--short", short);
    this.ribbon?.resizeLayout();
    // Dock the key based on the central viewport, including desktop pane combinations.
    const legendDocked = short || (this.viewport?.clientWidth ?? this.root.clientWidth) < 600;
    const legendHost = legendDocked ? (this.viewport ?? this.workspace) : this.canvasHost;
    if (this.legend.parentNode !== legendHost) legendHost.append(this.legend);
    this.navigation?.resize(legendDocked);
    if (enteringShort && this.ribbon) this.ribbon.collapse(true);
    if (compact !== this.compact) {
      this.compact = compact;
      this.root.classList.toggle("eplusr-viewer--compact", compact);
    }
    const width = Math.max(1, this.canvasHost.clientWidth);
    const height = Math.max(1, this.canvasHost.clientHeight);
    const previousAspect = this.perspectiveCamera.aspect;
    // Recheck native scaling when an IDE or browser window moves between displays.
    this.renderer.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
    this.renderer.setSize(width, height, false);
    this.perspectiveCamera.aspect = width / height;
    // Keep a fitted model on screen when an IDE pane becomes narrower, without resetting its orbit.
    if (previousAspect !== this.perspectiveCamera.aspect && this.positions.length) {
      const offset = this.perspectiveCamera.position.clone().sub(this.controls.target);
      this.perspectiveCamera.position.copy(this.controls.target).add(offset.multiplyScalar(Math.min(1, previousAspect) / Math.min(1, this.perspectiveCamera.aspect)));
    }
    this.perspectiveCamera.updateProjectionMatrix();
    const radius = Math.max(1, this.visibleBox().getSize(new THREE.Vector3()).length() / 2) * 1.14;
    const halfHeight = this.orthoFitHeight ?? radius / Math.min(1, width / height);
    this.orthographicCamera.left = -halfHeight * width / height;
    this.orthographicCamera.right = halfHeight * width / height;
    this.orthographicCamera.top = halfHeight;
    this.orthographicCamera.bottom = -halfHeight;
    this.orthographicCamera.updateProjectionMatrix();
    this.queryPanel?.resize();
    this.ribbon?.syncOverflow();
    this.requestRender();
  }

  // Mark the canvas dirty so static scenes do not consume GPU time between interactions.
  requestRender() {
    this.needsRender = true;
  }

  // Fit depth precision to the displayed model on orbit/zoom, including after filtering or explosion.
  updateCameraDepth() {
    const sphere = this.mesh?.geometry.boundingSphere;
    if (!sphere) return;
    const direction = this.camera.getWorldDirection(new THREE.Vector3());
    const depth = sphere.center.clone().sub(this.camera.position).dot(direction);
    const margin = Math.max(sphere.radius * 1.05, 0.1);
    const near = this.camera.isPerspectiveCamera ? Math.max(0.001, depth - margin) : depth - margin;
    const far = Math.max(near + 0.1, depth + margin);
    if (this.camera.near !== near || this.camera.far !== far) {
      this.camera.near = near;
      this.camera.far = far;
      this.camera.updateProjectionMatrix();
    }
  }

  // Render only when controls or scene state change.
  animate() {
    this.animationFrame = window.requestAnimationFrame(() => this.animate());
    const controlsChanged = this.controls.update();
    if (controlsChanged || this.needsRender) {
      this.updateCameraDepth();
      this.renderer.render(this.scene, this.camera);
      this.syncNavigation();
      this.updateViewportFeedback();
      this.needsRender = false;
    }
  }

  // Release timers, controls, geometries, materials and the renderer.
  dispose() {
    this.pause();
    if (this.resultAbortController) this.resultAbortController.abort();
    if (this.animationFrame) window.cancelAnimationFrame(this.animationFrame);
    this.layoutObserver?.disconnect();
    this.navigation?.dispose();
    this.ribbon?.hints.dispose();
    this.controls.dispose();
    if (this.mesh) this.mesh.geometry.dispose();
    [
      this.surfaceMaterial, this.resultMaterial, this.backMaterial, this.pairedSurfaceMaterial, this.pairedResultMaterial, this.hiddenBackMaterial, this.glassMaterial, this.pairedGlassMaterial, this.hoverOutline?.material, this.edges?.material, this.points?.material,
      this.diagnosticPoints?.material, this.selectionOutline?.material,
      this.measurePointsObject?.material, this.measureLine?.material, this.clipCapMesh?.material,
      this.ground?.material, this.northLabel?.material
    ].filter(Boolean).forEach((material) => material.dispose());
    [
      this.edges?.geometry, this.hoverOutline?.geometry, this.points?.geometry, this.diagnosticPoints?.geometry,
      this.selectionOutline?.geometry, this.measurePointsObject?.geometry,
      this.measureLine?.geometry, this.clipCapMesh?.geometry, this.ground?.geometry
    ].filter(Boolean).forEach((geometry) => geometry.dispose());
    if (this.northLabel?.material?.map) this.northLabel.material.map.dispose();
    this.renderer.dispose();
    this.container.innerHTML = "";
  }
}
