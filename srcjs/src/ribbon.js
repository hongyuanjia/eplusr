import { icon, buttonIcon } from "./icons.js";
import { CommandHints } from "./command-hints.js";
import { ConnectionStatus } from "./connection-status.js";
import { ModelBrowser } from "./model-browser.js";

// Build text-only DOM nodes so source model names never become markup.
function node(tag, className = "", text = "") {
  const item = document.createElement(tag);
  item.className = className;
  item.textContent = text;
  return item;
}

// Own the compact command strip and persistent, on-demand workspace surfaces.
export class ViewerRibbon {
  // Move existing controls once, retaining their listeners, values and R bindings.
  constructor(viewer, sections, actions) {
    this.viewer = viewer;
    this.hints = new CommandHints(viewer.root, viewer.panel.id + "-command-hint");
    this.dialogs = {};
    this.tabs = {};
    this.pages = {};
    this.timelineVisible = false;
    this.root = node("header", "eplusr-viewer__ribbon");
    this.header = node("div", "eplusr-viewer__ribbon-header");
    const brand = node("span", "eplusr-viewer__brand", "IDFView");
    brand.prepend(icon("object"));
    const strip = node("div", "eplusr-viewer__ribbon-tabs");
    strip.setAttribute("role", "tablist");
    strip.setAttribute("aria-label", "Viewer ribbon");
    this.quick = node("div", "eplusr-viewer__quick-actions");
    this.header.append(brand, strip, this.quick);
    this.content = node("div", "eplusr-viewer__ribbon-content");
    const names = ["home", "view", "results"];
    names.forEach((name, index) => {
      const tab = node("button", "", name === "home" ? "Model" : name[0].toUpperCase() + name.slice(1));
      tab.type = "button";
      tab.id = viewer.panel.id + "-ribbon-" + name;
      tab.setAttribute("role", "tab");
      const page = node("div", "eplusr-viewer__ribbon-page");
      page.id = tab.id + "-commands";
      page.setAttribute("role", "tabpanel");
      page.setAttribute("aria-labelledby", tab.id);
      tab.setAttribute("aria-controls", page.id);
      tab.addEventListener("click", () => this.select(name));
      tab.addEventListener("keydown", event => {
        const next = event.key === "ArrowRight" ? (index + 1) % names.length
          : event.key === "ArrowLeft" ? (index + names.length - 1) % names.length
          : event.key === "Home" ? 0 : event.key === "End" ? names.length - 1 : null;
        if (next === null) return;
        event.preventDefault(); event.stopPropagation();
        this.select(names[next]); this.tabs[names[next]].focus();
      });
      this.tabs[name] = tab; this.pages[name] = page;
      strip.append(tab); this.content.append(page);
    });
    const band = node("div", "eplusr-viewer__ribbon-band");
    this.previous = this.button("Previous commands", "previous", () => this.scroll(-1), true);
    this.next = this.button("More commands", "next", () => this.scroll(1), true);
    this.previous.classList.add("eplusr-viewer__ribbon-scroll", "eplusr-viewer__ribbon-scroll--previous");
    this.next.classList.add("eplusr-viewer__ribbon-scroll", "eplusr-viewer__ribbon-scroll--next");
    band.append(this.content, this.previous, this.next);
    this.content.addEventListener("scroll", () => this.syncOverflow());
    this.root.append(this.header, band);
    viewer.toolbar.replaceWith(this.root);
    viewer.toolbar = this.quick;

    // A central viewport reserves space for docked panes without obscuring the model.
    viewer.viewport = node("div", "eplusr-viewer__viewport");
    viewer.canvasHost.replaceWith(viewer.viewport);
    viewer.viewport.append(viewer.canvasHost);
    this.browser = new ModelBrowser(viewer, this);
    viewer.workspace.prepend(this.browser.root);
    this.taskPane = node("aside", "eplusr-viewer__task-pane");
    this.taskPane.hidden = true;
    this.taskPane.setAttribute("aria-label", "Task panel");
    const taskHeader = node("div", "eplusr-viewer__pane-header");
    this.taskHeading = node("h2");
    this.taskHeading.id = viewer.panel.id + "-task-title";
    this.taskPane.setAttribute("aria-labelledby", this.taskHeading.id);
    this.closeTaskButton = this.button("Close task panel", "clear", () => this.closeEditors(), true);
    taskHeader.append(this.taskHeading, this.closeTaskButton);
    this.taskPane.append(taskHeader);
    viewer.workspace.append(this.taskPane);
    this.taskPane.addEventListener("keydown", event => {
      // Searchable lists consume their own Escape before the task pane handles it.
      if (event.key === "Escape" && !event.defaultPrevented) {
        event.preventDefault(); this.closeEditors();
      }
      event.stopPropagation();
    });

    this.restore = this.button("Show controls", "controls", () => this.modelOnly(false), true);
    this.restore.classList.add("eplusr-viewer__restore-controls");
    this.restore.hidden = true; viewer.canvasHost.append(this.restore);
    this.treeButton = this.button("Model tree", "tree", () => this.showBrowser(!this.browserVisible), true);
    this.modelButton = this.button("Model only", "focus_model", () => this.modelOnly(true), true);
    this.collapseButton = this.button("Collapse ribbon", "collapse", () => this.collapse(!this.collapsed), true);
    this.helpButton = this.button("Navigation help", "help", () => this.open("help"), true);
    viewer.connectionStatus = new ConnectionStatus();
    this.quick.append(this.treeButton, this.modelButton, this.collapseButton, this.helpButton, viewer.connectionStatus.root);

    this.dialog("objects", "Find and filter", [viewer.selectionModeSelect.closest("label"), sections.search]);
    this.dialog("components", "Visible components", [sections.components]);
    this.dialog("appearance", "Model appearance", [this.field("Color by", viewer.renderSelect), viewer.windowStyleSelect.closest("label")]);
    this.dialog("camera", "Camera", [this.field("View", viewer.viewSelect), this.field("Projection", viewer.projectionSelect)]);
    this.dialog("explode", "Exploded view", [viewer.explodeSelect.closest("label"), viewer.explodeSlider]);
    this.dialog("display", "Section plane", [sections.clipping]);
    const help = node("div", "eplusr-viewer__navigation-help");
    help.append(node("p", "", "Drag to orbit · Right-drag to pan · Scroll to zoom"),
      node("p", "", "Double-click: select Zone, then surface · I: isolate selection"),
      node("p", "", "Ctrl / ⌘: add to selection · Shift: select a range in the model tree"),
      node("p", "", "F: fit selection (or visible model) · I: isolate · M: measure · Esc: exit the current tool"),
      node("p", "", "Model tree: arrows navigate and expand · H toggles visibility"));
    this.dialog("help", "Navigation help", [help]);
    this.dialog("saved", "Saved views", [sections.saved]);
    this.snapshotImage = node("img", "eplusr-viewer__snapshot-preview");
    this.snapshotImage.alt = "Snapshot of the current 3D view";
    this.snapshotStatus = node("p", "eplusr-viewer__status");
    this.snapshotLink = node("a", "eplusr-viewer__snapshot-download", "Download PNG");
    this.dialog("snapshot", "Snapshot", [this.snapshotImage, this.snapshotStatus, this.snapshotLink]);
    this.dialog("diagnostics", "Geometry diagnostics", [sections.diagnostics]);
    this.dialog("properties", "Selection properties", [sections.selection]);
    this.dialog("results", "Result data", [viewer.queryPanel.section, viewer.resultEmpty]);
    const timeFields = [viewer.aggregationSelect.closest("label"), viewer.timeJumpInput.closest("label")];
    this.dialog("time", "Result time settings", timeFields);
    sections.results.querySelector(".eplusr-viewer__section-body").prepend(viewer.variableSelect.closest("label"));
    this.dialog("colors", "Result color scale", [sections.results]);
    viewer.panel.remove();
    viewer.resultStatus.classList.add("eplusr-viewer__result-status");
    viewer.canvasHost.append(viewer.resultStatus);
    // Status belongs to the workspace footer, leaving a single compact command area above.
    viewer.root.append(viewer.scopeBar);

    const home = this.pages.home;
    this.group(home, "Navigate").append(viewer.fitModelButton, viewer.fitSelectionButton);
    this.group(home, "Objects").append(this.launcher("Find / filter", "filters", "objects"), this.launcher("Components", "components", "components"));
    this.group(home, "Selection").append(viewer.selectionActions);
    buttonIcon(viewer.panelButton, "properties", "Properties");
    viewer.panelButton.setAttribute("aria-controls", this.taskPane.id = viewer.panel.id + "-task-pane");
    this.group(home, "Inspect").append(viewer.panelButton, actions.reset, this.launcher("Appearance", "appearance", "appearance"));
    const view = this.pages.view;
    this.group(view, "Camera").append(this.launcher("Camera", "camera", "camera"), this.launcher("Saved views", "save", "saved"), actions.capture);
    this.group(view, "Display").append(viewer.wireButton, viewer.xrayButton, viewer.axisButton, viewer.groundButton);
    this.group(view, "Tools").append(viewer.measureButton, viewer.clearMeasureButton, this.launcher("Section plane", "section", "display"), this.launcher("Exploded view", "explode", "explode"), this.launcher("Diagnostics", "diagnostics", "diagnostics"));
    const results = this.pages.results;
    this.group(results, "Source").append(this.launcher("Select result data", "data", "results"));
    this.colorScaleButton = this.launcher("Color scale", "colors", "colors");
    this.group(results, "Appearance").append(this.colorScaleButton, this.launcher("Glass / color by", "glass", "appearance"));
    this.timelineButton = this.button("Timeline", "timeline", () => this.showTimeline(!this.timelineVisible));
    this.timeSettingsButton = this.launcher("Time settings", "time", "time");
    this.group(results, "Time").append(this.timeSettingsButton, this.timelineButton);
    viewer.timelineVariableLabel = node("span", "eplusr-viewer__timeline-variable");
    viewer.timeline.querySelector(".eplusr-viewer__timeline-heading").prepend(viewer.timelineVariableLabel);
    viewer.timeline.querySelector(".eplusr-viewer__timeline-heading").append(this.button("Hide timeline", "clear", () => this.showTimeline(false), true));
    viewer.root.addEventListener("keydown", event => {
      if (event.key !== "Escape" || event.defaultPrevented) return;
      if (this.clean) { event.preventDefault(); event.stopImmediatePropagation(); this.modelOnly(false); }
      else if (this.activeTask && !this.taskPane.contains(event.target)) {
        event.preventDefault(); event.stopImmediatePropagation(); this.closeEditors();
      }
    }, true);
    // All top commands share the same compact target; business labels remain in task forms.
    this.root.querySelectorAll("button[aria-label]:not([role=tab])").forEach(button => button.classList.add("eplusr-viewer__icon-only"));
    const descriptions = {
      "Fit model": "Fit all visible objects", "Fit selection": "Fit the selected objects · F",
      "Isolate": "Show only the selection · I", "Measure": "Measure between two model points · M",
      "Select result data": "Choose variables, environments and other result filters",
      "Color scale": "Choose the result variable, palette and numerical range",
      "Time settings": "Aggregate results or jump to a time",
      "Timeline": "Show or hide the result playback strip",
      "Glass / color by": "Choose transparent glass or quantitative window colors",
      "Model only": "Focus on the 3D model; hide interface controls · Esc restores them",
      "Show controls": "Restore the model tree, commands and task panel · Esc"
    };
    this.root.querySelectorAll("button[aria-label]").forEach(button => {
      button.dataset.hint = descriptions[button.getAttribute("aria-label")] ?? button.getAttribute("aria-label");
    });
    this.restore.dataset.hint = descriptions["Show controls"];
    this.select("home");
    this.resizeLayout();
  }

  // Keep SVG symbols and accessible names consistent across commands and panes.
  button(label, symbol, callback, iconOnly = false) {
    const button = node("button"); button.type = "button";
    buttonIcon(button, symbol, label, iconOnly);
    button.addEventListener("click", callback);
    return button;
  }

  // Associate a native control with a readable field caption.
  field(label, input) {
    const field = node("label", "eplusr-viewer__field");
    field.append(node("span", "", label), input);
    return field;
  }

  // Compact groups share one command row and a small caption underneath.
  group(page, label) {
    const group = node("section", "eplusr-viewer__ribbon-group");
    group.setAttribute("aria-label", label);
    const body = node("div", "eplusr-viewer__ribbon-commands");
    group.append(body, node("div", "eplusr-viewer__ribbon-caption", label));
    page.append(group); return body;
  }

  // Point commands at the persistent right-side task rather than a modal dialog.
  launcher(label, symbol, name) {
    const button = this.button(label, symbol, () => this.open(name));
    button.dataset.task = name;
    return button;
  }

  // Each task retains a separate scroll container and its original form instances.
  dialog(name, title, sections) {
    const body = node("div", "eplusr-viewer__tab-body eplusr-viewer__task-body");
    body.id = this.viewer.panel.id + "-task-" + name;
    body.hidden = true;
    sections.forEach(section => {
      if (section.tagName === "DETAILS") { section.open = true; section.classList.add("eplusr-viewer__task-section"); }
      body.append(section);
    });
    this.taskPane.append(body);
    this.dialogs[name] = { element: body, title };
  }

  // Reveal one task without resetting query drafts, scrolling, filters or selection.
  open(name) {
    if (!this.dialogs[name]) return;
    if (this.clean) this.modelOnly(false);
    if (!this.taskPane.contains(document.activeElement)) this.returnFocus = document.activeElement;
    this.activeTask = name;
    Object.entries(this.dialogs).forEach(([key, task]) => { task.element.hidden = key !== name; });
    this.taskHeading.textContent = this.dialogs[name].title;
    this.taskPane.hidden = false;
    if (this.drawers) this.browserVisible = false;
    this.syncLayout(); this.viewer.resize();
    this.closeTaskButton.focus();
    this.viewer.root.dispatchEvent(new CustomEvent("eplusr-view"));
  }

  // Keep a visible export preview even in embedded browsers without download notifications.
  previewSnapshot(url, filename, width, height) {
    this.snapshotImage.src = url;
    this.snapshotLink.href = url;
    this.snapshotLink.download = filename;
    this.snapshotStatus.textContent = width + " × " + height + " px · PNG";
    this.open("snapshot");
  }

  // Close the pane but leave every task's DOM and scroll position intact.
  closeEditors() {
    this.activeTask = null; this.taskPane.hidden = true;
    this.syncLayout(); this.viewer.resize();
    const target = this.returnFocus?.isConnected && this.returnFocus.getClientRects().length ? this.returnFocus : this.tabs[this.active];
    target?.focus();
    this.viewer.root.dispatchEvent(new CustomEvent("eplusr-view"));
  }

  // Toggle the model browser; temporary drawers never stack on top of each other.
  showBrowser(visible) {
    this.browserVisible = Boolean(visible);
    if (visible && this.drawers) { this.activeTask = null; this.taskPane.hidden = true; }
    this.syncLayout(); this.viewer.resize();
    if (visible) this.browser.search.focus();
    else this.treeButton.focus();
    this.viewer.root.dispatchEvent(new CustomEvent("eplusr-view"));
  }

  // Default to an open browser on desktop and a clear canvas in narrow widgets.
  resizeLayout() {
    const drawers = this.viewer.root.clientWidth < 1100;
    if (this.drawers !== drawers) {
      this.drawers = drawers;
      this.browserVisible = !drawers;
      if (drawers && this.activeTask) this.browserVisible = false;
    }
    this.syncLayout();
  }

  // Derive grid columns and pressed states from the single workspace state.
  syncLayout() {
    const root = this.viewer.root;
    root.classList.toggle("eplusr-viewer--drawers", this.drawers);
    root.classList.toggle("eplusr-viewer--browser-open", Boolean(this.browserVisible));
    root.classList.toggle("eplusr-viewer--task-open", Boolean(this.activeTask));
    this.browser.root.hidden = !this.browserVisible;
    this.treeButton.setAttribute("aria-pressed", String(Boolean(this.browserVisible)));
    this.treeButton.dataset.hint = this.browserVisible ? "Hide model tree" : "Show model tree";
    this.treeButton.setAttribute("aria-expanded", String(Boolean(this.browserVisible)));
    this.treeButton.setAttribute("aria-controls", this.browser.root.id);
    this.viewer.panelButton.setAttribute("aria-expanded", String(this.activeTask === "properties"));
    this.root.querySelectorAll("[data-task]").forEach(button => button.setAttribute("aria-pressed", String(button.dataset.task === this.activeTask)));
  }

  // Keyboard-operable tabs change commands without dismissing a task or its draft.
  select(name) {
    Object.keys(this.tabs).forEach(key => {
      this.tabs[key].setAttribute("aria-selected", String(key === name));
      this.tabs[key].tabIndex = key === name ? 0 : -1;
      this.pages[key].hidden = key !== name;
    });
    this.active = name; this.content.scrollLeft = 0; this.collapse(false);
  }

  // Expose horizontally overflowing commands in small host panes.
  scroll(direction) {
    this.content.scrollBy({ left: direction * Math.max(160, this.content.clientWidth - 100), behavior: "instant" });
    this.syncOverflow();
  }

  // Give the overflow strip explicit previous/next affordances.
  syncOverflow() {
    const overflow = !this.collapsed && this.content.scrollWidth > this.content.parentElement.clientWidth + 1;
    this.previous.hidden = !overflow; this.next.hidden = !overflow;
    this.previous.disabled = this.content.scrollLeft < 2;
    this.next.disabled = this.content.scrollLeft + this.content.clientWidth >= this.content.scrollWidth - 2;
  }

  // Collapse only commands while retaining the workspace tabs and quick access.
  collapse(hidden) {
    this.collapsed = hidden; this.content.hidden = hidden;
    this.collapseButton.setAttribute("aria-expanded", String(!hidden));
    buttonIcon(this.collapseButton, hidden ? "expand" : "collapse", hidden ? "Expand ribbon" : "Collapse ribbon", true);
    this.viewer.resize(); this.syncOverflow();
    this.viewer.root.dispatchEvent(new CustomEvent("eplusr-view"));
  }

  // Show playback only on request; hiding it pauses without changing the frame.
  showTimeline(visible) {
    const restoreFocus = !visible && this.viewer.timeline.contains(document.activeElement);
    this.timelineVisible = Boolean(visible);
    if (!visible) this.viewer.pause();
    this.syncResults(); this.viewer.resize();
    if (restoreFocus) this.tabs.results.focus();
    this.viewer.root.dispatchEvent(new CustomEvent("eplusr-view"));
  }

  // Keep availability separate from user preferences for clean view and playback.
  syncResults() {
    const available = Boolean(this.viewer.resultSet.length);
    this.viewer.timeline.hidden = !available || !this.timelineVisible || this.clean;
    [this.timelineButton, this.colorScaleButton, this.timeSettingsButton, this.viewer.variableSelect,
      this.viewer.paletteSelect, this.viewer.rangeModeSelect, this.viewer.aggregationSelect, this.viewer.timeJumpInput]
      .filter(Boolean).forEach(control => { control.disabled = !available; });
    this.timelineButton.setAttribute("aria-pressed", String(available && this.timelineVisible));
  }

  // Temporarily hide chrome while retaining the task, browser, draft and result frame.
  modelOnly(clean) {
    if (clean) { this.viewer.pause(); this.viewer.toggleMeasureMode(false); }
    this.clean = Boolean(clean);
    this.viewer.root.classList.toggle("eplusr-viewer--model-only", this.clean);
    this.root.hidden = this.clean; this.viewer.scopeBar.hidden = this.clean;
    this.restore.hidden = !this.clean;
    this.syncResults(); this.viewer.resize();
    (this.clean ? this.restore : this.modelButton).focus();
    this.viewer.root.dispatchEvent(new CustomEvent("eplusr-view"));
  }
}
