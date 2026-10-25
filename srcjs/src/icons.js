import {
  createElement, Box, Boxes, Camera, ChartNoAxesCombined, ChevronLeft, ChevronRight, ChevronUp, ChevronDown,
  Clock3, Database, Expand, Palette, Paintbrush, Orbit, PanelsTopLeft, TriangleAlert, CircleHelp, Check,
  Circle, Eye, EyeOff, FolderTree, Focus, Grid2X2, Grid3X3, Layers, ListFilter, Move3D,
  PanelRight, PanelRightClose, Pause, Play, RotateCcw, Ruler, Save, Scan,
  ScanEye, Search, Settings2, SlidersHorizontal, Trash2, Undo2, X
} from "lucide";

// Import only the symbols used by this viewer; the generated widget needs no network icon service.
const symbols = {
  object: Box, snapshot: Camera, results: ChartNoAxesCombined, previous: ChevronLeft,
  next: ChevronRight, connection: Circle, hide: EyeOff, isolate: Focus,
  ground: Grid2X2, wireframe: Grid3X3, layers: Layers, filters: ListFilter,
  axes: Move3D, panel: PanelRight, close: PanelRightClose, pause: Pause, play: Play,
  reset: RotateCcw, measure: Ruler, save: Save, fit: Scan, xray: ScanEye,
  search: Search, display: Settings2, properties: SlidersHorizontal,
  delete: Trash2, undo: Undo2, clear: X, eye: Eye, tree: FolderTree,
  components: Boxes, colors: Palette, appearance: Paintbrush, camera: Orbit, data: Database,
  time: Clock3, timeline: PanelRight, explode: Expand, diagnostics: TriangleAlert,
  collapse: ChevronUp, expand: ChevronDown, controls: PanelsTopLeft, help: CircleHelp, check: Check
};

// Keep every SVG at the same optical size, stroke, and accessible decorative role.
export function icon(name) {
  // Composite symbols retain the same stroke grammar while separating 3D-specific actions.
  if (["focus_model", "section", "glass", "timeline"].includes(name)) {
    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("viewBox","0 0 24 24"); svg.setAttribute("width","18"); svg.setAttribute("height","18");
    svg.setAttribute("fill","none"); svg.setAttribute("stroke","currentColor"); svg.setAttribute("stroke-width","1.7");
    svg.setAttribute("stroke-linecap","round"); svg.setAttribute("stroke-linejoin","round");
    svg.setAttribute("aria-hidden","true"); svg.setAttribute("focusable","false"); svg.classList.add("eplusr-viewer__icon");
    const paths = {
      focus_model: ["M3 8V3h5m8 0h5v5M3 16v5h5m8 0h5v-5", "m12 7 5 3v5l-5 3-5-3v-5Zm-5 3 5 3 5-3m-5 3v5"],
      section: ["m12 3 8 4v10l-8 4-8-4V7Zm-8 4 8 4 8-4m-8 4v10", "M2 12h20"],
      glass: ["M4 3h16v18H4Zm8 0v18M4 11h16", "m7 6 2-2m6 14 2-2"],
      timeline: ["M3 4h18v16H3Zm0 11h18M7 14v3m5-3v3m5-3v3"]
    };
    for (const d of paths[name]) { const path=document.createElementNS(svg.namespaceURI,"path");path.setAttribute("d",d);svg.append(path); }
    return svg;
  }
  return createElement(symbols[name], {
    width: 16, height: 16, "stroke-width": 1.7, "aria-hidden": "true",
    focusable: "false", class: "eplusr-viewer__icon"
  });
}

// Preserve the button's name when its visible label is condensed to an icon.
export function buttonIcon(button, name, label, iconOnly = false) {
  const text = document.createElement("span");
  text.className = "eplusr-viewer__button-label";
  text.textContent = label;
  button.replaceChildren(icon(name), text);
  button.classList.add("eplusr-viewer__icon-button");
  button.classList.toggle("eplusr-viewer__icon-only", iconOnly);
  button.setAttribute("aria-label", label);
  button.dataset.hint = label;
  button.removeAttribute("title");
}
