import * as THREE from "three";

// Keep the world axes and EnergyPlus true north available without covering model geometry.
export class ViewNavigation {
  // Native buttons provide the same standard-view actions for mouse, keyboard, and touch users.
  constructor(viewer) {
    this.viewer = viewer;
    this.element = document.createElement("div");
    this.element.className = "eplusr-viewer__navigation";
    this.element.setAttribute("role", "group");
    this.element.setAttribute("aria-label", "View orientation and true north");
    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("viewBox", "0 0 100 100");
    svg.setAttribute("aria-hidden", "true");
    svg.classList.add("eplusr-viewer__navigation-lines");
    this.element.appendChild(svg);
    const axes = [
      ["X", "Right view (+X)", "right", [1, 0, 0], "#a8534c"],
      ["−X", "Left view (−X)", "left", [-1, 0, 0], "#a8534c"],
      ["N", "North / Back view (+Y)", "back", [0, 1, 0], "#3c7657"],
      ["−Y", "Front view (−Y)", "front", [0, -1, 0], "#3c7657"],
      ["Z", "Top view (+Z)", "top", [0, 0, 1], "#426d9c"],
      ["−Z", "Bottom view (−Z)", "bottom", [0, 0, -1], "#426d9c"]
    ];
    this.axes = axes.map(([label, title, view, vector, color]) => {
      const line = document.createElementNS(svg.namespaceURI, "line");
      line.setAttribute("x1", "50");
      line.setAttribute("y1", "50");
      line.setAttribute("stroke", color);
      svg.appendChild(line);
      const button = document.createElement("button");
      button.type = "button";
      button.textContent = label;
      button.title = title;
      button.setAttribute("aria-label", title);
      button.style.setProperty("--axis-color", color);
      button.addEventListener("click", () => {
        const opposite = { right: "left", left: "right", back: "front", front: "back", top: "bottom", bottom: "top" };
        viewer.setView(viewer.viewSelect.value === view ? opposite[view] : view);
      });
      this.element.appendChild(button);
      return { button, line, vector: new THREE.Vector3(...vector), view };
    });
    this.iso = document.createElement("button");
    this.iso.type = "button";
    this.iso.textContent = "ISO";
    this.iso.className = "eplusr-viewer__navigation-iso";
    this.iso.title = "Isometric view";
    this.iso.setAttribute("aria-label", "Isometric view");
    this.iso.addEventListener("click", () => viewer.setView("iso"));
    this.caption = document.createElement("div");
    this.caption.className = "eplusr-viewer__navigation-caption";
    this.element.append(this.iso, this.caption);
    this.collapseButton = document.createElement("button");
    this.collapseButton.type = "button";
    this.collapseButton.className = "eplusr-viewer__navigation-collapse";
    this.collapseButton.addEventListener("click", () => this.setCollapsed(!this.collapsed));
    this.element.appendChild(this.collapseButton);
    viewer.canvasHost.appendChild(this.element);
  }

  // A compact ISO/axes control leaves narrow model views clear while retaining access to full orientation.
  resize(narrow) {
    if (narrow !== this.narrow) { this.narrow = narrow; this.setCollapsed(narrow); }
    this.collapseButton.hidden = !narrow;
    this.element.classList.toggle("eplusr-viewer__navigation--collapsible", narrow);
  }

  // Expand orientation controls on demand without changing the actual camera.
  setCollapsed(collapsed) {
    this.collapsed = collapsed;
    this.element.classList.toggle("eplusr-viewer__navigation--collapsed", collapsed);
    this.collapseButton.textContent = collapsed ? "Axes" : "Less";
    this.collapseButton.setAttribute("aria-label", collapsed ? "Expand compass" : "Collapse compass");
    this.collapseButton.setAttribute("aria-expanded", String(!collapsed));
  }

  // Project the real camera orientation so the compass stays truthful during orbit and projection changes.
  update() {
    const viewer = this.viewer;
    const inverse = viewer.camera.quaternion.clone().invert();
    this.axes.forEach(({ button, line, vector, view }) => {
      const direction = vector.clone().applyQuaternion(inverse);
      const x = 50 + direction.x * 34;
      const y = 50 - direction.y * 34;
      button.style.left = `${x}%`;
      button.style.top = `${y * 0.86 + 10}px`;
      button.style.zIndex = direction.z >= 0 ? "3" : "2";
      button.classList.toggle("eplusr-viewer__axis-back", direction.z < -0.1);
      // Face-on positive and negative axes coincide; show the axis facing the camera.
      button.hidden = Math.hypot(direction.x, direction.y) < 0.25 && direction.z < 0;
      button.setAttribute("aria-pressed", String(viewer.viewSelect.value === view));
      line.setAttribute("x2", String(x));
      line.setAttribute("y2", String(y));
      line.style.opacity = button.hidden ? "0" : direction.z < 0 ? "0.35" : "0.8";
    });
    this.iso.setAttribute("aria-pressed", String(viewer.viewSelect.value === "iso"));
    this.caption.textContent = viewer.viewSelect.selectedOptions[0]?.textContent ?? "Custom";
  }

  // The helper owns only DOM nodes; scene and camera resources stay with the widget.
  dispose() {
    this.element.remove();
  }
}
