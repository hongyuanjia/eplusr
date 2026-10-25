import rLogo from "./assets/Rlogo.svg";

// Display the identity of the owning R session, independently of whether it is live.
export class ConnectionStatus {
  // Keep the logo bundled and expose the same state to hover, focus and screen readers.
  constructor() {
    this.root = document.createElement("span");
    this.root.className = "eplusr-viewer__connection";
    this.root.setAttribute("role", "status");
    this.root.tabIndex = 0;
    const image = document.createElement("img");
    image.src = rLogo; image.alt = ""; image.setAttribute("aria-hidden","true");
    image.width = 24; image.height = 19;
    this.text = document.createElement("span");
    this.text.className = "eplusr-viewer__sr-only";
    this.root.append(image,this.text);
    this.set("Static view · not connected to R", "static");
  }

  // Shape distinguishes disconnection; grayscale alone never carries the full meaning.
  set(label, state) {
    this.root.dataset.state = state;
    this.root.dataset.hint = label;
    this.text.textContent = label;
  }
}
