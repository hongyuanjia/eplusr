// Provide one instance-local hover/focus tooltip without browser-specific interest APIs.
export class CommandHints {
  // Delegate events so rebuilt selection controls keep the same accessible hint behavior.
  constructor(root, id) {
    this.root = root;
    this.tip = document.createElement("div");
    this.tip.id = id;
    this.tip.className = "eplusr-viewer__command-hint";
    this.tip.setAttribute("role", "tooltip");
    this.tip.hidden = true;
    root.append(this.tip);
    this.listeners = {
      pointerover: event => {
        const target = event.target.closest("[data-hint]");
        if (target && target !== this.target) this.schedule(target, 350);
        if (this.tip.contains(event.target)) window.clearTimeout(this.leaveTimer);
      },
      pointerout: event => {
        if (this.target?.contains(event.relatedTarget) || this.tip.contains(event.relatedTarget)) return;
        if (this.target?.contains(document.activeElement) && document.activeElement.matches(":focus-visible")) return;
        this.leaveTimer = window.setTimeout(() => this.hide(), 120);
      },
      focusin: event => {
        const target = event.target.closest("[data-hint]");
        if (target && target.matches(":focus-visible")) this.schedule(target, 0);
      },
      focusout: () => this.hide(),
      pointerdown: () => this.hide(),
      scroll: () => this.hide(),
      keydown: event => {
        if (event.key !== "Escape" || this.tip.hidden) return;
        event.preventDefault(); event.stopImmediatePropagation(); this.hide();
      }
    };
    for (const [name, callback] of Object.entries(this.listeners)) root.addEventListener(name, callback, true);
  }

  // Delay pointer hints while keyboard focus receives an immediate explanation.
  schedule(target, delay) {
    this.hide();
    this.target = target;
    this.timer = window.setTimeout(() => this.show(target), delay);
  }

  // Clamp the tooltip to this widget, including small embedded RStudio viewers.
  show(target) {
    if (!target.isConnected || !target.getClientRects().length || target !== this.target) return;
    this.tip.textContent = target.dataset.hint;
    this.tip.hidden = false;
    const descriptions = (target.getAttribute("aria-describedby") ?? "").split(/\s+/).filter(Boolean);
    if (!descriptions.includes(this.tip.id)) descriptions.push(this.tip.id);
    target.setAttribute("aria-describedby", descriptions.join(" "));
    const rect = target.getBoundingClientRect(), root = this.root.getBoundingClientRect();
    const width = this.tip.offsetWidth, height = this.tip.offsetHeight;
    this.tip.style.left = Math.max(5,Math.min(root.width-width-5,rect.left-root.left+(rect.width-width)/2)) + "px";
    this.tip.style.top = Math.max(5,Math.min(root.height-height-5,
      rect.bottom-root.top+8+height <= root.height ? rect.bottom-root.top+8 : rect.top-root.top-height-8)) + "px";
  }

  // Remove only this tooltip's description, preserving pre-existing accessibility text.
  hide() {
    window.clearTimeout(this.timer); window.clearTimeout(this.leaveTimer);
    if (this.target) {
      const descriptions = (this.target.getAttribute("aria-describedby") ?? "").split(/\s+/).filter(id => id && id !== this.tip.id);
      if (descriptions.length) this.target.setAttribute("aria-describedby", descriptions.join(" "));
      else this.target.removeAttribute("aria-describedby");
    }
    this.target = null; this.tip.hidden = true;
  }

  // Detached widgets release delegated handlers and both pending timers.
  dispose() {
    this.hide();
    for (const [name, callback] of Object.entries(this.listeners)) this.root.removeEventListener(name,callback,true);
    this.tip.remove();
  }
}
