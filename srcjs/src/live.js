// Connect a read-only viewer to its owning R session without introducing an App.
export class ViewerConnection {
  // Keep callbacks instance-local so multiple widgets can connect independently.
  constructor(viewer, config) {
    this.viewer = viewer;
    this.config = config;
    this.closed = false;
    this.applying = false;
    this.queue = Promise.resolve();
    this.reportTimer = null;
    this.status = viewer.connectionStatus.root;
    this.setStatus("Connecting to R…", "connecting");
    viewer.toolbar.appendChild(this.status);
    this.onSelection = () => { if (!this.applying) this.report("selection"); };
    this.onChange = () => this.scheduleReport();
    this.onQuery = event => {
      if (this.closed || this.socket.readyState !== WebSocket.OPEN) {
        viewer.queryPanel.fail("R connection is unavailable. The displayed results are unchanged.");
        return;
      }
      this.socket.send(JSON.stringify({type: "query_results", scene_id: config.scene_id, filters: event.detail}));
    };
    this.onPageHide = () => this.dispose();
    viewer.container.addEventListener("eplusr-selection", this.onSelection);
    viewer.root.addEventListener("change", this.onChange);
    viewer.root.addEventListener("click", this.onChange);
    viewer.root.addEventListener("eplusr-frame", this.onChange);
    viewer.root.addEventListener("eplusr-view", this.onChange);
    viewer.root.addEventListener("eplusr-result-query", this.onQuery);
    viewer.controls.addEventListener("end", this.onChange);
    window.addEventListener("pagehide", this.onPageHide);
    this.socket = new WebSocket(config.websocket);
    this.socket.addEventListener("open", () => {
      this.setStatus("Connected to R", "connected");
      this.report();
    });
    this.socket.addEventListener("close", () => {
      this.setStatus("R connection closed", "closed");
      if (viewer.queryPanel.pending) viewer.queryPanel.fail("R connection closed before the query completed. Reopen the live widget to reconnect.");
    });
    this.socket.addEventListener("error", () => {
      this.setStatus("R connection unavailable", "error");
    });
    this.socket.addEventListener("message", (event) => {
      // Serialize asynchronous frame changes so an older command cannot acknowledge newer state.
      this.queue = this.queue.then(() => this.receive(JSON.parse(event.data)))
        .catch((error) => this.report("error", { error: error.message }));
    });
  }

  // Keep the compact status dot and the accessible full connection message in agreement.
  setStatus(label, state) {
    this.viewer.connectionStatus.set(label, state);
  }

  // Coalesce high-frequency UI changes while selection events remain immediate.
  scheduleReport() {
    if (this.closed || this.applying) return;
    window.clearTimeout(this.reportTimer);
    this.reportTimer = window.setTimeout(() => this.report(), 40);
  }

  // Send model IDs and view state; R reconstructs metadata from its own object table.
  report(type = "state", extra = {}) {
    if (this.closed || this.socket.readyState !== WebSocket.OPEN) return;
    const state = this.viewer.readState();
    this.socket.send(JSON.stringify({
      type, scene_id: this.config.scene_id,
      selected_ids: state.selected_ids, state, ...extra
    }));
  }

  // Apply trusted named commands only; the transport never evaluates R-supplied scripts.
  async receive(message) {
    if (this.closed || message.scene_id !== this.config.scene_id) return;
    this.applying = true;
    try {
      const args = message.args ?? {};
      if (message.command === "time") await this.viewer.setTime(args.frame ?? 0);
      else if (message.command === "variable") await this.viewer.setResultVariable(args.index ?? 0);
      else if (message.command === "aggregation") await this.viewer.setAggregation(args.value ?? "frame");
      else if (message.command === "results") await this.viewer.setResults(args.results);
      else this.viewer.applyCommand(message.command, args);
      this.report("ack", { sequence: message.sequence });
    } finally {
      this.applying = false;
    }
  }

  // Remove every listener and timer when an output is replaced or removed from the document.
  dispose() {
    if (this.closed) return;
    this.closed = true;
    window.clearTimeout(this.reportTimer);
    this.viewer.container.removeEventListener("eplusr-selection", this.onSelection);
    this.viewer.root.removeEventListener("change", this.onChange);
    this.viewer.root.removeEventListener("click", this.onChange);
    this.viewer.root.removeEventListener("eplusr-frame", this.onChange);
    this.viewer.root.removeEventListener("eplusr-view", this.onChange);
    this.viewer.root.removeEventListener("eplusr-result-query", this.onQuery);
    this.viewer.controls.removeEventListener("end", this.onChange);
    window.removeEventListener("pagehide", this.onPageHide);
    this.socket.close(1000, "Viewer disposed");
    this.setStatus("R connection closed", "closed");
  }
}
