HTMLWidgets.widget({
  name: "idfviewer",
  type: "output",

  // Own exactly one renderer and optional R connection per output element.
  factory: function(el) {
    let viewer = null;
    let connection = null;
    let observer = null;

    // Release resources on both re-render and actual removal from the document.
    function dispose() {
      observer?.disconnect();
      observer = null;
      connection?.dispose();
      connection = null;
      viewer?.dispose();
      viewer = null;
      el.eplusrViewer = null;
    }

    return {
      // Full payload replacement is reserved for a new scene; R commands are incremental.
      renderValue: function(x) {
        dispose();
        viewer = new window.EplusrIdfViewer(el);
        el.eplusrViewer = viewer;
        viewer.load(x);
        viewer.resize();
        if (x.live) connection = new window.EplusrViewerConnection(viewer, x.live);
        observer = new MutationObserver(function() {
          if (!el.isConnected) dispose();
        });
        observer.observe(document.body, { childList: true, subtree: true });
      },

      // Let htmlwidgets handle host sizing and forward the resulting dimensions.
      resize: function() { viewer?.resize(); }
    };
  }
});
