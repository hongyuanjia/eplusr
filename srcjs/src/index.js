import { ViewerConnection } from "./live.js";
import { IdfViewer } from "./viewer.js";
import "./viewer.css";
import "./workspace.css";

// Expose only the domain viewer to the htmlwidgets binding; Three.js stays bundled.
window.EplusrIdfViewer = IdfViewer;

window.EplusrViewerConnection = ViewerConnection;
