import { defineConfig } from "vite";
import { resolve } from "node:path";
import { copyFileSync } from "node:fs";

// Build a self-contained browser runtime so installed R packages never need Node.js.
export default defineConfig({
  plugins: [{
    name: "viewer-icon-license",
    // Preserve the original R logo and icon licenses after Vite clears its output folder.
    closeBundle() {
      copyFileSync(resolve(import.meta.dirname, "src/assets/Rlogo.svg"),
        resolve(import.meta.dirname, "../inst/htmlwidgets/lib/Rlogo.svg"));
      copyFileSync(resolve(import.meta.dirname, "src/assets/LICENSE-Rlogo.txt"),
        resolve(import.meta.dirname, "../inst/htmlwidgets/lib/LICENSE-Rlogo.txt"));
      copyFileSync(
        resolve(import.meta.dirname, "node_modules/lucide/LICENSE"),
        resolve(import.meta.dirname, "../inst/htmlwidgets/lib/LICENSE-lucide.txt")
      );
    }
  }],
  build: {
    outDir: resolve(import.meta.dirname, "../inst/htmlwidgets/lib"),
    emptyOutDir: true,
    sourcemap: false,
    minify: "esbuild",
    lib: {
      entry: resolve(import.meta.dirname, "src/index.js"),
      name: "EplusrViewer",
      formats: ["iife"],
      fileName: () => "eplusr-viewer.js",
      cssFileName: "eplusr-viewer"
    }
  }
});
