# Read-only IDF viewer

The R package has one Three.js/htmlwidgets runtime. It contains no bemgeo, OCCT, independent App or simulation controls.

Build browser assets with `pnpm install --frozen-lockfile` and `pnpm build` in `srcjs/`. End users do not need Node.js. R viewing requires the suggested `htmlwidgets` and `decido` packages; live connections also require `httpuv` and `later`.

In R, use `viewer <- idf_viewer(idf)` and print `viewer$widget(live = TRUE)`. The R object owns its loopback listener; use `viewer$close()` to release it. Portable reports use `viewer$widget()`.

## Querying an existing job or SQL file

```r
view <- idf_viewer(idf)
view$results()                       # uses idf$last_job()
# Equivalent explicit sources:
# view$results(idf$last_job())
# view$results(job)                  # completed EplusJob
# view$results(sql)                  # EplusSql
view$widget(live = TRUE)
```

This attaches existing output; a missing last job is an error. The Results tab queries the original source through its owning R session, so it can widen as well as narrow the initial R selectors. Apply result query replaces the numerical result set only after a successful query. A failed or empty query preserves the previous colors, entered criteria, geometry visibility, selection and camera.

Both R and the webpage support `name` (`variable` is an alias), `key_value`, `year`, `tz`, `period`, `month`, `day`, `hour`, `minute`, `interval`, `simulation_days`, `day_type`, `environment_name`, and `reporting_frequency`. These reuse the `EplusSql$report_data()` filters. `period` selects exact dates or date-times: two endpoints are not an inclusive range. Integer controls accept comma-separated values and literal ranges such as `8:18`. Result keys select values for coloring; use geometry filters to hide objects.

A `key_value/name` dictionary data frame passed from R keeps its pair-wise meaning until keys or variables are edited in the browser. `case`, `all` and `wide` format output tables and have no 3D equivalent. The variable catalog lists outputs with keys matching this model. Portable widgets retain the attached frames, with source query controls disabled because querying other source results requires a live R connection.

Run focused tests with `devtools::test(filter = "viewer")`. When checking an older installed EnergyPlus fixture set, set `EPLUSR_VIEWER_TEST_VERSION` to that version; tests otherwise use the package's latest supported version. The tests read examples and existing result fixtures; they do not run EnergyPlus.

## Inspection layout

See [DESIGN.md](../../DESIGN.md) for the accepted interaction and icon rules. The compact top ribbon groups commands by task, with a collapsible left model tree and one on-demand right task pane. Filter chips and counts reflect the same effective criteria as R. Time controls are opt-in, and narrow widgets use one temporary drawer at a time plus horizontally scrollable commands. Multi-selection properties show shared or mixed metadata, with individual details under “Inspect one selected object”.

The browser bundle includes only the used Lucide SVGs and its license; no runtime icon CDN or icon font is required.

## Spatial tools and result ranges

The viewport compass provides six orthogonal directions, ISO, and true north. Projection is explicit. Fit model and Fit selection use separate toolbar actions. Measurement shows a line and distance beside the model; Escape leaves a completed dimension visible until Clear measurement.

Result fills remain quantitative during hover and selection. The Results tab exposes All frames, Current frame, and Manual range policies, with Min/Max, percentile coverage, and a consistent legend. Manual bounds belong to each variable. Streamed global percentiles are unavailable because only global extrema are supplied; choose Current frame for its percentile distribution.

R can control the same behavior with `viewer$update("projection", value = "orthographic")`, `viewer$update("fit_model")`, `viewer$update("fit_selection")`, `viewer$update("range_mode", value = "frame")`, and `viewer$update("range", minimum = 18, maximum = 30)`. Read `viewer$state()$view` for the current tool, completed measurement, projection, view, range policy, and bounds. The listener owns and closes its sockets; stopping one viewer leaves other widgets running.

## Existing EnergyPlus results example

From the package source root, run `source("tools/viewer/real-results.R")` in an R console. It loads this worktree and opens a live widget with real hourly zone temperatures from a completed EnergyPlus 23.1 large-office run. The example includes Zone filtering, multi-selection, frame changes and playback. [Source files and provenance](examples/large-office/README.md) accompany the [R script](real-results.R). This is one summer design day with 24 hourly frames, not an annual run. No simulation is started.

## Ribbon workspace

Model groups fit, filtering, selection and appearance commands. View groups camera, saved views, display and geometry tools. Results contains commands for Result data, Color scale, Time settings and Timeline; its forms live in the right task pane. Snapshot opens a PNG preview with actual pixel dimensions and a Download PNG link. Only one task opens at a time. Selection updates properties without opening a task. Closing or switching tasks preserves draft input and each task's scroll position.

The left Model browser follows Zone > Space (where present) > surfaces. Tree search does not filter the geometry. Click a Zone or surface to select its visible members; Ctrl/Command adds, Shift ranges between visible surfaces, and H toggles visibility. Arrow keys navigate and expand branches. Eye controls respect existing filters instead of silently clearing them. Closing the browser returns its canvas space.

The time strip is opt-in through Results > Timeline. Closing it pauses playback and retains the frame and colors. Model only hides all chrome and pauses; Show controls or Escape restores the workspace and its open task. Default window appearance is transparent glass; Model > Appearance or Results > Glass / color by can switch to Opaque colors. On desktop, open panes occupy real columns; below 1100 CSS pixels, the model browser and task pane become mutually exclusive drawers. The widget's live session restores the latest applied result metadata on browser refresh so time labels and color bounds agree with provider frames.

The CUA regression module is tools/viewer/check-workspace.cua.mjs. Pass the host's existing tab, viewport capability, filesystem module and an evidence output directory. It uses CUA locator interactions and read-only DOM assertions; it does not inject changes into the viewer runtime.

Name search selects matching visible objects and respects current Zone/component filters. Empty matches preserve the current selection and identify the search field. The existing R `update("panel", hidden = ...)` command now controls the transient property inspector.

## Icon-first controls

Top commands use semantic icons with hover and keyboard-focus hints. The R logo identifies the live session: normal when connected, gray with a slash when disconnected, and gray without a slash for static browsing. Navigation help opens through the question-mark icon or ? key. The current result time stays in the color legend when playback controls are hidden. Section plane and Exploded view open separate tasks; existing R clipping and explode commands remain unchanged.

The icon-specific CUA regression module is tools/viewer/check-icon-controls.cua.mjs, using the same host tab/viewport/filesystem arguments as the workspace regression. The official R SVG and its license/source notice are in srcjs/src/assets; the build also retains the notice with the installed htmlwidget resources.

## Hierarchical selection and local preview

Run `Rscript --vanilla tools/viewer/serve-real-results.R` from the package source root to serve the archived real-result example at port 40680. This is a preview harness for the R htmlwidget. No simulation is started.

Double-click a surface to select its visible Zone. Double-click again inside that Zone to inspect a face. Further double-clicks remain at face level until the context changes. Another Zone starts at Zone level. Single-click uses the explicit picking mode; Control/Command-click adds/removes selection. Double-click no longer isolates. Use Isolate or I explicitly. The tree is the keyboard alternative for choosing Zones and surfaces.
