# IDF viewer interaction and appearance

## Purpose and scope

Inspect existing EnergyPlus geometry, select and filter model objects, and color them using existing results. The R htmlwidget remains the entry point. This interface does not edit geometry or run simulations. The model is the main visual content.

## Confirmed direction

On 2026-10-19 the user selected the SolidCraft-inspired workspace: compact top commands, a left model tree, and a right on-demand task pane. This supersedes the earlier two-row Office ribbon and floating/modal editors. Keep the R htmlwidget entry point and the existing light engineering-tool appearance. (User confirmed.)

- Model, View and Results tabs contain commands, not forms. Each group has one 30-pixel command row and a shared caption baseline. Tabs support Arrow keys, Home and End. Explicit scroll arrows expose overflowing commands; Collapse retains the tabs. (Agent implementation.)
- The desktop model browser occupies the left grid column and the currently requested task occupies the right column. Closing either gives space back to the canvas. Below 1100 CSS pixels, panes become temporary drawers; opening one closes the other. No task panel opens automatically on object selection. (User direction, agent implementation.)
- The model tree follows actual Zone, optional Space and surface data. Search changes tree matches only. Click selects visible members, Ctrl/Command adds, Shift selects a range of visible leaves, and H toggles visibility. Disclosure is independent of selection; standard tree arrow keys move focus. Counts, selections and eyes synchronize with viewport picks and R commands. An eye cannot silently clear other filters. (Agent implementation.)
- Result data, color mapping, camera, appearance, properties, saved views and spatial tools share one right task pane. Each task keeps its original controls, draft and scroll container. Closing or switching tasks preserves them. Escape first closes an open option list, then the task; closing returns focus to its visible launcher. (User direction, agent implementation.)
- Keep the timeline hidden by default. Results > Timeline opts in; Hide timeline pauses and retains the current frame and colors. R playback opens it. Model only hides commands, model tree, task pane, status, timeline, compass and legend, pauses playback, and leaves Show controls. Escape restores the prior workspace, including draft and open task. (User confirmed direction, agent implementation.)
- Show effective spatial restrictions as removable footer chips, plus visible and selected counts. Single R filters appear in their native controls; multi-value filters have an explicit summary. Browser changes replace the corresponding criterion and report to R. (Agent implementation.)
- Snapshot opens a right task showing the current camera PNG, its dimensions and an explicit download link. (Agent implementation.)
- The loaded variable, palette and range controls live in Result color scale; aggregation and time jump live in Time settings. These tasks remain available with the timeline hidden. A static frame disables stepping/play, and a viewer without results has no empty timeline. (Agent implementation.)
- For multiple objects, summarize common metadata, explicitly label mixed fields, and separate the active member's details in a disclosure. Face area sums rendered triangle areas, deducts openings, and counts shared boundaries per IDF surface. Result object means are unweighted means across selected display objects.

## Result inspection and spatial tools

The user accepted the next four improvements on 2026-10-18: preserve quantitative result fills, clarify measurement and tool feedback, improve spatial navigation, and make result legends explicit.

- Use unlit quantitative fills so camera lighting does not change mapped result colors. Selection uses an orange boundary and hover a blue boundary; neither replaces the quantitative fill. A probe includes the numerical value, display unit, variable, and active time. Canonical result metadata stays unchanged; display units use °C and common superscripts.
- Keep the active tool visible at the upper left of the canvas. Two real surface picks create a line and a projected distance label. Escape exits measurement while preserving a completed dimension and the selection; restart and clear are separate actions. Exploded distances are identified as display distances, and changing explosion clears the old measurement. Picking respects clipping.
- Keep a compact, camera-synchronized compass at the upper right. Its axes and ISO are native buttons; the current standard view is selected, orbiting changes it to Custom, and an active face-on axis can flip to its opposite. Perspective and orthographic projection preserve target and apparent scale. Fit model and Fit selection are separate actions. The canvas creates a stacking context so its overlays remain below a narrow panel drawer.
- Show five numeric legend ticks, units, missing-value color, and the active range policy. All frames fixes the original bounds over time; Current frame follows the displayed frame; Manual preserves explicit bounds per variable through time and aggregation changes. Invalid bounds preserve the previous display and identify the invalid fields. Percentile coverage is explicit; streamed results offer it for the current frame because their provider exposes global extrema rather than a global distribution. Pending streamed frames show missing values until the matching data arrives.
- Keep existing short panel/tab/action transitions and reduced-motion support. Camera direction changes are immediate and deterministic. Measurement dimensions track the camera without added entrance motion. Avoid animating result colors because intermediate colors would imply values that were not supplied.

## Icon and control rules

On 2026-10-19 the user requested icon-first commands with hover explanations and a recognizable R-language connection badge. This supersedes the earlier text-bearing command strip. (User confirmed.)

- Top commands and quick access use icons with full accessible names. Model/View/Results tabs, task headings, object/source names, numeric values, units, filter chips and counts remain text. Apply and Reset retain short text in the task form. Group captions remain compact. (User direction, agent implementation.)
- Use bundled Lucide symbols, with matching-stroke custom symbols for model focus, section plane, glazing and timeline. Components, appearance, camera, data source, palette and time have distinct glyphs. Section plane and Exploded view are separate commands/tasks, retaining each control instance and its state. (Agent implementation.)
- One widget-local hint handles pointer hover and keyboard focus. Hints remain readable when hovered, stay within the widget, and Escape dismisses the hint before the panel or model shortcuts. Buttons retain accessible names when visible labels are clipped. Removing a widget releases hint listeners/timers. (Agent implementation.)
- The R badge uses the original bundled R Foundation SVG, with its GPL-2 notice. Connected shows the normal logo; connecting is gray with a progress ring; closed/error is gray with a separate slash; a static widget is gray without the slash and explicitly identified in its hint. No permanent Connected to R sentence or green dot. (User direction, agent implementation.)
- A single chosen source value needs no repeated 1 selected count. Larger selections retain their count and full removable tags. Clear is an icon with an explanation. Routine query success is a check mark with accessible status text; draft, loading, disconnected and error messages stay explicit. (Agent implementation.)
- Idle surface inspection has no permanent mouse tutorial. Navigation help is available through the question-mark command and ? key. Measurement, clipping, explosion and non-default picking modes keep their active feedback. Model-only restore is an icon at the upper right. (Agent implementation.)
- Result color legends show the current time even while the time strip is hidden. Semantic labels such as missing values and transparent glass remain visible. (User-approved analysis, agent implementation.)

## Visual rules

Keep the system sans-serif, light gray canvas, white controls, neutral borders, restrained panel shadow, and orange selection outlines and a blue active-tab indicator. Use layout and grouping to establish hierarchy. Panel movement, tab changes, and action-state feedback use short consistent transitions; reduced-motion disables them. Avoid introducing a separate dashboard or application shell.

## Verification targets

Use real IDF geometry and real R/browser commands. Validate R-to-control synchronization, control-to-R synchronization, filter removal, no selection loss when inspecting an active member, playback, keyboard tabs, independent embedded widgets, no-results and static-results states. Inspect desktop, 760-pixel, 480-pixel, and short IDE containers. Demonstration result values used by the preview harness are synthetic and are not EnergyPlus simulation output.

Glazed Window and GlassDoor objects use neutral transparent glass by default, including Results mode. Opaque colors restores quantitative window colors. Glass has a separate legend entry and is explicitly excluded from color mapping. Door objects remain opaque. Boundary strokes adapt to the adjacent fill/background so the darkest result colors retain visible frames; this changes neither source geometry nor result values. (User confirmation and agent implementation.)

For hosts below 500 CSS pixels high, initially collapse the command band. A user can expand it through a tab or the explicit Expand action. A central viewport below 600 CSS pixels wide, or a short host, docks its color key below the canvas and starts with compact Axes/ISO navigation. Closing the timeline restores focus to Results. (Agent implementation.)

Workspace source: srcjs/src/ribbon.js, model-browser.js and workspace.css. The earlier editor/dialog and two-track ribbon CSS is removed. Existing neutral surface/border/text variables and bundled Lucide symbols remain the visual system. Task changes, drawer entry and active tab feedback use short transitions; reduced-motion disables these. (Agent implementation.)
