# eplusr 3D viewer sources

This directory contains the JavaScript source and build configuration for the
Three.js-based IDF viewer. The viewer is read-only and has no App, CAD, OCCT or simulation runner.
The generated browser assets remain under
`inst/htmlwidgets/`, where R packages expect installed htmlwidget resources.

## Setup

Install the pinned dependencies from this directory:

```sh
pnpm install --frozen-lockfile
```

## Build

Create the production JavaScript and CSS bundles:

```sh
pnpm build
```

The build writes `eplusr-viewer.js` and `eplusr-viewer.css` to
`../inst/htmlwidgets/lib/`.

For development with automatic rebuilds, run:

```sh
pnpm dev
```
