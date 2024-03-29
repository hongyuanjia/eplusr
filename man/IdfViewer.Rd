% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/viewer.R
\docType{class}
\name{IdfViewer}
\alias{IdfViewer}
\alias{idf_viewer}
\title{Visualize an EnergyPlus Model Geometry and Simulation Results}
\usage{
idf_viewer(geometry)
}
\arguments{
\item{geometry}{An \link{IdfGeometry} object. \code{geometry} can also be a
path to an IDF file or an \link{Idf} object. In this case, an
\code{IdfGeometry} is created based on input \link{Idf}.}
}
\value{
An \code{IdfViewer} object.
}
\description{
\code{IdfViewer} is a class designed to view geometry of an \link{Idf} and map
simulation results to the geometries.
}
\examples{

## ------------------------------------------------
## Method `IdfViewer$new`
## ------------------------------------------------

\dontrun{
# example model shipped with eplusr from EnergyPlus v8.8
path_idf <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr") # v8.8

# create from an Idf object
idf <- read_idf(path_idf, use_idd(8.8, "auto"))
viewer <- idf_viewer(idf)
viewer <- IdfViewer$new(idf)

# create from an IDF file
viewer <- idf_viewer(path_idf)
viewer <- IdfViewer$new(path_idf)
}

## ------------------------------------------------
## Method `IdfViewer$parent`
## ------------------------------------------------

\dontrun{
viewer$parent()
}

## ------------------------------------------------
## Method `IdfViewer$geometry`
## ------------------------------------------------

\dontrun{
viewer$geometry()
}

## ------------------------------------------------
## Method `IdfViewer$device`
## ------------------------------------------------

\dontrun{
viewer$device()
}

## ------------------------------------------------
## Method `IdfViewer$background`
## ------------------------------------------------

\dontrun{
viewer$background("blue")
}

## ------------------------------------------------
## Method `IdfViewer$viewpoint`
## ------------------------------------------------

\dontrun{
viewer$viewpoint()
}

## ------------------------------------------------
## Method `IdfViewer$win_size`
## ------------------------------------------------

\dontrun{
viewer$win_size(0, 0, 400, 500)
}

## ------------------------------------------------
## Method `IdfViewer$mouse_mode`
## ------------------------------------------------

\dontrun{
viewer$mouse_mode()
}

## ------------------------------------------------
## Method `IdfViewer$axis`
## ------------------------------------------------

\dontrun{
viewer$axis()
}

## ------------------------------------------------
## Method `IdfViewer$ground`
## ------------------------------------------------

\dontrun{
viewer$ground()
}

## ------------------------------------------------
## Method `IdfViewer$wireframe`
## ------------------------------------------------

\dontrun{
viewer$wireframe()
}

## ------------------------------------------------
## Method `IdfViewer$x_ray`
## ------------------------------------------------

\dontrun{
viewer$x_ray()
}

## ------------------------------------------------
## Method `IdfViewer$render_by`
## ------------------------------------------------

\dontrun{
viewer$render_by()
}

## ------------------------------------------------
## Method `IdfViewer$show`
## ------------------------------------------------

\dontrun{
viewer$show()
}

## ------------------------------------------------
## Method `IdfViewer$focus`
## ------------------------------------------------

\dontrun{
viewer$top()
}

## ------------------------------------------------
## Method `IdfViewer$close`
## ------------------------------------------------

\dontrun{
viewer$close()
}

## ------------------------------------------------
## Method `IdfViewer$snapshot`
## ------------------------------------------------

\dontrun{
viewer$show()
viewer$snapshot(tempfile(fileext = ".png"))
}


## ------------------------------------------------
## Method `IdfViewer$print`
## ------------------------------------------------

\dontrun{
viewer$print()
}
}
\seealso{
\link{IdfGeometry} class
}
\author{
Hongyuan Jia
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-IdfViewer-new}{\code{IdfViewer$new()}}
\item \href{#method-IdfViewer-parent}{\code{IdfViewer$parent()}}
\item \href{#method-IdfViewer-geometry}{\code{IdfViewer$geometry()}}
\item \href{#method-IdfViewer-device}{\code{IdfViewer$device()}}
\item \href{#method-IdfViewer-background}{\code{IdfViewer$background()}}
\item \href{#method-IdfViewer-viewpoint}{\code{IdfViewer$viewpoint()}}
\item \href{#method-IdfViewer-win_size}{\code{IdfViewer$win_size()}}
\item \href{#method-IdfViewer-mouse_mode}{\code{IdfViewer$mouse_mode()}}
\item \href{#method-IdfViewer-axis}{\code{IdfViewer$axis()}}
\item \href{#method-IdfViewer-ground}{\code{IdfViewer$ground()}}
\item \href{#method-IdfViewer-wireframe}{\code{IdfViewer$wireframe()}}
\item \href{#method-IdfViewer-x_ray}{\code{IdfViewer$x_ray()}}
\item \href{#method-IdfViewer-render_by}{\code{IdfViewer$render_by()}}
\item \href{#method-IdfViewer-show}{\code{IdfViewer$show()}}
\item \href{#method-IdfViewer-focus}{\code{IdfViewer$focus()}}
\item \href{#method-IdfViewer-close}{\code{IdfViewer$close()}}
\item \href{#method-IdfViewer-snapshot}{\code{IdfViewer$snapshot()}}
\item \href{#method-IdfViewer-print}{\code{IdfViewer$print()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-new"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-new}{}}}
\subsection{Method \code{new()}}{
Create an \code{IdfViewer} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$new(geometry)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{geometry}}{An \link{IdfGeometry} object. \code{geometry} can also be a
path to an IDF file or an \link{Idf} object. In this case, an
\code{IdfGeometry} is created based on input \link{Idf}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An \code{IdfViewer} object.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
# example model shipped with eplusr from EnergyPlus v8.8
path_idf <- system.file("extdata/1ZoneUncontrolled.idf", package = "eplusr") # v8.8

# create from an Idf object
idf <- read_idf(path_idf, use_idd(8.8, "auto"))
viewer <- idf_viewer(idf)
viewer <- IdfViewer$new(idf)

# create from an IDF file
viewer <- idf_viewer(path_idf)
viewer <- IdfViewer$new(path_idf)
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-parent"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-parent}{}}}
\subsection{Method \code{parent()}}{
Get parent \link{Idf} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$parent()}\if{html}{\out{</div>}}
}

\subsection{Details}{
\verb{$parent()} returns the parent \link{Idf} object of current \code{IdfGeometry}
object.
}

\subsection{Returns}{
An \link{Idf} object.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$parent()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-geometry"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-geometry}{}}}
\subsection{Method \code{geometry()}}{
Get parent \link{IdfGeometry} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$geometry()}\if{html}{\out{</div>}}
}

\subsection{Details}{
\verb{$geometry()} returns the parent \link{IdfGeometry} object.
}

\subsection{Returns}{
An \link{IdfGeometry} object.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$geometry()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-device"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-device}{}}}
\subsection{Method \code{device()}}{
Get Rgl device ID
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$device()}\if{html}{\out{</div>}}
}

\subsection{Details}{
If Rgl is used, the Rgl device ID is returned. If WebGL is
used, the \code{elementID} is returned. If no viewer has been open, \code{NULL}
is returned.
}

\subsection{Returns}{
A number or \code{NULL}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$device()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-background"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-background}{}}}
\subsection{Method \code{background()}}{
Set the background color of the scene
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$background(color = "white")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{color}}{A single string giving the background color. Default:
\code{white}.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$background("blue")
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-viewpoint"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-viewpoint}{}}}
\subsection{Method \code{viewpoint()}}{
Set the viewpoint orientation of the scene
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$viewpoint(
  look_at = "iso",
  theta = NULL,
  phi = NULL,
  fov = NULL,
  zoom = NULL,
  scale = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{look_at}}{A single string indicating a standard view. If
specified, \code{theta} and \code{phi} will be ignored. Should be
one of \code{c("top", "bottom", "left", "right", "front", "back", "iso")}. \code{look_at} will be ignored if any of \code{theta} and \code{phi}
is specified. Default: \code{iso} (i.e. isometric).}

\item{\code{theta}}{Theta in polar coordinates. If \code{NULL}, no changes will
be made to current scene. Default: \code{NULL}.}

\item{\code{phi}}{Phi in polar coordinates. If \code{NULL}, no changes will be
made to current scene. Default: \code{NULL}.}

\item{\code{fov}}{Field-of-view angle in degrees. If \code{0}, a parallel or
orthogonal projection is used. If \code{NULL}, no changes will
be made to current scene. Default: \code{NULL}.}

\item{\code{zoom}}{Zoom factor. If \code{NULL}, no changes will be made to
current scene. Default: \code{NULL}.}

\item{\code{scale}}{A numeric vector of length 3 giving the rescaling to
apply to each axis. If \code{NULL}, no changes will be made to
current scene. Default: \code{NULL}.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$viewpoint()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-win_size"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-win_size}{}}}
\subsection{Method \code{win_size()}}{
Set the window size
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$win_size(left = 0, top = 0, right = 600, bottom = 600)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{left, top, right, bottom}}{A single number indicating the pixels of
the displayed window. Defaults: \code{0} (\code{left}), \code{0} (\code{top}),
\code{600} (\code{right}) and \code{600} (\code{bottom}).}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$win_size(0, 0, 400, 500)
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-mouse_mode"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-mouse_mode}{}}}
\subsection{Method \code{mouse_mode()}}{
Set the handlers of mouse control
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$mouse_mode(
  left = "trackball",
  right = "pan",
  middle = "fov",
  wheel = "pull"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{left, right, middle}}{Refer to the buttons on a three button
mouse, or simulations of them on other mice. Defaults:
\code{"trackball"} (\code{left}), \code{"pan"} (\code{right}) and \code{"fov"}
(\code{middle}).}

\item{\code{wheel}}{Refer to the mouse wheel. Default: \code{"pull"}.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Possible values are:\tabular{ll}{
   Mode \tab Description \cr
   "none" \tab No action \cr
   "trackball" \tab The mouse acts as a virtual trackball. Clicking and dragging rotates the scene \cr
   "xAxis", "yAxis", "zAxis" \tab Like "trackball", but restricted to rotation about one axis \cr
   "polar" \tab The mouse affects rotations by controlling polar coordinates directly \cr
   "zoom" \tab The mouse zooms the display \cr
   "fov" \tab The mouse affects perspective by changing the field of view \cr
   "pull" \tab Rotating the mouse wheel towards the user “ pulls the scene closer” \cr
   "push" \tab The same rotation “pushes the scene away” \cr
   "pan" \tab Pan the camera view vertically or horizontally \cr
}
}

\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$mouse_mode()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-axis"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-axis}{}}}
\subsection{Method \code{axis()}}{
Toggle axis in the scene
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$axis(
  add = TRUE,
  expand = 2,
  width = 1.5,
  color = c("red", "green", "blue", "orange"),
  alpha = 1
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{add}}{If \code{TRUE}, axis is added to the scene. If \code{FALSE}, axis is
removed in the scene.}

\item{\code{expand}}{A single number giving the factor to expand based on
the largest X, Y and Z coordinate values. Default: \code{2.0}.}

\item{\code{width}}{A number giving the line width of axis. \code{width * 2} is
used for the true north axis. Default: \code{1.5}.}

\item{\code{color}}{A character of length 4 giving the color of X, Y, Z and
true north axis. Default: \code{c("red", "green", "blue", "orange")}.}

\item{\code{alpha}}{A number giving the alpha value of axis. Default: \code{1.0}.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
\verb{$axis()} adds or removes X, Y and Z axis in the scene.
}

\subsection{Returns}{
A single logical value as \code{add}.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$axis()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-ground"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-ground}{}}}
\subsection{Method \code{ground()}}{
Toggle ground in the scene
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$ground(add = TRUE, expand = 1.02, color = "#EDEDEB", alpha = 1)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{add}}{If \code{TRUE}, ground is added to the scene. If \code{FALSE},
ground is removed in the scene.}

\item{\code{expand}}{A single number giving the factor to expand based on
the largest X, Y and Z coordinate values. Default: \code{1.02}.}

\item{\code{color}}{A string giving the color of ground. Default: \verb{#EDEDEB}.}

\item{\code{alpha}}{A number giving the alpha value of ground. Default: \code{1.0}.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
\verb{$ground()} adds or removes ground in the scene.
}

\subsection{Returns}{
A single logical value as \code{add}.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$ground()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-wireframe"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-wireframe}{}}}
\subsection{Method \code{wireframe()}}{
Toggle wireframe
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$wireframe(add = TRUE, width = 1.5, color = "black", alpha = 1)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{add}}{If \code{TRUE}, wireframe is turned on. If \code{FALSE}, wireframe
is turned off. Default: \code{TRUE}.}

\item{\code{width}}{A number giving the line width of axis. Default: \code{1.5}.}

\item{\code{color}}{A character of length 3 giving the color of X, Y and Z
axis. Default: \code{c("red", "green", "blue")}.}

\item{\code{alpha}}{A number giving the alpha value of axis. Default: \code{1.0}.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
\verb{$wireframe()} turns on/off wireframes.
}

\subsection{Returns}{
A single logical value as \code{add}.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$wireframe()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-x_ray"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-x_ray}{}}}
\subsection{Method \code{x_ray()}}{
Toggle X-ray face style
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$x_ray(on = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{on}}{If \code{TRUE}, X-ray is turned on. If \code{FALSE}, X-ray is turned
off. Default: \code{TRUE}.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
\verb{$x_ray()} turns on/off X-ray face style.
}

\subsection{Returns}{
A single logical value as \code{on}.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$x_ray()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-render_by"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-render_by}{}}}
\subsection{Method \code{render_by()}}{
Set render style
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$render_by(type = "surface_type")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{type}}{A single string giving the render style. Should be one
of:
\itemize{
\item \code{"surface_type"}: Default. Render the model by surface type
model. Walls, roofs, windows, doors, floors, and shading
surfaces will have unique colors.
\item \code{"boundary"}: Render the model by outside boundary condition.
Only surfaces that have boundary conditions will be rendered
with a color. All other surfaces will be white.
\item \code{"construction"}: Render the model by surface constructions.
\item \code{"zone"}: Render the model by zones assigned.
\item \code{"space"}: Render the model by spaces assigned.
\item \code{"normal"}: Render the model by surface normal. The outside
face of a heat transfer face will be rendered as white and the
inside face will be rendered as red.
}}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
\verb{$render_by()} sets the render style of geometries.
}

\subsection{Returns}{
A same value as \code{style}.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$render_by()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-show"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-show}{}}}
\subsection{Method \code{show()}}{
Show \link{Idf} geometry
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$show(
  type = "all",
  zone = NULL,
  space = NULL,
  surface = NULL,
  width = 1.5,
  dayl_color = "red",
  dayl_size = 5
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{type}}{A character vector of geometry components to show. If
\code{"all"} (default), all geometry components will be shown. If
\code{NULL}, no geometry faces will be shown. Otherwise, should be
a subset of following:
\itemize{
\item \code{"floor"}
\item \code{"wall"}
\item \code{"roof"}
\item \code{"window"}
\item \code{"door"}
\item \code{"shading"}
\item \code{"daylighting"}
}}

\item{\code{zone}}{A character vector of names or an integer vector of IDs
of zones in current \link{Idf} to show. If \code{NULL}, no subsetting is
performed.}

\item{\code{space}}{A character vector of names or an integer vector of IDs
of spaces in current \link{Idf} to show. If \code{NULL}, no subsetting is
performed.}

\item{\code{surface}}{A character vector of names or an integer vector of IDs
of surfaces in current \link{Idf} to show. If \code{NULL}, no subsetting
is performed.}

\item{\code{width}}{The line width for the geometry components. Default:
\code{1.5}.}

\item{\code{dayl_color, dayl_size}}{The color and size of daylighting
reference points. Defaults: \code{"red"} (\code{dayl_color}) and \code{5}
(\code{dayl_size}).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The \code{IdfViewer} itself, invisibly.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$show()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-focus"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-focus}{}}}
\subsection{Method \code{focus()}}{
Bring the scene window to the top
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$focus()}\if{html}{\out{</div>}}
}

\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$top()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-close"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-close}{}}}
\subsection{Method \code{close()}}{
Close the scene window
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$close()}\if{html}{\out{</div>}}
}

\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-snapshot"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-snapshot}{}}}
\subsection{Method \code{snapshot()}}{
Capture and save current rgl view as an image
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$snapshot(filename, webshot = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{filename}}{A single string specifying the file name. Current
supported formats are \code{png}, \code{pdf}, \code{svg}, \code{ps}, \code{eps}, \code{tex}
and \code{pgf}.}

\item{\code{webshot}}{Whether to use the 'webshot2' package to take the
snapshot. For more details, please see \code{\link[rgl:snapshot]{rgl::snapshot3d()}}.
Default: \code{FALSE}.}

\item{\code{...}}{Arguments to pass to \code{webshot2::webshot()}.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
\verb{$snapshot()} captures the current rgl view and saves it as an image
file to disk using \code{\link[rgl:snapshot]{rgl::snapshot3d()}} and \code{\link[rgl:postscript]{rgl::rgl.postscript()}}.
}

\subsection{Returns}{
A single string of the file path.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$show()
viewer$snapshot(tempfile(fileext = ".png"))
}

}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-IdfViewer-print"></a>}}
\if{latex}{\out{\hypertarget{method-IdfViewer-print}{}}}
\subsection{Method \code{print()}}{
Print an \code{IdfViewer} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IdfViewer$print()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The \code{IdfViewer} itself, invisibly.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
viewer$print()
}
}
\if{html}{\out{</div>}}

}

}
}
