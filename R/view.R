# COLOR_MAP {{{
# Reference: 'openstudio\openstudiocore\ruby\openstudio\sketchup_plugin\lib\interfaces\MaterialsInterface.rb'
COLOR_MAP <- list(
    surface_type = c(
        undefined = grDevices::rgb(255/255, 255/255, 255/255, 1),
        normalmaterial = grDevices::rgb(255/255, 255/255, 255/255, 1),
        normalmaterial_ext = grDevices::rgb(255/255, 255/255, 255/255, 1),
        normalmaterial_int = grDevices::rgb(255/255, 0/255, 0/255, 1),
        floor = grDevices::rgb(128/255, 128/255, 128/255, 1),
        floor_ext = grDevices::rgb(128/255, 128/255, 128/255, 1),
        floor_int = grDevices::rgb(191/255, 191/255, 191/255, 1),
        wall = grDevices::rgb(204/255, 178/255, 102/255, 1),
        wall_ext = grDevices::rgb(204/255, 178/255, 102/255, 1),
        wall_int = grDevices::rgb(235/255, 226/255, 197/255, 1),
        roof = grDevices::rgb(153/255, 76/255, 76/255, 1),
        roof_ext = grDevices::rgb(153/255, 76/255, 76/255, 1),
        roof_int = grDevices::rgb(202/255, 149/255, 149/255, 1),
        ceiling = grDevices::rgb(153/255, 76/255, 76/255, 1),
        ceiling_ext = grDevices::rgb(153/255, 76/255, 76/255, 1),
        ceiling_int = grDevices::rgb(202/255, 149/255, 149/255, 1),
        window = grDevices::rgb(102/255, 178/255, 204/255, 0.6),
        window_ext = grDevices::rgb(102/255, 178/255, 204/255, 0.6),
        window_int = grDevices::rgb(192/255, 226/255, 235/255, 0.6),
        door = grDevices::rgb(153/255, 133/255, 76/255, 1),
        door_ext = grDevices::rgb(153/255, 133/255, 76/255, 1),
        door_int = grDevices::rgb(202/255, 188/255, 149/255, 1),
        glassdoor = grDevices::rgb(153/255, 133/255, 76/255, 1),
        glassdoor_ext = grDevices::rgb(153/255, 133/255, 76/255, 1),
        glassdoor_int = grDevices::rgb(202/255, 188/255, 149/255, 1),
        siteshading = grDevices::rgb(75/255, 124/255, 149/255, 1),
        siteshading_ext = grDevices::rgb(75/255, 124/255, 149/255, 1),
        siteshading_int = grDevices::rgb(187/255, 209/255, 220/255, 1),
        buildingshading = grDevices::rgb(113/255, 76/255, 153/255, 1),
        buildingshading_ext = grDevices::rgb(113/255, 76/255, 153/255, 1),
        buildingshading_int = grDevices::rgb(216/255, 203/255, 229/255, 1),
        zoneshading = grDevices::rgb(76/255, 110/255, 178/255, 1),
        zoneshading_ext = grDevices::rgb(76/255, 110/255, 178/255, 1),
        zoneshading_int = grDevices::rgb(183/255, 197/255, 224/255, 1),
        interiorpartitionsurface = grDevices::rgb(158/255, 188/255, 143/255, 1),
        interiorpartitionsurface_ext = grDevices::rgb(158/255, 188/255, 143/255, 1),
        interiorpartitionsurface_int = grDevices::rgb(213/255, 226/255, 207/255, 1)
    ),
    boundary = c(
        surface = grDevices::rgb(0/255, 153/255, 0/255),
        adiabatic = grDevices::rgb(255/255, 0/255, 0/255),
        zone = grDevices::rgb(255/255, 0/255, 0/255),
        outdoors = grDevices::rgb(163/255, 204/255, 204/255),
        outdoors_sun = grDevices::rgb(40/255, 204/255, 204/255),
        outdoors_wind = grDevices::rgb(9/255, 159/255, 162/255),
        outdoors_sunwind = grDevices::rgb(68/255, 119/255, 161/255),
        ground = grDevices::rgb(204/255, 183/255, 122/255),
        groundfcfactormethod = grDevices::rgb(153/255, 122/255, 30/255),
        groundslabpreprocessoraverage = grDevices::rgb(255/255, 191/255, 0/255),
        groundslabpreprocessorcore = grDevices::rgb(255/255, 182/255, 50/255),
        groundslabpreprocessorperimeter = grDevices::rgb(255/255, 178/255, 101/255),
        groundbasementpreprocessoraveragewall = grDevices::rgb(204/255, 51/255, 0/255),
        groundbasementpreprocessoraveragefloor = grDevices::rgb(204/255, 81/255, 40/255),
        groundbasementpreprocessorupperwall = grDevices::rgb(204/255, 112/255, 81/255),
        groundbasementpreprocessorlowerwall = grDevices::rgb(204/255, 173/255, 163/255),
        othersidecoefficients = grDevices::rgb(63/255, 63/255, 63/255),
        othersideconditionsmodel = grDevices::rgb(153/255, 0/255, 76/255)
    )
)
# }}}

# plot_idf {{{
plot_idf <- function (idf, render_by = c("surface_type", "boundary", "construction", "zone"),
                      new = TRUE, clear = TRUE, wireframe = FALSE) {
    render_by <- match.arg(render_by)
    dt <- extract_idf_surfaces(idf)

    if (is.null(dt)) return(invisible())
    plot_surface(dt, new = new, clear = clear, wireframe = wireframe, render_by = render_by)
}
# }}}

# map_color {{{
map_color <- function (dt, type = "surface_type") {
    cl <- data.table(type = names(COLOR_MAP[[type]]), color = COLOR_MAP[[type]])

    dt[cl, on = "type", color := i.color]
    dt[, alpha := 1.0]
    trans <- paste0(c("window", "glassdoor"), rep(c("", "_int", "_ext"), 2L))
    dt[J(trans), on = "type", alpha := 0.6]
}
# }}}

#' @importFrom rgl rgl.open rgl.quads
# plot_surface {{{
plot_surface <- function (dt, new = FALSE, clear = TRUE, wireframe = FALSE,
                          render_by = "surface_type", ...) {
    map_color(dt, type = render_by)

    pt_num <- dt[, .N, by = c("id")]

    # for triangles
    dt_tri <- dt[pt_num[J(3L), on = "N", -"N", nomatch = NULL], on = c("id")]
    # for quadrangles
    dt_quad <- dt[pt_num[J(4L), on = "N", -"N", nomatch = NULL], on = c("id")]
    # for polygons
    dt_poly <- dt[pt_num[!J(c(3L, 4L)), on = "N", -"N"], on = c("id")]

    get_matrix <- function (dt) as.matrix(dt[, .SD, .SDcols = c("x", "y", "z")])
    get_color <- function(dt) as.matrix(dt[, .SD, .SDcols = rep("color", 3L)])
    get_alpha <- function(dt) as.matrix(dt[, .SD, .SDcols = rep("alpha", 3L)])

    plot_type <- function (dt, type = c("tri", "quad", "poly")) {
        type <- match.arg(type)
        if (!nrow(dt)) return(NULL)

        rgl_fun <- switch(type,
            tri = rgl::triangles3d,
            quad = rgl::quads3d,
            poly = rgl::polygon3d
        )

        if (type != "poly") {
            mat <- get_matrix(dt)
            clr <- get_color(dt)
            alp <- get_alpha(dt)

            if (!wireframe) {
                rgl_fun(mat, color = clr, lit = FALSE, alpha = alp)
            }
            rgl_fun(mat, color = "black", lit = FALSE, front = "lines", back = "lines", lwd = 2.1)
        # for polygons, have to decide which are the two coordinates describe
        # the polygons
        } else {
            dt_base <- dt[!J("hole"), on = "category",
                list(x = length(unique(x)), y = length(unique(y)), z = length(unique(z))),
                by = "id"
            ]
                browser()

            # draw frame
            rgl::polygon3d(get_matrix(sep_surfaces(dt[!J("hole"), on = "category"])),
                color = "black", lwd = 2.1, fill = FALSE)

            plot_poly <- function (dt, dt_base, axis) {
                dt_axis <- dt_base[J(1L), on = axis, nomatch = NULL]
                if (!nrow(dt_axis)) return(NULL)

                dt_s <- split(dt[J(dt_axis$id), on = "id"], by = "id")

                base <- switch(axis, x = 1, y = 2, z = 3)

                for (surf in dt_s) {
                    # get accumulated vertice number per previous surface
                    n_vert <- surf[, .N, by = "name"][, cumsum(data.table::shift(N, fill = 0L))]
                    # get segment point index
                    surf[, by = "name",
                        c("segment_pnt1", "segment_pnt2") := {
                            .i <- seq_len(.N) + n_vert[.GRP]
                            list(.i, c(.i[-1L], .i[1L]))
                        }
                    ]

                    # get hole position
                    # use centroid of the hole
                    hole <- surf[J("hole"), on = "category", nomatch = NULL,
                        by = c("name"),
                        list(x = mean(x), y = mean(y), z = mean(z))
                    ]

                    # use RTriangle to triangulate polygon surfaces
                    p <- RTriangle::pslg(
                        P = surf[, .SD, .SDcols = setdiff(c("x", "y", "z"), axis)],
                        S = surf[, .SD, .SDcols = paste0("segment_pnt", 1:2)],
                        H = hole[, .SD, .SDcols = setdiff(c("x", "y", "z"), axis)]
                    )
                    tri <- RTriangle::triangulate(p)

                    if (!nrow(tri$T)) {
                        cat(sprintf("Failed to render surface '%s'", surf$name[1]))
                    }

                    shape <- rgl::tmesh3d(
                        vertices = t(surf[, list(x, y, z, n = 1L)]),
                        indices = t(tri$T),
                    )
                    rgl::shade3d(shape, color = get_color(surf)[1])
                }
            }

            plot_poly(dt, dt_base, "x")
            plot_poly(dt, dt_base, "y")
            plot_poly(dt, dt_base, "z")
        }
    }

    rgl_init(new = new, clear = clear)

    # Add x, y, and z Axes
    rgl::rgl.lines(c(0, max(dt$x) * 2), c(0, 0), c(0, 0), color = "red", lit = FALSE)
    rgl::rgl.lines(c(0, 0), c(0, max(dt$y) * 2), c(0, 0), color = "green", lit = FALSE)
    rgl::rgl.lines(c(0, 0), c(0, 0), c(0, max(dt$z) * 2), color = "blue", lit = FALSE)

    plot_type(dt_tri, "tri")
    plot_type(dt_quad, "quad")
    plot_type(dt_poly, "poly")
}
# }}}

#' @importFrom rgl rgl.open par3d rgl.bg rgl.viewpoint rgl.clear
# rgl_init {{{
rgl_init <- function (new = FALSE, clear = TRUE) {
    if (new) {
        rgl.open()
    } else if (clear) {
        rgl.clear(type = c("shapes"))
    }

    rgl.viewpoint(0, -60)

    cur <- par3d("mouseMode")
    cur[["wheel"]] <- "push"
    cur[["middle"]] <- "fov"
    par3d("mouseMode" = cur)
    trackball_origin(1L)
    pan3d(2L)

    rgl.bg(color = "white")
}
# }}}

#' @importFrom rgl rgl.cur currentSubscene3d par3d rgl.setMouseCallbacks translationMatrix
# pan3d {{{
# adapted from rgl examples
pan3d <- function(button, dev = rgl.cur(), subscene = currentSubscene3d(dev)) {
    start <- list()

    begin <- function(x, y) {
        activeSubscene <- par3d("activeSubscene", dev = dev)
        start$listeners <<- par3d("listeners", dev = dev, subscene = activeSubscene)
        for (sub in start$listeners) {
            init <- par3d(c("userProjection","viewport"), dev = dev, subscene = sub)
            init$pos <- c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5)
            start[[as.character(sub)]] <<- init
        }
    }

    update <- function(x, y) {
        for (sub in start$listeners) {
            init <- start[[as.character(sub)]]
            xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5) - init$pos)
            mouseMatrix <- translationMatrix(xlat[1], xlat[2], xlat[3])
            par3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev, subscene = sub )
        }
    }
    rgl.setMouseCallbacks(button, begin, update, dev = dev, subscene = subscene)
}
# }}}

#' @importFrom rgl rgl.cur par3d rgl.set rgl.setMouseCallbacks rotationMatrix translationMatrix
# trackball_origin {{{
# adapted from https://r.789695.n4.nabble.com/Defining-origin-for-rotation-in-RGL-device-td3472859.html
trackball_origin <- function(button = 1, dev = rgl.cur(), origin = c(0, 0, 0)) {
    width <- height <- rotBase <- NULL
    userMatrix <- list()
    cur <- rgl.cur()
    offset <- NULL
    scale <- NULL

    xprod <- function(a, b) {
        c(a[2]*b[3] - a[3]*b[2],
          a[3]*b[1] - a[1]*b[3],
          a[1]*b[2] - a[2]*b[1])
    }

    vlen <- function(a) sqrt(sum(a^2))

    angle <- function(a,b) {
        dot <- sum(a*b)
        acos(dot/vlen(a)/vlen(b))
    }

    screenToVector <- function(x, y) {
        radius <- max(width, height)/2
        centre <- c(width, height)/2
        pt <- (c(x, y) - centre)/radius
        len <- vlen(pt)

        if (len > 1.e-6) pt <- pt/len

        maxlen <- sqrt(2)
        angle <- (maxlen - len)/maxlen*pi/2
        z <- sin(angle)
        len <- sqrt(1 - z^2)
        pt <- pt * len
        return (c(pt, z))
    }

    trackballBegin <- function(x, y) {
        vp <- par3d("viewport")
        width <<- vp[3]
        height <<- vp[4]
        cur <<- rgl.cur()
        bbox <- par3d("bbox")
        center <- c(sum(bbox[1:2])/2, sum(bbox[3:4])/2, sum(bbox[5:6])/2)
        scale <<- par3d("scale")
        offset <<- (center - origin)*scale
        for (i in dev) {
            if (inherits(try(rgl.set(i, TRUE)), "try-error")) dev <<- dev[dev != i]
            else userMatrix[[i]] <<- par3d("userMatrix")
        }
        rgl.set(cur, TRUE)
        rotBase <<- screenToVector(x, height - y)
     }

     trackballUpdate <- function(x,y) {
         rotCurrent <- screenToVector(x, height - y)
         angle <- angle(rotBase, rotCurrent)
         axis <- xprod(rotBase, rotCurrent)
         mouseMatrix <- rotationMatrix(angle, axis[1], axis[2], axis[3])
         for (i in dev) {
             if (inherits(try(rgl.set(i, TRUE)), "try-error")) dev <<- dev[dev != i]
             else par3d(userMatrix = t(translationMatrix(-offset[1], -offset[2], -offset[3])) %*% mouseMatrix  %*% t(translationMatrix(offset[1], offset[2], offset[3])) %*% userMatrix[[i]])
         }
         rgl.set(cur, TRUE)
     }

     for (i in dev) {
         rgl.set(i, TRUE)
         rgl.setMouseCallbacks(button, begin = trackballBegin, update = trackballUpdate, end = NULL)
     }

     rgl.set(cur, TRUE)
}
# }}}
