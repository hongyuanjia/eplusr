# plot_idf {{{
plot_idf <- function (idf) {
    cls <- idf$class_name(by_group = TRUE)["Thermal Zones and Surfaces"][[1L]]

    surf <- data.table()
    win <- data.table()
    shade <- data.table()

    # surfaces
    if ("BuildingSurface:Detailed" %in% cls) {
        surf <- extract_surfaces_table(idf, "BuildingSurface:Detailed")
    }

    # other detailed surface specs
    cls_surf <- stri_subset_regex(cls, "(Wall|Roof|Floor|Ceiling).+Detailed")
    if (length(cls_surf)) {
        type <- stri_extract_first_regex(cls_surf, "(Wall|Roof|Floor)")
        surf_sep <- mapply(extract_surfaces_table, cls_surf, type,
            MoreArgs = list(idf = idf), SIMPLIFY = FALSE, USE.NAMES = FALSE

        )

        # combine
        surf <- rbindlist(list(surf, surf_sep))
    }

    # TODO: handle other surfaces

    if ("FenestrationSurface:Detailed" %in% cls) {
        win <- extract_surfaces_table(idf, "FenestrationSurface:Detailed")
    }

    if (length(cls_shade <- stri_subset_regex(cls, "Shading.+Detailed"))) {
        shade <- rbindlist(lapply(cls_shade, extract_surfaces_table, idf = idf, type = "shading"))
    }

    dt <- rbindlist(list(surf, win, shade), fill = TRUE)

    if (!nrow(dt)) {
        message("No supported surfaces are found.")
        return(invisible())
    }

    plot_surface(dt)
}
# }}}

# extract_surfaces_table {{{
extract_surfaces_table <- function (idf, class, type) {
    dt <- idf$to_table(class = class, wide = TRUE, string_value = FALSE, align = TRUE)
    if (!has_name(dt, "Surface Type")) set(dt, NULL, "Surface Type", as.character(type))

    # extra columns
    ext <- c("Construction Name", "Zone Name", "Outside Boundary Condition", "Outside Boundary Condition Object")
    has_ext <- has_name(dt, ext)

    dt_m <- melt.data.table(dt,
        id.vars = c("id", "name", "Surface Type", ext[has_ext]),
        measure.vars = patterns("X-coordinate", "Y-coordinate", "Z-coordinate")
    )
    setnames(dt_m, "Surface Type", "type")
    setnames(dt_m, paste0("value", 1:3), c("x", "y", "z"))
    set(dt_m, NULL, "variable", NULL)
    set(dt_m, NULL, "type", stri_trans_tolower(dt_m$type))
    setorderv(dt_m, "id")

    if (any(has_ext)) {
        setnames(dt_m, ext[has_ext], c("const", "zone", "boundary", "boundary_obj")[has_ext])
    }

    na.omit(dt_m, cols = c("x", "y", "z"))
}
# }}}

# map_color {{{
map_color <- function (dt) {
    colors <- c(
        wall = grDevices::rgb(204/255, 178/255, 102/255),
        floor = grDevices::rgb(128/255, 128/255, 128/255),
        roof = grDevices::rgb(153/255, 76/255, 76/255),
        ceiling = grDevices::rgb(153/255, 76/255, 76/255),
        window = grDevices::rgb(102/255, 178/255, 204/255, 0.6),
        glassdoor = grDevices::rgb(102/255, 178/255, 204/255, 0.6),
        shading = grDevices::rgb(76/255, 110/255, 178/255, 0.6)
    )
    cl <- data.table(type = names(colors), color = colors)

    dt[cl, on = "type", color := i.color]
}
# }}}

#' @importFrom rgl rgl.open rgl.quads
# plot_surface {{{
plot_surface <- function (dt, new = FALSE, ...) {
    map_color(dt)

    pt_num <- dt[, .N, by = .(id, name)]

    # for triangles
    dt_tri <- dt[pt_num[J(3L), on = "N", -"N", nomatch = NULL], on = c("id", "name")]
    # for quadrangles
    dt_quad <- dt[pt_num[J(4L), on = "N", -"N", nomatch = NULL], on = c("id", "name")]
    # for polygons
    dt_poly <- dt[pt_num[!J(c(3L, 4L)), on = "N", -"N"], on = c("id", "name")]

    get_matrix <- function (dt) {
        M <- as.matrix(dt[, .SD, .SDcols = c("x", "y", "z")])
        dimnames(M)[[1L]] <- dt$name
        M
    }

    get_color <- function(dt) as.matrix(dt[, .SD, .SDcols = rep("color", 3L)])

    plot_type <- function (dt, type = c("tri", "quad", "poly")) {
        type <- match.arg(type)
        if (!nrow(dt)) return(NULL)
        mat <- get_matrix(dt)
        clr <- get_color(dt)

        rgl_fun <- switch(type,
            tri = rgl::triangles3d,
            quad = rgl::quads3d,
            poly = rgl::polygon3d
        )

        rgl_fun(mat, color = clr, lit = FALSE)
        rgl_fun(mat, color = "black", lit = FALSE, front = "lines", back = "lines", lwd = 2.5)
    }

    rgl_init(new = new)

    # Add x, y, and z Axes
    rgl::rgl.lines(c(0, max(dt$x) * 2), c(0, 0), c(0, 0), color = "red", lit = FALSE)
    rgl::rgl.lines(c(0, 0), c(0, max(dt$y) * 2), c(0, 0), color = "green", lit = FALSE)
    rgl::rgl.lines(c(0, 0), c(0, 0), c(0, max(dt$z) * 2), color = "blue", lit = FALSE)

    plot_type(dt_tri, "tri")
    plot_type(dt_quad, "quad")
    plot_type(dt_poly, "poly")
}
# }}}

#' @importFrom rgl rgl.open par3d rgl.bg rgl.viewpoint
# rgl_init {{{
rgl_init <- function (new = FALSE) {
    if (new) {
        rgl.open()
    } else {
        rgl.clear(type = c("shapes"))
    }

    rgl.viewpoint(0, -60)

    cur <- par3d("mouseMode")
    cur[["wheel"]] <- "push"
    # cur[["right"]] <- "polar"
    cur[["middle"]] <- "fov"
    par3d("mouseMode" = cur)
    pan3d(1L)
    trackball_origin(2L)

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
