# tm_raster() layers of a tmap object.
#
# tmap normalizes every raster shape (stars, SpatRaster, Raster) to the
# same pair in step 1: `tms$shpTM$shp` is a stars `dimensions` object
# holding only the x/y georeferencing, and `tms$dt` is one row per cell
# keyed by `tmapID__ = (y - 1) * nx + x` — x-fastest and top-down, i.e.
# exactly terra's cell order. Rebuilding a SpatRaster from those two and
# writing it as a GeoTIFF is what this module does; the renderer comes
# from the same trained legends the vector path uses.
#
# What can be written is the grid tmap *draws*: step 1 has already
# downsampled it to `raster.max_cells` and warped it to the display CRS,
# and the original is not recoverable from the built object. Same
# decision as the tidyterra path (ADR 0004).
#
# Unlike the vector renderers, a raster renderer can express missing
# cells exactly (GeoTIFF nodata + the `nodataColor` attribute) and a
# constant alpha exactly (the `opacity` attribute), so neither the
# separate "(missing value)" layer nor the alpha TODO of ADR 0001
# applies here.
#
# TODO: tm_rgb()/tm_rgba() (a multibandcolor renderer over three bands)
# and tm_scale_discrete() are rejected; so is any grid that is not
# axis-aligned and regularly spaced.

# tmap's separator between a factor level and its color, introduced by
# tmapShape() for any factor column carrying a `colors` attribute (a
# stars factor attribute or a terra raster with a color table).
QGS_TMAP_LEVEL_SEP <- "=<>="

qgs_tmap_is_raster_group <- function(tms) {
  identical(tms$shpclass, "stars")
}

# The georeferencing of a raster shape group, validated: the assumptions
# terra::rast() is built on below are checked here rather than left to
# produce a silently misplaced raster.
qgs_tmap_group_grid <- function(tms, gi) {
  dims <- tms$shpTM$shp
  if (!inherits(dims, "dimensions") ||
    !all(c("x", "y") %in% names(unclass(dims)))) {
    stop(
      "shape ", gi, ": unsupported raster shape (no x/y dimensions)",
      call. = FALSE
    )
  }
  raster <- attr(dims, "raster")
  if (isTRUE(raster$curvilinear)) {
    stop("shape ", gi, ": curvilinear rasters are not supported", call. = FALSE)
  }
  if (!is.null(raster$affine) && any(raster$affine != 0)) {
    stop(
      "shape ", gi, ": rotated or sheared rasters are not supported",
      call. = FALSE
    )
  }

  x <- unclass(dims)$x
  y <- unclass(dims)$y
  # A rectilinear grid states its (irregular) cell boundaries in `values`
  # and leaves `delta` missing; QGIS has no renderer for one.
  if (!is.numeric(x$delta) || !is.numeric(y$delta) ||
    is.na(x$delta) || is.na(y$delta)) {
    stop(
      "shape ", gi, ": rasters with irregular cell sizes are not supported",
      call. = FALSE
    )
  }
  # tmap always hands over a north-up grid (x increasing, y decreasing);
  # anything else would put the cells of `dt` in a different order than
  # the extent implies.
  if (x$delta <= 0 || y$delta >= 0) {
    stop(
      "shape ", gi, ": only north-up rasters (x increasing, y decreasing) ",
      "are supported",
      call. = FALSE
    )
  }

  nx <- as.integer(x$to - x$from + 1L)
  ny <- as.integer(y$to - y$from + 1L)
  if (nx < 1L || ny < 1L) {
    stop("shape ", gi, ": the raster is empty", call. = FALSE)
  }
  crs <- sf::st_crs(dims)
  if (is.na(crs)) {
    stop("shape ", gi, ": the data has no CRS", call. = FALSE)
  }

  dt <- as.data.frame(tms$dt)
  if (!all(dt$sel__)) {
    stop(
      "shape ", gi, ": filtered shapes (tm_shape(filter = )) are not ",
      "supported",
      call. = FALSE
    )
  }
  if (nrow(dt) != nx * ny) {
    stop(
      "shape ", gi, ": the raster data has ", nrow(dt), " rows but the grid ",
      "has ", nx * ny, " cells",
      call. = FALSE
    )
  }
  # `dt` is sorted by tmapID__ by tmapShape(); the raster is rebuilt from
  # that order, so make the assumption explicit.
  if (is.unsorted(dt$tmapID__, strictly = TRUE)) {
    stop(
      "shape ", gi, ": the raster cells are not in ascending tmapID order",
      call. = FALSE
    )
  }

  list(
    nx = nx,
    ny = ny,
    xmin = x$offset,
    xmax = x$offset + nx * x$delta,
    ymin = y$offset + ny * y$delta,
    ymax = y$offset,
    crs = crs,
    dt = dt
  )
}

# One tm_raster() layer of one raster shape group -> a spec for
# raster_layer(): the SpatRaster to write, the default name and the
# style.
qgs_tmap_raster_layer_spec <- function(built, grid, tms, lyr, tml, i) {
  if (!"tm_data_raster" %in% lyr$mapping_fun) {
    stop(
      "layer ", i, " (", qgs_tmap_layer_label(lyr), "): a raster shape can ",
      "only be drawn with tm_raster()",
      call. = FALSE
    )
  }
  qgs_require_terra(i)
  # Before looking at the legends: tm_rgb()/tm_rgba() colors the cells
  # directly and leaves no active `col` legend, so its scale has to be
  # named here rather than reported as a missing mapping.
  qgs_tmap_check_scale(tml, "col", i)

  legends <- qgs_tmap_layer_legends(built, lyr)
  active <- names(legends)[vapply(
    legends,
    function(leg) isTRUE(leg$active),
    logical(1L)
  )]
  if (!identical(active, "col")) {
    other <- setdiff(active, "col")
    if (length(other) > 0L) {
      stop(
        "layer ", i, ": only `col` can be mapped to data on a raster layer; ",
        "the `", other[[1L]], "` scale is not supported",
        call. = FALSE
      )
    }
    stop(
      "layer ", i, ": a raster layer must map `col` to a variable",
      call. = FALSE
    )
  }

  attribute <- qgs_tmap_attribute(tml, "col", grid$dt, i)
  column <- qgs_tmap_raster_column(grid$dt, attribute, i)
  # The color tmap gave each cell, in the same order as `column$values`.
  cell_colors <- lyr$mapping_dt[["col"]][
    match(grid$dt$tmapID__, lyr$mapping_dt$tmapID__)
  ]
  opacity <- qgs_tmap_raster_opacity(lyr, i)
  nodata <- qgs_tmap_raster_nodata_color(cell_colors, column$values)
  style <- qgs_tmap_raster_style(
    legends[["col"]], tml, column, cell_colors, attribute, i, opacity, nodata
  )

  list(
    kind = "raster",
    raster = qgs_tmap_raster_band(grid, column$values),
    name = tms$shp_name,
    style = style
  )
}

# The layer's attribute column as band values: numbers as they are, a
# factor as its integer codes plus the level labels (which tmap may have
# mangled into "<label>=<>=<color>").
qgs_tmap_raster_column <- function(dt, attribute, i) {
  raw <- dt[[attribute]]
  if (is.factor(raw)) {
    levels <- sub(paste0(QGS_TMAP_LEVEL_SEP, ".*$"), "", levels(raw))
    return(list(values = unclass(raw), levels = levels))
  }
  if (!is.numeric(raw) && !is.logical(raw)) {
    stop(
      "layer ", i, ": `", attribute, "` is a ", class(raw)[[1L]],
      " variable, which a raster band cannot hold",
      call. = FALSE
    )
  }
  # Kept as-is (not coerced to double) so an integer variable is written
  # to an integer GeoTIFF band.
  list(values = raw, levels = NULL)
}

# The SpatRaster to write: the grid's georeferencing filled with `values`
# in tmapID order (x-fastest, top-down), which is terra's own cell order.
qgs_tmap_raster_band <- function(grid, values) {
  r <- terra::rast(
    nrows = grid$ny,
    ncols = grid$nx,
    xmin = grid$xmin,
    xmax = grid$xmax,
    ymin = grid$ymin,
    ymax = grid$ymax,
    crs = sf::st_crs(grid$crs)$wkt
  )
  terra::values(r) <- values
  r
}

# The constant `col_alpha` tmap resolved for the layer, as the renderer's
# opacity. A per-cell alpha has no raster-renderer representation.
qgs_tmap_raster_opacity <- function(lyr, i) {
  alpha <- lyr$mapping_dt[["col_alpha"]]
  if (is.null(alpha)) {
    return(1)
  }
  # Cells tmap does not draw carry NA here; they are handled by the
  # nodata color, so they must not count as a varying alpha.
  present <- unique(alpha[!is.na(alpha)])
  if (length(present) == 0L) {
    return(1)
  }
  if (length(present) > 1L) {
    stop(
      "layer ", i, ": a per-cell `col_alpha` is not supported on a raster ",
      "layer (only a constant one, which becomes the layer opacity)",
      call. = FALSE
    )
  }
  present[[1L]]
}

# The color tmap paints the layer's missing cells in, as c(r, g, b,
# alpha) — or NULL when there are none, or tmap does not draw them
# (which is also QGIS's default for nodata cells).
qgs_tmap_raster_nodata_color <- function(cell_colors, values) {
  na_cells <- which(is.na(values))
  if (length(na_cells) == 0L) {
    return(NULL)
  }
  color <- cell_colors[[na_cells[[1L]]]]
  if (is.na(color)) {
    return(NULL)
  }
  rgba <- grDevices::col2rgb(color, alpha = TRUE)[, 1L]
  if (rgba[[4L]] == 0L) {
    return(NULL)
  }
  rgba
}

# The trained `col` scale as a raster renderer. The legend structures are
# the vector path's, so the interval normalization and the continuous
# ramp sampling are shared; only the QGIS renderer differs.
qgs_tmap_raster_style <- function(leg, tml, column, cell_colors, attribute, i,
                                  opacity, nodata) {
  switch(leg$scale,
    intervals = {
      bins <- qgs_tmap_intervals(leg, attribute, i)
      style_raster_binned(
        band = 1L,
        boundaries = bins$boundaries,
        colors = bins$colors,
        labels = bins$labels,
        opacity = opacity,
        nodata_color = nodata
      )
    },
    categorical = qgs_tmap_raster_paletted_style(
      column, cell_colors, attribute, i, opacity, nodata
    ),
    continuous = {
      limits <- leg$limits
      if (length(limits) != 2L || anyNA(limits) ||
        limits[[2L]] <= limits[[1L]]) {
        stop(
          "layer ", i, ": cannot map `", attribute,
          "` to a color gradient (the scale's limits are degenerate)",
          call. = FALSE
        )
      }
      style_raster_pseudocolor(
        band = 1L,
        min = limits[[1L]],
        max = limits[[2L]],
        stops = qgs_tmap_ramp_stops(
          tml$mapping.aes$col$scale, "col", limits, i
        ),
        opacity = opacity,
        nodata_color = nodata
      )
    },
    stop(
      "layer ", i, ": unsupported scale type \"", leg$scale,
      "\" for `col`",
      call. = FALSE
    )
  )
}

# tm_scale_categorical() / tm_scale_ordinal() on a raster: one palette
# entry per distinct band value, pairing it with the color tmap gave a
# cell holding it — exact by construction, immune to legend reordering
# and level combining, like the vector path (ADR 0001 decision 3).
# Values with no cell are dropped (they would never render). Unlike the
# vector categorized renderer, the paletted one has a label attribute, so
# a factor's level labels are carried over; a numeric variable (tmap also
# picks a categorical scale for one with few distinct values) is labeled
# with the value itself.
qgs_tmap_raster_paletted_style <- function(column, cell_colors, attribute, i,
                                           opacity, nodata) {
  values <- column$values
  present <- sort(unique(values[!is.na(values)]))
  # A value whose cells tmap does not draw at all has no color to write.
  colors <- cell_colors[match(present, values)]
  present <- present[!is.na(colors)]
  colors <- colors[!is.na(colors)]
  if (length(present) == 0L) {
    stop(
      "layer ", i, ": the scale of `", attribute, "` has no drawn values",
      call. = FALSE
    )
  }
  labels <- if (is.null(column$levels)) {
    vapply(present, num, character(1L))
  } else {
    column$levels[present]
  }
  style_raster_paletted(
    band = 1L,
    values = present,
    colors = grDevices::col2rgb(colors),
    labels = labels,
    opacity = opacity,
    nodata_color = nodata
  )
}
