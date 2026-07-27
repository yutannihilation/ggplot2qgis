# tidyterra::geom_spatraster() and geom_spatraster_rgb() layers: the
# SpatRaster is written to a GeoTIFF and the layer becomes a GDAL raster
# layer whose renderer reproduces the plot's rendering — a single-band
# pseudocolor renderer for the fill scale of geom_spatraster() (see
# style_raster_pseudocolor()), a multiband true-color renderer for
# geom_spatraster_rgb() (see style_raster_multiband()).
#
# geom_spatraster() builds its layer from a tibble with one row per band:
# a `spatraster` list column (each element a single-band SpatRaster) and
# a `lyr` factor with the band names. A raster larger than its `maxcell`
# argument (5e5 cells by default) is resampled by tidyterra at layer
# construction — the original resolution is not recoverable from the plot
# object, so what is written here is exactly the data the plot draws.
# The fill is mapped to after_stat(value), so the plot's fill scale is an
# ordinary continuous scale trained on the band values.
#
# geom_spatraster_rgb() builds its layer from a one-row tibble whose
# `spatraster` column holds a 3-band SpatRaster. The data-shaping
# arguments — the r/g/b band selection and reordering, the zlim/stretch
# rescaling and the `maxcell` resampling — are applied before the layer
# is built, so the layer's raster is always three bands in
# red-green-blue order holding the values the plot draws. (The
# stat-time reprojection to the plot CRS and `mask_projection` are
# draw-time effects and are not captured, as for geom_spatraster().)
#
# TODO (advanced raster support, out of scope for now):
# - multi-band SpatRasters in geom_spatraster() (tidyterra facets by
#   band)
# - geom_spatraster_contour() (could be materialized as a vector source)
# - binned fill scales (the DISCRETE colorrampshader mode), color tables
#   (tidyterra's scale_fill_coltab()) and the scale's na.value color
#   (missing cells currently render transparent via the source nodata)
# - writing the original data source instead of the (possibly resampled)
#   layer data: a "copy" mode (copy the source file next to the project)
#   or a "reference" mode (point the datasource at the original path)

qgs_is_spatraster_layer <- function(layer) {
  inherits(layer$stat, "StatTerraSpatRaster")
}

# geom_spatraster_rgb() uses its own stat, which does not inherit from
# geom_spatraster()'s StatTerraSpatRaster.
qgs_is_spatraster_rgb_layer <- function(layer) {
  inherits(layer$stat, "StatTerraSpatRasterRGB")
}

qgs_require_terra <- function(i) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "layer ", i, ": the terra package must be installed to convert a ",
      "SpatRaster layer",
      call. = FALSE
    )
  }
}

# The layer data of a tidyterra raster layer: a tibble with a
# `spatraster` list column. `geom` names the geom in the errors.
qgs_spatraster_data <- function(layer, i, geom) {
  d <- layer$data
  if (!is.data.frame(d) || !is.list(d[["spatraster"]])) {
    stop(
      "layer ", i, ": unsupported ", geom, " layer (the layer data ",
      "has no `spatraster` column)",
      call. = FALSE
    )
  }
  d
}

# The first `spatraster` entry of such layer data, checked to be a
# SpatRaster.
qgs_spatraster_entry <- function(d, i, geom) {
  r <- d$spatraster[[1L]]
  if (!inherits(r, "SpatRaster")) {
    stop(
      "layer ", i, ": unsupported ", geom, " layer (the ",
      "`spatraster` column does not hold a SpatRaster)",
      call. = FALSE
    )
  }
  r
}

# The single-band SpatRaster of a geom_spatraster() layer. Errors spell
# out the unsupported cases instead of guessing.
qgs_spatraster <- function(layer, i) {
  qgs_require_terra(i)
  d <- qgs_spatraster_data(layer, i, "geom_spatraster()")
  if (nrow(d) > 1L) {
    stop(
      "layer ", i, ": a multi-band SpatRaster is not supported",
      call. = FALSE
    )
  }
  r <- qgs_spatraster_entry(d, i, "geom_spatraster()")
  qgs_check_spatraster_fill(layer, i)
  r
}

# The renderer reproduces the fill scale as a function of the band value,
# which is only faithful when fill is mapped to the band value itself —
# the default mapping geom_spatraster() installs. Anything else would be
# silently mis-rendered, so it is an error.
qgs_check_spatraster_fill <- function(layer, i) {
  quo <- layer$mapping[["fill"]]
  label <- if (is.null(quo)) "<missing>" else rlang::as_label(quo)
  if (!grepl("^(ggplot2::)?after_stat\\((\\.data\\$)?value\\)$", label)) {
    stop(
      "layer ", i, ": only the default `fill` mapping of geom_spatraster() ",
      "(the band value) is supported, got `", label, "`",
      call. = FALSE
    )
  }
}

# The trained fill scale of the plot as a raster pseudocolor style. Only
# a continuous scale is supported: a binned or discrete fill scale has no
# raster renderer path yet (see the TODOs above).
qgs_spatraster_style <- function(built, i) {
  scale <- built@plot@scales$get_scales("fill")
  if (is.null(scale) || scale$is_discrete() || inherits(scale, "ScaleBinned")) {
    stop(
      "layer ", i, ": only a continuous fill scale is supported for a ",
      "SpatRaster layer",
      call. = FALSE
    )
  }
  ramp <- qgs_gradient_ramp(scale, "value", i)
  style_raster_pseudocolor(
    band = 1L,
    min = ramp$limits[1L],
    max = ramp$limits[2L],
    stops = list(offsets = ramp$offsets, colors = ramp$colors)
  )
}

# The 3-band SpatRaster of a geom_spatraster_rgb() layer, already in
# red-green-blue order (see the module comment).
qgs_spatraster_rgb <- function(layer, i) {
  qgs_require_terra(i)
  d <- qgs_spatraster_data(layer, i, "geom_spatraster_rgb()")
  if (nrow(d) != 1L) {
    stop(
      "layer ", i, ": unsupported geom_spatraster_rgb() layer (expected ",
      "one row of layer data, got ", nrow(d), ")",
      call. = FALSE
    )
  }
  r <- qgs_spatraster_entry(d, i, "geom_spatraster_rgb()")
  if (terra::nlyr(r) != 3L) {
    stop(
      "layer ", i, ": unsupported geom_spatraster_rgb() layer (expected ",
      "3 bands, got ", terra::nlyr(r), ")",
      call. = FALSE
    )
  }
  r
}

# The multibandcolor style reproducing how geom_spatraster_rgb() colors
# a cell: each channel value is clamped to [0, max_col_value] and mapped
# linearly to a 0..255 intensity (rgb(r, g, b, maxColorValue =
# max_col_value)), and the layer's constant `alpha` applies uniformly.
# With the default max_col_value = 255 and all band values inside
# 0..255 that map is the identity, written the way QGIS itself saves a
# Byte RGB raster: NoEnhancement with the band statistics as the
# recorded min/max. Everything else — another max_col_value, values
# outside 0..255 (which tidyterra clamps), or a band with no values at
# all — becomes an explicit linear stretch from 0..max_col_value, which
# is exactly that clamp-and-scale rule.
qgs_spatraster_rgb_style <- function(layer, spat, i) {
  max_col_value <- layer$stat_params$max_col_value
  if (!is.numeric(max_col_value) || length(max_col_value) != 1L ||
    !is.finite(max_col_value) || max_col_value <= 0) {
    stop(
      "layer ", i, ": `max_col_value` must be a single positive number",
      call. = FALSE
    )
  }
  alpha <- layer$aes_params$alpha
  if (is.null(alpha) || (length(alpha) == 1L && is.na(alpha))) {
    # ggplot2 treats a missing/NA alpha as "keep the color's alpha",
    # i.e. fully opaque here.
    alpha <- 1
  }
  if (!is.numeric(alpha) || length(alpha) != 1L ||
    alpha < 0 || alpha > 1) {
    stop(
      "layer ", i, ": `alpha` must be a single number in [0, 1]",
      call. = FALSE
    )
  }

  # An all-NA band has no statistics (minmax() returns NaN), so it falls
  # through to the stretch form, which renders it the same (no values to
  # draw).
  mm <- if (max_col_value == 255) terra::minmax(spat, compute = TRUE)
  if (!is.null(mm) && all(is.finite(mm)) && all(mm >= 0 & mm <= 255)) {
    algorithm <- "NoEnhancement"
    mins <- mm[1L, ]
    maxs <- mm[2L, ]
  } else {
    algorithm <- "StretchToMinimumMaximum"
    mins <- rep(0, 3L)
    maxs <- rep(max_col_value, 3L)
  }
  channel <- function(band) {
    list(band = band, min = mins[[band]], max = maxs[[band]])
  }
  style_raster_multiband(
    red = channel(1L),
    green = channel(2L),
    blue = channel(3L),
    algorithm = algorithm,
    opacity = alpha
  )
}
