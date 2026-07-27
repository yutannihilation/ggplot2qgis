# tidyterra::geom_spatraster() layers: the SpatRaster is written to a
# GeoTIFF and the layer becomes a GDAL raster layer whose single-band
# pseudocolor renderer reproduces the plot's fill scale (see
# style_raster_pseudocolor()).
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
# TODO (advanced raster support, out of scope for now):
# - multi-band SpatRasters (tidyterra facets by band) and
#   geom_spatraster_rgb() (a multibandcolor renderer, see the generate-qgs
#   crate for the reference implementation)
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

# The single-band SpatRaster of a geom_spatraster() layer. Errors spell
# out the unsupported cases instead of guessing.
qgs_spatraster <- function(layer, i) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "layer ", i, ": the terra package must be installed to convert a ",
      "SpatRaster layer",
      call. = FALSE
    )
  }
  d <- layer$data
  if (!is.data.frame(d) || !is.list(d[["spatraster"]])) {
    stop(
      "layer ", i, ": unsupported geom_spatraster() layer (the layer data ",
      "has no `spatraster` column)",
      call. = FALSE
    )
  }
  if (nrow(d) > 1L) {
    stop(
      "layer ", i, ": a multi-band SpatRaster is not supported",
      call. = FALSE
    )
  }
  r <- d$spatraster[[1L]]
  if (!inherits(r, "SpatRaster")) {
    stop(
      "layer ", i, ": unsupported geom_spatraster() layer (the ",
      "`spatraster` column does not hold a SpatRaster)",
      call. = FALSE
    )
  }
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
