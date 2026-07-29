# tidyterra::geom_spatraster_contour() layers: unlike the other tidyterra
# raster geoms this one draws *lines*, so the layer becomes an ordinary
# GeoPackage-backed LineString layer rather than a GDAL raster layer.
#
# The isolines are not recomputed here — they are what
# StatTerraSpatRasterContour already put in the built data, so the geom's
# `breaks`/`bins`/`binwidth` (and the `maxcell` downsampling and
# `mask_projection`) are reproduced by construction. One consequence: the
# stat reprojects the raster to the coord CRS *before* contouring
# (reproject_raster_on_stat()), so the x/y it emits are in the panel CRS,
# not in the raster's own CRS.
#
# The layer data is the same internal tibble geom_spatraster() uses (a
# `spatraster` list column and a `lyr` factor of band names, see
# spatraster.R), which is all it is read for: the band count and the band
# name.

# The geoms sharing this machinery that are not supported. Detection has
# to be by geom, not by stat: geom_spatraster_contour_text() is a
# different geom on the *same* StatTerraSpatRasterContour.
QGS_UNSUPPORTED_CONTOUR_GEOMS <- c(
  GeomSpatRasterContourText = "geom_spatraster_contour_text()",
  GeomSpatRasterContourFilled = "geom_spatraster_contour_filled()"
)

qgs_is_spatraster_contour_layer <- function(layer) {
  inherits(layer$stat, "StatTerraSpatRasterContour") &&
    inherits(layer$geom, "GeomSpatRasterContour")
}

# Rejects the contour geoms above by name. They would otherwise fall
# through to the data.frame path, whose error blames the wrong thing
# ("unsupported geom for data.frame data").
qgs_check_contour_geom <- function(layer, i) {
  geom <- class(layer$geom)[[1L]]
  if (geom %in% names(QGS_UNSUPPORTED_CONTOUR_GEOMS)) {
    stop(
      "layer ", i, ": ", QGS_UNSUPPORTED_CONTOUR_GEOMS[[geom]],
      " is not supported",
      call. = FALSE
    )
  }
  invisible(NULL)
}

# The contour lines of a geom_spatraster_contour() layer as an sf object:
# one LINESTRING per contour piece, with the contour value (`level`) and
# the band name (`lyr`) as attributes.
qgs_contour_sf <- function(built, layer, i) {
  d <- qgs_spatraster_data(layer, i, "geom_spatraster_contour()")
  if (nrow(d) > 1L) {
    stop(
      "layer ", i, ": a multi-band SpatRaster is not supported",
      call. = FALSE
    )
  }

  computed <- built@data[[i]]
  absent <- setdiff(c("x", "y", "group", "level", "lyr"), names(computed))
  if (length(absent) > 0L) {
    stop(
      "layer ", i, ": unsupported geom_spatraster_contour() layer (the ",
      "computed data has no `", absent[[1L]], "` column)",
      call. = FALSE
    )
  }
  if (nrow(computed) == 0L) {
    stop(
      "layer ", i, ": geom_spatraster_contour() produced no contour lines",
      call. = FALSE
    )
  }

  # One feature per contour piece: the stat's `group` is unique per level
  # and per piece, and PANEL keeps a faceted plot's panels apart (like
  # the data.frame layers do). First-appearance order keeps the features
  # in the order the stat emits them.
  key <- paste(computed$PANEL, computed$group)
  key <- factor(key, levels = unique(key))
  rows_by_line <- split(seq_len(nrow(computed)), key)
  geoms <- lapply(rows_by_line, function(rows) {
    if (length(rows) < 2L) {
      stop(
        "layer ", i, ": a contour line needs at least 2 points",
        call. = FALSE
      )
    }
    sf::st_linestring(cbind(computed$x[rows], computed$y[rows]))
  })

  # `level` and `lyr` are constant within a piece, so the first row of
  # each carries them.
  first_rows <- unname(vapply(rows_by_line, `[[`, integer(1L), 1L))
  attrs <- data.frame(
    level = as.numeric(computed$level[first_rows]),
    lyr = as.character(computed$lyr[first_rows]),
    stringsAsFactors = FALSE
  )
  sf::st_sf(
    attrs,
    geometry = sf::st_sfc(geoms, crs = qgs_contour_crs(built))
  )
}

# The CRS the stat's x/y are in: the panel CRS, which is what
# reproject_raster_on_stat() projected the raster to before contouring.
# The NA CRS (no coord_sf(), a raster without a CRS) is left to the
# caller's "the data has no CRS" error.
qgs_contour_crs <- function(built) {
  crs <- built@layout$panel_params[[1L]]$crs
  if (is.null(crs)) {
    return(sf::st_crs(NA_integer_))
  }
  sf::st_crs(crs)
}

# Which aesthetic drives the varying color of a contour layer, in the
# shape qgs_style_attribute() returns. The contour data has no user
# columns, so the bare-column-name rule cannot apply: the only supported
# mapping is the stat's own `after_stat(level)` (the contour value), which
# is written as an attribute of the GeoPackage. Anything else would be
# silently mis-rendered, so it is an error rather than a guess.
qgs_contour_style_attribute <- function(plot, layer, i) {
  if (!is.null(layer$mapping[["fill"]] %||% plot@mapping[["fill"]])) {
    stop(
      "layer ", i, ": geom_spatraster_contour() draws lines, so a `fill` ",
      "mapping is not supported; map `colour` instead",
      call. = FALSE
    )
  }
  quo <- layer$mapping[["colour"]] %||% plot@mapping[["colour"]]
  if (is.null(quo)) {
    return(NULL)
  }
  label <- rlang::as_label(quo)
  if (!grepl("^(ggplot2::)?after_stat\\((\\.data\\$)?level\\)$", label)) {
    stop(
      "layer ", i, ": only `after_stat(level)` is supported for `colour` on ",
      "a geom_spatraster_contour() layer, got `", label, "`",
      call. = FALSE
    )
  }
  list(aes = "colour", attribute = "level")
}
