# tidyterra::geom_spatraster_contour(),
# geom_spatraster_contour_text() and geom_spatraster_contour_filled()
# layers: unlike the other tidyterra raster geoms these draw *vector*
# shapes — isolines and the bands between them — so the layer becomes an
# ordinary GeoPackage-backed LineString / Polygon layer rather than a GDAL
# raster layer.
#
# None of them is recomputed here — they are what
# StatTerraSpatRasterContour / StatTerraSpatRasterContourFill already put
# in the built data, so the geom's `breaks`/`bins`/`binwidth` (and the
# `maxcell` downsampling and `mask_projection`) are reproduced by
# construction. One consequence: the stat reprojects the raster to the
# coord CRS *before* contouring (reproject_raster_on_stat()), so the x/y
# it emits are in the panel CRS, not in the raster's own CRS.
#
# The layer data is the same internal tibble geom_spatraster() uses (a
# `spatraster` list column and a `lyr` factor of band names, see
# spatraster.R), which is all it is read for: the band count and the band
# name.

# The contour stats, and for each the geoms this file knows how to
# convert. Detection has to be by geom as well as by stat:
# geom_spatraster_contour() and geom_spatraster_contour_text() are
# different geoms on the *same* StatTerraSpatRasterContour, and they
# become different QGIS layers.
QGS_CONTOUR_GEOMS <- list(
  StatTerraSpatRasterContour = c(
    "GeomSpatRasterContour", "GeomSpatRasterContourText"
  ),
  StatTerraSpatRasterContourFill = "GeomSpatRasterContourFilled"
)

qgs_is_spatraster_contour_layer <- function(layer) {
  inherits(layer$stat, "StatTerraSpatRasterContour") &&
    inherits(layer$geom, "GeomSpatRasterContour")
}

# geom_spatraster_contour_text() draws the same isolines with the contour
# value written along them, so the QGIS layer is the isoline layer with
# labels enabled (see qgs_contour_text_label_spec()).
qgs_is_spatraster_contour_text_layer <- function(layer) {
  inherits(layer$stat, "StatTerraSpatRasterContour") &&
    inherits(layer$geom, "GeomSpatRasterContourText")
}

# geom_spatraster_contour_filled() has its own stat (not a subclass of
# the isoline one), but it is checked by geom as well for symmetry: an
# unknown geom on this stat is not something this file can draw.
qgs_is_spatraster_contour_filled_layer <- function(layer) {
  inherits(layer$stat, "StatTerraSpatRasterContourFill") &&
    inherits(layer$geom, "GeomSpatRasterContourFilled")
}

# Rejects a layer that uses one of the contour stats with a geom none of
# the predicates above claimed. It would otherwise fall through to the
# data.frame path, whose error blames the wrong thing ("unsupported geom
# for data.frame data").
qgs_check_contour_geom <- function(layer, i) {
  for (stat in names(QGS_CONTOUR_GEOMS)) {
    if (inherits(layer$stat, stat)) {
      stop(
        "layer ", i, ": unsupported geom on a ", stat, " layer: ",
        class(layer$geom)[[1L]],
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

# The contour lines of a geom_spatraster_contour() layer — or, with
# `text = TRUE`, of a geom_spatraster_contour_text() one — as an sf
# object: one LINESTRING per contour piece, with the contour value
# (`level`) and the band name (`lyr`) as attributes. The text layer also
# gets the text drawn along the line as a `label` attribute, which is the
# field QGIS labels the features with.
qgs_contour_sf <- function(built, layer, i, text = FALSE) {
  geom <- if (text) {
    "geom_spatraster_contour_text()"
  } else {
    "geom_spatraster_contour()"
  }
  columns <- c("x", "y", "group", "level", "lyr")
  if (text) {
    columns <- c(columns, "label")
  }
  computed <- qgs_contour_computed(
    built, layer, i, geom, columns, "contour lines"
  )

  # One feature per contour piece: the stat's `group` is unique per level
  # and per piece, and PANEL keeps a faceted plot's panels apart (like
  # the data.frame layers do).
  rows_by_line <- qgs_contour_rows_by_group(computed)
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
  if (text) {
    # NULL when the geom places no labels, which leaves the column (and
    # with it the layer's labeling, see qgs_contour_text_label_spec()) out.
    attrs$label <- qgs_contour_label_text(computed, layer, first_rows, i)
  }
  sf::st_sf(
    attrs,
    geometry = sf::st_sfc(geoms, crs = qgs_contour_crs(built))
  )
}

# The filled contour bands of a geom_spatraster_contour_filled() layer as
# an sf object: one MULTIPOLYGON per band, with the band label (`level`,
# e.g. "(70, 80]") and the band name (`lyr`) as attributes.
qgs_contour_filled_sf <- function(built, layer, i) {
  computed <- qgs_contour_computed(
    built, layer, i, "geom_spatraster_contour_filled()",
    c("x", "y", "group", "subgroup", "level", "lyr"), "contour bands"
  )

  # One feature per band: the stat emits one `group` per band, its
  # `subgroup` numbering the band's rings.
  rows_by_band <- qgs_contour_rows_by_group(computed)
  geoms <- lapply(rows_by_band, function(rows) {
    qgs_isoband_multipolygon(
      computed$x[rows], computed$y[rows], computed$subgroup[rows], i
    )
  })

  # `level` and `lyr` are constant within a band. The label is written as
  # a string: it is the value the fill scale's categories match on.
  first_rows <- unname(vapply(rows_by_band, `[[`, integer(1L), 1L))
  attrs <- data.frame(
    level = as.character(computed$level[first_rows]),
    lyr = as.character(computed$lyr[first_rows]),
    stringsAsFactors = FALSE
  )
  sf::st_sf(
    attrs,
    geometry = sf::st_sfc(geoms, crs = qgs_contour_crs(built))
  )
}

# The computed data of a contour layer, checked to be something this file
# can convert: a single band, the columns the conversion reads, and at
# least one shape. `what` names the shapes in the "nothing to write"
# error.
qgs_contour_computed <- function(built, layer, i, geom, columns, what) {
  d <- qgs_spatraster_data(layer, i, geom)
  if (nrow(d) > 1L) {
    stop(
      "layer ", i, ": a multi-band SpatRaster is not supported",
      call. = FALSE
    )
  }

  computed <- built@data[[i]]
  absent <- setdiff(columns, names(computed))
  if (length(absent) > 0L) {
    stop(
      "layer ", i, ": unsupported ", geom, " layer (the computed data has ",
      "no `", absent[[1L]], "` column)",
      call. = FALSE
    )
  }
  if (nrow(computed) == 0L) {
    stop(
      "layer ", i, ": ", geom, " produced no ", what,
      call. = FALSE
    )
  }
  computed
}

# The row indices of each shape the stat emitted, in the order it emitted
# them (first appearance of the key): the stat's `group`, plus PANEL to
# keep a faceted plot's panels apart, like the data.frame layers do.
qgs_contour_rows_by_group <- function(computed) {
  key <- paste(computed$PANEL, computed$group)
  key <- factor(key, levels = unique(key))
  split(seq_len(nrow(computed)), key)
}

# One band's rings as a MULTIPOLYGON. The rings are exactly what
# isoband::isobands() produced for that band — the stat passes its `x`,
# `y` and `id` on as `x`, `y` and `subgroup` — so they are handed back to
# isoband, whose iso_to_sfg() is what applies the even-odd rule that
# decides which ring is a hole of which polygon.
#
# The result can be an invalid simple feature (a self-touching ring),
# which isobanding produces when a break sits exactly on data values. It
# is written as it is: that is the shape ggplot2 draws, and
# st_make_valid() would replace it with a different geometry.
qgs_isoband_multipolygon <- function(x, y, id, i) {
  band <- list(list(x = x, y = y, id = as.integer(id)))
  class(band) <- "isobands"
  geom <- isoband::iso_to_sfg(band)[[1L]]
  if (!inherits(geom, "MULTIPOLYGON")) {
    stop(
      "layer ", i, ": a contour band did not convert to a MULTIPOLYGON",
      call. = FALSE
    )
  }
  geom
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

# The text a geom_spatraster_contour_text() layer draws along each of the
# lines `rows` indexes, computed the way the geom's draw_panel() does:
# the `label` aesthetic — the contour value unless it was mapped, which
# is what the geom's default `label = "a"` stands for — run through the
# geom's `label_format`.
#
# NULL when `label_format` is NULL, the geom's way of spelling "draw the
# lines but place no labels" (it swaps in isoband's label_placer_none()).
#
# `label_format` is applied to one value per level, not per line: it is a
# scales::label_*() function by default, and those decide their accuracy
# from the whole vector they are given, so a per-line call could format
# the same level differently.
qgs_contour_label_text <- function(computed, layer, rows, i) {
  fmt <- qgs_geom_param(layer, "label_format")
  if (is.null(fmt)) {
    return(NULL)
  }

  # The geom sorts by `group` (level, then piece) before taking the first
  # label of each level, so the order of the levels is that order too.
  ord <- order(computed$group)
  level <- computed$level[ord]
  label <- computed$label[ord]
  if (identical(as.character(label[[1L]]), "a")) {
    label <- level
  }
  levels <- unique(level)
  per_level <- label[match(levels, level)]

  text <- if (is.function(fmt)) {
    fmt(as.double(per_level))
  } else {
    # A character vector of labels, one per level, positionally (the geom
    # aborts on a length mismatch when it draws; it never gets that far
    # here).
    if (!is.character(fmt) || length(fmt) != length(levels)) {
      stop(
        "layer ", i, ": `label_format` must be a function, NULL, or one ",
        "label per contour level (", length(levels), "), got ",
        length(fmt), " of class ", class(fmt)[[1L]],
        call. = FALSE
      )
    }
    fmt
  }
  as.character(text)[match(computed$level[rows], levels)]
}

# The label settings of a geom_spatraster_contour_text() layer, in the
# shape qgs_label_spec() returns: the `label` attribute written by
# qgs_contour_sf() rendered by QGIS's labeling engine, in the text styles
# the geom computed. NULL when there is no such attribute, i.e. when the
# geom places no labels either.
#
# Unlike the text geoms this is *not* a labels-only layer: the geom draws
# the isolines as well, so the layer keeps its line symbol. It is masked,
# which is how the gap isoband::isolines_grob() breaks in each line under
# its label is reproduced (see write_label_mask()).
qgs_contour_text_label_spec <- function(built, layer, i, d) {
  if (!"label" %in% names(d)) {
    return(NULL)
  }
  styles <- qgs_label_text_styles(built@data[[i]], qgs_text_size_unit(layer, i))
  c(list(field = "label"), styles, list(background = NULL, mask = TRUE))
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
  qgs_check_contour_level_mapping(quo, "colour", "geom_spatraster_contour()", i)
  list(aes = "colour", attribute = "level")
}

# The same for a geom_spatraster_contour_text() layer, whose `colour`
# is the color of the lines *and* of the labels. The lines take the
# renderer, like the isolines do; the labels cannot follow it, since a
# QGIS labeling carries a single text color, so they are drawn in the
# first feature's color with a warning — the way the other aesthetics
# these renderers cannot vary are handled (see qgs_constant()). `fill` is
# not drawn at all, as on the isolines.
qgs_contour_text_style_attribute <- function(plot, layer, i) {
  if (!is.null(layer$mapping[["fill"]] %||% plot@mapping[["fill"]])) {
    stop(
      "layer ", i, ": geom_spatraster_contour_text() draws lines and text, ",
      "so a `fill` mapping is not supported",
      call. = FALSE
    )
  }
  quo <- layer$mapping[["colour"]] %||% plot@mapping[["colour"]]
  if (is.null(quo)) {
    return(NULL)
  }
  qgs_check_contour_level_mapping(
    quo, "colour", "geom_spatraster_contour_text()", i
  )
  # Silent when the geom places no labels: there is then nothing the
  # renderer fails to color (see qgs_contour_label_text()).
  if (!is.null(qgs_geom_param(layer, "label_format"))) {
    warning(
      "layer ", i, ": a QGIS labeling has a single text color, so the ",
      "labels of a geom_spatraster_contour_text() layer cannot follow a ",
      "`colour` mapping; the first feature's color is used for every label",
      call. = FALSE
    )
  }
  list(aes = "colour", attribute = "level")
}

# The same for a geom_spatraster_contour_filled() layer, whose `fill`
# always varies: the stat's own default_aes maps it to
# `after_stat(level)`, the band label, which is written as an attribute of
# the GeoPackage and drives a categorized renderer. A user `fill` mapping
# has to be that same expression — the other computed values
# (`level_mid`, `nlevel`, ...) are not written — and a `colour` mapping is
# an error: a QGIS symbol varies one color, and here that is the fill.
qgs_contour_filled_style_attribute <- function(plot, layer, i) {
  colour <- layer$mapping[["colour"]] %||% plot@mapping[["colour"]]
  if (!is.null(colour)) {
    stop(
      "layer ", i, ": geom_spatraster_contour_filled() varies its `fill`, ",
      "so a `colour` mapping is not supported",
      call. = FALSE
    )
  }
  quo <- layer$mapping[["fill"]] %||% plot@mapping[["fill"]]
  if (!is.null(quo)) {
    qgs_check_contour_level_mapping(
      quo, "fill", "geom_spatraster_contour_filled()", i
    )
  }
  list(aes = "fill", attribute = "level")
}

# `level` is the only computed value written as an attribute, so it is the
# only expression an aesthetic may be mapped to. Anything else would be
# silently mis-rendered, so it is an error rather than a guess.
qgs_check_contour_level_mapping <- function(quo, aes_name, geom, i) {
  label <- rlang::as_label(quo)
  if (!grepl("^(ggplot2::)?after_stat\\((\\.data\\$)?level\\)$", label)) {
    stop(
      "layer ", i, ": only `after_stat(level)` is supported for `", aes_name,
      "` on a ", geom, " layer, got `", label, "`",
      call. = FALSE
    )
  }
  invisible(NULL)
}
