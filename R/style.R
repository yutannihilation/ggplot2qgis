# Symbology: vector styles and <renderer-v2> generation, plus raster
# styles and <rasterrenderer> generation.
#
# Ported from src/style.rs of the generate-qgs crate; the DISCRETE
# pseudocolor and paletted raster renderers have no counterpart there and
# were derived from what QGIS 4.2.0 itself writes (see
# .tmp/research/20260728_tmap_raster.md and docs/qgs-format.md).
#
# A vector style is a plain named list with a `type` field:
#   - "none":        no fields; draws nothing (null-symbol renderer)
#   - "single":      color
#   - "graduated":   attribute, classes, min, max, stops
#   - "binned":      attribute, boundaries, colors
#   - "continuous":  attribute, min, max, stops
#   - "categorized": attribute, values, colors, catch_all
# plus the shared fields target ("fill"/"stroke"), fill_color,
# outline_color, outline_width, linetype, marker and the two alphas (see
# style_defaults()). A color is an integer vector c(r, g, b) in 0..255,
# or NULL for "not drawn" (ggplot2's fill/colour = NA) where a symbol
# supports it: a polygon's fill, a marker's fill, or any outline.
# `stops` is list(offsets = <numeric n>, colors = <3 x n integer matrix>)
# with offsets ascending from 0 to 1. A linetype is either a QGIS
# pen-style preset ("no", "solid", "dash", "dot", "dash dot") or an
# integer vector of on/off run lengths in line-width units (see
# style_set_linetype()).
#
# A raster style is the same kind of list:
#   - "raster_pseudocolor": band, min, max, stops
#   - "raster_binned":      band, boundaries, colors, labels
#   - "raster_paletted":    band, values, colors, labels
#   - "raster_multiband":   red, green, blue, algorithm, opacity
# The single-band ones also carry `opacity` and `nodata_color` (an
# integer vector c(r, g, b, alpha), or NULL to leave the missing cells to
# QGIS's transparent default).

QGS_DEFAULT_OUTLINE_COLOR <- c(35L, 35L, 35L)
QGS_DEFAULT_OUTLINE_WIDTH <- 0.26
QGS_DEFAULT_FILL_COLOR <- c(229L, 229L, 229L)
# QGIS's default marker: a 2 mm circle, unrotated (see style_set_marker()).
QGS_DEFAULT_MARKER <- list(name = "circle", size = 2, angle = 0)
QGS_OPAQUE <- 255L

# The constants every vector style carries on top of its own fields: the
# stroke (color, width, pen style), the marker geometry of a Point layer,
# and the opacity of the two color slots. Spliced into each style_*()
# constructor so the defaults live in one place.
style_defaults <- function() {
  list(
    outline_color = QGS_DEFAULT_OUTLINE_COLOR,
    outline_width = QGS_DEFAULT_OUTLINE_WIDTH,
    linetype = "solid",
    marker = QGS_DEFAULT_MARKER,
    fill_alpha = QGS_OPAQUE,
    stroke_alpha = QGS_OPAQUE
  )
}

# Validates the shared constraints of ramp-based styles.
validate_ramp <- function(classes, min, max, stops) {
  if (classes < 2) {
    stop("style needs at least 2 classes, got ", classes, call. = FALSE)
  }
  if (min >= max) {
    stop(
      "invalid range: min (", num(min), ") must be smaller than max (",
      num(max), ")",
      call. = FALSE
    )
  }
  validate_color_stops(stops)
}

validate_color_stops <- function(stops) {
  offsets <- stops$offsets
  n <- length(offsets)
  if (n != ncol(stops$colors)) {
    stop(
      "color stop offsets and colors must have the same length",
      call. = FALSE
    )
  }
  if (n < 2L) {
    stop("style needs at least 2 color stops, got ", n, call. = FALSE)
  }
  if (offsets[1L] != 0 || offsets[n] != 1) {
    stop(
      "first color stop must be at offset 0.0 and last at 1.0",
      call. = FALSE
    )
  }
  if (any(diff(offsets) <= 0)) {
    stop("color stops must be in ascending offset order", call. = FALSE)
  }
  invisible(stops)
}

# Draws no features at all (a null-symbol renderer): used for the
# labels-only layers of the text/label geoms, where only the labeling is
# visible.
style_none <- function() {
  list(type = "none")
}

# Single symbol with the given fill color (NULL = not drawn, polygons
# only), dark gray 0.26 mm outline (QGIS defaults).
style_single <- function(color) {
  c(
    list(
      type = "single",
      color = color
    ),
    style_defaults()
  )
}

# Graduated coloring of `attribute` with `classes` equal-interval ranges
# between `min` and `max`. Class colors are interpolated along `stops`.
style_graduated <- function(attribute, classes, min, max, stops) {
  validate_ramp(classes, min, max, stops)
  c(
    list(
      type = "graduated",
      attribute = attribute,
      classes = as.integer(classes),
      min = min,
      max = max,
      stops = stops,
      target = "fill",
      fill_color = QGS_DEFAULT_FILL_COLOR
    ),
    style_defaults()
  )
}

# Binned coloring of `attribute`: bin i covers boundaries[i] ..
# boundaries[i + 1] and is drawn in colors[, i] as-is (no interpolation).
# `boundaries` is a strictly ascending numeric vector of length n + 1,
# `colors` a 3 x n integer matrix. Unlike a graduated style, the bins need
# not be equal-width (e.g. ggplot2's scale_*_steps() with custom breaks).
style_binned <- function(attribute, boundaries, colors) {
  n <- length(boundaries) - 1L
  if (n < 1L) {
    stop("binned style needs at least 1 bin", call. = FALSE)
  }
  if (any(diff(boundaries) <= 0)) {
    stop("bin boundaries must be strictly ascending", call. = FALSE)
  }
  if (n != ncol(colors)) {
    stop(
      "bin boundaries and colors must have matching lengths ",
      "(n + 1 boundaries for n colors)",
      call. = FALSE
    )
  }
  c(
    list(
      type = "binned",
      attribute = attribute,
      boundaries = boundaries,
      colors = colors,
      target = "fill",
      fill_color = QGS_DEFAULT_FILL_COLOR
    ),
    style_defaults()
  )
}

# Continuous coloring of `attribute`: the color is interpolated along
# `stops` from the attribute value rescaled so that `min` is at offset 0
# and `max` at 1 (values outside are clamped).
style_continuous <- function(attribute, min, max, stops) {
  if (min >= max) {
    stop(
      "invalid range: min (", num(min), ") must be smaller than max (",
      num(max), ")",
      call. = FALSE
    )
  }
  validate_color_stops(stops)
  c(
    list(
      type = "continuous",
      attribute = attribute,
      min = min,
      max = max,
      stops = stops,
      target = "fill",
      fill_color = QGS_DEFAULT_FILL_COLOR
    ),
    style_defaults()
  )
}

# Discrete coloring of `attribute`: each value/color pair becomes one
# <category> linked to one <symbol>. `values` is a character vector,
# `colors` a 3 x n integer matrix; `catch_all`, if given, is the color of
# the trailing "all other values" (value="NULL") category. `value_type`
# is the QGIS type of the category values ("string" or "double"): it must
# match the attribute's field type, because QGIS matches features against
# categories via the string form of the *typed* value (a REAL 1000000
# stringifies as "1e+06", which a string category "1000000" never equals).
style_categorized <- function(attribute, values, colors, catch_all = NULL,
                              value_type = c("string", "double")) {
  if (length(values) == 0L) {
    stop("categorized style needs at least 1 category", call. = FALSE)
  }
  if (length(values) != ncol(colors)) {
    stop(
      "category values and colors must have the same length",
      call. = FALSE
    )
  }
  c(
    list(
      type = "categorized",
      attribute = attribute,
      values = as.character(values),
      colors = colors,
      catch_all = catch_all,
      value_type = match.arg(value_type),
      target = "fill",
      fill_color = QGS_DEFAULT_FILL_COLOR
    ),
    style_defaults()
  )
}

# Sets the constant outline (stroke) color (NULL = not drawn) and width in
# millimeters. For a style whose varying color targets the stroke, the
# width still applies but the color is ignored.
style_set_outline <- function(style, color, width) {
  style$outline_color <- color
  style$outline_width <- width
  style
}

# Sets the line style of the stroke (a line's body, a polygon border or
# the ring around a marker): a QGIS pen-style preset ("no", "solid",
# "dash", "dot", "dash dot") or an integer vector of on/off run lengths
# in line-width units (R's longdash/twodash and hex-pattern linetypes,
# which have no preset). Lines draw run lengths as an exact custom dash;
# polygon borders and marker rings only support the presets, so there
# the run lengths are approximated by the nearest one.
style_set_linetype <- function(style, linetype) {
  style$linetype <- linetype
  style
}

# Sets the marker of a Point layer: a QGIS SimpleMarker shape name
# ("circle", "square", "diamond", "equilateral_triangle", "cross",
# "cross2"), its size in millimeters and its rotation in degrees. QGIS
# sizes a marker by its width, except `equilateral_triangle`, which is
# inscribed in the circle of that diameter — the caller converts (see
# QGS_PCH in marker.R). Ignored by the line and polygon symbols.
style_set_marker <- function(style, name, size, angle = 0) {
  if (size <= 0) {
    stop("marker size must be positive, got ", num(size), call. = FALSE)
  }
  style$marker <- list(name = name, size = size, angle = angle)
  style
}

# Sets the opacity of the two color slots, as integers in 0..255: the
# interior (a polygon's or marker's fill) and the stroke (a line's body, a
# polygon border, a marker ring). QGIS has a symbol-wide opacity too, but
# only per-color alpha can render a translucent fill under an opaque
# border, so the alphas ride along with the colors (see qgis_color()).
style_set_alpha <- function(style, fill_alpha, stroke_alpha) {
  style$fill_alpha <- as.integer(fill_alpha)
  style$stroke_alpha <- as.integer(stroke_alpha)
  style
}

# Makes the varying color of a graduated, continuous, or categorized style
# drive the outline (stroke) instead of the fill; every feature shares the
# constant `fill_color`. This is how e.g. ggplot2 renders a `colour`
# aesthetic on polygons.
style_set_stroke_target <- function(style, fill_color) {
  if (style$type == "single") {
    stop(
      "a single-symbol style has no varying color to move to the stroke",
      call. = FALSE
    )
  }
  style$target <- "stroke"
  style$fill_color <- fill_color
  style
}

# Shared validation of the single-band raster styles: a 1-based band, a
# layer opacity in 0..1, and an optional missing-cell color as an integer
# vector c(r, g, b, alpha).
validate_raster_band <- function(band) {
  if (!is.numeric(band) || length(band) != 1L || is.na(band) ||
    band < 1 || band %% 1 != 0) {
    stop(
      "band numbers are 1-based, got ",
      if (is.null(band)) "NULL" else paste(band, collapse = ", "),
      call. = FALSE
    )
  }
  as.integer(band)
}

validate_raster_opacity <- function(opacity) {
  if (!is.numeric(opacity) || length(opacity) != 1L || is.na(opacity) ||
    opacity < 0 || opacity > 1) {
    stop("`opacity` must be a single number in [0, 1]", call. = FALSE)
  }
  opacity
}

validate_nodata_color <- function(nodata_color) {
  if (is.null(nodata_color)) {
    return(NULL)
  }
  if (!is.numeric(nodata_color) || length(nodata_color) != 4L ||
    anyNA(nodata_color) || any(nodata_color < 0 | nodata_color > 255)) {
    stop(
      "`nodata_color` must be an integer vector c(r, g, b, alpha) in 0..255",
      call. = FALSE
    )
  }
  as.integer(nodata_color)
}

# Validates the per-class labels of a classed raster style.
validate_raster_labels <- function(labels, n) {
  if (!is.character(labels) || length(labels) != n || anyNA(labels)) {
    stop(
      "a raster style needs one label per class (expected ", n, ", got ",
      if (is.character(labels)) length(labels) else "a non-character vector",
      ")",
      call. = FALSE
    )
  }
  labels
}

# Continuous pseudocoloring of one raster band (1-based): the cell value
# is interpolated along `stops` between `min` and `max` (values outside
# are clamped). Written as an INTERPOLATED <colorrampshader>, which —
# unlike the vector renderers — reproduces the gradient exactly *and*
# shows a continuous ramp in the legend, so there is no graduated vs.
# continuous trade-off for rasters.
style_raster_pseudocolor <- function(band, min, max, stops, opacity = 1,
                                     nodata_color = NULL) {
  if (min >= max) {
    stop(
      "invalid range: min (", num(min), ") must be smaller than max (",
      num(max), ")",
      call. = FALSE
    )
  }
  validate_color_stops(stops)
  list(
    type = "raster_pseudocolor",
    band = validate_raster_band(band),
    min = min,
    max = max,
    stops = stops,
    opacity = validate_raster_opacity(opacity),
    nodata_color = validate_nodata_color(nodata_color)
  )
}

# Classed pseudocoloring of one raster band: `boundaries` are the n + 1
# break values of n classes and `colors` a 3 x n matrix of class colors,
# with `labels` the legend text of each class. Written as a DISCRETE
# <colorrampshader>, whose <item> values are the class upper bounds (the
# last one open-ended).
#
# Note the classes are right-closed in QGIS — a cell whose value is
# exactly an interior boundary falls into the class *below* it. Scales
# whose bins are left-closed (tmap's intervals) therefore differ from
# their QGIS rendering on exact boundary values; see the tmap section of
# ?write_qgs.
style_raster_binned <- function(band, boundaries, colors, labels,
                                opacity = 1, nodata_color = NULL) {
  n <- length(boundaries) - 1L
  if (n < 1L) {
    stop("binned style needs at least 1 bin", call. = FALSE)
  }
  if (any(diff(boundaries) <= 0)) {
    stop("bin boundaries must be strictly ascending", call. = FALSE)
  }
  if (n != ncol(colors)) {
    stop(
      "bin boundaries and colors must have matching lengths ",
      "(n + 1 boundaries for n colors)",
      call. = FALSE
    )
  }
  list(
    type = "raster_binned",
    band = validate_raster_band(band),
    boundaries = boundaries,
    colors = colors,
    labels = validate_raster_labels(labels, n),
    opacity = validate_raster_opacity(opacity),
    nodata_color = validate_nodata_color(nodata_color)
  )
}

# Exact-value coloring of one raster band (QGIS's "paletted/unique
# values" renderer): `values` are the cell values to match, `colors` a
# 3 x n matrix and `labels` the legend text of each entry. Cells matching
# no entry are not drawn.
style_raster_paletted <- function(band, values, colors, labels,
                                  opacity = 1, nodata_color = NULL) {
  n <- length(values)
  if (n < 1L) {
    stop("paletted style needs at least 1 value", call. = FALSE)
  }
  if (!is.numeric(values) || anyNA(values)) {
    stop("paletted style values must be non-missing numbers", call. = FALSE)
  }
  if (anyDuplicated(values) > 0L) {
    stop("paletted style values must be unique", call. = FALSE)
  }
  if (n != ncol(colors)) {
    stop(
      "paletted style values and colors must have matching lengths",
      call. = FALSE
    )
  }
  list(
    type = "raster_paletted",
    band = validate_raster_band(band),
    values = values,
    colors = colors,
    labels = validate_raster_labels(labels, n),
    opacity = validate_raster_opacity(opacity),
    nodata_color = validate_nodata_color(nodata_color)
  )
}

# True-color rendering of three raster bands (1-based) mapped to the
# red, green and blue channels. Each channel is list(band, min, max);
# `algorithm` says how a channel value becomes a 0..255 intensity:
#   - "NoEnhancement": used as-is (min/max are recorded but not
#     applied — QGIS's own default for a Byte RGB raster, so min/max
#     are the band statistics).
#   - "StretchToMinimumMaximum": stretched linearly from min..max to
#     0..255, values outside clamped (min/max are chosen values).
# `opacity` is the layer opacity in 0..1.
style_raster_multiband <- function(red, green, blue,
                                   algorithm = "NoEnhancement",
                                   opacity = 1) {
  algorithm <- match.arg(
    algorithm,
    c("NoEnhancement", "StretchToMinimumMaximum")
  )
  for (channel in list(red, green, blue)) {
    validate_raster_band(channel$band)
    for (field in c("min", "max")) {
      value <- channel[[field]]
      if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
        stop(
          "a channel's `", field, "` must be a single finite number",
          call. = FALSE
        )
      }
    }
    if (channel$min > channel$max) {
      stop(
        "invalid range: min (", num(channel$min),
        ") must not be greater than max (", num(channel$max), ")",
        call. = FALSE
      )
    }
  }
  list(
    type = "raster_multiband",
    red = red,
    green = green,
    blue = blue,
    algorithm = algorithm,
    opacity = validate_raster_opacity(opacity)
  )
}

# The bands a raster style references (the raster maplayer writes one
# <noDataList> entry per referenced band).
raster_style_bands <- function(style) {
  switch(style$type,
    raster_pseudocolor = ,
    raster_binned = ,
    raster_paletted = style$band,
    raster_multiband = sort(unique(c(
      style$red$band, style$green$band, style$blue$band
    ))),
    stop("unknown raster style type: ", style$type)
  )
}

# Geometry helpers. A geometry type is one of "Point", "LineString",
# "Polygon" (the <maplayer geometry=...> attribute values).
wkb_type_attr <- function(geom) {
  switch(geom,
    Point = "MultiPoint",
    LineString = "MultiLineString",
    Polygon = "MultiPolygon",
    stop("unknown geometry type: ", geom)
  )
}

symbol_type <- function(geom) {
  switch(geom,
    Point = "marker",
    LineString = "line",
    Polygon = "fill",
    stop("unknown geometry type: ", geom)
  )
}

# The recurring <(data[-_]defined[-_]properties)> boilerplate, with an
# optional list(name =, expression =) override in the properties map.
write_data_defined_properties <- function(w, tag, property = NULL) {
  xw_start(w, tag)
  xw_start(w, "Option")
  xw_attr(w, "type", "Map")
  xw_empty(w, "Option", c(name = "name", type = "QString", value = ""))
  if (is.null(property)) {
    xw_empty(w, "Option", c(name = "properties"))
  } else {
    xw_start(w, "Option")
    xw_attr(w, "name", "properties")
    xw_attr(w, "type", "Map")
    xw_start(w, "Option")
    xw_attr(w, "name", property$name)
    xw_attr(w, "type", "Map")
    xw_empty(w, "Option", c(name = "active", type = "bool", value = "true"))
    xw_empty(
      w,
      "Option",
      c(name = "expression", type = "QString", value = property$expression)
    )
    # 3 = expression-based property (QgsProperty::ExpressionBasedProperty).
    xw_empty(w, "Option", c(name = "type", type = "int", value = "3"))
    xw_end(w) # Option (property)
    xw_end(w) # Option (properties)
  }
  xw_empty(
    w,
    "Option",
    c(name = "type", type = "QString", value = "collection")
  )
  xw_end(w) # Option
  xw_end(w) # tag
}

# The nearest QGIS pen-style preset for a linetype, used where only the
# presets exist (SimpleFill and SimpleMarker outlines): a run-length
# pattern is classified by its "on" runs — all short is a dot line, a
# mix of short and long is dash-dot, anything else a dash. Presets pass
# through.
linetype_preset <- function(linetype) {
  if (is.character(linetype)) {
    return(linetype)
  }
  on <- linetype[seq(1L, length(linetype), by = 2L)]
  if (all(on <= 2L)) {
    "dot"
  } else if (any(on <= 2L)) {
    "dash dot"
  } else {
    "dash"
  }
}

# Options of the symbol layer (<Option type="Map"> children), sorted by
# name as QGIS writes them. A NULL fill (polygons) or outline color means
# "not drawn": the style attribute becomes "no" and the color value keeps
# the QGIS default, which QGIS ignores. SimpleMarker has no such flag for
# its interior, so a NULL marker fill (R's open pch) becomes a fully
# transparent color instead. A line's main color cannot be NULL (there
# would be nothing to draw); callers guard that.
# The remaining constants come from `style`: the linetype (a preset or
# run lengths, see style_set_linetype) — only SimpleLine can draw run
# lengths exactly, as a custom dash in millimeters, so a zero line width
# (nothing to scale the runs by) falls back to the preset approximation
# like the other symbol classes — the marker geometry and the alpha of
# each color slot.
symbol_options <- function(geom, style, color, outline_color) {
  scale <- "3x:0,0,0,0,0,0"
  outline_width <- style$outline_width
  linetype <- style$linetype
  custom_dash <- is.numeric(linetype) && outline_width > 0
  fill <- function(rgb) qgis_color(rgb, style$fill_alpha)
  stroke <- function(rgb) qgis_color(rgb, style$stroke_alpha)
  switch(geom,
    Polygon = list(
      border_width_map_unit_scale = scale,
      color = fill(color %||% QGS_DEFAULT_FILL_COLOR),
      joinstyle = "bevel",
      offset = "0,0",
      offset_map_unit_scale = scale,
      offset_unit = "MM",
      outline_color = stroke(outline_color %||% QGS_DEFAULT_OUTLINE_COLOR),
      outline_style = if (is.null(outline_color)) {
        "no"
      } else {
        linetype_preset(linetype)
      },
      outline_width = num(outline_width),
      outline_width_unit = "MM",
      style = if (is.null(color)) "no" else "solid"
    ),
    LineString = list(
      align_dash_pattern = "0",
      capstyle = "square",
      customdash = if (custom_dash) {
        paste(num(round(linetype * outline_width, 7)), collapse = ";")
      } else {
        "5;2" # the QGIS default, inactive placeholder
      },
      customdash_map_unit_scale = scale,
      customdash_unit = "MM",
      dash_pattern_offset = "0",
      dash_pattern_offset_map_unit_scale = scale,
      dash_pattern_offset_unit = "MM",
      draw_inside_polygon = "0",
      joinstyle = "bevel",
      line_color = stroke(color),
      line_style = if (custom_dash) "solid" else linetype_preset(linetype),
      line_width = num(outline_width),
      line_width_unit = "MM",
      offset = "0",
      offset_map_unit_scale = scale,
      offset_unit = "MM",
      ring_filter = "0",
      trim_distance_end = "0",
      trim_distance_end_map_unit_scale = scale,
      trim_distance_end_unit = "MM",
      trim_distance_start = "0",
      trim_distance_start_map_unit_scale = scale,
      trim_distance_start_unit = "MM",
      tweak_dash_pattern_on_corners = "0",
      use_custom_dash = if (custom_dash) "1" else "0",
      width_map_unit_scale = scale
    ),
    Point = list(
      angle = num(style$marker$angle),
      cap_style = "square",
      color = if (is.null(color)) {
        qgis_color(QGS_DEFAULT_FILL_COLOR, 0L)
      } else {
        fill(color)
      },
      horizontal_anchor_point = "1",
      joinstyle = "bevel",
      name = style$marker$name,
      offset = "0,0",
      offset_map_unit_scale = scale,
      offset_unit = "MM",
      outline_color = stroke(outline_color %||% QGS_DEFAULT_OUTLINE_COLOR),
      outline_style = if (is.null(outline_color)) {
        "no"
      } else {
        linetype_preset(linetype)
      },
      outline_width = num(outline_width),
      outline_width_map_unit_scale = scale,
      outline_width_unit = "MM",
      scale_method = "diameter",
      size = num(style$marker$size),
      size_map_unit_scale = scale,
      size_unit = "MM",
      vertical_anchor_point = "1"
    )
  )
}

# The data-defined property that drives the targeted color of a symbol
# layer, as QGIS serializes it (QgsSymbolLayer::propertyDefinitions()).
# SimpleMarker and SimpleFill color both map to PropertyFillColor;
# SimpleLine's color is its stroke, and an explicit stroke target maps to
# PropertyStrokeColor everywhere.
color_property_name <- function(geom, target) {
  if (geom == "LineString" || target == "stroke") {
    "outlineColor"
  } else {
    "fillColor"
  }
}

# Writes a <symbol> element with a single symbol layer: `color` and
# `outline_color` are the resolved colors of this symbol, everything else
# (width, linetype, marker, alphas, target) comes from `style`. The symbol
# layer's targeted color can carry a data-defined expression override.
write_symbol <- function(w, name, geom, style, color, outline_color,
                         color_expression = NULL) {
  target <- style$target %||% "fill"
  class <- switch(geom,
    Point = "SimpleMarker",
    LineString = "SimpleLine",
    Polygon = "SimpleFill",
    stop("unknown geometry type: ", geom)
  )
  xw_start(w, "symbol")
  xw_attr(w, "alpha", "1")
  xw_attr(w, "clip_to_extent", "1")
  xw_attr(w, "force_rhr", "0")
  xw_attr(w, "frame_rate", "10")
  xw_attr(w, "is_animated", "0")
  xw_attr(w, "name", name)
  xw_attr(w, "type", symbol_type(geom))
  write_data_defined_properties(w, "data_defined_properties")
  xw_start(w, "layer")
  xw_attr(w, "class", class)
  xw_attr(w, "enabled", "1")
  xw_attr(w, "id", paste0("{", qgs_uuid(), "}"))
  xw_attr(w, "locked", "0")
  xw_attr(w, "pass", "0")
  xw_start(w, "Option")
  xw_attr(w, "type", "Map")
  options <- symbol_options(geom, style, color, outline_color)
  for (key in names(options)) {
    xw_empty(
      w,
      "Option",
      c(name = key, type = "QString", value = options[[key]])
    )
  }
  xw_end(w) # Option
  property <- if (!is.null(color_expression)) {
    alpha <- if (target == "stroke" || geom == "LineString") {
      style$stroke_alpha
    } else {
      style$fill_alpha
    }
    list(
      name = color_property_name(geom, target),
      expression = with_alpha_expression(color_expression, alpha)
    )
  }
  write_data_defined_properties(w, "data_defined_properties", property)
  xw_end(w) # layer
  xw_end(w) # symbol
}

# A data-defined color expression at the given opacity. The colors a ramp
# expression returns are opaque, so a translucent layer has to override
# the alpha component per feature; an opaque slot keeps the bare
# expression, which is what every project written before had.
with_alpha_expression <- function(expression, alpha) {
  if (alpha >= QGS_OPAQUE) {
    return(expression)
  }
  sprintf("set_color_part(%s,'alpha',%d)", expression, alpha)
}

# Escapes a field name as a double-quoted QGIS expression identifier.
quote_field <- function(name) {
  paste0("\"", gsub("\"", "\"\"", name, fixed = TRUE), "\"")
}

# The ramp_color(create_ramp(...), ...) expression interpolating `stops`
# over the attribute value rescaled from min..max to 0..1. ramp_color()
# clamps values outside the ramp.
continuous_color_expression <- function(attribute, min, max, stops) {
  map_args <- paste(
    g6(stops$offsets),
    paste0("'", apply(stops$colors, 2L, color_hex), "'"),
    sep = ",",
    collapse = ","
  )
  # The span is spelled out as `max - min` so both numbers keep their
  # exact user-facing form (subtracting first would leak float noise like
  # 0.19899999999999998 into the expression).
  sprintf(
    "ramp_color(create_ramp(map(%s)),(%s - %s) / (%s - %s))",
    map_args, quote_field(attribute), num(min), num(max), num(min)
  )
}

# Slices a stops list (used for the intermediate control points of a
# colorramp: everything but the first and last stop).
stops_slice <- function(stops, idx) {
  list(offsets = stops$offsets[idx], colors = stops$colors[, idx, drop = FALSE])
}

# The intermediate control points of a classed style's informational
# colorramp (the one QGIS reuses when re-classifying): the interior bin
# colors placed at their bin midpoints rescaled to 0..1. The first and
# last bin colors are the ramp endpoints, so they are not repeated here;
# NULL when there are no interior bins. Shared by the vector binned
# renderer and the DISCRETE raster one.
binned_colorramp_stops <- function(boundaries, colors) {
  n <- ncol(colors)
  if (n <= 2L) {
    return(NULL)
  }
  interior <- seq(2L, n - 1L)
  mids <- (boundaries[interior] + boundaries[interior + 1L]) / 2
  list(
    offsets = (mids - boundaries[1L]) / (boundaries[n + 1L] - boundaries[1L]),
    colors = colors[, interior, drop = FALSE]
  )
}

# Writes the <colorramp name="[source]" type="gradient"> element for the
# [start, end] endpoints plus optional intermediate `mid_stops`.
write_gradient_colorramp <- function(w, start, end, mid_stops = NULL) {
  xw_start(w, "colorramp")
  xw_attr(w, "name", "[source]")
  xw_attr(w, "type", "gradient")
  xw_start(w, "Option")
  xw_attr(w, "type", "Map")
  values <- list(
    color1 = qgis_color(start),
    color2 = qgis_color(end),
    direction = "ccw",
    discrete = "0",
    rampType = "gradient",
    spec = "rgb"
  )
  for (key in names(values)) {
    xw_empty(
      w,
      "Option",
      c(name = key, type = "QString", value = values[[key]])
    )
  }
  # Intermediate control points, if any:
  # offset;color;rgb;ccw:offset;color;rgb;ccw:...
  if (!is.null(mid_stops) && length(mid_stops$offsets) > 0L) {
    stops <- paste(
      g6(mid_stops$offsets),
      apply(mid_stops$colors, 2L, qgis_color),
      "rgb;ccw",
      sep = ";",
      collapse = ":"
    )
    xw_empty(
      w,
      "Option",
      c(name = "stops", type = "QString", value = stops)
    )
  }
  xw_end(w) # Option
  xw_end(w) # colorramp
}

# Resolves the (fill, outline) colors of one symbol: the varying
# (ramp/category) color goes to the slot `target` points at, the other
# slot keeps its constant color.
target_colors <- function(target, varying, fill, outline) {
  if (target == "fill") {
    list(color = varying, outline = outline)
  } else {
    list(color = fill, outline = varying)
  }
}

# Writes the <renderer-v2> element for a vector layer.
write_renderer <- function(w, geom, style) {
  switch(style$type,
    none = write_null_renderer(w),
    single = write_single_renderer(w, geom, style),
    continuous = write_continuous_renderer(w, geom, style),
    graduated = write_graduated_renderer(w, geom, style),
    binned = write_binned_renderer(w, geom, style),
    categorized = write_categorized_renderer(w, geom, style),
    stop("unknown style type: ", style$type)
  )
}

write_null_renderer <- function(w) {
  xw_start(w, "renderer-v2")
  xw_attr(w, "enableorderby", "0")
  xw_attr(w, "forceraster", "0")
  xw_attr(w, "referencescale", "-1")
  xw_attr(w, "symbollevels", "0")
  xw_attr(w, "type", "nullSymbol")
  xw_end(w)
}

write_single_renderer <- function(w, geom, style) {
  xw_start(w, "renderer-v2")
  xw_attr(w, "enableorderby", "0")
  xw_attr(w, "forceraster", "0")
  xw_attr(w, "referencescale", "-1")
  xw_attr(w, "symbollevels", "0")
  xw_attr(w, "type", "singleSymbol")
  xw_start(w, "symbols")
  write_symbol(
    w, "0", geom, style, style$color, style$outline_color
  )
  xw_end(w) # symbols
  xw_empty(w, "rotation")
  xw_empty(w, "sizescale")
  write_data_defined_properties(w, "data-defined-properties")
  xw_end(w) # renderer-v2
}

write_continuous_renderer <- function(w, geom, style) {
  expression <- continuous_color_expression(
    style$attribute, style$min, style$max, style$stops
  )
  xw_start(w, "renderer-v2")
  xw_attr(w, "enableorderby", "0")
  xw_attr(w, "forceraster", "0")
  xw_attr(w, "referencescale", "-1")
  xw_attr(w, "symbollevels", "0")
  xw_attr(w, "type", "singleSymbol")
  xw_start(w, "symbols")
  # The static varying color (also the legend swatch) is the middle of
  # the ramp; per feature it is overridden by the expression on the
  # targeted color property.
  colors <- target_colors(
    style$target,
    sample_ramp(style$stops, 0.5),
    style$fill_color,
    style$outline_color
  )
  write_symbol(
    w, "0", geom, style, colors$color, colors$outline,
    color_expression = expression
  )
  xw_end(w) # symbols
  xw_empty(w, "rotation")
  xw_empty(w, "sizescale")
  write_data_defined_properties(w, "data-defined-properties")
  xw_end(w) # renderer-v2
}

write_graduated_renderer <- function(w, geom, style) {
  classes <- style$classes
  step <- (style$max - style$min) / classes
  precision <- label_precision(step)
  xw_start(w, "renderer-v2")
  xw_attr(w, "attr", style$attribute)
  xw_attr(w, "enableorderby", "0")
  xw_attr(w, "forceraster", "0")
  xw_attr(w, "graduatedMethod", "GraduatedColor")
  xw_attr(w, "referencescale", "-1")
  xw_attr(w, "symbollevels", "0")
  xw_attr(w, "type", "graduatedSymbol")
  xw_start(w, "ranges")
  for (i in seq_len(classes) - 1L) {
    lower <- style$min + step * i
    upper <- lower + step
    xw_start(w, "range")
    xw_attr(w, "label", range_label(lower, upper, precision))
    xw_attr(w, "lower", sprintf("%.15f", lower))
    xw_attr(w, "render", "true")
    xw_attr(w, "symbol", i)
    xw_attr(w, "upper", sprintf("%.15f", upper))
    xw_attr(w, "uuid", paste0("{", qgs_uuid(), "}"))
    xw_end(w)
  }
  xw_end(w) # ranges
  xw_start(w, "symbols")
  for (i in seq_len(classes) - 1L) {
    t <- i / (classes - 1L)
    colors <- target_colors(
      style$target,
      sample_ramp(style$stops, t),
      style$fill_color,
      style$outline_color
    )
    write_symbol(
      w, i, geom, style, colors$color, colors$outline
    )
  }
  xw_end(w) # symbols
  xw_start(w, "source-symbol")
  colors <- target_colors(
    style$target,
    style$stops$colors[, 1L],
    style$fill_color,
    style$outline_color
  )
  write_symbol(
    w, "0", geom, style, colors$color, colors$outline
  )
  xw_end(w) # source-symbol
  n_stops <- length(style$stops$offsets)
  write_gradient_colorramp(
    w,
    style$stops$colors[, 1L],
    style$stops$colors[, n_stops],
    stops_slice(style$stops, seq_len(n_stops)[-c(1L, n_stops)])
  )
  xw_start(w, "classificationMethod")
  xw_attr(w, "id", "Pretty")
  xw_empty(
    w,
    "symmetricMode",
    c(astride = "0", enabled = "0", symmetrypoint = "0")
  )
  xw_empty(
    w,
    "labelFormat",
    c(
      format = "%1 - %2",
      labelprecision = precision,
      trimtrailingzeroes = "1"
    )
  )
  xw_start(w, "parameters")
  xw_empty(w, "Option")
  xw_end(w) # parameters
  xw_empty(w, "extraInformation")
  xw_end(w) # classificationMethod
  xw_empty(w, "rotation")
  xw_empty(w, "sizescale")
  write_data_defined_properties(w, "data-defined-properties")
  xw_end(w) # renderer-v2
}

# A binned style is a graduated renderer too, but with the explicit
# (possibly unequal) bin boundaries as the ranges and each bin's exact
# color on its symbol, instead of equal intervals colored by sampling a
# ramp.
write_binned_renderer <- function(w, geom, style) {
  boundaries <- style$boundaries
  n <- length(boundaries) - 1L
  precision <- exact_label_precision(boundaries)
  xw_start(w, "renderer-v2")
  xw_attr(w, "attr", style$attribute)
  xw_attr(w, "enableorderby", "0")
  xw_attr(w, "forceraster", "0")
  xw_attr(w, "graduatedMethod", "GraduatedColor")
  xw_attr(w, "referencescale", "-1")
  xw_attr(w, "symbollevels", "0")
  xw_attr(w, "type", "graduatedSymbol")
  xw_start(w, "ranges")
  for (i in seq_len(n)) {
    xw_start(w, "range")
    xw_attr(w, "label", range_label(boundaries[i], boundaries[i + 1L], precision))
    xw_attr(w, "lower", sprintf("%.15f", boundaries[i]))
    xw_attr(w, "render", "true")
    xw_attr(w, "symbol", i - 1L)
    xw_attr(w, "upper", sprintf("%.15f", boundaries[i + 1L]))
    xw_attr(w, "uuid", paste0("{", qgs_uuid(), "}"))
    xw_end(w)
  }
  xw_end(w) # ranges
  xw_start(w, "symbols")
  for (i in seq_len(n)) {
    colors <- target_colors(
      style$target,
      style$colors[, i],
      style$fill_color,
      style$outline_color
    )
    write_symbol(
      w, i - 1L, geom, style, colors$color, colors$outline
    )
  }
  xw_end(w) # symbols
  xw_start(w, "source-symbol")
  colors <- target_colors(
    style$target,
    style$colors[, 1L],
    style$fill_color,
    style$outline_color
  )
  write_symbol(
    w, "0", geom, style, colors$color, colors$outline
  )
  xw_end(w) # source-symbol
  write_gradient_colorramp(
    w,
    style$colors[, 1L],
    style$colors[, n],
    binned_colorramp_stops(boundaries, style$colors)
  )
  xw_start(w, "classificationMethod")
  xw_attr(w, "id", "Pretty")
  xw_empty(
    w,
    "symmetricMode",
    c(astride = "0", enabled = "0", symmetrypoint = "0")
  )
  xw_empty(
    w,
    "labelFormat",
    c(
      format = "%1 - %2",
      labelprecision = precision,
      trimtrailingzeroes = "1"
    )
  )
  xw_start(w, "parameters")
  xw_empty(w, "Option")
  xw_end(w) # parameters
  xw_empty(w, "extraInformation")
  xw_end(w) # classificationMethod
  xw_empty(w, "rotation")
  xw_empty(w, "sizescale")
  write_data_defined_properties(w, "data-defined-properties")
  xw_end(w) # renderer-v2
}

write_categorized_renderer <- function(w, geom, style) {
  n <- length(style$values)
  xw_start(w, "renderer-v2")
  xw_attr(w, "attr", style$attribute)
  xw_attr(w, "enableorderby", "0")
  xw_attr(w, "forceraster", "0")
  xw_attr(w, "referencescale", "-1")
  xw_attr(w, "symbollevels", "0")
  xw_attr(w, "type", "categorizedSymbol")
  xw_start(w, "categories")
  for (i in seq_len(n)) {
    xw_start(w, "category")
    xw_attr(w, "label", style$values[i])
    xw_attr(w, "render", "true")
    xw_attr(w, "symbol", i - 1L)
    xw_attr(w, "type", style$value_type)
    xw_attr(w, "uuid", paste0("{", qgs_uuid(), "}"))
    xw_attr(w, "value", style$values[i])
    xw_end(w)
  }
  if (!is.null(style$catch_all)) {
    xw_start(w, "category")
    xw_attr(w, "label", "")
    xw_attr(w, "render", "true")
    xw_attr(w, "symbol", n)
    xw_attr(w, "type", "NULL")
    xw_attr(w, "uuid", paste0("{", qgs_uuid(), "}"))
    xw_attr(w, "value", "NULL")
    xw_end(w)
  }
  xw_end(w) # categories
  xw_start(w, "symbols")
  for (i in seq_len(n)) {
    colors <- target_colors(
      style$target,
      style$colors[, i],
      style$fill_color,
      style$outline_color
    )
    write_symbol(
      w, i - 1L, geom, style, colors$color, colors$outline
    )
  }
  if (!is.null(style$catch_all)) {
    colors <- target_colors(
      style$target,
      style$catch_all,
      style$fill_color,
      style$outline_color
    )
    write_symbol(
      w, n, geom, style, colors$color, colors$outline
    )
  }
  xw_end(w) # symbols
  xw_start(w, "source-symbol")
  colors <- target_colors(
    style$target,
    style$colors[, 1L],
    style$fill_color,
    style$outline_color
  )
  write_symbol(
    w, "0", geom, style, colors$color, colors$outline
  )
  xw_end(w) # source-symbol
  # The colorramp is only informational for a categorized renderer (used
  # when re-classifying); derive it from the first/last category color.
  last_color <- if (!is.null(style$catch_all)) {
    style$catch_all
  } else {
    style$colors[, n]
  }
  write_gradient_colorramp(w, style$colors[, 1L], last_color)
  xw_empty(w, "rotation")
  xw_empty(w, "sizescale")
  write_data_defined_properties(w, "data-defined-properties")
  xw_end(w) # renderer-v2
}

# Writes the <rasterrenderer> element for a GDAL raster layer.
write_raster_renderer <- function(w, style) {
  switch(style$type,
    raster_pseudocolor = write_pseudocolor_renderer(w, style),
    raster_binned = write_discrete_pseudocolor_renderer(w, style),
    raster_paletted = write_paletted_renderer(w, style),
    raster_multiband = write_multiband_renderer(w, style),
    stop("unknown raster style type: ", style$type)
  )
}

# The `nodataColor` attribute of a raster renderer: the color QGIS paints
# the source's nodata cells in, as a QGIS color string. Empty (the
# default) leaves them transparent.
nodata_color_attr <- function(nodata_color) {
  if (is.null(nodata_color)) {
    return("")
  }
  qgis_color(nodata_color[1:3], nodata_color[[4L]])
}

# Multiband color, a.k.a. true color (samples/true-color.qgs of the
# generate-qgs crate): three bands drive the red, green and blue
# channels through one <*ContrastEnhancement> element each; there is no
# <rastershader>.
write_multiband_renderer <- function(w, style) {
  xw_start(w, "rasterrenderer")
  xw_attr(w, "alphaBand", "-1")
  xw_attr(w, "blueBand", style$blue$band)
  xw_attr(w, "greenBand", style$green$band)
  xw_attr(w, "nodataColor", "")
  xw_attr(w, "opacity", num(style$opacity))
  xw_attr(w, "redBand", style$red$band)
  xw_attr(w, "type", "multibandcolor")
  xw_empty(w, "rasterTransparency")
  # <minMaxOrigin> records where the channel min/max come from: the band
  # statistics for NoEnhancement (what QGIS saves for a Byte RGB
  # raster), user-chosen values ("None") for an explicit stretch.
  write_min_max_origin(
    w,
    if (style$algorithm == "NoEnhancement") "MinMax" else "None"
  )
  channels <- list(
    redContrastEnhancement = style$red,
    greenContrastEnhancement = style$green,
    blueContrastEnhancement = style$blue
  )
  for (tag in names(channels)) {
    channel <- channels[[tag]]
    xw_start(w, tag)
    xw_elem(w, "minValue", num(channel$min))
    xw_elem(w, "maxValue", num(channel$max))
    xw_elem(w, "algorithm", style$algorithm)
    xw_end(w) # tag
  }
  xw_end(w) # rasterrenderer
}

# Single-band pseudocolor (samples/elevation.qgs): the band value is
# colored through <rastershader>/<colorrampshader colorRampType=
# "INTERPOLATED">, one <item> per color stop, interpolated linearly in
# between (values outside min..max are clamped to the end colors).
write_pseudocolor_renderer <- function(w, style) {
  xw_start(w, "rasterrenderer")
  xw_attr(w, "alphaBand", "-1")
  xw_attr(w, "band", style$band)
  xw_attr(w, "classificationMax", num(style$max))
  xw_attr(w, "classificationMin", num(style$min))
  xw_attr(w, "nodataColor", nodata_color_attr(style$nodata_color))
  xw_attr(w, "opacity", num(style$opacity))
  xw_attr(w, "type", "singlebandpseudocolor")
  xw_empty(w, "rasterTransparency")
  write_min_max_origin(w, "None")
  xw_start(w, "rastershader")
  xw_start(w, "colorrampshader")
  xw_attr(w, "classificationMode", "1")
  xw_attr(w, "clip", "0")
  xw_attr(w, "colorRampType", "INTERPOLATED")
  xw_attr(w, "labelPrecision", "0")
  xw_attr(w, "maximumValue", num(style$max))
  xw_attr(w, "minimumValue", num(style$min))
  n <- length(style$stops$offsets)
  write_gradient_colorramp(
    w,
    style$stops$colors[, 1L],
    style$stops$colors[, n],
    stops_slice(style$stops, seq_len(n)[-c(1L, n)])
  )
  for (i in seq_len(n)) {
    value <- num(style$min + (style$max - style$min) * style$stops$offsets[i])
    xw_empty(
      w,
      "item",
      c(
        alpha = "255",
        color = color_hex(style$stops$colors[, i]),
        label = value,
        value = value
      )
    )
  }
  write_ramp_legend_settings(w)
  xw_end(w) # colorrampshader
  xw_end(w) # rastershader
  xw_end(w) # rasterrenderer
}

# Classed single-band pseudocolor: the same element as the INTERPOLATED
# writer above with colorRampType="DISCRETE", where each <item> `value`
# is the *inclusive upper bound* of its class and the last one is
# open-ended ("inf"). Verified against what QGIS 4.2.0 writes for a
# QgsColorRampShader in Discrete mode.
write_discrete_pseudocolor_renderer <- function(w, style) {
  boundaries <- style$boundaries
  n <- ncol(style$colors)
  min <- boundaries[[1L]]
  max <- boundaries[[n + 1L]]
  xw_start(w, "rasterrenderer")
  xw_attr(w, "alphaBand", "-1")
  xw_attr(w, "band", style$band)
  xw_attr(w, "classificationMax", num(max))
  xw_attr(w, "classificationMin", num(min))
  xw_attr(w, "nodataColor", nodata_color_attr(style$nodata_color))
  xw_attr(w, "opacity", num(style$opacity))
  xw_attr(w, "type", "singlebandpseudocolor")
  xw_empty(w, "rasterTransparency")
  write_min_max_origin(w, "None")
  xw_start(w, "rastershader")
  xw_start(w, "colorrampshader")
  xw_attr(w, "classificationMode", "1")
  xw_attr(w, "clip", "0")
  xw_attr(w, "colorRampType", "DISCRETE")
  xw_attr(w, "labelPrecision", "0")
  xw_attr(w, "maximumValue", num(max))
  xw_attr(w, "minimumValue", num(min))
  write_gradient_colorramp(
    w,
    style$colors[, 1L],
    style$colors[, n],
    binned_colorramp_stops(boundaries, style$colors)
  )
  for (i in seq_len(n)) {
    # The upper bound closes every class but the last, which catches
    # everything above the final break.
    value <- if (i == n) "inf" else num(boundaries[[i + 1L]])
    xw_empty(
      w,
      "item",
      c(
        alpha = "255",
        color = color_hex(style$colors[, i]),
        label = style$labels[[i]],
        value = value
      )
    )
  }
  write_ramp_legend_settings(w)
  xw_end(w) # colorrampshader
  xw_end(w) # rastershader
  xw_end(w) # rasterrenderer
}

# Paletted, a.k.a. "unique values": one <paletteEntry> per exact cell
# value. Cells matching no entry are not drawn. There is no
# <rastershader> and no classificationMin/Max. Verified against what
# QGIS 4.2.0 writes for a QgsPalettedRasterRenderer.
write_paletted_renderer <- function(w, style) {
  xw_start(w, "rasterrenderer")
  xw_attr(w, "alphaBand", "-1")
  xw_attr(w, "band", style$band)
  xw_attr(w, "nodataColor", nodata_color_attr(style$nodata_color))
  xw_attr(w, "opacity", num(style$opacity))
  xw_attr(w, "type", "paletted")
  xw_empty(w, "rasterTransparency")
  write_min_max_origin(w, "None")
  xw_start(w, "colorPalette")
  for (i in seq_along(style$values)) {
    xw_empty(
      w,
      "paletteEntry",
      c(
        alpha = "255",
        color = color_hex(style$colors[, i]),
        label = style$labels[[i]],
        value = num(style$values[[i]])
      )
    )
  }
  xw_end(w) # colorPalette
  xw_end(w) # rasterrenderer
}

# Static legend boilerplate of a <colorrampshader> (byte-identical across
# the QGIS-saved samples).
write_ramp_legend_settings <- function(w) {
  xw_start(w, "rampLegendSettings")
  xw_attr(w, "direction", "0")
  xw_attr(w, "maximumLabel", "")
  xw_attr(w, "minimumLabel", "")
  xw_attr(w, "orientation", "2")
  xw_attr(w, "prefix", "")
  xw_attr(w, "suffix", "")
  xw_attr(w, "useContinuousLegend", "1")
  xw_start(w, "numericFormat")
  xw_attr(w, "id", "basic")
  xw_start(w, "Option")
  xw_attr(w, "type", "Map")
  xw_empty(w, "Option", c(name = "decimal_separator", type = "invalid"))
  xw_empty(w, "Option", c(name = "decimals", type = "int", value = "6"))
  xw_empty(w, "Option", c(name = "rounding_type", type = "int", value = "0"))
  xw_empty(w, "Option", c(name = "show_plus", type = "bool", value = "false"))
  xw_empty(
    w,
    "Option",
    c(name = "show_thousand_separator", type = "bool", value = "true")
  )
  xw_empty(
    w,
    "Option",
    c(name = "show_trailing_zeros", type = "bool", value = "false")
  )
  xw_empty(w, "Option", c(name = "thousand_separator", type = "invalid"))
  xw_end(w) # Option
  xw_end(w) # numericFormat
  xw_end(w) # rampLegendSettings
}
