# Symbology: vector styles and <renderer-v2> generation.
#
# Ported from src/style.rs of the generate-qgs crate (the vector renderers;
# the raster renderers were not ported because write_qgs() has no raster
# path yet — the Rust crate remains the reference if one is added).
#
# A style is a plain named list with a `type` field:
#   - "single":      color
#   - "graduated":   attribute, classes, min, max, stops
#   - "binned":      attribute, boundaries, colors
#   - "continuous":  attribute, min, max, stops
#   - "categorized": attribute, values, colors, catch_all
# plus the shared fields target ("fill"/"stroke"), fill_color,
# outline_color and outline_width. A color is an integer vector c(r, g, b)
# in 0..255; `stops` is list(offsets = <numeric n>, colors = <3 x n
# integer matrix>) with offsets ascending from 0 to 1.

QGS_DEFAULT_OUTLINE_COLOR <- c(35L, 35L, 35L)
QGS_DEFAULT_OUTLINE_WIDTH <- 0.26
QGS_DEFAULT_FILL_COLOR <- c(229L, 229L, 229L)

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

# Single symbol with the given fill color, dark gray 0.26 mm outline
# (QGIS defaults).
style_single <- function(color) {
  list(
    type = "single",
    color = color,
    outline_color = QGS_DEFAULT_OUTLINE_COLOR,
    outline_width = QGS_DEFAULT_OUTLINE_WIDTH
  )
}

# Graduated coloring of `attribute` with `classes` equal-interval ranges
# between `min` and `max`. Class colors are interpolated along `stops`.
style_graduated <- function(attribute, classes, min, max, stops) {
  validate_ramp(classes, min, max, stops)
  list(
    type = "graduated",
    attribute = attribute,
    classes = as.integer(classes),
    min = min,
    max = max,
    stops = stops,
    target = "fill",
    fill_color = QGS_DEFAULT_FILL_COLOR,
    outline_color = QGS_DEFAULT_OUTLINE_COLOR,
    outline_width = QGS_DEFAULT_OUTLINE_WIDTH
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
  list(
    type = "binned",
    attribute = attribute,
    boundaries = boundaries,
    colors = colors,
    target = "fill",
    fill_color = QGS_DEFAULT_FILL_COLOR,
    outline_color = QGS_DEFAULT_OUTLINE_COLOR,
    outline_width = QGS_DEFAULT_OUTLINE_WIDTH
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
  list(
    type = "continuous",
    attribute = attribute,
    min = min,
    max = max,
    stops = stops,
    target = "fill",
    fill_color = QGS_DEFAULT_FILL_COLOR,
    outline_color = QGS_DEFAULT_OUTLINE_COLOR,
    outline_width = QGS_DEFAULT_OUTLINE_WIDTH
  )
}

# Discrete coloring of `attribute`: each value/color pair becomes one
# <category> linked to one <symbol>. `values` is a character vector,
# `colors` a 3 x n integer matrix; `catch_all`, if given, is the color of
# the trailing "all other values" (value="NULL") category.
style_categorized <- function(attribute, values, colors, catch_all = NULL) {
  if (length(values) == 0L) {
    stop("categorized style needs at least 1 category", call. = FALSE)
  }
  if (length(values) != ncol(colors)) {
    stop(
      "category values and colors must have the same length",
      call. = FALSE
    )
  }
  list(
    type = "categorized",
    attribute = attribute,
    values = as.character(values),
    colors = colors,
    catch_all = catch_all,
    target = "fill",
    fill_color = QGS_DEFAULT_FILL_COLOR,
    outline_color = QGS_DEFAULT_OUTLINE_COLOR,
    outline_width = QGS_DEFAULT_OUTLINE_WIDTH
  )
}

# Sets the constant outline (stroke) color and width in millimeters. For a
# style whose varying color targets the stroke, the width still applies
# but the color is ignored.
style_set_outline <- function(style, color, width) {
  style$outline_color <- color
  style$outline_width <- width
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

# Options of the symbol layer (<Option type="Map"> children), sorted by
# name as QGIS writes them.
symbol_options <- function(geom, color, outline_color, outline_width) {
  scale <- "3x:0,0,0,0,0,0"
  switch(geom,
    Polygon = list(
      border_width_map_unit_scale = scale,
      color = qgis_color(color),
      joinstyle = "bevel",
      offset = "0,0",
      offset_map_unit_scale = scale,
      offset_unit = "MM",
      outline_color = qgis_color(outline_color),
      outline_style = "solid",
      outline_width = num(outline_width),
      outline_width_unit = "MM",
      style = "solid"
    ),
    LineString = list(
      align_dash_pattern = "0",
      capstyle = "square",
      customdash = "5;2",
      customdash_map_unit_scale = scale,
      customdash_unit = "MM",
      dash_pattern_offset = "0",
      dash_pattern_offset_map_unit_scale = scale,
      dash_pattern_offset_unit = "MM",
      draw_inside_polygon = "0",
      joinstyle = "bevel",
      line_color = qgis_color(color),
      line_style = "solid",
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
      use_custom_dash = "0",
      width_map_unit_scale = scale
    ),
    Point = list(
      angle = "0",
      cap_style = "square",
      color = qgis_color(color),
      horizontal_anchor_point = "1",
      joinstyle = "bevel",
      name = "circle",
      offset = "0,0",
      offset_map_unit_scale = scale,
      offset_unit = "MM",
      outline_color = qgis_color(outline_color),
      outline_style = "solid",
      outline_width = num(outline_width),
      outline_width_map_unit_scale = scale,
      outline_width_unit = "MM",
      scale_method = "diameter",
      size = "2",
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

# Writes a <symbol> element with a single symbol layer. The symbol layer's
# targeted color can carry a data-defined expression override.
write_symbol <- function(w, name, geom, color, outline_color, outline_width,
                         color_expression = NULL, target = "fill") {
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
  options <- symbol_options(geom, color, outline_color, outline_width)
  for (key in names(options)) {
    xw_empty(
      w,
      "Option",
      c(name = key, type = "QString", value = options[[key]])
    )
  }
  xw_end(w) # Option
  property <- if (!is.null(color_expression)) {
    list(
      name = color_property_name(geom, target),
      expression = color_expression
    )
  }
  write_data_defined_properties(w, "data_defined_properties", property)
  xw_end(w) # layer
  xw_end(w) # symbol
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
    single = write_single_renderer(w, geom, style),
    continuous = write_continuous_renderer(w, geom, style),
    graduated = write_graduated_renderer(w, geom, style),
    binned = write_binned_renderer(w, geom, style),
    categorized = write_categorized_renderer(w, geom, style),
    stop("unknown style type: ", style$type)
  )
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
    w, "0", geom, style$color, style$outline_color, style$outline_width
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
    w, "0", geom, colors$color, colors$outline, style$outline_width,
    color_expression = expression, target = style$target
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
      w, i, geom, colors$color, colors$outline, style$outline_width
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
    w, "0", geom, colors$color, colors$outline, style$outline_width
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
      w, i - 1L, geom, colors$color, colors$outline, style$outline_width
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
    w, "0", geom, colors$color, colors$outline, style$outline_width
  )
  xw_end(w) # source-symbol
  # The colorramp is only informational (used when re-classifying):
  # first/last bin colors as the endpoints, the interior bin colors as
  # control points at their bin midpoints rescaled to 0..1.
  mid_stops <- NULL
  if (n > 2L) {
    interior <- seq(2L, n - 1L)
    mids <- (boundaries[interior] + boundaries[interior + 1L]) / 2
    mid_stops <- list(
      offsets = (mids - boundaries[1L]) / (boundaries[n + 1L] - boundaries[1L]),
      colors = style$colors[, interior, drop = FALSE]
    )
  }
  write_gradient_colorramp(
    w, style$colors[, 1L], style$colors[, n], mid_stops
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
    xw_attr(w, "type", "string")
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
      w, i - 1L, geom, colors$color, colors$outline, style$outline_width
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
      w, n, geom, colors$color, colors$outline, style$outline_width
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
    w, "0", geom, colors$color, colors$outline, style$outline_width
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
