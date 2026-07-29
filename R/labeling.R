# Text/label layers: geom_sf_text / geom_sf_label / geom_text / geom_label
# converted to a labels-only QGIS layer (a null-symbol renderer plus a
# <labeling type="simple"> element).
#
# The expected XML was pinned down against samples/nc_text.qgs and
# samples/nc_label.qgs (QGIS 4.2-saved projects with labels enabled): the
# whole block is written the way QGIS serializes it, and only the few
# styles carried over from ggplot2 are parameterized — the labeled field,
# the text size, the font family, the text color and (for the label
# geoms) the background fill. Everything else (font style, buffer,
# shadow, rounded corners, ...) keeps the QGIS defaults.

# Points per ggplot2 size unit: the text size is in millimeters and QGIS
# wants points (ggplot2's .pt, 72.27 pt per inch / 25.4 mm per inch).
QGS_PT_PER_MM <- 72.27 / 25.4

QGS_TEXT_GEOMS <- c("GeomText", "GeomLabel")

qgs_is_text_layer <- function(layer) {
  class(layer$geom)[[1L]] %in% QGS_TEXT_GEOMS
}

# Resolves the `label` aesthetic and the constant text styles of a text/
# label layer into the settings written by write_labeling(). Like
# fill/colour, only a bare column name is supported for `label` (the raw
# data is what's written to the GeoPackage, so the labeled field must be
# one of its columns).
#
# The result is a list:
#   - field — the labeled column.
#   - size — text size in points.
#   - color — text color as c(r, g, b).
#   - family — font family, or NULL for the QGIS default font.
#   - background — background fill as c(r, g, b) (geom_label's `fill`),
#     or NULL for no background (text geoms, or fill = NA).
qgs_label_spec <- function(plot, built, layer, i, d) {
  quo <- layer$mapping[["label"]] %||% plot@mapping[["label"]]
  if (is.null(quo)) {
    stop(
      "layer ", i, ": a text/label layer needs a `label` aesthetic",
      call. = FALSE
    )
  }
  if (!(rlang::is_quosure(quo) && rlang::quo_is_symbol(quo))) {
    stop(
      "layer ", i, ": only a bare column name is supported for `label`, ",
      "got `", rlang::as_label(quo), "`",
      call. = FALSE
    )
  }
  field <- rlang::as_string(rlang::quo_get_expr(quo))
  if (!field %in% names(d)) {
    stop(
      "layer ", i, ": column `", field, "` not found in the layer data",
      call. = FALSE
    )
  }

  # The constant styles ggplot2 computed for the layer, from its first
  # feature (like qgs_layer_constants(); only meaningful when they are
  # not mapped).
  computed <- built@data[[i]]
  first_or <- function(name, default) {
    v <- computed[[name]]
    if (length(v) == 0L || is.na(v[[1L]])) default else v[[1L]]
  }

  family <- as.character(first_or("family", ""))
  # Only geom_label has a background; its fill = NA means "no background",
  # which qgs_rgb() maps to NULL.
  background <- if (class(layer$geom)[[1L]] == "GeomLabel") {
    fill <- computed[["fill"]]
    qgs_rgb(if (length(fill) == 0L) "white" else fill[[1L]])
  }

  list(
    field = field,
    # Rounded so binary float noise stays out of the project file.
    size = round(first_or("size", 3.88) * QGS_PT_PER_MM, 7),
    color = qgs_rgb(first_or("colour", "black")),
    family = if (nzchar(family)) family,
    background = background
  )
}

# Rejects a position adjustment on a text/label layer: QGIS places the
# labels itself, so a nudge cannot be represented. The text geoms default
# to PositionNudge with no nudge (a plain identity), which is fine; the
# nudge amounts live outside the position object — `nudge_x`/`nudge_y`
# constants land in aes_params (mapped ones in the mapping), and
# position_nudge() sets the position's own x/y.
qgs_check_text_position <- function(layer, i) {
  pos <- layer$position
  nudged <- if (inherits(pos, "PositionNudge")) {
    some_nonzero <- function(v) !is.null(v) && any(v != 0)
    some_nonzero(pos$x) || some_nonzero(pos$y) ||
      some_nonzero(layer$aes_params[["nudge_x"]]) ||
      some_nonzero(layer$aes_params[["nudge_y"]]) ||
      !is.null(layer$mapping[["nudge_x"]]) ||
      !is.null(layer$mapping[["nudge_y"]])
  } else {
    !inherits(pos, "PositionIdentity")
  }
  if (nudged) {
    stop(
      "layer ", i, ": a position adjustment (nudging) is not supported ",
      "for a text/label layer",
      call. = FALSE
    )
  }
}

# Converts a geom_text()/geom_label() data.frame layer to point features,
# one per row with every column kept, under the same rules as the
# data.frame geoms (see df_layer.R): coord_sf() provides the CRS, x/y must
# be bare untransformed columns, and the stat and position must be
# identity.
qgs_text_df_sf <- function(plot, built, layer, i, d) {
  if (inherits(d, "sf")) {
    stop(
      "layer ", i, ": use geom_sf_text()/geom_sf_label() for sf data",
      call. = FALSE
    )
  }
  if (!inherits(layer$stat, "StatIdentity")) {
    stop(
      "layer ", i, ": only the identity stat is supported for ",
      "geom_text()/geom_label(), got ", class(layer$stat)[[1L]],
      call. = FALSE
    )
  }
  crs <- qgs_df_layer_crs(plot, built, i)
  xy <- qgs_df_xy(plot, layer, d, i)
  qgs_df_aligned(built@data[[i]], d, xy, i)
  qgs_df_points_sf(d, xy, crs)
}

# Writes the <labeling type="simple"> element of a vector maplayer.
# `label` is a qgs_label_spec() result; `geom` drives the placement (see
# write_label_placement()).
write_labeling <- function(w, geom, label) {
  xw_start(w, "labeling")
  xw_attr(w, "type", "simple")
  xw_start(w, "settings")
  xw_attr(w, "calloutType", "simple")
  write_label_text_style(w, label)
  xw_empty(
    w,
    "text-format",
    c(
      addDirectionSymbol = "0",
      autoWrapLength = "0",
      decimals = "3",
      formatNumbers = "0",
      leftDirectionSymbol = "<",
      multilineAlign = "3",
      placeDirectionSymbol = "0",
      plussign = "0",
      reverseDirectionSymbol = "0",
      rightDirectionSymbol = ">",
      useMaxLineLengthForAutoWrap = "1",
      wrapChar = ""
    )
  )
  write_label_placement(w, geom)
  xw_empty(
    w,
    "rendering",
    c(
      drawLabels = "1",
      fontLimitPixelSize = "0",
      fontMaxPixelSize = "10000",
      fontMinPixelSize = "3",
      limitNumLabels = "0",
      maxNumLabels = "2000",
      mergeLines = "0",
      minFeatureSize = "0",
      obstacle = "1",
      obstacleFactor = "1",
      obstacleType = "1",
      scaleMax = "0",
      scaleMin = "0",
      scaleVisibility = "0",
      unplacedVisibility = "0",
      upsidedownLabels = "0",
      zIndex = "0"
    )
  )
  write_data_defined_properties(w, "dd_properties")
  write_label_callout(w)
  xw_end(w) # settings
  xw_end(w) # labeling
}

write_label_text_style <- function(w, label) {
  scale <- "3x:0,0,0,0,0,0"
  xw_start(w, "text-style")
  xw_attr(w, "allowHtml", "0")
  xw_attr(w, "blendMode", "0")
  xw_attr(w, "capitalization", "0")
  xw_attr(w, "fieldName", label$field)
  # No fontFamily attribute means the QGIS default font (attributes are
  # alphabetical, the QDom order, so it slots in here).
  if (!is.null(label$family)) {
    xw_attr(w, "fontFamily", label$family)
  }
  xw_attr(w, "fontItalic", "0")
  xw_attr(w, "fontKerning", "1")
  xw_attr(w, "fontLetterSpacing", "0")
  xw_attr(w, "fontSize", num(label$size))
  xw_attr(w, "fontSizeMapUnitScale", scale)
  xw_attr(w, "fontSizeUnit", "Point")
  xw_attr(w, "fontStrikeout", "0")
  xw_attr(w, "fontUnderline", "0")
  xw_attr(w, "fontWeight", "400")
  xw_attr(w, "fontWordSpacing", "0")
  xw_attr(w, "forcedBold", "0")
  xw_attr(w, "forcedItalic", "0")
  xw_attr(w, "isExpression", "0")
  xw_attr(w, "legendString", "Aa")
  xw_attr(w, "multilineHeight", "1")
  xw_attr(w, "multilineHeightUnit", "Percentage")
  xw_attr(w, "namedStyle", "Regular")
  xw_attr(w, "previewBkgrdColor", "255,255,255,255,rgb:1,1,1,1")
  xw_attr(w, "stretchFactor", "100")
  xw_attr(w, "tabStopDistance", "80")
  xw_attr(w, "tabStopDistanceMapUnitScale", scale)
  xw_attr(w, "tabStopDistanceUnit", "Point")
  xw_attr(w, "textColor", qgis_color(label$color))
  xw_attr(w, "textOpacity", "1")
  xw_attr(w, "textOrientation", "horizontal")
  xw_attr(w, "useSubstitutions", "0")
  xw_empty(w, "families")
  xw_empty(
    w,
    "text-buffer",
    c(
      bufferBlendMode = "0",
      bufferColor = "250,250,250,255,rgb:0.9803922,0.9803922,0.9803922,1",
      bufferDraw = "0",
      bufferJoinStyle = "128",
      bufferNoFill = "1",
      bufferOpacity = "1",
      bufferSize = "1",
      bufferSizeMapUnitScale = scale,
      bufferSizeUnits = "MM"
    )
  )
  xw_empty(
    w,
    "text-mask",
    c(
      maskEnabled = "0",
      maskJoinStyle = "128",
      maskOpacity = "1",
      maskSize = "1.5",
      maskSize2 = "1.5",
      maskSizeMapUnitScale = scale,
      maskSizeUnits = "MM",
      maskType = "0",
      maskedSymbolLayers = ""
    )
  )
  write_label_background(w, label$background)
  xw_empty(
    w,
    "shadow",
    c(
      shadowBlendMode = "6",
      shadowColor = "0,0,0,255,rgb:0,0,0,1",
      shadowDraw = "0",
      shadowOffsetAngle = "135",
      shadowOffsetDist = "1",
      shadowOffsetGlobal = "1",
      shadowOffsetMapUnitScale = scale,
      shadowOffsetUnit = "MM",
      shadowOpacity = "0.69999999999999996",
      shadowRadius = "1.5",
      shadowRadiusAlphaOnly = "0",
      shadowRadiusMapUnitScale = scale,
      shadowRadiusUnit = "MM",
      shadowScale = "100",
      shadowUnder = "0"
    )
  )
  write_data_defined_properties(w, "dd_properties")
  xw_empty(w, "substitutions")
  xw_end(w) # text-style
}

# The label background: a plain rectangle (shapeType 0) in `background`,
# or disabled (shapeDraw 0) when `background` is NULL. QGIS draws the
# rectangle with the embedded fillSymbol, so its color must mirror
# shapeFillColor; the markerSymbol is only used for marker-shaped
# backgrounds but QGIS always writes both.
write_label_background <- function(w, background) {
  scale <- "3x:0,0,0,0,0,0"
  fill <- background %||% c(255L, 255L, 255L)
  xw_start(w, "background")
  xw_attr(w, "shapeBlendMode", "0")
  xw_attr(
    w,
    "shapeBorderColor",
    "128,128,128,255,rgb:0.5019608,0.5019608,0.5019608,1"
  )
  xw_attr(w, "shapeBorderWidth", "0")
  xw_attr(w, "shapeBorderWidthMapUnitScale", scale)
  xw_attr(w, "shapeBorderWidthUnit", "Point")
  xw_attr(w, "shapeDraw", if (is.null(background)) "0" else "1")
  xw_attr(w, "shapeFillColor", qgis_color(fill))
  xw_attr(w, "shapeJoinStyle", "64")
  xw_attr(w, "shapeOffsetMapUnitScale", scale)
  xw_attr(w, "shapeOffsetUnit", "Point")
  xw_attr(w, "shapeOffsetX", "0")
  xw_attr(w, "shapeOffsetY", "0")
  xw_attr(w, "shapeOpacity", "1")
  xw_attr(w, "shapeRadiiMapUnitScale", scale)
  xw_attr(w, "shapeRadiiUnit", "Point")
  xw_attr(w, "shapeRadiiX", "0")
  xw_attr(w, "shapeRadiiY", "0")
  xw_attr(w, "shapeRotation", "0")
  xw_attr(w, "shapeRotationType", "0")
  xw_attr(w, "shapeSVGFile", "")
  xw_attr(w, "shapeSizeMapUnitScale", scale)
  xw_attr(w, "shapeSizeType", "0")
  xw_attr(w, "shapeSizeUnit", "Point")
  xw_attr(w, "shapeSizeX", "0")
  xw_attr(w, "shapeSizeY", "0")
  xw_attr(w, "shapeType", "0")
  write_label_background_symbol(
    w,
    "markerSymbol",
    "marker",
    "SimpleMarker",
    list(
      angle = "0",
      cap_style = "square",
      color = qgis_color(c(225L, 89L, 137L)),
      horizontal_anchor_point = "1",
      joinstyle = "bevel",
      name = "circle",
      offset = "0,0",
      offset_map_unit_scale = scale,
      offset_unit = "MM",
      outline_color = qgis_color(QGS_DEFAULT_OUTLINE_COLOR),
      outline_style = "solid",
      outline_width = "0",
      outline_width_map_unit_scale = scale,
      outline_width_unit = "MM",
      scale_method = "diameter",
      size = "2",
      size_map_unit_scale = scale,
      size_unit = "MM",
      vertical_anchor_point = "1"
    )
  )
  write_label_background_symbol(
    w,
    "fillSymbol",
    "fill",
    "SimpleFill",
    list(
      border_width_map_unit_scale = scale,
      color = qgis_color(fill),
      joinstyle = "bevel",
      offset = "0,0",
      offset_map_unit_scale = scale,
      offset_unit = "MM",
      outline_color = qgis_color(c(128L, 128L, 128L)),
      outline_style = "no",
      outline_width = "0",
      outline_width_unit = "Point",
      style = "solid"
    )
  )
  xw_end(w) # background
}

# A background <symbol>: like write_symbol() but named, with an empty
# layer id (as QGIS writes them) and the caller's verbatim options.
write_label_background_symbol <- function(w, name, type, class, options) {
  xw_start(w, "symbol")
  xw_attr(w, "alpha", "1")
  xw_attr(w, "clip_to_extent", "1")
  xw_attr(w, "force_rhr", "0")
  xw_attr(w, "frame_rate", "10")
  xw_attr(w, "is_animated", "0")
  xw_attr(w, "name", name)
  xw_attr(w, "type", type)
  write_data_defined_properties(w, "data_defined_properties")
  xw_start(w, "layer")
  xw_attr(w, "class", class)
  xw_attr(w, "enabled", "1")
  xw_attr(w, "id", "")
  xw_attr(w, "locked", "0")
  xw_attr(w, "pass", "0")
  xw_start(w, "Option")
  xw_attr(w, "type", "Map")
  for (key in names(options)) {
    xw_empty(
      w,
      "Option",
      c(name = key, type = "QString", value = options[[key]])
    )
  }
  xw_end(w) # Option
  write_data_defined_properties(w, "data_defined_properties")
  xw_end(w) # layer
  xw_end(w) # symbol
}

# Label placement is left to QGIS; only the geometry-dependent parts vary:
# point labels are drawn over the point (placement 1, matching ggplot2's
# centered text), line labels along the line (2) and polygon labels
# around the centroid (0, what QGIS writes when labels are enabled on a
# polygon layer).
write_label_placement <- function(w, geom) {
  scale <- "3x:0,0,0,0,0,0"
  xw_empty(
    w,
    "placement",
    c(
      allowDegraded = "0",
      centroidInside = "0",
      centroidWhole = "0",
      dist = "0",
      distMapUnitScale = scale,
      distUnits = "MM",
      fitInPolygonOnly = "0",
      geometryGenerator = "",
      geometryGeneratorEnabled = "0",
      geometryGeneratorType = "PointGeometry",
      labelOffsetMapUnitScale = scale,
      layerType = switch(geom,
        Point = "PointGeometry",
        LineString = "LineGeometry",
        Polygon = "PolygonGeometry",
        stop("unknown geometry type: ", geom)
      ),
      lineAnchorClipping = "0",
      lineAnchorPercent = "0.5",
      lineAnchorTextPoint = "FollowPlacement",
      lineAnchorType = "0",
      maxCurvedCharAngleIn = "25",
      maxCurvedCharAngleOut = "-25",
      maximumDistance = "0",
      maximumDistanceMapUnitScale = scale,
      maximumDistanceUnit = "MM",
      multipartBehavior = "LabelLargestPartOnly",
      offsetType = "0",
      offsetUnits = "MM",
      overlapHandling = "PreventOverlap",
      overrunDistance = "0",
      overrunDistanceMapUnitScale = scale,
      overrunDistanceUnit = "MM",
      placement = switch(geom, Point = "1", LineString = "2", Polygon = "0"),
      placementFlags = "10",
      polygonPlacementFlags = "2",
      predefinedPositionOrder = "TR,TL,BR,BL,R,L,TSR,BSR",
      preserveRotation = "1",
      prioritization = "PreferCloser",
      priority = "5",
      quadOffset = "4",
      repeatDistance = "0",
      repeatDistanceMapUnitScale = scale,
      repeatDistanceUnits = "MM",
      rotationAngle = "0",
      rotationUnit = "AngleDegrees",
      xOffset = "0",
      yOffset = "0"
    )
  )
}

# The default (disabled) simple callout, as QGIS writes it.
write_label_callout <- function(w) {
  scale <- "3x:0,0,0,0,0,0"
  xw_start(w, "callout")
  xw_attr(w, "type", "simple")
  xw_start(w, "Option")
  xw_attr(w, "type", "Map")
  xw_empty(
    w,
    "Option",
    c(name = "anchorPoint", type = "QString", value = "pole_of_inaccessibility")
  )
  xw_empty(w, "Option", c(name = "blendMode", type = "int", value = "0"))
  xw_start(w, "Option")
  xw_attr(w, "name", "ddProperties")
  xw_attr(w, "type", "Map")
  xw_empty(w, "Option", c(name = "name", type = "QString", value = ""))
  xw_empty(w, "Option", c(name = "properties"))
  xw_empty(w, "Option", c(name = "type", type = "QString", value = "collection"))
  xw_end(w) # Option (ddProperties)
  xw_empty(w, "Option", c(name = "drawToAllParts", type = "bool", value = "false"))
  xw_empty(w, "Option", c(name = "enabled", type = "QString", value = "0"))
  xw_empty(
    w,
    "Option",
    c(name = "labelAnchorPoint", type = "QString", value = "point_on_exterior")
  )
  xw_empty(
    w,
    "Option",
    c(name = "lineSymbol", type = "QString", value = callout_line_symbol())
  )
  xw_empty(w, "Option", c(name = "minLength", type = "double", value = "0"))
  xw_empty(
    w,
    "Option",
    c(name = "minLengthMapUnitScale", type = "QString", value = scale)
  )
  xw_empty(w, "Option", c(name = "minLengthUnit", type = "QString", value = "MM"))
  xw_empty(w, "Option", c(name = "offsetFromAnchor", type = "double", value = "0"))
  xw_empty(
    w,
    "Option",
    c(name = "offsetFromAnchorMapUnitScale", type = "QString", value = scale)
  )
  xw_empty(
    w,
    "Option",
    c(name = "offsetFromAnchorUnit", type = "QString", value = "MM")
  )
  xw_empty(w, "Option", c(name = "offsetFromLabel", type = "double", value = "0"))
  xw_empty(
    w,
    "Option",
    c(name = "offsetFromLabelMapUnitScale", type = "QString", value = scale)
  )
  xw_empty(
    w,
    "Option",
    c(name = "offsetFromLabelUnit", type = "QString", value = "MM")
  )
  xw_end(w) # Option
  xw_end(w) # callout
}

# The callout's default line symbol, serialized the way QGIS embeds it: a
# single-line XML document stored in an attribute value (the XML writer
# escapes it there). The options are the standard SimpleLine set with
# QGIS's default callout color and width.
callout_line_symbol <- function() {
  collection <- paste0(
    '<Option type="Map">',
    '<Option name="name" type="QString" value=""/>',
    '<Option name="properties"/>',
    '<Option name="type" type="QString" value="collection"/>',
    "</Option>"
  )
  style <- style_set_outline(style_single(NULL), NULL, 0.3)
  options <- symbol_options("LineString", style, c(60L, 60L, 60L), NULL)
  paste0(
    '<symbol alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10"',
    ' is_animated="0" name="symbol" type="line">',
    "<data_defined_properties>", collection, "</data_defined_properties>",
    '<layer class="SimpleLine" enabled="1" id="{', qgs_uuid(), '}"',
    ' locked="0" pass="0">',
    '<Option type="Map">',
    paste0(
      '<Option name="', names(options),
      '" type="QString" value="', unlist(options), '"/>',
      collapse = ""
    ),
    "</Option>",
    "<data_defined_properties>", collection, "</data_defined_properties>",
    "</layer></symbol>"
  )
}
