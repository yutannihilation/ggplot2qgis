# Layer definitions and their XML representation.
#
# Each layer is referenced from four places in the project file (see
# docs/qgs-format.md in the generate-qgs crate); this module renders all
# of them. Ported from src/layers.rs (vector and XYZ tile layers; GDAL
# raster layers were not ported because write_qgs() has no raster path
# yet — the Rust crate remains the reference if one is added).
#
# A layer is a plain named list with a `kind` field ("vector" or "xyz").

# A GeoPackage vector layer.
#
# * path — path to the .gpkg file, embedded verbatim into the project. A
#   relative path is resolved by QGIS against the location of the saved
#   .qgs file.
# * name — table/layer name inside the .gpkg (<path>|layername=<name>);
#   also used as the display name.
# * srs — SRS of the data (cannot be derived without reading the file, so
#   it must be stated): anything resolve_srs() accepts.
# * geometry — "Point", "LineString" or "Polygon".
# * style — how features are rendered, see style_*() in style.R.
# * checked — whether the layer is visible (checked in the layer tree);
#   FALSE writes the layer hidden (e.g. an alternative basemap).
# * table — the table name inside the .gpkg when it differs from the
#   display name (several QGIS layers sharing one GeoPackage).
vector_layer <- function(path, name, srs, geometry, style, checked = TRUE,
                         table = name) {
  geometry <- match.arg(geometry, c("Point", "LineString", "Polygon"))
  list(
    kind = "vector",
    id = qgs_layer_id(name),
    name = name,
    path = path,
    srs = resolve_srs(srs),
    geometry = geometry,
    style = style,
    checked = isTRUE(checked),
    table = table
  )
}

# An XYZ tile layer (e.g. OpenStreetMap-like tiles). The `url` should
# contain {z}/{x}/{y} placeholders. XYZ tiles are always in EPSG:3857.
xyz_tile_layer <- function(name, url, zmin, zmax, checked = TRUE) {
  list(
    kind = "xyz",
    id = qgs_layer_id(name),
    name = name,
    url = url,
    zmin = as.integer(zmin),
    zmax = as.integer(zmax),
    srs = resolve_srs(3857),
    checked = isTRUE(checked)
  )
}

# Percent-encodes everything except unreserved characters (RFC 3986),
# matching how QGIS writes the `url` parameter of an XYZ datasource
# ({z} -> %7Bz%7D, ...). Operates on UTF-8 bytes, comparing byte values
# directly so multibyte characters never form invalid one-byte strings.
percent_encode <- function(s) {
  bytes <- as.integer(charToRaw(enc2utf8(s)))
  unreserved <-
    (bytes >= 0x41 & bytes <= 0x5A) | # A-Z
      (bytes >= 0x61 & bytes <= 0x7A) | # a-z
      (bytes >= 0x30 & bytes <= 0x39) | # 0-9
      bytes %in% c(0x2D, 0x5F, 0x2E, 0x7E) # - _ . ~
  out <- sprintf("%%%02X", bytes)
  out[unreserved] <- rawToChar(as.raw(bytes[unreserved]), multiple = TRUE)
  paste0(out, collapse = "")
}

layer_provider_key <- function(layer) {
  switch(layer$kind,
    xyz = "wms",
    vector = "ogr",
    stop("unknown layer kind: ", layer$kind)
  )
}

layer_datasource <- function(layer) {
  switch(layer$kind,
    # The WMS-provider datasource URI for XYZ tiles.
    xyz = paste0(
      if (!is.na(layer$srs$epsg)) sprintf("crs=EPSG%%3A%d&", layer$srs$epsg),
      "format&type=xyz&url=", percent_encode(layer$url),
      "&zmax=", layer$zmax, "&zmin=", layer$zmin, "&http-header:referer="
    ),
    # The ogr-provider datasource: <path>|layername=<table>.
    vector = paste0(layer$path, "|layername=", layer$table),
    stop("unknown layer kind: ", layer$kind)
  )
}

# <layer-tree-group> entry.
write_layer_tree_layer <- function(w, layer) {
  xw_start(w, "layer-tree-layer")
  xw_attr(w, "checked", if (layer$checked) "Qt::Checked" else "Qt::Unchecked")
  xw_attr(w, "expanded", "1")
  xw_attr(w, "id", layer$id)
  xw_attr(w, "legend_exp", "")
  xw_attr(w, "legend_split_behavior", "0")
  xw_attr(w, "name", layer$name)
  xw_attr(w, "patch_size", "-1,-1")
  xw_attr(w, "providerKey", layer_provider_key(layer))
  xw_attr(w, "source", layer_datasource(layer))
  xw_start(w, "customproperties")
  xw_empty(w, "Option")
  xw_end(w) # customproperties
  xw_end(w) # layer-tree-layer
}

# <legend> entry.
write_legend_layer <- function(w, layer) {
  xw_start(w, "legendlayer")
  xw_attr(w, "checked", if (layer$checked) "Qt::Checked" else "Qt::Unchecked")
  xw_attr(w, "drawingOrder", "-1")
  xw_attr(w, "name", layer$name)
  xw_attr(w, "open", "true")
  xw_attr(w, "showFeatureCount", "0")
  xw_start(w, "filegroup")
  xw_attr(w, "hidden", "false")
  xw_attr(w, "open", "true")
  xw_empty(
    w,
    "legendlayerfile",
    c(
      isInOverview = "0",
      layerid = layer$id,
      visible = if (layer$checked) "1" else "0"
    )
  )
  xw_end(w) # filegroup
  xw_end(w) # legendlayer
}

# <projectlayers> entry.
write_maplayer <- function(w, layer) {
  switch(layer$kind,
    xyz = write_xyz_maplayer(w, layer),
    vector = write_vector_maplayer(w, layer),
    stop("unknown layer kind: ", layer$kind)
  )
}

# The <flags> block shared by all layers.
write_flags <- function(w) {
  xw_start(w, "flags")
  xw_elem(w, "Identifiable", "1")
  xw_elem(w, "Removable", "1")
  xw_elem(w, "Searchable", "1")
  xw_elem(w, "Private", "0")
  xw_end(w)
}

# <extent> with the four corners as child elements.
write_extent <- function(w, tag, corners) {
  xw_start(w, tag)
  xw_elem(w, "xmin", corners[1L])
  xw_elem(w, "ymin", corners[2L])
  xw_elem(w, "xmax", corners[3L])
  xw_elem(w, "ymax", corners[4L])
  xw_end(w)
}

# The <pipe><provider><resampling .../></provider> part of a raster
# maplayer.
write_pipe_provider <- function(w) {
  xw_start(w, "provider")
  xw_empty(
    w,
    "resampling",
    c(
      enabled = "false",
      maxOversampling = "2",
      zoomedInResamplingMethod = "nearestNeighbour",
      zoomedOutResamplingMethod = "nearestNeighbour"
    )
  )
  xw_end(w) # provider
}

# The <pipe> tail after the renderer.
write_pipe_tail <- function(w) {
  xw_empty(
    w,
    "brightnesscontrast",
    c(brightness = "0", contrast = "0", gamma = "1")
  )
  xw_empty(
    w,
    "huesaturation",
    c(
      colorizeBlue = "128",
      colorizeGreen = "128",
      colorizeOn = "0",
      colorizeRed = "255",
      colorizeStrength = "100",
      grayscaleMode = "0",
      invertColors = "0",
      saturation = "0"
    )
  )
  xw_empty(w, "rasterresampler", c(maxOversampling = "2"))
  xw_elem(w, "resamplingStage", "resamplingFilter")
}

# The <minMaxOrigin> block inside <rasterrenderer>.
write_min_max_origin <- function(w, limits) {
  xw_start(w, "minMaxOrigin")
  xw_elem(w, "limits", limits)
  xw_elem(w, "extent", "WholeRaster")
  xw_elem(w, "statAccuracy", "Estimated")
  xw_elem(w, "cumulativeCutLower", "0.02")
  xw_elem(w, "cumulativeCutUpper", "0.98")
  xw_elem(w, "stdDevFactor", "2")
  xw_end(w) # minMaxOrigin
}

write_xyz_maplayer <- function(w, layer) {
  xw_start(w, "maplayer")
  xw_attr(w, "autoRefreshMode", "Disabled")
  xw_attr(w, "autoRefreshTime", "0")
  xw_attr(w, "hasScaleBasedVisibilityFlag", "0")
  xw_attr(w, "layerType", "Raster")
  xw_attr(w, "legendPlaceholderImage", "")
  xw_attr(w, "maxScale", "0")
  xw_attr(w, "minScale", "1e+08")
  xw_attr(w, "refreshOnNotifyEnabled", "0")
  xw_attr(w, "refreshOnNotifyMessage", "")
  xw_attr(w, "styleCategories", "AllStyleCategories")
  xw_attr(w, "type", "raster")
  # XYZ tiles cover the whole EPSG:3857 world.
  write_extent(
    w,
    "extent",
    c(
      "-20037508.34278924390673637",
      "-20037508.34278924763202667",
      "20037508.34278924390673637",
      "20037508.34278924763202667"
    )
  )
  write_extent(
    w,
    "wgs84extent",
    c(
      "-180",
      "-85.05112877980660357",
      "180",
      "85.05112877980660357"
    )
  )
  xw_elem(w, "id", layer$id)
  xw_elem(w, "datasource", layer_datasource(layer))
  xw_elem(w, "layername", layer$name)
  xw_start(w, "srs")
  write_spatialrefsys(w, layer$srs)
  xw_end(w) # srs
  xw_elem(w, "provider", "wms")
  xw_start(w, "noData")
  xw_empty(w, "noDataList", c(bandNo = "1", useSrcNoData = "0"))
  xw_end(w) # noData
  write_flags(w)
  xw_start(w, "customproperties")
  xw_start(w, "Option")
  xw_attr(w, "type", "Map")
  xw_empty(
    w,
    "Option",
    c(name = "identify/format", type = "QString", value = "Undefined")
  )
  xw_end(w) # Option
  xw_end(w) # customproperties
  xw_start(w, "pipe")
  write_pipe_provider(w)
  xw_start(w, "rasterrenderer")
  xw_attr(w, "alphaBand", "-1")
  xw_attr(w, "band", "1")
  xw_attr(w, "nodataColor", "")
  xw_attr(w, "opacity", "1")
  xw_attr(w, "type", "singlebandcolordata")
  xw_empty(w, "rasterTransparency")
  write_min_max_origin(w, "None")
  xw_end(w) # rasterrenderer
  write_pipe_tail(w)
  xw_end(w) # pipe
  xw_elem(w, "blendMode", "0")
  xw_empty(w, "legend")
  xw_end(w) # maplayer
}

# <extent>/<wgs84extent> and the metadata boilerplate (<resourceMetadata>,
# <temporal>, <elevation>, ...) are omitted: QGIS recomputes them from the
# data source on load.
write_vector_maplayer <- function(w, layer) {
  xw_start(w, "maplayer")
  xw_attr(w, "autoRefreshMode", "Disabled")
  xw_attr(w, "autoRefreshTime", "0")
  xw_attr(w, "geometry", layer$geometry)
  xw_attr(w, "hasScaleBasedVisibilityFlag", "0")
  xw_attr(w, "labelsEnabled", "0")
  xw_attr(w, "layerType", "Vector")
  xw_attr(w, "legendPlaceholderImage", "")
  xw_attr(w, "maxScale", "0")
  xw_attr(w, "minScale", "100000000")
  xw_attr(w, "readOnly", "0")
  xw_attr(w, "refreshOnNotifyEnabled", "0")
  xw_attr(w, "refreshOnNotifyMessage", "")
  xw_attr(w, "simplifyAlgorithm", "0")
  xw_attr(w, "simplifyDrawingHints", "1")
  xw_attr(w, "simplifyDrawingTol", "1")
  xw_attr(w, "simplifyLocal", "1")
  xw_attr(w, "simplifyMaxScale", "1")
  xw_attr(w, "styleCategories", "AllStyleCategories")
  xw_attr(w, "symbologyReferenceScale", "-1")
  xw_attr(w, "type", "vector")
  xw_attr(w, "wkbType", wkb_type_attr(layer$geometry))
  xw_elem(w, "id", layer$id)
  xw_elem(w, "datasource", layer_datasource(layer))
  xw_elem(w, "layername", layer$name)
  xw_start(w, "srs")
  write_spatialrefsys(w, layer$srs)
  xw_end(w) # srs
  xw_start(w, "provider")
  xw_attr(w, "encoding", "UTF-8")
  xw_text(w, "ogr")
  xw_end(w)
  xw_empty(w, "vectorjoins")
  xw_empty(w, "layerDependencies")
  xw_empty(w, "dataDependencies")
  xw_empty(w, "expressionfields")
  write_flags(w)
  write_renderer(w, layer$geometry, layer$style)
  xw_start(w, "selection")
  xw_attr(w, "mode", "Default")
  xw_empty(w, "selectionColor", c(invalid = "1"))
  xw_end(w) # selection
  xw_start(w, "customproperties")
  xw_empty(w, "Option")
  xw_end(w) # customproperties
  xw_elem(w, "blendMode", "0")
  xw_elem(w, "featureBlendMode", "0")
  xw_elem(w, "layerOpacity", "1")
  xw_start(w, "geometryOptions")
  xw_attr(w, "geometryPrecision", "0")
  xw_attr(w, "removeDuplicateNodes", "0")
  xw_start(w, "activeChecks")
  xw_attr(w, "type", "StringList")
  xw_empty(w, "Option", c(type = "QString", value = ""))
  xw_end(w) # activeChecks
  xw_empty(w, "checkConfiguration")
  xw_end(w) # geometryOptions
  xw_empty(
    w,
    "legend",
    c(showLabelLegend = "0", type = "default-vector")
  )
  xw_empty(w, "referencedLayers")
  xw_empty(w, "referencingLayers")
  xw_end(w) # maplayer
}
