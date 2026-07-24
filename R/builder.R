# Builds .qgs project files: the static boilerplate of a project (taken
# from an empty project saved by QGIS) is used as a template, and only the
# layer-related parts are generated and spliced in at literal string
# anchors. Ported from src/lib.rs of the generate-qgs crate; see
# docs/qgs-format.md there for the findings about the file format.

# The static project scaffold. Kept in sync with src/template.qgs of the
# generate-qgs crate (byte-identical copies).
read_template <- function() {
  path <- system.file("template.qgs", package = "ggplot2qgis", mustWork = TRUE)
  out <- readChar(path, file.size(path), useBytes = TRUE)
  Encoding(out) <- "UTF-8"
  out
}

# Replaces the first occurrence of the literal `anchor` with `replacement`
# (also literal — sub() would mangle `\\` and `&`). Errors if the anchor
# is missing, so a template edit fails loudly instead of silently emitting
# a broken project.
splice <- function(out, anchor, replacement) {
  pos <- regexpr(anchor, out, fixed = TRUE)
  if (pos < 0L) {
    stop("template anchor not found: ", anchor)
  }
  paste0(
    substr(out, 1L, pos - 1L),
    replacement,
    substr(out, pos + attr(pos, "match.length"), nchar(out))
  )
}

# Renders the project file content. `layers` is a list of layers from
# vector_layer()/xyz_tile_layer(), bottom-most first. `project_srs` is the
# CRS of the map canvas (a resolve_srs() result); if NULL, the template's
# default (EPSG:3857) is kept. Layers whose SRS differs are reprojected on
# the fly by QGIS. `extent` is the initial map-canvas view as
# c(xmin, ymin, xmax, ymax) in the project CRS; if NULL, the template's
# whole-world default is kept.
qgs_build <- function(layers, project_srs = NULL, extent = NULL) {
  out <- splice(read_template(), "{{SAVE_DATETIME}}", qgs_now_iso8601())

  if (!is.null(extent)) {
    out <- write_canvas_extent(out, extent)
  }

  if (!is.null(project_srs)) {
    # The template carries the canvas CRS in two places (<projectCrs> and
    # <mapcanvas>'s <destinationsrs>, which the canvas <extent> is
    # interpreted in) plus the canvas <units>; all three must agree.
    out <- replace_crs_block(out, "projectCrs", 1L, project_srs)
    out <- replace_crs_block(out, "destinationsrs", 2L, project_srs)
    out <- splice(
      out,
      "<units>meters</units>",
      paste0(
        "<units>",
        if (project_srs$geographic) "degrees" else "meters",
        "</units>"
      )
    )
  }

  if (length(layers) == 0L) {
    return(out)
  }

  # Layer tree and legend: top-most layer first, i.e. the reverse of the
  # insertion order.
  tree <- xml_writer(2L)
  legend <- xml_writer(2L)
  for (layer in rev(layers)) {
    write_layer_tree_layer(tree, layer)
    write_legend_layer(legend, layer)
  }
  # Drawing order (<custom-order>, <layerorder>): bottom-most first.
  order <- xml_writer(3L)
  layerorder <- xml_writer(2L)
  projectlayers <- xml_writer(2L)
  for (layer in layers) {
    xw_elem(order, "item", layer$id)
    xw_start(layerorder, "layer")
    xw_attr(layerorder, "id", layer$id)
    xw_end(layerorder)
    write_maplayer(projectlayers, layer)
  }

  # The replacement anchors include their leading indentation so the
  # spliced fragments stay flush with the template.
  out <- splice(
    out,
    "\n    <custom-order enabled=\"0\"/>",
    paste0(
      xw_finish(tree),
      "\n    <custom-order enabled=\"0\">",
      xw_finish(order),
      "\n    </custom-order>"
    )
  )
  out <- splice(
    out,
    "\n  <legend updateDrawingOrder=\"true\"/>",
    paste0(
      "\n  <legend updateDrawingOrder=\"true\">",
      xw_finish(legend),
      "\n  </legend>"
    )
  )
  out <- splice(
    out,
    "\n  <projectlayers/>",
    paste0(
      "\n  <projectlayers>",
      xw_finish(projectlayers),
      "\n  </projectlayers>"
    )
  )
  out <- splice(
    out,
    "\n  <layerorder/>",
    paste0("\n  <layerorder>", xw_finish(layerorder), "\n  </layerorder>")
  )
  out
}

# Replaces a template CRS block (`<tag>` at the given indent level, whose
# content is a <spatialrefsys>) with `srs`. The tag must occur exactly
# once in the template; a missing anchor fails loudly.
replace_crs_block <- function(out, tag, indent, srs) {
  w <- xml_writer(indent)
  xw_start(w, tag)
  write_spatialrefsys(w, srs)
  xw_end(w)

  start_anchor <- paste0("\n", strrep("  ", indent), "<", tag, ">")
  start <- regexpr(start_anchor, out, fixed = TRUE)
  if (start < 0L) {
    stop("template anchor not found: <", tag, ">")
  }
  end_tag <- paste0("</", tag, ">")
  end <- regexpr(end_tag, out, fixed = TRUE)
  if (end < start) {
    stop("template anchor not found: ", end_tag)
  }
  paste0(
    substr(out, 1L, start - 1L),
    xw_finish(w),
    substr(out, end + nchar(end_tag), nchar(out))
  )
}

# Sets the initial map-canvas view. The template ships zoomed to the whole
# world in both places QGIS reads on open: <mapcanvas>'s <extent> and
# <ProjectViewSettings>'s <DefaultViewExtent> (attributes). `extent` is
# c(xmin, ymin, xmax, ymax) in the project CRS. The world-extent literals
# are used as splice anchors, so a template change that moves them fails
# loudly rather than leaving the view unzoomed.
write_canvas_extent <- function(out, extent) {
  canvas_anchor <- paste0(
    "    <extent>\n",
    "      <xmin>-20037508.34278924390673637</xmin>\n",
    "      <ymin>-20037508.34278924763202667</ymin>\n",
    "      <xmax>20037508.34278924390673637</xmax>\n",
    "      <ymax>20037508.34278924763202667</ymax>\n",
    "    </extent>"
  )
  canvas_block <- paste0(
    "    <extent>\n",
    "      <xmin>", num(extent[1L]), "</xmin>\n",
    "      <ymin>", num(extent[2L]), "</ymin>\n",
    "      <xmax>", num(extent[3L]), "</xmax>\n",
    "      <ymax>", num(extent[4L]), "</ymax>\n",
    "    </extent>"
  )
  out <- splice(out, canvas_anchor, canvas_block)

  view_anchor <- paste0(
    "<DefaultViewExtent xmax=\"20037508.34278924390673637\"",
    " xmin=\"-20037508.34278924390673637\"",
    " ymax=\"20037508.34278924763202667\"",
    " ymin=\"-20037508.34278924763202667\">"
  )
  view_block <- paste0(
    "<DefaultViewExtent xmax=\"", num(extent[3L]), "\"",
    " xmin=\"", num(extent[1L]), "\"",
    " ymax=\"", num(extent[4L]), "\"",
    " ymin=\"", num(extent[2L]), "\">"
  )
  splice(out, view_anchor, view_block)
}

# Writes the project to a .qgs file. Binary mode keeps LF newlines on
# Windows and avoids re-encoding.
qgs_write <- function(layers, path, project_srs = NULL, extent = NULL) {
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeChar(
    enc2utf8(qgs_build(layers, project_srs, extent)),
    con,
    eos = NULL,
    useBytes = TRUE
  )
  invisible(path)
}
