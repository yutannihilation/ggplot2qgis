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
# the fly by QGIS.
qgs_build <- function(layers, project_srs = NULL) {
  out <- splice(read_template(), "{{SAVE_DATETIME}}", qgs_now_iso8601())

  if (!is.null(project_srs)) {
    w <- xml_writer(1L)
    xw_start(w, "projectCrs")
    write_spatialrefsys(w, project_srs)
    xw_end(w)

    start <- regexpr("\n  <projectCrs>", out, fixed = TRUE)
    if (start < 0L) {
      stop("template anchor not found: <projectCrs>")
    }
    end_tag <- "</projectCrs>"
    end <- regexpr(end_tag, out, fixed = TRUE)
    if (end < start) {
      stop("template anchor not found: </projectCrs>")
    }
    out <- paste0(
      substr(out, 1L, start - 1L),
      xw_finish(w),
      substr(out, end + nchar(end_tag), nchar(out))
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

# Writes the project to a .qgs file. Binary mode keeps LF newlines on
# Windows and avoids re-encoding.
qgs_write <- function(layers, path, project_srs = NULL) {
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeChar(
    enc2utf8(qgs_build(layers, project_srs)),
    con,
    eos = NULL,
    useBytes = TRUE
  )
  invisible(path)
}
