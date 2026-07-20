# Number of equal-interval classes of a graduated renderer. QGIS classifies
# the attribute into this many ranges; the colors are interpolated from the
# gradient stops. High enough to approximate ggplot2's continuous gradient,
# at the cost of a long legend. (style_continuous() would reproduce the
# gradient exactly, but QGIS shows no color ramp in the legend for it.)
QGS_GRADUATED_CLASSES <- 25L

# Number of gradient stops sampled from a continuous scale. QGIS
# interpolates between stops in RGB space while ggplot2 interpolates in Lab
# space, so sample densely enough that the difference is invisible.
QGS_GRADIENT_STOPS <- 21L

# Millimeters per ggplot2 linewidth unit: 1 linewidth is .pt (72.27 / 25.4)
# lwd units of 1/96 inch each, i.e. 72.27 / 96 mm.
QGS_MM_PER_LINEWIDTH <- 72.27 / 96

#' Write a ggplot2 map plot as a QGIS project
#'
#' Converts a ggplot2 plot whose layers are drawn from sf objects into a
#' QGIS project (`.qgs`) file. The data of each layer is saved as a
#' GeoPackage under `<path minus extension>_data/`, and the layer is styled
#' after the plot's trained color scale:
#'
#' - a continuous `fill`/`colour` scale becomes a graduated renderer with
#'   fine-grained equal-interval classes (or a continuously interpolated
#'   color, see `gradient_style`),
#' - a discrete one becomes a categorized renderer,
#' - a layer with no `fill`/`colour` mapping becomes a single symbol with
#'   the color ggplot2 would have used.
#'
#' Following ggplot2's semantics for polygons, `fill` is the interior and
#' `colour` is the border: a `colour` scale on a polygon layer colors the
#' outlines while the interior keeps the constant fill. Constant outline
#' colors and widths are taken from the plot as well. Mapping both `fill`
#' and `colour` on the same layer is not supported.
#'
#' Only a bare column name is supported for the `fill`/`colour` aesthetics;
#' a constant or a computed expression (e.g. `aes(fill = AREA * 2)`) is an
#' error.
#'
#' @param plot A ggplot object. All layers must be backed by sf data.
#' @param path Path of the `.qgs` file to write. Tilde paths (e.g. `~/x.qgs`)
#'   are expanded.
#' @param use_plot_crs If `TRUE`, the project (map canvas) CRS is the plot's
#'   CRS, resolved the way [ggplot2::coord_sf()] does: its `crs` argument if
#'   specified, otherwise the CRS of the first layer that defines one. If
#'   `FALSE` (the default), the project CRS is EPSG:3857 (Web Mercator).
#'   Either way the layers keep the CRS of their data; QGIS reprojects them
#'   on the fly.
#' @param gradient_style How a continuous `fill`/`colour` scale is rendered:
#'
#'   - `"graduated"` (the default): a graduated renderer with 25
#'     equal-interval classes. The gradient is slightly banded, but the
#'     legend shows the classes with their value ranges.
#'   - `"continuous"`: the exact ggplot2 look. The color is interpolated
#'     per feature by a data-defined expression on the symbol color
#'     (`ramp_color(create_ramp(...), ...)`). Caveats: QGIS cannot display
#'     a color ramp in the legend for a data-defined color, so the legend
#'     is a single swatch without any value labels, and the gradient is
#'     only discoverable in the layer styling panel behind the
#'     data-defined override of the symbol color, not in the renderer
#'     dropdown.
#' @param overwrite If `FALSE` (the default), writing to a `path` that already
#'   exists is an error. Set to `TRUE` to overwrite it.
#' @returns `path`, invisibly.
#' @examples
#' library(ggplot2)
#'
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' p <- ggplot(nc) +
#'   geom_sf(aes(fill = AREA))
#'
#' write_qgs(p, tempfile(fileext = ".qgs"))
#' @importFrom rlang %||%
#' @export
write_qgs <- function(plot, path, use_plot_crs = FALSE,
                      gradient_style = c("graduated", "continuous"),
                      overwrite = FALSE) {
  if (!inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot object, got ", class(plot)[1], call. = FALSE)
  }
  layers <- plot@layers
  if (length(layers) == 0L) {
    stop("`plot` must have at least one layer", call. = FALSE)
  }
  if (!isTRUE(use_plot_crs) && !isFALSE(use_plot_crs)) {
    stop("`use_plot_crs` must be TRUE or FALSE", call. = FALSE)
  }
  if (!isTRUE(overwrite) && !isFALSE(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE", call. = FALSE)
  }
  gradient_style <- match.arg(gradient_style)

  path <- path.expand(path)

  if (!overwrite && file.exists(path)) {
    stop(
      "`path` already exists: ", path,
      "\nSet `overwrite = TRUE` to overwrite it.",
      call. = FALSE
    )
  }

  # Build the plot first so that the scales are trained by the data.
  built <- ggplot2::ggplot_build(plot)

  data_dir_name <- paste0(tools::file_path_sans_ext(basename(path)), "_data")
  data_dir <- file.path(dirname(path), data_dir_name)
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  qgs_layers <- vector("list", length(layers))

  # coord_sf() uses its crs argument if specified, otherwise the CRS of
  # the first layer that defines one; the built plot carries the result
  # (as given, so e.g. a bare EPSG code needs normalization).
  plot_crs <- NULL
  if (use_plot_crs) {
    plot_crs <- built@layout$panel_params[[1]]$crs
    if (!is.null(plot_crs)) {
      plot_crs <- sf::st_crs(plot_crs)
    }
  }

  # ggplot2's first layer is the bottom-most one, which is also the order
  # qgs_build() expects.
  for (i in seq_along(layers)) {
    layer <- layers[[i]]

    # The raw data, not the computed data of ggplot_build(), which no
    # longer has the original values.
    d <- layer$data
    if (is.null(d) || inherits(d, "waiver")) {
      d <- plot@data
    }
    if (is.null(d) || inherits(d, "waiver")) {
      stop("layer ", i, " has no data", call. = FALSE)
    }
    if (!inherits(d, "sf")) {
      stop(
        "layer ", i, ": only sf data is supported at the moment, got ",
        class(d)[1],
        call. = FALSE
      )
    }
    if (nrow(d) == 0L) {
      stop("layer ", i, ": the data has no rows", call. = FALSE)
    }

    crs <- sf::st_crs(d)
    if (is.na(crs)) {
      stop("layer ", i, ": the data has no CRS", call. = FALSE)
    }
    if (use_plot_crs && (is.null(plot_crs) || is.na(plot_crs))) {
      plot_crs <- crs
    }

    layer_name <- paste0("layer", i)
    gpkg_file <- paste0(layer_name, ".gpkg")
    gpkg_path <- file.path(data_dir, gpkg_file)
    if (file.exists(gpkg_path)) {
      unlink(gpkg_path)
    }
    sf::st_write(d, gpkg_path, layer = layer_name, quiet = TRUE)

    geometry <- qgs_geometry_type(d, i)
    qgs_layers[[i]] <- vector_layer(
      # relative to the project file
      paste0(data_dir_name, "/", gpkg_file),
      layer_name,
      crs,
      geometry,
      qgs_vector_style(plot, built, layer, i, d, gradient_style, geometry)
    )
  }

  project_srs <- if (use_plot_crs) resolve_srs(plot_crs)
  qgs_write(qgs_layers, path, project_srs)

  invisible(path)
}

qgs_geometry_type <- function(d, i) {
  type <- as.character(sf::st_geometry_type(d, by_geometry = FALSE))
  switch(type,
    POINT = ,
    MULTIPOINT = "Point",
    LINESTRING = ,
    MULTILINESTRING = "LineString",
    POLYGON = ,
    MULTIPOLYGON = "Polygon",
    stop("layer ", i, ": unsupported geometry type ", type, call. = FALSE)
  )
}

# Resolves which aesthetic drives the color of the layer and returns the
# matching style. The layer's mapping takes precedence over the
# plot's, following how ggplot2 itself resolves aesthetics.
qgs_vector_style <- function(plot, built, layer, i, d, gradient_style, geometry) {
  # aes() normalizes `color` to `colour`, so only these two keys exist.
  fill <- layer$mapping[["fill"]] %||% plot@mapping[["fill"]]
  colour <- layer$mapping[["colour"]] %||% plot@mapping[["colour"]]
  if (!is.null(fill) && !is.null(colour)) {
    stop(
      "layer ", i,
      ": mapping both `fill` and `colour` on the same layer is not supported",
      call. = FALSE
    )
  }

  const <- qgs_layer_constants(built@data[[i]])
  # Rounded so binary float noise (0.15056250000000002) stays out of the
  # project file.
  outline_width <- round(const$linewidth * QGS_MM_PER_LINEWIDTH, 7)
  is_polygon <- geometry == "Polygon"

  if (is.null(fill) && is.null(colour)) {
    return(qgs_single_style(const, is_polygon, outline_width))
  }

  aes_name <- if (is.null(fill)) "colour" else "fill"
  quo <- fill %||% colour

  if (!(rlang::is_quosure(quo) && rlang::quo_is_symbol(quo))) {
    stop(
      "layer ", i, ": only a bare column name is supported for `", aes_name,
      "`, got `", rlang::as_label(quo), "`",
      call. = FALSE
    )
  }
  attribute <- rlang::as_string(rlang::quo_get_expr(quo))
  if (!attribute %in% names(d)) {
    stop(
      "layer ", i, ": column `", attribute, "` not found in the layer data",
      call. = FALSE
    )
  }

  scale <- built@plot@scales$get_scales(aes_name)
  style <- if (scale$is_discrete()) {
    qgs_categorized_style(scale, attribute, i)
  } else if (gradient_style == "continuous") {
    qgs_continuous_style(scale, attribute, i)
  } else {
    qgs_graduated_style(scale, attribute, i)
  }

  if (aes_name == "fill") {
    # A constant border around the varying fill.
    style <- style_set_outline(style, qgs_rgb(const$colour), outline_width)
  } else if (is_polygon) {
    # ggplot2 draws a colour aesthetic on polygons as the border color;
    # the interior keeps the constant fill. The outline color is ignored
    # for a stroke target, only its width applies.
    style <- style_set_stroke_target(style, qgs_rgb(const$fill))
    style <- style_set_outline(style, qgs_rgb(const$fill), outline_width)
  } else if (geometry == "LineString") {
    # The line color is the varying one; only the width is constant.
    style <- style_set_outline(style, qgs_rgb(const$fill), outline_width)
  }
  # Points with a varying colour keep the QGIS marker defaults for the
  # ring around the marker.

  style
}

# The constant aesthetics ggplot2 computed for a layer, taken from its
# first feature (only meaningful for aesthetics that are not mapped).
# The fallbacks are geom_sf()'s defaults.
qgs_layer_constants <- function(computed) {
  first_or <- function(name, default) {
    v <- computed[[name]]
    if (length(v) == 0L || is.na(v[[1L]])) default else v[[1L]]
  }
  list(
    colour = first_or("colour", "grey35"),
    fill = first_or("fill", "grey90"),
    linewidth = first_or("linewidth", 0.2)
  )
}

# For a layer without a fill/colour mapping, reproduce ggplot2's constant
# colors: interior + border for polygons; for lines and points the single
# color is the stroke/marker color, with a matching ring (ggplot2 points
# have no distinct border).
qgs_single_style <- function(const, is_polygon, outline_width) {
  main <- if (is_polygon) const$fill else const$colour
  style_set_outline(
    style_single(qgs_rgb(main)),
    qgs_rgb(const$colour),
    outline_width
  )
}

# The gradient of a trained continuous scale, sampled at evenly spaced
# points so QGIS reproduces ggplot2's gradient regardless of the scale's
# palette.
qgs_gradient_ramp <- function(scale, attribute, i) {
  limits <- scale$get_limits()
  if (anyNA(limits) || limits[2L] <= limits[1L]) {
    stop(
      "layer ", i, ": cannot map `", attribute,
      "` to a color gradient (the scale's limits are degenerate)",
      call. = FALSE
    )
  }

  offsets <- seq(0, 1, length.out = QGS_GRADIENT_STOPS)
  values <- limits[1L] + offsets * (limits[2L] - limits[1L])
  list(
    limits = limits,
    offsets = offsets,
    colors = grDevices::col2rgb(scale$map(values))
  )
}

qgs_graduated_style <- function(scale, attribute, i) {
  ramp <- qgs_gradient_ramp(scale, attribute, i)

  style_graduated(
    attribute,
    classes = QGS_GRADUATED_CLASSES,
    min = ramp$limits[1L],
    max = ramp$limits[2L],
    stops = list(offsets = ramp$offsets, colors = ramp$colors)
  )
}

qgs_continuous_style <- function(scale, attribute, i) {
  ramp <- qgs_gradient_ramp(scale, attribute, i)

  style_continuous(
    attribute,
    min = ramp$limits[1L],
    max = ramp$limits[2L],
    stops = list(offsets = ramp$offsets, colors = ramp$colors)
  )
}

qgs_categorized_style <- function(scale, attribute, i) {
  values <- scale$get_breaks()
  values <- values[!is.na(values)]
  if (length(values) == 0L) {
    stop(
      "layer ", i, ": the scale of `", attribute, "` has no values",
      call. = FALSE
    )
  }
  colors <- grDevices::col2rgb(scale$map(values))

  style_categorized(attribute, as.character(values), colors)
}

qgs_rgb <- function(color) {
  grDevices::col2rgb(color)[, 1L]
}
