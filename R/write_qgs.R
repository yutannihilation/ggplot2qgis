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

# Characters a layer name cannot contain: the name becomes the GeoPackage
# file name (so path separators and the characters Windows forbids in file
# names are out) and the `|layername=` part of the ogr datasource URI (so
# `|`, its delimiter, is out too).
QGS_LAYER_NAME_FORBIDDEN <- "[/\\\\|:*?\"<>[:cntrl:]]"

# Predefined XYZ basemaps `basemap` accepts by key. Each is the display
# name, the {z}/{x}/{y} URL template, and the tile set's zoom range.
QGS_BASEMAPS <- list(
  osm = list(
    name = "OpenStreetMap",
    url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    zmin = 0L, zmax = 19L
  ),
  gsi_standard = list(
    # GSI standard map, name escaped so the R source stays ASCII (R CMD
    # check warns on non-ASCII characters in code).
    name = "\u5730\u7406\u9662\u30bf\u30a4\u30eb\uff08\u6a19\u6e96\u5730\u56f3\uff09",
    url = "https://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png",
    zmin = 0L, zmax = 18L
  ),
  gsi_pale = list(
    # GSI pale map (name is the escaped literal below).
    name = "\u5730\u7406\u9662\u30bf\u30a4\u30eb\uff08\u6de1\u8272\u5730\u56f3\uff09",
    url = "https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png",
    zmin = 0L, zmax = 18L
  )
)

#' Write a ggplot2 or tmap map as a QGIS project
#'
#' Converts a ggplot2 plot whose layers are drawn from sf objects (or, for
#' [ggplot2::geom_point()], [ggplot2::geom_path()], [ggplot2::geom_line()]
#' and [ggplot2::geom_polygon()], from plain data frames) into a QGIS
#' project (`.qgs`) file. The data of each layer is saved as a GeoPackage
#' under `<path minus extension>_data/`, and the layer is styled after the
#' plot's trained color scale:
#'
#' - a continuous `fill`/`colour` scale becomes a graduated renderer with
#'   fine-grained equal-interval classes (or a continuously interpolated
#'   color, see `gradient_style`),
#' - a binned one (e.g. [ggplot2::scale_fill_steps()]) becomes a graduated
#'   renderer with one class per bin, using the scale's exact bin
#'   boundaries and colors,
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
#' # Data frame layers
#'
#' A `geom_point()`, `geom_path()`, `geom_line()` or `geom_polygon()` layer
#' drawn from a plain data frame is converted to an sf layer: one point per
#' row, or one linestring/polygon per group (ggplot2's grouping — an
#' explicit `group` aesthetic or the interaction of the discrete
#' aesthetics; `geom_line()` orders each line by `x` like ggplot2 does,
#' and polygon rings are closed). The plot must use [ggplot2::coord_sf()],
#' and the `x`/`y` values are taken to be coordinates in the panel CRS:
#' `coord_sf()`'s `crs` argument if given, otherwise the CRS of the first
#' sf layer (`coord_sf(default_crs = )` is not supported). Like
#' `fill`/`colour`, the `x`/`y` aesthetics must be bare column names, and
#' the layer must use the identity stat and position.
#'
#' A point layer keeps every column of the data frame as attributes; a
#' line/polygon layer keeps the columns that are constant within every
#' group, one feature per group (a mapped `fill`/`colour` column must be
#' constant within each group). `fill = NA`/`colour = NA` (including
#' `geom_polygon()`'s default `colour`) render as "not drawn" in QGIS.
#' The `size`, `shape`, `linetype` and `alpha` aesthetics are not carried
#' over; those symbol properties keep the QGIS defaults.
#'
#' The project opens zoomed to the plot's displayed range (the panel range,
#' including the default expansion and any [ggplot2::coord_sf()] `xlim`/
#' `ylim`), reprojected to the project CRS, rather than the whole world.
#'
#' # tmap plots
#'
#' A tmap (>= 4.4) object with vector layers is converted the same way:
#' `tm_polygons()`/`tm_fill()`/`tm_borders()`, `tm_lines()`, and
#' `tm_symbols()`/`tm_dots()`/`tm_bubbles()`/`tm_squares()` (on point data)
#' are supported, including [tmap::qtm()] maps. The color scales are
#' reproduced from tmap's own trained scales:
#'
#' - [tmap::tm_scale_intervals()] (any classification style) becomes a
#'   graduated renderer with tmap's exact break boundaries and colors
#'   (zero-width bins from tied breaks are collapsed; the continuous-style
#'   legend variants, `label.style`, are not supported),
#' - [tmap::tm_scale_categorical()] and [tmap::tm_scale_ordinal()] become a
#'   categorized renderer keyed by the raw data values (missing values
#'   become the "all other values" category); tmap's formatted legend
#'   labels are not carried over,
#' - [tmap::tm_scale_continuous()] (including the transformed variants,
#'   approximated with piecewise-linear color stops) becomes a graduated
#'   renderer with 25 equal-interval classes, or an exact continuous
#'   gradient with `gradient_style = "continuous"`.
#'
#' A layer maps either `fill` or `col` to a data column (not both); other
#' visual constants (symbol size, line type, alpha) keep the QGIS defaults.
#' Layers sharing one [tmap::tm_shape()] share one GeoPackage: the data is
#' written once and every layer of the shape references the same table.
#' Features whose value is missing are not drawn by an intervals/continuous
#' renderer (QGIS has no missing-value class; tmap paints them in
#' `value.na`). [tmap::tm_basemap()] layers become XYZ tile layers
#' (overriding the `basemap` argument): a URL template is used as is, a
#' provider name is resolved via [maptiles::get_providers()], and with
#' several basemaps only the first one is checked (visible) in the layer
#' tree. Rasters, facets, `tm_text()` and the other scale types are errors.
#'
#' The project CRS defaults to the tmap display CRS (`use_plot_crs = TRUE`
#' for tmap objects): [tmap::tm_crs()] or the main shape's CRS — or
#' EPSG:3857 when the map has basemaps, which is how tmap itself resolves
#' it — so the project opens looking like the tmap plot, zoomed to the
#' main shape's bounding box. `use_plot_crs = FALSE` forces EPSG:3857.
#'
#' The conversion relies on tmap internals that are not part of its public
#' API, so a tmap version older than 4.4 is rejected.
#'
#' @param plot A ggplot object whose layers are backed by sf data or one of
#'   the supported data.frame geoms (see *Data frame layers*), or a tmap
#'   object with vector layers (see *tmap plots*).
#' @param path Path of the `.qgs` file to write. Tilde paths (e.g. `~/x.qgs`)
#'   are expanded.
#' @param use_plot_crs If `TRUE`, the project (map canvas) CRS is the plot's
#'   display CRS: for a ggplot, resolved the way [ggplot2::coord_sf()] does
#'   (its `crs` argument if specified, otherwise the CRS of the first layer
#'   that defines one); for a tmap object, tmap's own display CRS (see
#'   *tmap plots*). If `FALSE`, the project CRS is EPSG:3857 (Web
#'   Mercator). The default is `FALSE` for ggplot plots and `TRUE` for tmap
#'   objects. Either way the layers keep the CRS of their data; QGIS
#'   reprojects them on the fly.
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
#'
#'   Binned scales are unaffected: their bins are exact in a graduated
#'   renderer, so there is nothing to trade off. Requesting `"continuous"`
#'   for a layer with a binned scale keeps the bins, with a warning.
#' @param basemap An XYZ tile layer to add below the vector layers, or `NULL`
#'   (the default) for none. Either a predefined key or an arbitrary XYZ URL
#'   template (a string containing the `{z}`, `{x}` and `{y}` placeholders,
#'   e.g. `"https://tile.openstreetmap.org/{z}/{x}/{y}.png"`). The predefined
#'   keys are:
#'
#'   - `"osm"`: OpenStreetMap.
#'   - `"gsi_standard"`: the GSI standard map
#'     (<https://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png>).
#'   - `"gsi_pale"`: the GSI pale map
#'     (<https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png>).
#'
#'   XYZ tiles are in EPSG:3857; QGIS reprojects them to the project CRS on
#'   the fly.
#' @param overwrite If `FALSE` (the default), writing to a `path` that already
#'   exists is an error. Set to `TRUE` to overwrite it.
#' @param layer_names Names for the layers, used for the GeoPackage files
#'   and in the QGIS layer tree: a character vector with one name per
#'   layer, bottom-most first. `/`, `\`, `|`, `:`, `*`, `?`, `"`, `<`, `>`
#'   and control characters cannot be used (the name becomes a file name).
#'   If `NULL` (the default), each layer is named after the first of these
#'   that applies:
#'
#'   - the layer's own name (e.g. `geom_sf(name = "counties")`),
#'   - the variable its data came from (e.g. `nc` for
#'     `geom_sf(data = nc)`, or for `ggplot(nc)` when the variable is
#'     unambiguous),
#'   - the geom (e.g. `geom_sf`),
#'
#'   with a numbered suffix (`nc_2`) on collision.
#' @param ... Passed on to the methods.
#' @returns `path`, invisibly.
#' @examples
#' library(ggplot2)
#'
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' p <- ggplot(nc) +
#'   geom_sf(aes(fill = AREA))
#'
#' write_qgs(p, tempfile(fileext = ".qgs"))
#'
#' # tmap objects work the same way
#' if (requireNamespace("tmap", quietly = TRUE)) {
#'   x <- tmap::tm_shape(nc) + tmap::tm_polygons(fill = "AREA")
#'   write_qgs(x, tempfile(fileext = ".qgs"))
#' }
#' @importFrom rlang %||%
#' @export
write_qgs <- function(plot, path, ...) {
  UseMethod("write_qgs")
}

#' @export
write_qgs.default <- function(plot, path, ...) {
  stop(
    "`plot` must be a ggplot or tmap object, got ", class(plot)[1],
    call. = FALSE
  )
}

#' @rdname write_qgs
#' @export
write_qgs.ggplot <- function(plot, path, use_plot_crs = FALSE,
                             gradient_style = c("graduated", "continuous"),
                             overwrite = FALSE, layer_names = NULL,
                             basemap = NULL, ...) {
  rlang::check_dots_empty()
  layers <- plot@layers
  if (length(layers) == 0L) {
    stop("`plot` must have at least one layer", call. = FALSE)
  }
  layer_names <- qgs_layer_names(plot, layer_names)
  if (!isTRUE(use_plot_crs) && !isFALSE(use_plot_crs)) {
    stop("`use_plot_crs` must be TRUE or FALSE", call. = FALSE)
  }
  if (!isTRUE(overwrite) && !isFALSE(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE", call. = FALSE)
  }
  gradient_style <- match.arg(gradient_style)
  basemap_layer <- qgs_basemap_layer(basemap)

  path <- path.expand(path)

  if (!overwrite && file.exists(path)) {
    stop(
      "`path` already exists: ", path,
      "\nSet `overwrite = TRUE` to overwrite it.",
      call. = FALSE
    )
  }

  # The raw data of each layer, validated before ggplot_build() so a bad
  # layer fails with a specific error, not somewhere inside the build.
  layer_data <- lapply(seq_along(layers), function(i) {
    qgs_layer_data(plot, layers[[i]], i)
  })

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

    d <- layer_data[[i]]
    sf_data <- inherits(d, "sf")
    if (!sf_data) {
      d <- qgs_df_layer_sf(plot, built, layer, i, d)
    }

    crs <- sf::st_crs(d)
    if (is.na(crs)) {
      stop("layer ", i, ": the data has no CRS", call. = FALSE)
    }
    if (use_plot_crs && (is.null(plot_crs) || is.na(plot_crs))) {
      plot_crs <- crs
    }

    layer_name <- layer_names[[i]]
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
      qgs_vector_style(
        plot, built, layer, i, d, gradient_style, geometry, sf_data
      )
    )
  }

  # Open the project zoomed to the plot's displayed range instead of the
  # template's whole-world extent.
  panel <- built@layout$panel_params[[1]]
  project_crs <- if (use_plot_crs) sf::st_crs(plot_crs) else sf::st_crs(3857L)
  extent <- qgs_canvas_extent(panel, project_crs)

  # The basemap draws under every vector layer, so it is the bottom-most
  # entry (qgs_build() expects bottom-most first).
  if (!is.null(basemap_layer)) {
    qgs_layers <- c(list(basemap_layer), qgs_layers)
  }

  project_srs <- if (use_plot_crs) resolve_srs(plot_crs)
  qgs_write(qgs_layers, path, project_srs, extent)

  invisible(path)
}

# The raw data of a layer (its own, or inherited from ggplot()) — not the
# computed data of ggplot_build(), which no longer has the original
# values. Must be an sf object or a data.frame with at least one row.
qgs_layer_data <- function(plot, layer, i) {
  d <- layer$data
  if (is.null(d) || inherits(d, "waiver")) {
    d <- plot@data
  }
  if (is.null(d) || inherits(d, "waiver")) {
    stop("layer ", i, " has no data", call. = FALSE)
  }
  if (!is.data.frame(d)) {
    stop(
      "layer ", i, ": the layer data must be an sf object or a ",
      "data.frame, got ", class(d)[1],
      call. = FALSE
    )
  }
  if (nrow(d) == 0L) {
    stop("layer ", i, ": the data has no rows", call. = FALSE)
  }
  d
}

# Resolves the `basemap` argument to an xyz_tile_layer(), or NULL when no
# basemap was requested. A predefined key (see QGS_BASEMAPS) wins; otherwise
# the string must be an XYZ URL template with the {z}/{x}/{y} placeholders.
qgs_basemap_layer <- function(basemap) {
  if (is.null(basemap)) {
    return(NULL)
  }
  if (!is.character(basemap) || length(basemap) != 1L || is.na(basemap)) {
    stop("`basemap` must be a single string or NULL", call. = FALSE)
  }

  spec <- QGS_BASEMAPS[[basemap]]
  if (!is.null(spec)) {
    return(xyz_tile_layer(spec$name, spec$url, spec$zmin, spec$zmax))
  }

  # Not a known key: treat it as a URL template. Require the placeholders so
  # a mistyped key fails loudly instead of producing a broken tile source.
  if (!qgs_is_xyz_template(basemap)) {
    stop(
      "`basemap` must be one of ",
      paste0('"', names(QGS_BASEMAPS), '"', collapse = ", "),
      ", or an XYZ URL template containing {z}, {x} and {y}; got: ", basemap,
      call. = FALSE
    )
  }
  xyz_tile_layer("basemap", basemap, 0L, 19L)
}

# Whether a string is an XYZ tile URL template (shared by the `basemap`
# argument and tm_basemap() resolution).
qgs_is_xyz_template <- function(url) {
  all(vapply(c("{z}", "{x}", "{y}"), grepl, logical(1L), url, fixed = TRUE))
}

# The name of every layer, bottom-most first. `layer_names` is the
# user-supplied override (already documented in write_qgs()); NULL means
# derive a name per layer:
#
# 1. the ggplot2 layer name (user-set, so a forbidden character is an
#    error, like in `layer_names`),
# 2. the variable the data came from,
# 3. the geom that created the layer,
# 4. "layer<i>",
#
# where 2.-4. are derived, so they are silently sanitized instead. A name
# colliding with an earlier one gets a "_2", "_3", ... suffix.
qgs_layer_names <- function(plot, layer_names) {
  layers <- plot@layers
  n <- length(layers)

  if (!is.null(layer_names)) {
    return(qgs_validate_layer_names(layer_names, n))
  }

  out <- character(n)
  for (i in seq_len(n)) {
    name <- layers[[i]]$name
    if (!is.null(name)) {
      qgs_check_layer_name(name, paste0("layer ", i, ": the layer name"))
    } else {
      name <- qgs_derived_layer_name(plot, layers[[i]], i)
    }
    out[[i]] <- qgs_uncollide_name(name, out[seq_len(i - 1L)])
  }
  out
}

# Validates a user-supplied `layer_names` override (shared by the ggplot
# and tmap methods): one non-empty, unique, file-name-safe name per layer.
qgs_validate_layer_names <- function(layer_names, n) {
  if (!is.character(layer_names) || length(layer_names) != n) {
    stop(
      "`layer_names` must be a character vector with one name per layer (",
      n, ")",
      call. = FALSE
    )
  }
  if (anyNA(layer_names) || !all(nzchar(layer_names))) {
    stop("`layer_names` must not contain NA or empty names", call. = FALSE)
  }
  if (anyDuplicated(layer_names)) {
    stop("`layer_names` must be unique", call. = FALSE)
  }
  qgs_check_layer_name(layer_names, "`layer_names`")
  layer_names
}

qgs_check_layer_name <- function(names, what) {
  bad <- grepl(QGS_LAYER_NAME_FORBIDDEN, names)
  if (any(bad)) {
    stop(
      what, " cannot contain any of /\\|:*?\"<> or control characters: ",
      names[bad][1L],
      call. = FALSE
    )
  }
}

qgs_derived_layer_name <- function(plot, layer, i) {
  # The layer's constructor is the geom call as the user wrote it, so a
  # bare symbol passed as its `data` argument is the variable name (`data`
  # is never positional: `mapping` comes first in every geom).
  cons <- layer$constructor
  if (is.call(cons)) {
    data_arg <- rlang::call_args(cons)[["data"]]
    if (rlang::is_symbol(data_arg)) {
      return(qgs_sanitize_layer_name(rlang::as_string(data_arg), i))
    }
  }

  # The variable the layer's data (own or inherited from ggplot()) is
  # bound to in the environment the plot was created in, e.g. `nc` for
  # ggplot(nc). Only when the match is unambiguous; a guess is worse
  # than the geom fallback.
  d <- layer$data
  if (is.null(d) || inherits(d, "waiver")) {
    d <- plot@data
  }
  name <- qgs_data_binding_name(d, plot@plot_env)
  if (!is.null(name)) {
    return(qgs_sanitize_layer_name(name, i))
  }

  if (is.call(cons)) {
    fn <- rlang::call_name(cons)
    if (!is.null(fn)) {
      return(qgs_sanitize_layer_name(fn, i))
    }
  }

  paste0("layer", i)
}

# The single variable in `env` (not its parents) bound to exactly `d`, or
# NULL if there is none or more than one. Bindings that cannot be read
# (e.g. an active binding that errors) are skipped.
qgs_data_binding_name <- function(d, env) {
  if (!is.data.frame(d) || !is.environment(env)) {
    return(NULL)
  }
  hit <- NULL
  for (name in ls(env, sorted = TRUE)) {
    obj <- tryCatch(
      get(name, envir = env, inherits = FALSE),
      error = function(e) NULL
    )
    if (identical(obj, d)) {
      if (!is.null(hit)) {
        return(NULL)
      }
      hit <- name
    }
  }
  hit
}

qgs_sanitize_layer_name <- function(name, i) {
  name <- gsub(QGS_LAYER_NAME_FORBIDDEN, "_", name)
  # A leading dot would hide the .gpkg file; Windows forbids a trailing one.
  name <- gsub("^[ .]+|[ .]+$", "", name)
  if (nzchar(name)) name else paste0("layer", i)
}

qgs_uncollide_name <- function(name, taken) {
  if (!name %in% taken) {
    return(name)
  }
  k <- 2L
  while (paste0(name, "_", k) %in% taken) {
    k <- k + 1L
  }
  paste0(name, "_", k)
}

# The initial map-canvas extent reproducing what the plot displays.
# ggplot2's panel range (`x_range`/`y_range`, already including the default
# expansion and any coord_sf() xlim/ylim) is in the panel CRS; QGIS wants
# the extent in the project CRS. When they differ, the rectangle is
# densified before transforming so a curved reprojected edge is bounded by
# its whole arc, not just the four corners. Returns c(xmin, ymin, xmax,
# ymax), or NULL if the range is unavailable (keeping the world default).
qgs_canvas_extent <- function(panel, project_crs) {
  x_range <- panel$x_range
  y_range <- panel$y_range
  if (is.null(x_range) || is.null(y_range) ||
      anyNA(x_range) || anyNA(y_range)) {
    return(NULL)
  }

  src_crs <- sf::st_crs(panel$crs)
  qgs_reproject_extent(
    c(x_range[1L], y_range[1L], x_range[2L], y_range[2L]),
    src_crs,
    project_crs
  )
}

# Reprojects a c(xmin, ymin, xmax, ymax) extent, densifying the rectangle
# before transforming so a curved reprojected edge is bounded by its
# whole arc, not just the four corners. The rectangle is clipped to the
# destination CRS's area of use first, so e.g. a pole-touching extent
# does not blow up in Web Mercator (whose domain ends at about +-85
# degrees). Same-CRS (or unknown-CRS) extents pass through unchanged.
qgs_reproject_extent <- function(extent, src_crs, dst_crs) {
  if (is.na(src_crs) || is.na(dst_crs) || src_crs == dst_crs) {
    return(extent)
  }

  n <- 100L
  xs <- seq(extent[1L], extent[3L], length.out = n)
  ys <- seq(extent[2L], extent[4L], length.out = n)
  ring <- rbind(
    cbind(xs, extent[2L]),
    cbind(extent[3L], ys),
    cbind(rev(xs), extent[4L]),
    cbind(extent[1L], rev(ys))
  )
  ring <- rbind(ring, ring[1L, , drop = FALSE])
  poly <- sf::st_sfc(sf::st_polygon(list(ring)), crs = src_crs)

  aou <- qgs_crs_area_of_use(dst_crs)
  if (!is.null(aou)) {
    # The area of use is stated in longitude/latitude: clamp the ring
    # there, then transform on to the destination.
    lonlat <- sf::st_crs(4326L)
    coords <- sf::st_coordinates(sf::st_transform(poly, lonlat))[, 1:2]
    coords[, 1L] <- pmin(pmax(coords[, 1L], aou$lon[1L]), aou$lon[2L])
    coords[, 2L] <- pmin(pmax(coords[, 2L], aou$lat[1L]), aou$lat[2L])
    poly <- sf::st_sfc(sf::st_polygon(list(coords)), crs = lonlat)
  }

  bbox <- sf::st_bbox(sf::st_transform(poly, dst_crs))
  as.numeric(bbox[c("xmin", "ymin", "xmax", "ymax")])
}

# The area of use of a CRS as list(lon = c(min, max), lat = c(min, max)),
# parsed from the USAGE BBOX of its WKT (stated as south, west, north,
# east in degrees); NULL when the WKT carries none.
qgs_crs_area_of_use <- function(crs) {
  m <- regmatches(crs$wkt, regexpr("BBOX\\[[^]]+\\]", crs$wkt))
  if (length(m) == 0L) {
    return(NULL)
  }
  v <- suppressWarnings(
    as.numeric(strsplit(substr(m, 6L, nchar(m) - 1L), ",", fixed = TRUE)[[1L]])
  )
  if (length(v) != 4L || anyNA(v)) {
    return(NULL)
  }
  list(lon = c(v[2L], v[4L]), lat = c(v[1L], v[3L]))
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

# Resolves which of `fill`/`colour` drives the varying color of a layer:
# NULL when neither is mapped, else list(aes =, attribute =). The layer's
# mapping takes precedence over the plot's, following how ggplot2 itself
# resolves aesthetics. Only a bare column name is supported (the raw data
# is what's written to the GeoPackage). Shared between the style
# resolution below and the data.frame conversion (df_layer.R), which must
# know the styled column to keep it in the per-group attributes.
qgs_style_attribute <- function(plot, layer, i, d) {
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
  if (is.null(fill) && is.null(colour)) {
    return(NULL)
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
  list(aes = aes_name, attribute = attribute)
}

# Resolves the aesthetics of a layer into the matching style. `sf_data`
# says whether the layer was sf originally (see qgs_layer_constants()).
qgs_vector_style <- function(plot, built, layer, i, d, gradient_style,
                             geometry, sf_data) {
  mapped <- qgs_style_attribute(plot, layer, i, d)

  const <- qgs_layer_constants(built@data[[i]], sf_defaults = sf_data)
  # Rounded so binary float noise (0.15056250000000002) stays out of the
  # project file.
  outline_width <- round(const$linewidth * QGS_MM_PER_LINEWIDTH, 7)
  is_polygon <- geometry == "Polygon"

  if (is.null(mapped)) {
    return(qgs_single_style(const, is_polygon, outline_width, i))
  }
  aes_name <- mapped$aes
  attribute <- mapped$attribute

  scale <- built@plot@scales$get_scales(aes_name)
  # ScaleBinned must be checked before is_discrete(): a binned scale is not
  # discrete, but falling through to the gradient paths would smooth away
  # the steps.
  style <- if (inherits(scale, "ScaleBinned")) {
    if (gradient_style == "continuous") {
      # Per layer, not per plot: a mixed plot can have a continuous scale
      # on another layer that the option legitimately applies to.
      warning(
        "layer ", i, ": `gradient_style = \"continuous\"` does not apply ",
        "to a binned scale; the exact bins are kept",
        call. = FALSE
      )
    }
    qgs_binned_style(scale, attribute, i)
  } else if (scale$is_discrete()) {
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
# With `sf_defaults` (sf layers), NA is GeomSf's "use the per-geometry
# default" sentinel, so it falls back to geom_sf()'s defaults. For
# data.frame geoms the built values are theme-resolved and concrete, so
# NA really means "not drawn" and becomes NULL; only a missing linewidth
# (geom_point has none) keeps the 0.2 default, matching the marker
# outline sf points get today.
qgs_layer_constants <- function(computed, sf_defaults) {
  first_or <- function(name, default) {
    v <- computed[[name]]
    if (length(v) == 0L || is.na(v[[1L]])) default else v[[1L]]
  }
  if (sf_defaults) {
    return(list(
      colour = first_or("colour", "grey35"),
      fill = first_or("fill", "grey90"),
      linewidth = first_or("linewidth", 0.2)
    ))
  }
  list(
    colour = first_or("colour", NULL),
    fill = first_or("fill", NULL),
    linewidth = first_or("linewidth", 0.2)
  )
}

# For a layer without a fill/colour mapping, reproduce ggplot2's constant
# colors: interior + border for polygons; for lines and points the single
# color is the stroke/marker color, with a matching ring (ggplot2 points
# have no distinct border). A NULL color means "not drawn", which only a
# polygon's fill or an outline can express — a layer that would draw
# nothing at all is an error.
qgs_single_style <- function(const, is_polygon, outline_width, i) {
  main <- if (is_polygon) const$fill else const$colour
  if (is.null(main) && (!is_polygon || is.null(const$colour))) {
    stop(
      "layer ", i, ": the layer would not be drawn (",
      if (is_polygon) "both `fill` and `colour` are" else "`colour` is",
      " NA)",
      call. = FALSE
    )
  }
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

# A trained binned scale (scale_fill_steps() etc.) as explicit bins: the
# boundaries are the scale limits plus the inner breaks, and each bin's
# color is what ggplot2 maps its midpoint to (constant within a bin).
qgs_binned_style <- function(scale, attribute, i) {
  limits <- scale$get_limits()
  if (anyNA(limits) || limits[2L] <= limits[1L]) {
    stop(
      "layer ", i, ": cannot map `", attribute,
      "` to binned colors (the scale's limits are degenerate)",
      call. = FALSE
    )
  }

  breaks <- scale$get_breaks()
  # ggplot2 NA-s out-of-bounds breaks; also drop breaks sitting exactly on
  # a limit so no zero-width bin is emitted.
  breaks <- breaks[is.finite(breaks) & breaks > limits[1L] & breaks < limits[2L]]
  boundaries <- c(limits[1L], sort(breaks), limits[2L])

  mids <- (boundaries[-length(boundaries)] + boundaries[-1L]) / 2
  style_binned(attribute, boundaries, grDevices::col2rgb(scale$map(mids)))
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

# NULL (or NA, its data.frame-layer source) stays NULL: "not drawn".
qgs_rgb <- function(color) {
  if (is.null(color) || is.na(color[[1L]])) {
    return(NULL)
  }
  grDevices::col2rgb(color)[, 1L]
}
