# write_qgs() for tmap objects.
#
# tmap (v4) has no ggplot2 conversion to piggyback on — it renders with
# grid/leaflet — but its internal pipeline separates data/scale processing
# from drawing: step1_rearrange() normalizes the spec and step2_data()
# trains the scales, producing per-feature visual values (mapping_dt) and
# the trained legends (breaks, palette colors, limits). That is exactly
# what a QGIS renderer needs, so this module runs steps 1-2 and maps the
# result onto the style_*() constructors in style.R.
#
# step1_rearrange()/step2_data() and the .TMAP environment are unexported
# tmap internals: their use is version-gated (QGS_TMAP_MIN_VERSION), and
# structure mismatches are made to fail loudly rather than guessed around.
# See .tmp/research/20260724_tmap_support.md for the verified structures.

# Millimeters per lwd unit: tmap line widths are grid/base-R lwd, where
# 1 lwd is 1/96 inch.
QGS_MM_PER_LWD <- 25.4 / 96

# The lowest tmap version whose internals this module was verified
# against.
QGS_TMAP_MIN_VERSION <- "4.4"

# The tmap scale constructors whose trained result can be represented as
# a QGIS renderer. tm_scale() (auto) dispatches to one of the concrete
# ones by data type; which one won is read from the trained legend.
# Everything else (rank, bivariate, rgb, asis, discrete) remaps the data
# in ways a value-driven QGIS renderer cannot reproduce.
QGS_TMAP_SCALE_FUNS <- c(
  "tmapScaleAuto",
  "tmapScaleIntervals",
  "tmapScaleCategorical", # also tm_scale_ordinal()
  "tmapScaleContinuous"
)

#' @rdname write_qgs
#' @export
write_qgs.tmap <- function(plot, path, use_plot_crs = TRUE,
                           gradient_style = c("graduated", "continuous"),
                           overwrite = FALSE, layer_names = NULL,
                           basemap = NULL, ...) {
  rlang::check_dots_empty()
  qgs_tmap_check()
  if (!isTRUE(use_plot_crs) && !isFALSE(use_plot_crs)) {
    stop("`use_plot_crs` must be TRUE or FALSE", call. = FALSE)
  }
  if (!isTRUE(overwrite) && !isFALSE(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE", call. = FALSE)
  }
  gradient_style <- match.arg(gradient_style)
  basemap_arg_layer <- qgs_basemap_layer(basemap)

  path <- path.expand(path)
  if (!overwrite && file.exists(path)) {
    stop(
      "`path` already exists: ", path,
      "\nSet `overwrite = TRUE` to overwrite it.",
      call. = FALSE
    )
  }

  built <- qgs_tmap_build(plot)
  specs <- qgs_tmap_layer_specs(built, gradient_style)
  names <- qgs_tmap_layer_names(specs, layer_names)

  basemap_layers <- qgs_tmap_basemap_layers(built)
  if (length(basemap_layers) > 0L) {
    if (!is.null(basemap_arg_layer)) {
      warning(
        "the tmap object has tm_basemap() layers; the `basemap` argument ",
        "is ignored",
        call. = FALSE
      )
    }
  } else if (!is.null(basemap_arg_layer)) {
    basemap_layers <- list(basemap_arg_layer)
  }

  data_dir_name <- paste0(tools::file_path_sans_ext(basename(path)), "_data")
  data_dir <- file.path(dirname(path), data_dir_name)
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  # Layers sharing one tm_shape share one GeoPackage: the group's data is
  # written once (named after its first layer) and later layers of the
  # group reference the same table under their own display name.
  qgs_layers <- vector("list", length(specs))
  written <- list()
  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    layer_name <- names[[i]]
    group_key <- as.character(spec$group)
    info <- written[[group_key]]
    if (is.null(info)) {
      gpkg_file <- paste0(layer_name, ".gpkg")
      gpkg_path <- file.path(data_dir, gpkg_file)
      if (file.exists(gpkg_path)) {
        unlink(gpkg_path)
      }
      sf::st_write(spec$data, gpkg_path, layer = layer_name, quiet = TRUE)
      info <- list(file = gpkg_file, table = layer_name)
      written[[group_key]] <- info
    }

    qgs_layers[[i]] <- vector_layer(
      # relative to the project file
      paste0(data_dir_name, "/", info$file),
      layer_name,
      sf::st_crs(spec$data),
      spec$geometry,
      spec$style,
      table = info$table
    )
  }

  project_crs <- if (use_plot_crs) qgs_tmap_crs(built) else sf::st_crs(3857L)
  extent <- qgs_tmap_extent(built, project_crs)

  # The basemaps draw under every vector layer, so they are the
  # bottom-most entries (qgs_build() expects bottom-most first).
  qgs_layers <- c(basemap_layers, qgs_layers)

  project_srs <- if (use_plot_crs) resolve_srs(project_crs)
  qgs_write(qgs_layers, path, project_srs, extent)

  invisible(path)
}

qgs_tmap_check <- function() {
  if (!requireNamespace("tmap", quietly = TRUE)) {
    stop(
      "the tmap package is required to write a tmap object; ",
      "install it with install.packages(\"tmap\")",
      call. = FALSE
    )
  }
  if (utils::packageVersion("tmap") < QGS_TMAP_MIN_VERSION) {
    stop(
      "tmap >= ", QGS_TMAP_MIN_VERSION, " is required, got ",
      utils::packageVersion("tmap"),
      call. = FALSE
    )
  }
}

# Fetches an unexported tmap internal, failing with a pointer to the
# version requirement instead of a bare "object not found".
qgs_tmap_internal <- function(name) {
  ns <- asNamespace("tmap")
  if (!exists(name, envir = ns, inherits = FALSE)) {
    stop(
      "the installed tmap version (", utils::packageVersion("tmap"),
      ") lacks the internal `", name, "` this package relies on",
      call. = FALSE
    )
  }
  get(name, envir = ns, inherits = FALSE)
}

# Runs tmap's step 1 (spec normalization) and step 2 (scale training) and
# snapshots the trained legends. print.tmap initializes some fields of the
# .TMAP environment before step 1 and restores sf's s2 setting afterwards;
# both are reproduced here.
qgs_tmap_build <- function(x) {
  tmap_env <- qgs_tmap_internal(".TMAP")
  tmap_env$in.shiny <- FALSE
  tmap_env$proxy <- FALSE
  tmap_env$set_s2 <- NA
  tmap_env$animate <- NULL
  tmap_env$raster_wrap <- FALSE
  on.exit({
    if (!is.na(tmap_env$set_s2)) {
      suppressMessages(sf::sf_use_s2(tmap_env$set_s2))
    }
  })

  step1 <- qgs_tmap_internal("step1_rearrange")
  step2 <- qgs_tmap_internal("step2_data")

  x2 <- step1(x)
  x3 <- step2(x2)
  # step 2 skips the legend store for a map without data layers.
  legs <- if (exists("legs", envir = tmap_env, inherits = FALSE)) {
    get("legs", envir = tmap_env)
  } else {
    list()
  }

  list(x2 = x2, x3 = x3, legs = legs)
}

# Converts the built tmap object into per-layer specs:
# list(data = <sf>, name = <default layer name>, geometry =, style =).
# Anything that cannot be represented faithfully in a QGIS project is an
# error, never silently dropped.
qgs_tmap_layer_specs <- function(built, gradient_style) {
  x2 <- built$x2
  x3 <- built$x3

  if (is.null(x3$tmo) || length(x3$tmo) == 0L) {
    stop("the tmap object has no data layers", call. = FALSE)
  }
  if (any(x3$o$fn > 1L)) {
    stop(
      "faceted tmap objects (tm_facets(), or multiple variables per ",
      "aesthetic) are not supported",
      call. = FALSE
    )
  }
  qgs_tmap_check_aux(x3$aux)

  specs <- list()
  i <- 0L
  for (gi in seq_along(x3$tmo)) {
    tms <- x2$tmo[[gi]]$tms
    d <- qgs_tmap_group_sf(tms, gi)
    for (li in seq_along(x3$tmo[[gi]]$layers)) {
      i <- i + 1L
      lyr <- x3$tmo[[gi]]$layers[[li]]
      tml <- x2$tmo[[gi]]$tmls[[li]]
      spec <- qgs_tmap_layer_spec(
        built, d, tms, lyr, tml, i, gradient_style
      )
      if (!is.null(spec)) {
        spec$group <- gi
        specs[[length(specs) + 1L]] <- spec
      }
    }
  }
  if (length(specs) == 0L) {
    stop("the tmap object has no drawable layers", call. = FALSE)
  }
  specs
}

# Aux layers: tm_basemap() is handled by qgs_tmap_basemap_layers();
# everything else (graticules, overlay tiles, ...) has no QGIS-project
# representation here.
qgs_tmap_check_aux <- function(aux) {
  for (a in aux) {
    if (!inherits(a, "tm_basemap")) {
      stop(
        "unsupported tmap element: ", class(a)[1L],
        call. = FALSE
      )
    }
  }
}

# The group's shape + attributes as the original sf object: the step-1
# data table keeps every original column (plus tmap's bookkeeping
# columns), and shpTM holds the geometries keyed by tmapID.
qgs_tmap_group_sf <- function(tms, gi) {
  shp <- tms$shpTM$shp
  if (!inherits(shp, "sfc")) {
    stop(
      "shape ", gi, ": only sf objects are supported; raster shapes ",
      "(stars/terra) are not",
      call. = FALSE
    )
  }
  dt <- as.data.frame(tms$dt)
  if (!all(dt$sel__)) {
    stop(
      "shape ", gi, ": filtered shapes (tm_shape(filter = )) are not ",
      "supported",
      call. = FALSE
    )
  }
  attrs <- dt[setdiff(names(dt), c("tmapID__", "sel__"))]
  geometry <- shp[match(dt$tmapID__, tms$shpTM$tmapID)]
  d <- sf::st_sf(attrs, geometry = geometry)
  if (is.na(sf::st_crs(d))) {
    stop("shape ", gi, ": the data has no CRS", call. = FALSE)
  }
  d
}

# The user-facing layer function name (e.g. "tm_dots"), for error
# messages. mapping_fun is a vector whose first element is the specific
# layer (e.g. "tm_data_dots"), with the base kind last.
qgs_tmap_layer_label <- function(lyr) {
  gsub("^tm_data_", "tm_", lyr$mapping_fun[[1L]])
}

# One layer of one shape group -> a spec for vector_layer().
qgs_tmap_layer_spec <- function(built, d, tms, lyr, tml, i, gradient_style) {
  mfun <- lyr$mapping_fun
  kind <- if ("tm_data_polygons" %in% mfun) {
    "polygons"
  } else if ("tm_data_lines" %in% mfun) {
    "lines"
  } else if ("tm_data_symbols" %in% mfun) {
    "symbols"
  } else {
    stop(
      "layer ", i, ": unsupported tmap layer type (",
      qgs_tmap_layer_label(lyr), ")",
      call. = FALSE
    )
  }

  geometry <- qgs_geometry_type(d, i)
  expected <- switch(kind,
    polygons = "Polygon",
    lines = "LineString",
    symbols = "Point"
  )
  if (geometry != expected) {
    # A strict geometry filter ("yes", e.g. the sub-layers qtm() adds for
    # every geometry type) means tmap draws nothing for a mismatched
    # shape: skip the layer. "ifany" would make tmap transform the shape
    # (centroids for symbols, boundaries for lines), which is not
    # reproduced here.
    # TODO: support tm_symbols()/tm_dots() on polygon shapes by computing
    # centroids (tmap's step-3 transformation).
    only <- switch(kind,
      polygons = tml$trans.args$polygons.only,
      lines = tml$trans.args$lines.only,
      symbols = tml$trans.args$points_only
    )
    if (identical(only, "yes")) {
      return(NULL)
    }
    stop(
      "layer ", i, " (", qgs_tmap_layer_label(lyr), "): ", expected,
      " geometry is required, got ", geometry,
      call. = FALSE
    )
  }

  legends <- qgs_tmap_layer_legends(built, lyr)
  active <- names(legends)[vapply(
    legends,
    function(leg) isTRUE(leg$active),
    logical(1L)
  )]

  color_aes <- intersect(active, c("fill", "col"))
  if (length(color_aes) == 2L) {
    stop(
      "layer ", i, ": mapping both `fill` and `col` on the same layer is ",
      "not supported",
      call. = FALSE
    )
  }
  other <- setdiff(active, c("fill", "col"))
  if (length(other) > 0L) {
    # TODO: a graduated-size renderer could cover a mapped `size`.
    stop(
      "layer ", i, ": only `fill` and `col` can be mapped to data; the `",
      other[[1L]], "` scale is not supported",
      call. = FALSE
    )
  }

  # The constant visual values tmap computed for this layer, taken from
  # its first feature (only meaningful for aesthetics that are not
  # mapped). A missing column (e.g. `fill` of a line layer) is NULL.
  # TODO: `size`, `shape`, `lty`, `fill_alpha` and `col_alpha` constants
  # are not carried over; those symbol properties keep the QGIS defaults.
  md <- lyr$mapping_dt
  const <- function(name) {
    if (name %in% names(md)) md[[name]][[1L]] else NULL
  }
  fill_const <- const("fill")
  col_const <- const("col")
  lwd_const <- const("lwd") %||% 1
  # Rounded so binary float noise stays out of the project file.
  outline_width <- round(lwd_const * QGS_MM_PER_LWD, 7)

  style <- if (length(color_aes) == 0L) {
    qgs_tmap_single_style(
      kind, fill_const, col_const, outline_width, i
    )
  } else {
    qgs_tmap_mapped_style(
      d, tms$dt$tmapID__, lyr, tml, i, kind, color_aes,
      legends[[color_aes]], gradient_style,
      fill_const, col_const, outline_width
    )
  }

  list(
    data = d,
    name = tms$shp_name,
    geometry = geometry,
    style = style
  )
}

# The trained legend of each aesthetic of a layer. step 2 stores the
# legends in the .TMAP environment; the layer only carries indices.
qgs_tmap_layer_legends <- function(built, lyr) {
  lapply(lyr$mapping_legend, function(ref) {
    built$legs[[ref$legnr[[1L]]]]
  })
}

# A layer without a mapped color: tmap's constant colors. `fill` is the
# interior (polygons, symbols), `col` the outline (or the line color).
# NA colors mean "not drawn", which only a polygon's fill or an outline
# can express.
qgs_tmap_single_style <- function(kind, fill_const, col_const,
                                  outline_width, i) {
  main <- if (kind == "lines") col_const else fill_const
  main_rgb <- qgs_rgb(main)
  col_rgb <- qgs_rgb(col_const)
  if (is.null(main_rgb) && (kind != "polygons" || is.null(col_rgb))) {
    stop(
      "layer ", i, ": the layer would not be drawn (the colors are NA)",
      call. = FALSE
    )
  }
  style_set_outline(style_single(main_rgb), col_rgb, outline_width)
}

# A layer with a mapped fill/col: resolve the trained scale into a style
# and wire the varying color to the right slot (fill vs stroke). `ids` is
# the tmapID of each row of `d`, the key into the layer's mapping_dt.
qgs_tmap_mapped_style <- function(d, ids, lyr, tml, i, kind, aes, leg,
                                  gradient_style, fill_const, col_const,
                                  outline_width) {
  aes_spec <- tml$mapping.aes[[aes]]
  scale_fun <- aes_spec$scale$FUN
  if (!scale_fun %in% QGS_TMAP_SCALE_FUNS) {
    stop(
      "layer ", i, ": unsupported scale (", class(aes_spec$scale)[[1L]],
      ") for `", aes, "`",
      call. = FALSE
    )
  }
  attribute <- unname(aes_spec$vars[[1L]])
  if (!attribute %in% names(d)) {
    stop(
      "layer ", i, ": column `", attribute, "` not found in the shape data",
      call. = FALSE
    )
  }

  style <- switch(leg$scale,
    intervals = qgs_tmap_binned_style(leg, attribute, i, gradient_style),
    categorical = qgs_tmap_categorized_style(d, ids, lyr, attribute, aes, i),
    continuous = qgs_tmap_continuous_style(
      leg, aes_spec$scale, attribute, aes, i, gradient_style
    ),
    stop(
      "layer ", i, ": unsupported scale type \"", leg$scale, "\" for `",
      aes, "`",
      call. = FALSE
    )
  )

  if (aes == "fill" || kind == "lines") {
    # fill: a constant border around the varying fill. Lines: the line
    # color is the varying one; only the width is constant.
    style <- style_set_outline(style, qgs_rgb(col_const), outline_width)
  } else {
    # A varying `col` on polygons is the border color, on symbols the
    # ring around the marker; the interior keeps the constant fill.
    style <- style_set_stroke_target(style, qgs_rgb(fill_const))
    style <- style_set_outline(style, qgs_rgb(fill_const), outline_width)
  }
  style
}

# tm_scale_intervals(): the legend carries the resolved break boundaries
# (dvalues) and one color per bin (vvalues; plus a trailing NA color when
# the legend shows missings — QGIS graduated renderers have no NA slot,
# so NA features are simply not drawn).
qgs_tmap_binned_style <- function(leg, attribute, i, gradient_style) {
  if (gradient_style == "continuous") {
    # Per layer, not per plot, like the ggplot2 path.
    warning(
      "layer ", i, ": `gradient_style = \"continuous\"` does not apply ",
      "to an intervals scale; the exact bins are kept",
      call. = FALSE
    )
  }
  colors <- leg$vvalues
  # tm_scale_intervals(label.style = "cont"/"log10") stores the legend as
  # collapsed gradient strings instead of one color per bin.
  if (any(grepl("_", colors, fixed = TRUE))) {
    stop(
      "layer ", i, ": tm_scale_intervals() with a continuous-style legend ",
      "(label.style) is not supported",
      call. = FALSE
    )
  }
  if (isTRUE(leg$na.show)) {
    colors <- colors[-length(colors)]
  }
  # tmap stores the legend colors bottom-up when tm_legend(reverse = TRUE)
  # while dvalues stay ascending; the features themselves are colored from
  # the unreversed palette.
  if (isTRUE(leg$reverse)) {
    colors <- rev(colors)
  }
  boundaries <- leg$dvalues
  n <- length(boundaries) - 1L
  if (n < 1L || length(colors) != n) {
    stop(
      "layer ", i, ": cannot map `", attribute,
      "` to binned colors (the trained scale is degenerate)",
      call. = FALSE
    )
  }
  # Data-driven classifications can produce tied breaks (e.g. quantiles of
  # a column dominated by one value). tmap assigns every feature to the
  # surviving bins, so the zero-width bins are dropped with their colors.
  keep <- diff(boundaries) > 0
  if (!any(keep)) {
    stop(
      "layer ", i, ": cannot map `", attribute,
      "` to binned colors (all bins are zero-width)",
      call. = FALSE
    )
  }
  if (!all(keep)) {
    colors <- colors[keep]
    boundaries <- c(boundaries[[1L]], boundaries[-1L][keep])
  }
  style_binned(attribute, boundaries, grDevices::col2rgb(colors))
}

# tm_scale_categorical() / tm_scale_ordinal(): the category values and
# colors are taken from the per-feature pairs of (raw column value,
# mapped color) — exact by construction, immune to label formatting and
# level combining. Levels without a feature are dropped (they would never
# render). NA features become the catch-all category.
# TODO: tmap's (possibly reformatted) legend labels are not carried over;
# QGIS labels each category with the raw value.
qgs_tmap_categorized_style <- function(d, ids, lyr, attribute, aes, i) {
  md <- lyr$mapping_dt
  raw <- d[[attribute]]
  colors <- md[[aes]][match(ids, md$tmapID__)]

  value_type <- "string"
  if (is.numeric(raw)) {
    # A numeric column: keep numeric order, write the values in plain
    # notation (as.character() would give "1e+06"), and type the
    # categories as double so QGIS compares them numerically against the
    # field.
    value_type <- "double"
    present <- sort(unique(raw[!is.na(raw)]))
    value_colors <- colors[match(present, raw)]
    values <- vapply(present, num, character(1L))
  } else {
    levels <- if (is.factor(raw)) {
      levels(raw)
    } else {
      sort(unique(as.character(raw[!is.na(raw)])))
    }
    raw_chr <- as.character(raw)
    present <- levels[levels %in% raw_chr]
    value_colors <- colors[match(present, raw_chr)]
    values <- present
  }
  if (length(values) == 0L) {
    stop(
      "layer ", i, ": the scale of `", attribute, "` has no values",
      call. = FALSE
    )
  }

  catch_all <- NULL
  if (anyNA(raw)) {
    catch_all <- grDevices::col2rgb(colors[[which(is.na(raw))[[1L]]]])[, 1L]
  }

  style_categorized(
    attribute,
    values,
    grDevices::col2rgb(value_colors),
    catch_all = catch_all,
    value_type = value_type
  )
}

# tm_scale_continuous(): the trained legend carries the limits; the ramp
# colors are sampled by re-running tmap's own scale machinery on
# synthetic data spanning the same limits (see qgs_tmap_ramp_stops), so
# any palette, values.range, midpoint or transformation comes out exactly
# as tmap computes it.
qgs_tmap_continuous_style <- function(leg, scale, attribute, aes, i,
                                      gradient_style) {
  limits <- leg$limits
  if (length(limits) != 2L || anyNA(limits) || limits[[2L]] <= limits[[1L]]) {
    stop(
      "layer ", i, ": cannot map `", attribute,
      "` to a color gradient (the scale's limits are degenerate)",
      call. = FALSE
    )
  }
  stops <- qgs_tmap_ramp_stops(scale, aes, limits, i)
  if (gradient_style == "continuous") {
    style_continuous(attribute, limits[[1L]], limits[[2L]], stops)
  } else {
    style_graduated(
      attribute,
      classes = QGS_GRADUATED_CLASSES,
      min = limits[[1L]],
      max = limits[[2L]],
      stops = stops
    )
  }
}

# Samples the trained continuous scale at evenly spaced values by running
# steps 1-2 on a synthetic layer: n points whose attribute is
# seq(limits[1], limits[2]) with the user's scale spec. Every data-driven
# part of the scale (limits, midpoint, transformation) is derived from
# the data range, and the synthetic data spans the same range, so the
# mapping is identical to the real layer's. tm_symbols() is used
# regardless of the original layer type: tmap resolves scale defaults per
# aesthetic and data class, not per layer (get_scale_defaults() is called
# with layer = NA).
#
# For a transformed scale (e.g. tm_scale_continuous_log()) the stops are
# a piecewise-linear approximation sampled evenly in data space.
qgs_tmap_ramp_stops <- function(scale, aes, limits, i) {
  values <- seq(limits[[1L]], limits[[2L]], length.out = QGS_GRADIENT_STOPS)
  synth <- sf::st_sf(
    qgs_value = values,
    geometry = sf::st_sfc(
      lapply(seq_along(values), function(k) sf::st_point(c(k, 0))),
      crs = 4326L
    )
  )
  args <- stats::setNames(
    list("qgs_value", scale),
    c(aes, paste0(aes, ".scale"))
  )
  x <- tmap::tm_shape(synth) + do.call(tmap::tm_symbols, args)
  built <- suppressMessages(qgs_tmap_build(x))

  md <- built$x3$tmo[[1L]]$layers[[1L]]$mapping_dt
  colors <- md[[aes]][match(seq_along(values), md$tmapID__)]
  if (anyNA(colors)) {
    stop(
      "layer ", i, ": failed to sample the continuous color scale",
      call. = FALSE
    )
  }
  list(
    offsets = seq(0, 1, length.out = QGS_GRADIENT_STOPS),
    colors = grDevices::col2rgb(colors)
  )
}

# The default name of every layer (its tm_shape's name, sanitized), with
# collisions suffixed; or the user-supplied override, validated like the
# ggplot2 path's `layer_names`.
qgs_tmap_layer_names <- function(specs, layer_names) {
  n <- length(specs)
  if (!is.null(layer_names)) {
    return(qgs_validate_layer_names(layer_names, n))
  }

  out <- character(n)
  for (i in seq_len(n)) {
    name <- qgs_sanitize_layer_name(specs[[i]]$name %||% "", i)
    out[[i]] <- qgs_uncollide_name(name, out[seq_len(i - 1L)])
  }
  out
}

# The tmap object's display CRS: tm_crs()/tm_shape(crs = ) if resolved,
# otherwise the CRS of the main shape. Only tmap's known "not resolved"
# sentinels fall back to the shape CRS; anything else must resolve, and a
# malformed value fails loudly instead of being guessed around.
qgs_tmap_crs <- function(built) {
  crs <- built$x3$o$crs_step4
  unresolved <- is.null(crs) ||
    (is.logical(crs) && all(is.na(crs))) ||
    identical(crs, "auto") ||
    (is.character(crs) && length(crs) == 1L && is.na(crs)) ||
    (is.numeric(crs) && length(crs) == 1L && (is.na(crs) || crs == 0))
  if (!unresolved) {
    crs <- sf::st_crs(crs)
    if (!is.na(crs)) {
      return(crs)
    }
  }
  main <- qgs_tmap_main_groups(built)
  sf::st_crs(built$x2$tmo[[main[[1L]]]]$tms$shpTM$shp)
}

# The groups whose bbox defines the map extent: tm_shape(is.main = TRUE)
# if set (possibly several), otherwise the first, following tmap.
qgs_tmap_main_groups <- function(built) {
  is_main <- vapply(
    built$x2$tmo,
    function(tmg) isTRUE(tmg$tms$is.main),
    logical(1L)
  )
  if (any(is_main)) which(is_main) else 1L
}

# The initial map-canvas extent: the union of the main shapes' bounding
# boxes (honoring tm_shape(bbox = ) when it holds one), reprojected to
# the project CRS.
qgs_tmap_extent <- function(built, project_crs) {
  extent <- NULL
  for (gi in qgs_tmap_main_groups(built)) {
    tms <- built$x2$tmo[[gi]]$tms
    bb <- tms$bbox
    if (!(is.numeric(bb) && length(bb) == 4L)) {
      bb <- sf::st_bbox(tms$shpTM$shp)
    }
    if (all(c("xmin", "ymin", "xmax", "ymax") %in% names(bb))) {
      bb <- bb[c("xmin", "ymin", "xmax", "ymax")]
    }
    bb <- as.numeric(bb)
    bb <- qgs_reproject_extent(bb, sf::st_crs(tms$shpTM$shp), project_crs)
    extent <- if (is.null(extent)) {
      bb
    } else {
      c(
        pmin(extent[1:2], bb[1:2]),
        pmax(extent[3:4], bb[3:4])
      )
    }
  }
  extent
}

# tm_basemap() elements -> XYZ tile layers, bottom-most first. Multiple
# basemaps are alternatives (a radio control in tmap's view mode): all
# are written, but only the first is checked.
# TODO: the basemap `alpha` argument is not carried over.
qgs_tmap_basemap_layers <- function(built) {
  aux <- Filter(
    function(a) inherits(a, "tm_basemap") && !isTRUE(a$args$disable),
    built$x2$aux
  )
  layers <- list()
  for (a in aux) {
    args <- a$args
    # `server` may name several alternative providers in one tm_basemap();
    # each becomes its own XYZ layer. NA (the default) resolves to tmap's
    # default provider.
    servers <- args$server[!is.na(args$server)]
    if (length(servers) == 0L) {
      servers <- built$x3$o$basemap.server[[1L]]
    }
    for (server in servers) {
      resolved <- qgs_tmap_resolve_basemap(server, args$sub)
      layers[[length(layers) + 1L]] <- xyz_tile_layer(
        resolved$name,
        resolved$url,
        0L,
        args$max.native.zoom,
        checked = length(layers) == 0L
      )
    }
  }
  layers
}

# Resolves a tm_basemap() server to an XYZ URL template QGIS understands:
# either a URL template as-is, or a provider name looked up in maptiles'
# provider table (maptiles is an Import of tmap, so it is present
# whenever tmap is). The {s} subdomain placeholder is substituted with
# the first subdomain and the leaflet retina placeholder {r} is dropped,
# since QGIS supports neither.
qgs_tmap_resolve_basemap <- function(server, sub) {
  # tm_basemap()'s `sub` is a compact string, one character per subdomain
  # ("abc" = a, b, c); normalize it to a vector of whole tokens up front,
  # since maptiles providers use a character vector whose tokens may be
  # longer than one character.
  sub <- sub[!is.na(sub)]
  if (length(sub) == 1L && nchar(sub) > 1L) {
    sub <- strsplit(sub, "", fixed = TRUE)[[1L]]
  }

  if (qgs_is_xyz_template(server)) {
    url <- server
    name <- "basemap"
  } else {
    provider <- maptiles::get_providers()[[server]]
    if (is.null(provider)) {
      stop("unknown basemap provider: ", server, call. = FALSE)
    }
    url <- provider$q
    name <- server
    if (!anyNA(provider$sub)) {
      sub <- provider$sub
    }
  }

  if (grepl("{s}", url, fixed = TRUE)) {
    if (length(sub) == 0L || !nzchar(sub[[1L]])) {
      stop(
        "the basemap URL has a {s} placeholder but no subdomains: ", url,
        call. = FALSE
      )
    }
    url <- gsub("{s}", sub[[1L]], url, fixed = TRUE)
  }
  url <- gsub("{r}", "", url, fixed = TRUE)

  list(name = qgs_sanitize_layer_name(name, 1L), url = url)
}
