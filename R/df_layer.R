# data.frame layers: geom_point / geom_path / geom_line / geom_polygon
# drawn from a plain data.frame, converted to sf so the rest of the
# pipeline (GeoPackage + style) treats them like a geom_sf layer.
#
# The raw data is what gets written to the GeoPackage, so the x/y (and
# fill/colour) aesthetics must be bare column names, and the stat and
# position must be identity — anything that would make the drawn
# coordinates differ from the raw columns is an error, never silently
# reinterpreted. The built plot is used for two things only: grouping
# (built@data[[i]]$group is ggplot2's authoritative grouping, including
# the default grouping derived from discrete aesthetics) and verifying
# that the plotted x/y really are the raw columns.
#
# CRS: a data.frame has no CRS, so the plot must use coord_sf(), and the
# x/y values are taken to be coordinates in the panel CRS — coord_sf's
# `crs` argument if given, otherwise the CRS of the first sf layer
# (exactly what the built panel carries). coord_sf(default_crs =) would
# make ggplot2 interpret the x/y in yet another CRS; that is not
# supported, and rejecting it keeps the QGIS project from diverging from
# the plot.

# The supported geoms, by exact class: GeomStep also inherits GeomPath
# but is not a plain path, so dispatch must not use inherits().
QGS_DF_GEOMS <- c("GeomPoint", "GeomPath", "GeomLine", "GeomPolygon")

# Converts one data.frame layer to sf, or errors when the layer cannot be
# represented faithfully. `d` is the layer's raw data (a data.frame).
qgs_df_layer_sf <- function(plot, built, layer, i, d) {
  geom_class <- class(layer$geom)[[1L]]
  if (!geom_class %in% QGS_DF_GEOMS) {
    stop(
      "layer ", i, ": unsupported geom for data.frame data (", geom_class,
      "); only geom_point(), geom_path(), geom_line() and geom_polygon() ",
      "are supported (or use sf data with geom_sf())",
      call. = FALSE
    )
  }
  if (!inherits(layer$stat, "StatIdentity")) {
    stop(
      "layer ", i, ": only the identity stat is supported for data.frame ",
      "data, got ", class(layer$stat)[[1L]],
      call. = FALSE
    )
  }
  if (!inherits(layer$position, "PositionIdentity")) {
    stop(
      "layer ", i, ": only the identity position is supported for ",
      "data.frame data, got ", class(layer$position)[[1L]],
      call. = FALSE
    )
  }
  if (!is.null(layer$mapping[["subgroup"]])) {
    stop(
      "layer ", i, ": the `subgroup` aesthetic (polygon holes) is not ",
      "supported",
      call. = FALSE
    )
  }

  crs <- qgs_df_layer_crs(plot, built, i)
  xy <- qgs_df_xy(plot, layer, d, i)
  computed <- qgs_df_aligned(built@data[[i]], d, xy, i)
  if (isTRUE(any(computed$flipped_aes))) {
    stop(
      "layer ", i, ": a flipped orientation (`orientation = \"y\"`) is not ",
      "supported",
      call. = FALSE
    )
  }

  if (geom_class == "GeomPoint") {
    return(qgs_df_points_sf(d, xy, crs))
  }

  # One feature per group *within a panel*, so a faceted plot does not
  # connect rows across panels. First-appearance order keeps the features
  # in the order the user's data introduces the groups.
  key <- paste(computed$PANEL, computed$group)
  key <- factor(key, levels = unique(key))
  attribute <- qgs_style_attribute(plot, layer, i, d)$attribute

  switch(geom_class,
    GeomPath = qgs_df_lines_sf(d, xy, key, crs, i, FALSE, attribute),
    GeomLine = qgs_df_lines_sf(d, xy, key, crs, i, TRUE, attribute),
    GeomPolygon = qgs_df_polygons_sf(d, xy, key, crs, i, attribute)
  )
}

# The CRS the layer's x/y values are in (see the header comment).
qgs_df_layer_crs <- function(plot, built, i) {
  coord <- plot@coordinates
  if (!inherits(coord, "CoordSf")) {
    stop(
      "layer ", i, ": a data.frame layer requires coord_sf(); add an sf ",
      "layer or `+ coord_sf(crs = )` to the plot",
      call. = FALSE
    )
  }
  if (!is.null(coord$default_crs)) {
    stop(
      "layer ", i, ": coord_sf(default_crs = ) is not supported for ",
      "data.frame layers",
      call. = FALSE
    )
  }
  crs <- built@layout$panel_params[[1L]]$crs
  if (is.null(crs) || is.na(sf::st_crs(crs))) {
    stop(
      "layer ", i, ": cannot determine the CRS of the x/y coordinates; ",
      "add an sf layer or set coord_sf(crs = )",
      call. = FALSE
    )
  }
  sf::st_crs(crs)
}

# Resolves the x and y aesthetics to column names: c(x = <col>, y = <col>).
# Like fill/colour, only a bare column name is supported, and the column
# must be numeric without NAs (an NA coordinate would silently split or
# skip features when drawn, which a GIS layer cannot reproduce).
qgs_df_xy <- function(plot, layer, d, i) {
  resolve <- function(aes_name) {
    quo <- layer$mapping[[aes_name]] %||% plot@mapping[[aes_name]]
    if (is.null(quo)) {
      stop(
        "layer ", i, ": a data.frame layer needs an `", aes_name,
        "` aesthetic",
        call. = FALSE
      )
    }
    if (!(rlang::is_quosure(quo) && rlang::quo_is_symbol(quo))) {
      stop(
        "layer ", i, ": only a bare column name is supported for `",
        aes_name, "`, got `", rlang::as_label(quo), "`",
        call. = FALSE
      )
    }
    column <- rlang::as_string(rlang::quo_get_expr(quo))
    if (!column %in% names(d)) {
      stop(
        "layer ", i, ": column `", column, "` not found in the layer data",
        call. = FALSE
      )
    }
    v <- d[[column]]
    if (!is.numeric(v)) {
      stop(
        "layer ", i, ": column `", column, "` (`", aes_name,
        "`) must be numeric",
        call. = FALSE
      )
    }
    if (anyNA(v)) {
      stop(
        "layer ", i, ": column `", column, "` (`", aes_name,
        "`) must not contain NA",
        call. = FALSE
      )
    }
    column
  }
  c(x = resolve("x"), y = resolve("y"))
}

# The layer's built data reordered to the raw row order, verified hard.
# ggplot_build() keeps the raw row indices as row names (and GeomLine is
# the only supported geom that reorders rows, by x within group); if any
# of that ever changes, or a scale transformed the coordinates (e.g.
# scale_x_log10()), this errors instead of producing a wrong layer.
qgs_df_aligned <- function(computed, d, xy, i) {
  n <- nrow(d)
  if (nrow(computed) != n) {
    stop(
      "layer ", i, ": the plotted data does not have one row per data row; ",
      "cannot map the layer back to the raw data",
      call. = FALSE
    )
  }
  idx <- suppressWarnings(as.integer(rownames(computed)))
  if (anyNA(idx) || !setequal(idx, seq_len(n))) {
    stop(
      "layer ", i, ": the plotted data cannot be mapped back to the raw ",
      "data rows",
      call. = FALSE
    )
  }
  computed <- computed[order(idx), , drop = FALSE]
  same <- function(plotted, raw) {
    isTRUE(all.equal(as.numeric(plotted), as.numeric(raw)))
  }
  if (!same(computed$x, d[[xy[["x"]]]]) || !same(computed$y, d[[xy[["y"]]]])) {
    stop(
      "layer ", i, ": the plotted x/y don't match the data (a transformed ",
      "scale like scale_x_log10()?); only untransformed scales are supported",
      call. = FALSE
    )
  }
  computed
}

# geom_point: one POINT per row, all raw columns kept as attributes
# (including the coordinate columns).
qgs_df_points_sf <- function(d, xy, crs) {
  sf::st_as_sf(d, coords = unname(xy), crs = crs, remove = FALSE)
}

# geom_path / geom_line: one LINESTRING per group, vertices in data order
# (`sort_by_x = TRUE` reproduces geom_line's sort by x within each group;
# order() is stable, like GeomLine's own sort, so ties keep data order).
qgs_df_lines_sf <- function(d, xy, key, crs, i, sort_by_x, attribute) {
  x <- d[[xy[["x"]]]]
  y <- d[[xy[["y"]]]]
  rows_by_group <- split(seq_len(nrow(d)), key)
  geoms <- lapply(rows_by_group, function(rows) {
    if (length(rows) < 2L) {
      stop(
        "layer ", i, ": a line needs at least 2 points per group",
        call. = FALSE
      )
    }
    if (sort_by_x) {
      rows <- rows[order(x[rows])]
    }
    sf::st_linestring(cbind(x[rows], y[rows]))
  })
  qgs_df_build_sf(d, rows_by_group, geoms, crs, i, attribute)
}

# geom_polygon: one POLYGON per group; the ring is closed by repeating the
# first vertex when the data doesn't close it itself.
qgs_df_polygons_sf <- function(d, xy, key, crs, i, attribute) {
  x <- d[[xy[["x"]]]]
  y <- d[[xy[["y"]]]]
  rows_by_group <- split(seq_len(nrow(d)), key)
  geoms <- lapply(rows_by_group, function(rows) {
    if (length(rows) < 3L) {
      stop(
        "layer ", i, ": a polygon needs at least 3 points per group",
        call. = FALSE
      )
    }
    ring <- cbind(x[rows], y[rows])
    if (any(ring[1L, ] != ring[nrow(ring), ])) {
      ring <- rbind(ring, ring[1L, ])
    }
    sf::st_polygon(list(ring))
  })
  qgs_df_build_sf(d, rows_by_group, geoms, crs, i, attribute)
}

# Assembles the per-group features: one row of attributes per group (see
# qgs_df_group_data()) plus the geometries.
qgs_df_build_sf <- function(d, rows_by_group, geoms, crs, i, attribute) {
  attrs <- qgs_df_group_data(d, rows_by_group, i, attribute)
  sf::st_sf(attrs, geometry = sf::st_sfc(geoms, crs = crs))
}

# The attribute table of a grouped layer: the atomic columns that are
# constant within every group, one row (the group's first) per group. The
# coordinate columns vary within a group, so they drop out naturally. The
# styled fill/colour column (`attribute`, may be NULL) must survive —
# the renderer references it by name.
qgs_df_group_data <- function(d, rows_by_group, i, attribute) {
  constant <- vapply(names(d), function(col) {
    v <- d[[col]]
    is.atomic(v) &&
      all(vapply(
        rows_by_group,
        function(rows) length(unique(v[rows])) == 1L,
        logical(1L)
      ))
  }, logical(1L))
  if (!is.null(attribute) && !constant[[attribute]]) {
    stop(
      "layer ", i, ": `", attribute, "` must be constant within each group ",
      "to be mapped to `fill`/`colour`",
      call. = FALSE
    )
  }
  first_rows <- vapply(rows_by_group, `[[`, integer(1L), 1L)
  attrs <- d[first_rows, constant, drop = FALSE]
  rownames(attrs) <- NULL
  attrs
}
