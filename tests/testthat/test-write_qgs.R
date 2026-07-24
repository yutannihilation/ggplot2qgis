test_that("a continuous fill becomes a graduated style", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  expect_invisible(write_qgs(p, path))

  expect_true(file.exists(path))
  expect_true(file.exists(file.path(dir, "proj_data", "nc.gpkg")))

  out <- read_qgs(path)
  expect_match(out, "proj_data/nc.gpkg|layername=nc", fixed = TRUE)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="AREA"', fixed = TRUE)
  expect_match(out, 'geometry="Polygon"', fixed = TRUE)
  # nc.shp is in NAD27
  expect_match(out, "<authid>EPSG:4267</authid>", fixed = TRUE)

  # Fine-grained classes to approximate the continuous gradient.
  expect_length(
    regmatches(out, gregexpr("<range ", out, fixed = TRUE))[[1]],
    25L
  )

  # The label precision follows the class width, so narrow classes don't
  # collapse into duplicate labels like "0.1 - 0.1".
  expect_match(out, 'label="0.042 - 0.05"', fixed = TRUE)
  expect_match(out, 'labelprecision="3"', fixed = TRUE)

  # The terminal gradient stops are the colors of the trained scale limits.
  b <- ggplot2::ggplot_build(p)
  s <- b@plot@scales$get_scales("fill")
  ends <- grDevices::col2rgb(s$map(s$get_limits()))
  expect_match(out, paste(ends[, 1], collapse = ","), fixed = TRUE)
  expect_match(out, paste(ends[, 2], collapse = ","), fixed = TRUE)

  # The constant border matches ggplot2: grey35, linewidth 0.2 in mm.
  expect_match(
    out,
    '<Option name="outline_color" type="QString" value="89,89,89,255,rgb:',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="outline_width" type="QString" value="0.1505625"/>',
    fixed = TRUE
  )
})

test_that("the written gpkg keeps the raw data", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  dir <- local_out_dir()
  write_qgs(p, file.path(dir, "proj.qgs"))

  d <- sf::st_read(
    file.path(dir, "proj_data", "nc.gpkg"),
    layer = "nc",
    quiet = TRUE
  )
  expect_equal(nrow(d), nrow(nc))
  expect_equal(d$AREA, nc$AREA)
  expect_equal(d$NAME, nc$NAME)
})

test_that("gradient_style = 'continuous' interpolates the color per feature", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path, gradient_style = "continuous")

  out <- read_qgs(path)
  # A single-symbol renderer whose fill is a data-defined expression
  # interpolating the color from the attribute value.
  expect_match(out, 'type="singleSymbol"', fixed = TRUE)
  expect_no_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, '<Option name="fillColor" type="Map">', fixed = TRUE)
  expect_match(out, "ramp_color(create_ramp(map(", fixed = TRUE)
  expect_match(out, "&quot;AREA&quot;", fixed = TRUE)

  # The terminal gradient stops are the colors of the trained scale limits.
  b <- ggplot2::ggplot_build(p)
  s <- b@plot@scales$get_scales("fill")
  ends <- tolower(s$map(s$get_limits()))
  expect_match(out, paste0("0,'", ends[1L], "'"), fixed = TRUE)
  expect_match(out, paste0("1,'", ends[2L], "'"), fixed = TRUE)
})

test_that("an unknown gradient_style is an error", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  expect_error(
    write_qgs(p, tempfile(fileext = ".qgs"), gradient_style = "smooth"),
    "'arg' should be one of"
  )
})

test_that("a binned fill becomes one graduated class per bin", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA)) +
    ggplot2::scale_fill_steps()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="AREA"', fixed = TRUE)

  # One range per bin (breaks + 1), not the 25 gradient classes.
  b <- ggplot2::ggplot_build(p)
  s <- b@plot@scales$get_scales("fill")
  limits <- s$get_limits()
  breaks <- s$get_breaks()
  breaks <- breaks[!is.na(breaks) & breaks > limits[1] & breaks < limits[2]]
  n_bins <- length(breaks) + 1L
  expect_length(
    regmatches(out, gregexpr("<range ", out, fixed = TRUE))[[1]],
    n_bins
  )

  # The range boundaries are the scale's bin edges...
  boundaries <- c(limits[1], breaks, limits[2])
  for (bound in boundaries) {
    expect_match(out, sprintf('lower="%.15f"|upper="%.15f"', bound, bound))
  }
  # ...and each bin keeps the exact color ggplot2 assigned to it.
  mids <- (head(boundaries, -1) + boundaries[-1]) / 2
  bin_colors <- grDevices::col2rgb(s$map(mids))
  for (i in seq_len(n_bins)) {
    expect_match(
      out,
      paste0(paste(bin_colors[, i], collapse = ","), ",255,rgb:"),
      fixed = TRUE
    )
  }
})

test_that("custom binned breaks carry over verbatim", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA)) +
    ggplot2::scale_fill_steps(breaks = c(0.08, 0.12, 0.2))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  # Unequal bins with the exact break values as boundaries.
  expect_length(
    regmatches(out, gregexpr("<range ", out, fixed = TRUE))[[1]],
    4L
  )
  expect_match(out, 'lower="0.080000000000000"', fixed = TRUE)
  expect_match(out, 'upper="0.120000000000000"', fixed = TRUE)
  expect_match(out, 'lower="0.200000000000000"', fixed = TRUE)
  # Labels show the user-chosen break values exactly.
  expect_match(out, 'label="0.08 - 0.12"', fixed = TRUE)
})

test_that("a binned colour on polygons colors the borders", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(colour = AREA)) +
    ggplot2::scale_colour_steps()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  # The bin colors drive the outline; the interior keeps the constant
  # grey90 fill everywhere.
  b <- ggplot2::ggplot_build(p)
  s <- b@plot@scales$get_scales("colour")
  limits <- s$get_limits()
  low <- grDevices::col2rgb(s$map(limits[1] + 1e-9))[, 1]
  expect_match(
    out,
    paste0(
      '<Option name="outline_color" type="QString" value="',
      paste(low, collapse = ","), ",255,rgb:"
    ),
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="color" type="QString" value="229,229,229,255,rgb:',
    fixed = TRUE
  )
})

test_that("gradient_style = 'continuous' on a binned scale warns and is ignored", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA)) +
    ggplot2::scale_fill_steps()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  expect_warning(
    write_qgs(p, path, gradient_style = "continuous"),
    "layer 1: `gradient_style = \"continuous\"` does not apply"
  )

  out <- read_qgs(path)
  # Bins are exact: still a graduated renderer, no expression.
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_no_match(out, "ramp_color", fixed = TRUE)

  # The default gradient_style stays silent.
  expect_no_warning(write_qgs(p, file.path(dir, "proj2.qgs")))
})

test_that("the binned warning names only the binned layer in a mixed plot", {
  nc <- read_nc()
  # gradient_style = "continuous" legitimately applies to the continuous
  # fill of layer 1; only the binned colour of layer 2 warns.
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA)) +
    ggplot2::geom_sf(ggplot2::aes(colour = BIR74), fill = NA) +
    ggplot2::scale_colour_steps()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  expect_warning(
    write_qgs(p, path, gradient_style = "continuous"),
    "layer 2: `gradient_style = \"continuous\"` does not apply"
  )

  out <- read_qgs(path)
  # Layer 1 still gets the expression-based continuous rendering.
  expect_match(out, "ramp_color(create_ramp(map(", fixed = TRUE)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
})

test_that("a discrete fill becomes a categorized style", {
  nc <- read_nc()
  nc$side <- ifelse(seq_len(nrow(nc)) %% 2 == 0, "even", "odd")
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = side))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  expect_match(out, 'type="categorizedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="side"', fixed = TRUE)
  expect_match(out, 'value="even"', fixed = TRUE)
  expect_match(out, 'value="odd"', fixed = TRUE)
})

test_that("a colour mapping on polygons colors the borders", {
  nc <- read_nc()
  # color= is normalized to colour by aes()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(color = AREA))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="AREA"', fixed = TRUE)

  # The gradient goes to the outline; every interior is the constant
  # fill ggplot2 computed (grey90).
  b <- ggplot2::ggplot_build(p)
  s <- b@plot@scales$get_scales("colour")
  ends <- grDevices::col2rgb(s$map(s$get_limits()))
  expect_match(
    out,
    paste0(
      '<Option name="outline_color" type="QString" value="',
      paste(ends[, 1], collapse = ",")
    ),
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="color" type="QString" value="229,229,229,255,rgb:',
    fixed = TRUE
  )
})

test_that("a continuous colour on polygons targets the outline property", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(color = AREA))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path, gradient_style = "continuous")

  out <- read_qgs(path)
  expect_match(out, '<Option name="outlineColor" type="Map">', fixed = TRUE)
  expect_no_match(out, '<Option name="fillColor"', fixed = TRUE)
  expect_match(out, "ramp_color(create_ramp(map(", fixed = TRUE)
})

test_that("mapping both fill and colour is an error", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA, color = AREA))

  expect_error(
    write_qgs(p, tempfile(fileext = ".qgs")),
    "both `fill` and `colour`"
  )
})

test_that("a plot-level mapping is picked up too", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc, ggplot2::aes(fill = AREA)) +
    ggplot2::geom_sf()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="AREA"', fixed = TRUE)
})

test_that("no fill/colour mapping becomes a single style", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  expect_match(out, 'type="singleSymbol"', fixed = TRUE)
  expect_no_match(out, "ramp_color", fixed = TRUE)

  # geom_sf() constants: grey90 fill, grey35 border, linewidth 0.2 in mm.
  expect_match(
    out,
    '<Option name="color" type="QString" value="229,229,229,255,rgb:',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="outline_color" type="QString" value="89,89,89,255,rgb:',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="outline_width" type="QString" value="0.1505625"/>',
    fixed = TRUE
  )
})

test_that("each layer gets its own gpkg, bottom-most first", {
  nc <- read_nc()
  centers <- sf::st_centroid(sf::st_geometry(nc))
  points <- sf::st_sf(NAME = nc$NAME, geometry = centers)
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA)) +
    ggplot2::geom_sf(data = points)

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  expect_true(file.exists(file.path(dir, "proj_data", "nc.gpkg")))
  expect_true(file.exists(file.path(dir, "proj_data", "points.gpkg")))

  out <- read_qgs(path)
  expect_match(out, 'geometry="Polygon"', fixed = TRUE)
  expect_match(out, 'geometry="Point"', fixed = TRUE)
  # The QGIS layer tree lists the top-most layer first, so the points
  # (ggplot2's last layer) must come before the polygons.
  tree <- regmatches(out, regexpr("<layer-tree-group>.*</layer-tree-group>", out))
  expect_lt(
    regexpr('name="points"', tree, fixed = TRUE),
    regexpr('name="nc"', tree, fixed = TRUE)
  )
})

test_that("a layer whose data is passed as a symbol is named after it", {
  nc <- read_nc()
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = nc)

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  expect_true(file.exists(file.path(dir, "proj_data", "nc.gpkg")))
  expect_match(read_qgs(path), "|layername=nc", fixed = TRUE)
})

test_that("a ggplot2 layer name wins over the data variable", {
  nc <- read_nc()
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = nc, name = "counties")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  expect_true(file.exists(file.path(dir, "proj_data", "counties.gpkg")))
  expect_match(read_qgs(path), "|layername=counties", fixed = TRUE)
})

test_that("layer_names overrides every derived name", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA)) +
    ggplot2::geom_sf(data = nc, name = "counties")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path, layer_names = c("base", "top"))

  expect_true(file.exists(file.path(dir, "proj_data", "base.gpkg")))
  expect_true(file.exists(file.path(dir, "proj_data", "top.gpkg")))
  out <- read_qgs(path)
  expect_match(out, "|layername=base", fixed = TRUE)
  expect_match(out, "|layername=top", fixed = TRUE)
})

test_that("an invalid layer_names is an error", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  path <- tempfile(fileext = ".qgs")
  # One name per layer.
  expect_error(write_qgs(p, path, layer_names = c("a", "b")), "one name per layer")
  expect_error(write_qgs(p, path, layer_names = 1), "one name per layer")
  expect_error(write_qgs(p, path, layer_names = NA_character_), "NA or empty")
  expect_error(write_qgs(p, path, layer_names = ""), "NA or empty")
  expect_error(write_qgs(p, path, layer_names = "a|b"), "cannot contain")

  p2 <- p + ggplot2::geom_sf(data = nc)
  expect_error(
    write_qgs(p2, tempfile(fileext = ".qgs"), layer_names = c("a", "a")),
    "must be unique"
  )
})

test_that("a ggplot2 layer name with a forbidden character is an error", {
  nc <- read_nc()
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = nc, name = "a|b")

  expect_error(write_qgs(p, tempfile(fileext = ".qgs")), "cannot contain")
})

test_that("colliding derived names get a numbered suffix", {
  nc <- read_nc()
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = nc) +
    ggplot2::geom_sf(data = nc)

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  expect_true(file.exists(file.path(dir, "proj_data", "nc.gpkg")))
  expect_true(file.exists(file.path(dir, "proj_data", "nc_2.gpkg")))
  expect_match(read_qgs(path), "|layername=nc_2", fixed = TRUE)
})

test_that("inline data falls back to the geom name", {
  nc <- read_nc()
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = sf::st_sf(geometry = sf::st_centroid(sf::st_geometry(nc)))
    )

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  expect_true(file.exists(file.path(dir, "proj_data", "geom_sf.gpkg")))
  expect_match(read_qgs(path), "|layername=geom_sf", fixed = TRUE)
})

project_crs_block <- function(out) {
  start <- regexpr("<projectCrs>", out, fixed = TRUE)
  end <- regexpr("</projectCrs>", out, fixed = TRUE)
  substr(out, start, end)
}

# The <mapcanvas> initial view: the first <extent>...</extent> block in the
# document, returned as c(xmin, ymin, xmax, ymax).
canvas_extent <- function(out) {
  block <- regmatches(out, regexpr("(?s)<extent>.*?</extent>", out, perl = TRUE))
  get <- function(tag) {
    as.numeric(sub(
      paste0("(?s).*<", tag, ">([^<]*)</", tag, ">.*"), "\\1", block,
      perl = TRUE
    ))
  }
  c(xmin = get("xmin"), ymin = get("ymin"), xmax = get("xmax"), ymax = get("ymax"))
}

default_view_extent <- function(out) {
  attrs <- regmatches(out, regexpr("<DefaultViewExtent [^>]*>", out))
  get <- function(name) {
    as.numeric(sub(paste0('.*', name, '="([^"]*)".*'), "\\1", attrs))
  }
  c(xmin = get("xmin"), ymin = get("ymin"), xmax = get("xmax"), ymax = get("ymax"))
}

# The world extent the template ships with (EPSG:3857 bounds).
WORLD_EXTENT_XMIN <- -20037508.34278924

test_that("the map canvas zooms to the plot extent, reprojected to the project CRS", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  ext <- canvas_extent(read_qgs(path))
  expect_true(all(is.finite(ext)))
  # Not the template's whole-world default.
  expect_gt(ext[["xmin"]], WORLD_EXTENT_XMIN + 1)

  # The panel range (in the layer CRS, EPSG:4267) reprojected to the
  # project CRS (EPSG:3857).
  pp <- ggplot2::ggplot_build(p)@layout$panel_params[[1]]
  bb <- sf::st_bbox(
    c(xmin = pp$x_range[1], ymin = pp$y_range[1],
      xmax = pp$x_range[2], ymax = pp$y_range[2]),
    crs = sf::st_crs(pp$crs)
  )
  expected <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bb), 3857))
  expect_equal(
    unname(ext),
    unname(expected[c("xmin", "ymin", "xmax", "ymax")]),
    tolerance = 1e-3
  )
})

test_that("the default view extent is zoomed too", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  expect_equal(canvas_extent(out), default_view_extent(out))
  expect_gt(default_view_extent(out)[["xmin"]], WORLD_EXTENT_XMIN + 1)
})

test_that("with use_plot_crs the canvas extent is the panel range as-is", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path, use_plot_crs = TRUE)

  ext <- canvas_extent(read_qgs(path))
  pp <- ggplot2::ggplot_build(p)@layout$panel_params[[1]]
  expect_equal(
    unname(ext),
    c(pp$x_range[1], pp$y_range[1], pp$x_range[2], pp$y_range[2]),
    tolerance = 1e-6
  )
})

test_that("coord_sf() xlim/ylim narrow the canvas extent", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA)) +
    ggplot2::coord_sf(xlim = c(-80, -78), ylim = c(35, 36))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path, use_plot_crs = TRUE)

  ext <- canvas_extent(read_qgs(path))
  # Expansion widens the range slightly, so compare loosely to the request.
  expect_gt(ext[["xmin"]], -80.5)
  expect_lt(ext[["xmin"]], -79.5)
  expect_gt(ext[["xmax"]], -78.5)
  expect_lt(ext[["xmax"]], -77.5)
})

test_that("the project CRS defaults to EPSG:3857", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA)) +
    # coord_sf() does not affect the project CRS
    ggplot2::coord_sf(crs = 4326)

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  block <- project_crs_block(read_qgs(path))
  expect_match(block, "<authid>EPSG:3857</authid>", fixed = TRUE)
  # The layer itself keeps the CRS of its data.
  expect_match(read_qgs(path), "<authid>EPSG:4267</authid>", fixed = TRUE)
})

test_that("use_plot_crs = TRUE takes the CRS of the first layer", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path, use_plot_crs = TRUE)

  block <- project_crs_block(read_qgs(path))
  expect_match(block, "<authid>EPSG:4267</authid>", fixed = TRUE)
})

test_that("use_plot_crs = TRUE respects the crs argument of coord_sf()", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA)) +
    ggplot2::coord_sf(crs = 4326)

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path, use_plot_crs = TRUE)

  block <- project_crs_block(read_qgs(path))
  expect_match(block, "<authid>EPSG:4326</authid>", fixed = TRUE)
  # The layer itself keeps its own CRS; QGIS reprojects on the fly.
  expect_match(read_qgs(path), "<authid>EPSG:4267</authid>", fixed = TRUE)
})

test_that("a non-logical use_plot_crs is an error", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  path <- tempfile(fileext = ".qgs")
  expect_error(write_qgs(p, path, use_plot_crs = NA), "TRUE or FALSE")
  expect_error(write_qgs(p, path, use_plot_crs = "yes"), "TRUE or FALSE")
})

test_that("writing to an existing path is an error unless overwrite = TRUE", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  expect_error(write_qgs(p, path), "already exists")
  expect_invisible(write_qgs(p, path, overwrite = TRUE))
})

test_that("a non-logical overwrite is an error", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  path <- tempfile(fileext = ".qgs")
  expect_error(write_qgs(p, path, overwrite = NA), "TRUE or FALSE")
  expect_error(write_qgs(p, path, overwrite = "yes"), "TRUE or FALSE")
})

test_that("a tilde in the output path is expanded", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  # On Windows, path.expand() resolves "~" to the personal folder determined
  # at R startup, so it cannot be redirected by setting HOME at runtime.
  skip_on_os("windows")

  dir <- local_out_dir()
  withr::local_envvar(HOME = dir)

  write_qgs(p, "~/proj.qgs")

  expect_true(file.exists(file.path(dir, "proj.qgs")))
  expect_true(file.exists(file.path(dir, "proj_data", "nc.gpkg")))
})

test_that("non-data-frame data is an error", {
  # A layer cannot be constructed with non-fortifiable data, so force it in.
  p <- ggplot2::ggplot(mtcars) +
    ggplot2::geom_point(ggplot2::aes(wt, mpg))
  assign("data", as.matrix(mtcars), envir = p@layers[[1]])

  dir <- local_out_dir()
  expect_error(
    write_qgs(p, file.path(dir, "proj.qgs")),
    "must be an sf object or a data.frame"
  )
})

test_that("a non-symbol aesthetic is an error", {
  nc <- read_nc()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")

  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA * 2))
  expect_error(write_qgs(p, path), "only a bare column name")

  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = 1))
  expect_error(write_qgs(p, path), "only a bare column name")
})

test_that("a layer backed by empty sf data is an error", {
  nc <- read_nc()
  empty <- nc[0, ]
  p <- ggplot2::ggplot(empty) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  dir <- local_out_dir()
  expect_error(
    write_qgs(p, file.path(dir, "proj.qgs")),
    "layer 1: the data has no rows"
  )
})

test_that("a plot without layers is an error", {
  nc <- read_nc()
  expect_error(
    write_qgs(ggplot2::ggplot(nc), tempfile(fileext = ".qgs")),
    "at least one layer"
  )
})

test_that("qgs_reproject_extent clips to the target CRS's valid area", {
  # A whole-world extent (latitudes to the poles) must not blow up in Web
  # Mercator (valid to about +-85.06 degrees).
  out <- qgs_reproject_extent(
    c(-180, -90, 180, 90),
    sf::st_crs(4326),
    sf::st_crs(3857)
  )
  expect_true(all(abs(out) < 2.1e7))
  expect_lt(out[2], out[4])

  # Same-CRS extents pass through unchanged, even beyond the area of use.
  expect_equal(
    qgs_reproject_extent(
      c(-180, -90, 180, 90),
      sf::st_crs(4326),
      sf::st_crs(4326)
    ),
    c(-180, -90, 180, 90)
  )
})

test_that("a non-ggplot object is an error", {
  expect_error(
    write_qgs(1, tempfile(fileext = ".qgs")),
    "must be a ggplot or tmap object"
  )
})

test_that("no basemap is added by default", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)

  expect_no_match(read_qgs(path), "type=xyz", fixed = TRUE)
})

test_that("a predefined basemap adds an XYZ layer below the vector layers", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path, basemap = "osm")

  out <- read_qgs(path)
  expect_match(out, "type=xyz", fixed = TRUE)
  expect_match(
    out,
    "url=https%3A%2F%2Ftile.openstreetmap.org%2F%7Bz%7D%2F%7Bx%7D%2F%7By%7D.png",
    fixed = TRUE
  )
  expect_match(out, "&amp;zmax=19&amp;zmin=0", fixed = TRUE)
  expect_match(out, 'name="OpenStreetMap"', fixed = TRUE)
  # The basemap is a raster layer; the vector layer stays.
  expect_match(out, 'type="raster"', fixed = TRUE)
  expect_match(out, 'geometry="Polygon"', fixed = TRUE)

  # The layer tree lists the top-most layer first, so the vector layer must
  # come before the basemap (which draws at the bottom).
  tree <- regmatches(
    out, regexpr("<layer-tree-group>.*</layer-tree-group>", out)
  )
  expect_lt(
    regexpr('name="nc"', tree, fixed = TRUE),
    regexpr('name="OpenStreetMap"', tree, fixed = TRUE)
  )
})

test_that("the GSI basemaps carry their Japanese names and URLs", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  dir <- local_out_dir()

  path1 <- file.path(dir, "std.qgs")
  write_qgs(p, path1, basemap = "gsi_standard")
  out1 <- read_qgs(path1)
  expect_match(out1, "cyberjapandata.gsi.go.jp%2Fxyz%2Fstd", fixed = TRUE)
  expect_match(out1, "&amp;zmax=18&amp;zmin=0", fixed = TRUE)
  expect_match(out1, "地理院タイル（標準地図）", fixed = TRUE)

  path2 <- file.path(dir, "pale.qgs")
  write_qgs(p, path2, basemap = "gsi_pale")
  out2 <- read_qgs(path2)
  expect_match(out2, "cyberjapandata.gsi.go.jp%2Fxyz%2Fpale", fixed = TRUE)
  expect_match(out2, "地理院タイル（淡色地図）", fixed = TRUE)
})

test_that("an arbitrary XYZ URL is accepted as a basemap", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path, basemap = "https://example.com/tiles/{z}/{x}/{y}.png")

  out <- read_qgs(path)
  expect_match(out, "type=xyz", fixed = TRUE)
  expect_match(
    out,
    "url=https%3A%2F%2Fexample.com%2Ftiles%2F%7Bz%7D%2F%7Bx%7D%2F%7By%7D.png",
    fixed = TRUE
  )
  expect_match(out, 'name="basemap"', fixed = TRUE)
})

test_that("an invalid basemap is an error", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  path <- tempfile(fileext = ".qgs")
  # An unknown key (no placeholders) lists the valid keys.
  expect_error(write_qgs(p, path, basemap = "gsi"), "must be one of")
  # A string missing the {z}/{x}/{y} placeholders.
  expect_error(
    write_qgs(p, path, basemap = "https://example.com/tiles.png"),
    "must be one of"
  )
  # Not a single string.
  expect_error(write_qgs(p, path, basemap = c("osm", "osm")), "single string")
  expect_error(write_qgs(p, path, basemap = NA_character_), "single string")
  expect_error(write_qgs(p, path, basemap = 1), "single string")
})
