read_nc <- function() {
  sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
}

local_out_dir <- function(env = parent.frame()) {
  dir <- tempfile("write_qgs_test")
  dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE), envir = env)
  dir
}

read_qgs <- function(path) {
  readChar(path, file.size(path), useBytes = TRUE)
}

test_that("a continuous fill becomes a graduated style", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  expect_invisible(write_qgs(p, path))

  expect_true(file.exists(path))
  expect_true(file.exists(file.path(dir, "proj_data", "layer1.gpkg")))

  out <- read_qgs(path)
  expect_match(out, "proj_data/layer1.gpkg|layername=layer1", fixed = TRUE)
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
    file.path(dir, "proj_data", "layer1.gpkg"),
    layer = "layer1",
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

  expect_true(file.exists(file.path(dir, "proj_data", "layer1.gpkg")))
  expect_true(file.exists(file.path(dir, "proj_data", "layer2.gpkg")))

  out <- read_qgs(path)
  expect_match(out, 'geometry="Polygon"', fixed = TRUE)
  expect_match(out, 'geometry="Point"', fixed = TRUE)
  # The QGIS layer tree lists the top-most layer first, so the points
  # (ggplot2's last layer) must come before the polygons.
  tree <- regmatches(out, regexpr("<layer-tree-group>.*</layer-tree-group>", out))
  expect_lt(
    regexpr('name="layer2"', tree, fixed = TRUE),
    regexpr('name="layer1"', tree, fixed = TRUE)
  )
})

project_crs_block <- function(out) {
  start <- regexpr("<projectCrs>", out, fixed = TRUE)
  end <- regexpr("</projectCrs>", out, fixed = TRUE)
  substr(out, start, end)
}

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

  dir <- local_out_dir()
  withr::local_envvar(HOME = dir)

  write_qgs(p, "~/proj.qgs")

  expect_true(file.exists(file.path(dir, "proj.qgs")))
  expect_true(file.exists(file.path(dir, "proj_data", "layer1.gpkg")))
})

test_that("non-sf data is an error", {
  p <- ggplot2::ggplot(mtcars) +
    ggplot2::geom_point(ggplot2::aes(wt, mpg))

  dir <- local_out_dir()
  expect_error(
    write_qgs(p, file.path(dir, "proj.qgs")),
    "only sf data is supported"
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

test_that("a non-ggplot object is an error", {
  expect_error(write_qgs(1, tempfile(fileext = ".qgs")), "must be a ggplot")
})
