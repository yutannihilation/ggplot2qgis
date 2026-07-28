# write_qgs() for tmap objects whose shape is a raster (tm_raster()).
# Like test-tmap.R, the tests go through the public write_qgs() surface
# and assert on the written .qgs XML plus the GeoTIFF on disk.

skip_if_no_tmap_raster <- function() {
  testthat::skip_if_not_installed("tmap", "4.4")
  testthat::skip_if_not_installed("terra")
}

# tmap emits session tips and scale advice while building.
write_qgs_quiet <- function(...) {
  suppressMessages(write_qgs(...))
}

# A tiny north-up EPSG:4326 raster. `values` fills it in terra's cell
# order (row-major from the top-left), the same order tmap keys by.
test_raster <- function(values, nrows = 4L, ncols = 5L, name = "v") {
  r <- terra::rast(
    nrows = nrows, ncols = ncols,
    xmin = 0, xmax = ncols, ymin = 0, ymax = nrows,
    crs = "EPSG:4326"
  )
  terra::values(r) <- values
  names(r) <- name
  r
}

# A categorical raster with three levels.
test_cat_raster <- function(values, labels = c("forest", "water", "urban")) {
  r <- test_raster(values, nrows = 2L, ncols = 3L, name = "cls")
  levels(r) <- data.frame(id = seq_along(labels), cls = labels)
  r
}

test_that("a tmap intervals scale on a raster becomes a DISCRETE shader", {
  skip_if_no_tmap_raster()

  r <- test_raster(c(1:19, NA))
  x <- tmap::tm_shape(r) + tmap::tm_raster()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  expect_invisible(write_qgs_quiet(x, path))

  tif <- file.path(dir, "proj_data", "r.tif")
  expect_true(file.exists(tif))

  out <- read_qgs(path)
  expect_match(out, 'layerType="Raster"', fixed = TRUE)
  expect_match(out, "proj_data/r.tif", fixed = TRUE)
  expect_match(out, 'type="singlebandpseudocolor"', fixed = TRUE)
  expect_match(out, 'colorRampType="DISCRETE"', fixed = TRUE)
  expect_match(out, '<noDataList bandNo="1" useSrcNoData="1"/>', fixed = TRUE)
  expect_match(out, "<authid>EPSG:4326</authid>", fixed = TRUE)

  # tmap's bins are 0-4, 5-9, 10-14, 15-20; QGIS <item>s carry the
  # class upper bound, the last one open-ended, and tmap's own labels.
  items <- regmatches(out, gregexpr("<item [^/]*/>", out))[[1L]]
  expect_length(items, 4L)
  expect_match(items[[1L]], 'label="0 - 4" value="5"', fixed = TRUE)
  expect_match(items[[3L]], 'label="10 - 14" value="15"', fixed = TRUE)
  expect_match(items[[4L]], 'label="15 - 20" value="inf"', fixed = TRUE)
  expect_match(out, 'classificationMin="0"', fixed = TRUE)
  expect_match(out, 'classificationMax="20"', fixed = TRUE)
})

test_that("the written GeoTIFF is the grid tmap draws", {
  skip_if_no_tmap_raster()

  values <- c(1:19, NA)
  r <- test_raster(values)
  x <- tmap::tm_shape(r) + tmap::tm_raster()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  back <- terra::rast(file.path(dir, "proj_data", "r.tif"))
  expect_equal(as.vector(terra::values(back)), values)
  expect_equal(unname(as.vector(terra::ext(back))), c(0, 5, 0, 4))
  expect_equal(terra::nrow(back), 4L)
  expect_equal(terra::ncol(back), 5L)
  expect_true(sf::st_crs(terra::crs(back)) == sf::st_crs(4326L))
})

test_that("a tmap continuous raster scale becomes an INTERPOLATED shader", {
  skip_if_no_tmap_raster()

  r <- test_raster(c(1:19, NA))
  x <- tmap::tm_shape(r) +
    tmap::tm_raster(col.scale = tmap::tm_scale_continuous())

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'colorRampType="INTERPOLATED"', fixed = TRUE)
  items <- regmatches(out, gregexpr("<item [^/]*/>", out))[[1L]]
  expect_length(items, 21L)
  expect_match(items[[1L]], 'value="1"', fixed = TRUE)
  expect_match(items[[21L]], 'value="19"', fixed = TRUE)
  expect_match(out, 'classificationMin="1"', fixed = TRUE)
  expect_match(out, 'classificationMax="19"', fixed = TRUE)
})

test_that("a categorical raster becomes a paletted renderer with labels", {
  skip_if_no_tmap_raster()

  r <- test_cat_raster(c(1, 2, 3, 1, 2, NA))
  x <- tmap::tm_shape(r) + tmap::tm_raster()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'type="paletted"', fixed = TRUE)
  entries <- regmatches(out, gregexpr("<paletteEntry [^/]*/>", out))[[1L]]
  expect_length(entries, 3L)
  # The band holds the factor codes; the levels supply the legend labels.
  expect_match(entries[[1L]], 'label="forest" value="1"', fixed = TRUE)
  expect_match(entries[[2L]], 'label="water" value="2"', fixed = TRUE)
  expect_match(entries[[3L]], 'label="urban" value="3"', fixed = TRUE)

  back <- terra::rast(file.path(dir, "proj_data", "r.tif"))
  expect_equal(as.vector(terra::values(back)), c(1, 2, 3, 1, 2, NA))
})

test_that("a terra color table survives tmap's level mangling", {
  skip_if_no_tmap_raster()

  r <- test_cat_raster(c(1, 2, 3, 1, 2, NA))
  terra::coltab(r) <- data.frame(
    value = 1:3,
    red = c(0L, 0L, 200L),
    green = c(120L, 60L, 60L),
    blue = c(0L, 200L, 60L),
    alpha = 255L
  )

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(tmap::tm_shape(r) + tmap::tm_raster(), path)

  out <- read_qgs(path)
  entries <- regmatches(out, gregexpr("<paletteEntry [^/]*/>", out))[[1L]]
  # tmap encodes the color table into the factor levels as
  # "<label>=<>=<color>"; the labels must come out clean and the colors
  # must be the table's.
  expect_match(entries[[1L]], 'color="#007800" label="forest"', fixed = TRUE)
  expect_match(entries[[2L]], 'color="#003cc8" label="water"', fixed = TRUE)
  expect_match(entries[[3L]], 'color="#c83c3c" label="urban"', fixed = TRUE)
  expect_false(grepl("=<>=", out, fixed = TRUE))
})

test_that("levels with no cell are dropped from the palette", {
  skip_if_no_tmap_raster()

  # Only levels 1 and 3 occur; level 2 would never render.
  r <- test_cat_raster(c(1, 3, 1, 3, 1, NA))
  x <- tmap::tm_shape(r) + tmap::tm_raster()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  entries <- regmatches(out, gregexpr("<paletteEntry [^/]*/>", out))[[1L]]
  expect_length(entries, 2L)
  expect_match(entries[[1L]], 'label="forest" value="1"', fixed = TRUE)
  expect_match(entries[[2L]], 'label="urban" value="3"', fixed = TRUE)
})

test_that("a categorical scale on a numeric raster labels with the value", {
  skip_if_no_tmap_raster()

  # tmap picks a categorical scale for a numeric variable with a single
  # distinct value.
  r <- test_raster(c(rep(7, 19), NA))
  x <- tmap::tm_shape(r) + tmap::tm_raster()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'type="paletted"', fixed = TRUE)
  expect_match(out, 'label="7" value="7"', fixed = TRUE)
})

test_that("missing cells use the color tmap paints them in", {
  skip_if_no_tmap_raster()

  dir <- local_out_dir()

  # By default tmap leaves them transparent, which is also QGIS's
  # default for nodata cells.
  r <- test_raster(c(1:19, NA))
  default_path <- file.path(dir, "default.qgs")
  write_qgs_quiet(tmap::tm_shape(r) + tmap::tm_raster(), default_path)
  expect_match(read_qgs(default_path), 'nodataColor=""', fixed = TRUE)

  # An explicit value.na becomes the renderer's nodataColor.
  na_path <- file.path(dir, "na.qgs")
  write_qgs_quiet(
    tmap::tm_shape(r) +
      tmap::tm_raster(col.scale = tmap::tm_scale_intervals(value.na = "red")),
    na_path
  )
  expect_match(
    read_qgs(na_path),
    'nodataColor="255,0,0,255,rgb:1,0,0,1"',
    fixed = TRUE
  )

  # A raster with no missing cells has nothing to color.
  full <- test_raster(1:20)
  full_path <- file.path(dir, "full.qgs")
  write_qgs_quiet(tmap::tm_shape(full) + tmap::tm_raster(), full_path)
  expect_match(read_qgs(full_path), 'nodataColor=""', fixed = TRUE)
})

test_that("a constant col_alpha becomes the layer opacity", {
  skip_if_no_tmap_raster()

  r <- test_raster(c(1:19, NA))
  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(
    tmap::tm_shape(r) + tmap::tm_raster(col_alpha = 0.5),
    path
  )
  expect_match(read_qgs(path), 'opacity="0.5"', fixed = TRUE)

  default_path <- file.path(dir, "default.qgs")
  write_qgs_quiet(tmap::tm_shape(r) + tmap::tm_raster(), default_path)
  expect_match(read_qgs(default_path), 'opacity="1"', fixed = TRUE)
})

test_that("tm_legend(reverse = TRUE) does not reverse the raster bins", {
  skip_if_no_tmap_raster()

  dir <- local_out_dir()
  plain <- file.path(dir, "plain.qgs")
  reversed <- file.path(dir, "reversed.qgs")
  r <- test_raster(c(1:19, NA))
  write_qgs_quiet(tmap::tm_shape(r) + tmap::tm_raster(), plain)
  write_qgs_quiet(
    tmap::tm_shape(r) +
      tmap::tm_raster(col.legend = tmap::tm_legend(reverse = TRUE)),
    reversed
  )

  # tmap stores a reversed legend bottom-up while the cells keep the
  # unreversed palette, so the renderer must be identical.
  items <- function(path) {
    regmatches(read_qgs(path), gregexpr("<item [^/]*/>", read_qgs(path)))[[1L]]
  }
  expect_equal(items(reversed), items(plain))
})

test_that("two raster layers on one shape get one GeoTIFF each", {
  skip_if_no_tmap_raster()

  r <- test_raster(c(1:19, NA))
  r$w <- c(rep(1, 10), rep(2, 10))

  x <- tmap::tm_shape(r) + tmap::tm_raster("v") + tmap::tm_raster("w")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  # Unlike vector layers (which share their tm_shape's GeoPackage), each
  # raster layer writes its own single-band file.
  expect_setequal(
    list.files(file.path(dir, "proj_data")),
    c("r.tif", "r_2.tif")
  )
  out <- read_qgs(path)
  expect_length(gregexpr('layerType="Raster"', out)[[1L]], 2L)
  # Both reference band 1 of their own file.
  expect_false(grepl('band="2"', out, fixed = TRUE))
})

test_that("raster and vector layers can share a project", {
  skip_if_no_tmap_raster()

  nc <- read_nc()
  r <- terra::rast(
    nrows = 4L, ncols = 4L,
    xmin = -84.5, xmax = -75.5, ymin = 33.8, ymax = 36.6,
    crs = "EPSG:4326"
  )
  terra::values(r) <- 1:16
  names(r) <- "v"

  x <- tmap::tm_shape(r) + tmap::tm_raster() +
    tmap::tm_shape(nc) + tmap::tm_polygons(fill = "AREA")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  expect_true(file.exists(file.path(dir, "proj_data", "r.tif")))
  expect_true(file.exists(file.path(dir, "proj_data", "nc.gpkg")))
  out <- read_qgs(path)
  expect_match(out, 'layerType="Raster"', fixed = TRUE)
  expect_match(out, 'layerType="Vector"', fixed = TRUE)
})

test_that("the project CRS and extent follow a raster shape", {
  skip_if_no_tmap_raster()

  r <- terra::rast(
    nrows = 4L, ncols = 4L,
    xmin = 500000, xmax = 501000, ymin = 4649000, ymax = 4650000,
    crs = "EPSG:32633"
  )
  terra::values(r) <- 1:16
  names(r) <- "v"

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(tmap::tm_shape(r) + tmap::tm_raster(), path)

  out <- read_qgs(path)
  expect_match(out, "<authid>EPSG:32633</authid>", fixed = TRUE)
  expect_match(out, "500000", fixed = TRUE)
})

test_that("unsupported raster cases are errors", {
  skip_if_no_tmap_raster()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  r <- test_raster(c(1:19, NA))

  # tm_rgb()/tm_rgba() need a multibandcolor renderer.
  rgb <- terra::rast(
    nrows = 2L, ncols = 2L, nlyrs = 3L,
    xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:4326"
  )
  terra::values(rgb) <- rep(c(10, 100, 200), each = 4L)
  expect_error(
    write_qgs_quiet(tmap::tm_shape(rgb) + tmap::tm_rgb(), path),
    "unsupported scale"
  )

  # A filtered shape.
  expect_error(
    write_qgs_quiet(
      tmap::tm_shape(r, filter = c(TRUE, rep(FALSE, 19))) + tmap::tm_raster(),
      path
    ),
    "filtered shapes"
  )

  # col_alpha mapped to a column: rejected as a mapped aesthetic.
  expect_error(
    write_qgs_quiet(
      tmap::tm_shape(r) + tmap::tm_raster(col_alpha = "v"),
      path
    ),
    "`col_alpha` scale is not supported"
  )

  # A vector layer on a raster shape.
  expect_error(
    write_qgs_quiet(tmap::tm_shape(r) + tmap::tm_polygons(), path),
    "can only be drawn with tm_raster"
  )
})

test_that("qgs_tmap_raster_opacity() takes the constant col_alpha", {
  layer <- function(alpha) list(mapping_dt = list(col_alpha = alpha))

  expect_equal(qgs_tmap_raster_opacity(layer(NULL), 1L), 1)
  expect_equal(qgs_tmap_raster_opacity(layer(rep(0.5, 4L)), 1L), 0.5)
  # Cells tmap does not draw carry NA; the nodata color handles those, so
  # they must not read as a varying alpha.
  expect_equal(qgs_tmap_raster_opacity(layer(c(0.5, NA, 0.5)), 1L), 0.5)
  expect_equal(qgs_tmap_raster_opacity(layer(c(NA, NA)), 1L), 1)
  # A genuinely per-cell alpha has no raster-renderer representation.
  # tmap facets rather than produce this, so the guard is defensive.
  expect_error(
    qgs_tmap_raster_opacity(layer(c(0.2, 0.8)), 1L),
    "per-cell `col_alpha`"
  )
})

# The grid checks guard assumptions terra::rast() is built on, so they
# are exercised directly: producing a curvilinear or rotated grid through
# tmap would take a fixture that tmap itself mostly refuses to build.
tms_grid <- function(..., nx = 2L, ny = 3L, cells = nx * ny) {
  dims <- structure(
    list(
      x = structure(
        list(
          from = 1L, to = nx, offset = 0, delta = 1,
          refsys = sf::st_crs(4326L), point = NULL, values = NULL
        ),
        class = "dimension"
      ),
      y = structure(
        list(
          from = 1L, to = ny, offset = ny, delta = -1,
          refsys = sf::st_crs(4326L), point = NULL, values = NULL
        ),
        class = "dimension"
      )
    ),
    class = "dimensions",
    raster = structure(
      list(affine = c(0, 0), dimensions = c("x", "y"), curvilinear = FALSE),
      class = "stars_raster"
    )
  )
  overrides <- list(...)
  for (nm in names(overrides)) {
    if (nm %in% c("affine", "curvilinear")) {
      attr(dims, "raster")[[nm]] <- overrides[[nm]]
    } else {
      axis <- substr(nm, 1L, 1L)
      dims[[axis]][[substring(nm, 3L)]] <- overrides[[nm]]
    }
  }
  list(
    shpclass = "stars",
    shpTM = list(shp = dims),
    dt = data.frame(v = seq_len(cells), tmapID__ = seq_len(cells), sel__ = TRUE)
  )
}

test_that("qgs_tmap_group_grid() accepts a plain north-up grid", {
  grid <- qgs_tmap_group_grid(tms_grid(), 1L)
  expect_equal(grid$nx, 2L)
  expect_equal(grid$ny, 3L)
  expect_equal(c(grid$xmin, grid$xmax, grid$ymin, grid$ymax), c(0, 2, 0, 3))
  expect_equal(grid$crs, sf::st_crs(4326L))
})

test_that("qgs_tmap_group_grid() rejects grids terra cannot reproduce", {
  expect_error(
    qgs_tmap_group_grid(tms_grid(curvilinear = TRUE), 1L),
    "curvilinear"
  )
  expect_error(
    qgs_tmap_group_grid(tms_grid(affine = c(0.5, 0)), 1L),
    "rotated or sheared"
  )
  # A rectilinear grid states its boundaries in `values`, not a delta.
  expect_error(
    qgs_tmap_group_grid(tms_grid(x_delta = NA_real_), 1L),
    "irregular cell sizes"
  )
  expect_error(
    qgs_tmap_group_grid(tms_grid(y_delta = 1), 1L),
    "north-up"
  )
  expect_error(
    qgs_tmap_group_grid(tms_grid(cells = 5L), 1L),
    "5 rows but the grid has 6 cells"
  )
  no_crs <- tms_grid(x_refsys = sf::NA_crs_, y_refsys = sf::NA_crs_)
  expect_error(qgs_tmap_group_grid(no_crs, 1L), "no CRS")
})
