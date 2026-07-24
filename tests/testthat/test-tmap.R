# write_qgs() for tmap objects. The tests go through the public
# write_qgs() surface and assert on the written .qgs XML, like
# test-write_qgs.R does for ggplot objects.

skip_if_no_tmap <- function() {
  testthat::skip_if_not_installed("tmap", "4.4")
}

tmap_data <- function(name) {
  e <- new.env()
  utils::data(list = name, package = "tmap", envir = e)
  e[[name]]
}

# tmap emits session tips and CRS suggestions while building; they are not
# part of the contract.
write_qgs_quiet <- function(...) {
  suppressMessages(write_qgs(...))
}

test_that("a tmap intervals scale becomes a binned (graduated) style", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  expect_invisible(write_qgs_quiet(x, path))

  expect_true(file.exists(path))
  expect_true(file.exists(file.path(dir, "proj_data", "World.gpkg")))

  out <- read_qgs(path)
  expect_match(out, "proj_data/World.gpkg|layername=World", fixed = TRUE)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="HPI"', fixed = TRUE)
  expect_match(out, 'geometry="Polygon"', fixed = TRUE)
  expect_match(out, "<authid>EPSG:4326</authid>", fixed = TRUE)

  # The default intervals scale of World$HPI has breaks 10, 20, ..., 60:
  # 5 bins with tmap's exact boundaries and colors.
  expect_length(
    regmatches(out, gregexpr("<range ", out, fixed = TRUE))[[1]],
    5L
  )
  expect_match(out, 'lower="10.0', fixed = TRUE)
  expect_match(out, 'upper="60.0', fixed = TRUE)
  # First bin color (#DFEDFF).
  expect_match(out, 'value="223,237,255,255,rgb:', fixed = TRUE)
})

test_that("a reversed legend keeps the bin-color pairing", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) +
    tmap::tm_polygons(
      fill = "HPI",
      fill.legend = tmap::tm_legend(reverse = TRUE)
    )

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  # The first symbol of the graduated renderer covers the lowest bin
  # (10 - 20), whose tmap color is #DFEDFF regardless of the legend
  # direction.
  renderer <- regmatches(
    out,
    regexpr('<renderer-v2[^>]*type="graduatedSymbol".*?</renderer-v2>', out)
  )
  first_symbol <- regmatches(
    renderer,
    regexpr('<symbol[^>]*name="0"[^>]*>.*?</symbol>', renderer)
  )
  expect_match(first_symbol, "223,237,255", fixed = TRUE)
})

test_that("tied classification breaks collapse into the surviving bins", {
  skip_if_no_tmap()
  World <- tmap_data("World")
  World$dup <- c(rep(1, 150), seq_len(nrow(World) - 150))

  x <- tmap::tm_shape(World) +
    tmap::tm_polygons(
      fill = "dup",
      fill.scale = tmap::tm_scale_intervals(style = "quantile")
    )

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  # tmap's quantile breaks are 1,1,1,1,1,27 and every feature gets the
  # last bin's color (#00548F): one zero-width-free bin survives.
  expect_length(
    regmatches(out, gregexpr("<range ", out, fixed = TRUE))[[1]],
    1L
  )
  expect_match(out, 'value="0,84,143,255,rgb:', fixed = TRUE)
})

test_that("a continuous-style intervals legend is a clear error", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) +
    tmap::tm_polygons(
      fill = "HPI",
      fill.scale = tmap::tm_scale_intervals(label.style = "cont")
    )

  expect_error(
    write_qgs_quiet(x, tempfile(fileext = ".qgs")),
    "label.style"
  )
})

test_that("explicit interval breaks are kept exactly", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) +
    tmap::tm_polygons(
      fill = "HPI",
      fill.scale = tmap::tm_scale_intervals(breaks = c(0, 30, 70))
    )

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_length(
    regmatches(out, gregexpr("<range ", out, fixed = TRUE))[[1]],
    2L
  )
  expect_match(out, 'lower="0.0', fixed = TRUE)
  expect_match(out, 'upper="70.0', fixed = TRUE)
})

test_that("missing values become a separate layer by default", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  # World$HPI has NAs; tmap paints them in value.na grey (#BFBFBF).
  x <- tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'name="World (missing value)"', fixed = TRUE)
  # The NA layer points at the same GeoPackage, filtered to the NULLs;
  # no second .gpkg is written.
  expect_match(
    out,
    '|layername=World|subset="HPI" IS NULL<',
    fixed = TRUE
  )
  expect_false(
    file.exists(file.path(dir, "proj_data", "World (missing value).gpkg"))
  )
  # tmap's value.na grey.
  expect_match(out, 'value="191,191,191,255,rgb:', fixed = TRUE)
  # The NA layer draws below the main layer: the tree lists the top-most
  # layer first.
  tree <- regmatches(
    out,
    regexpr("<layer-tree-group>.*</layer-tree-group>", out)
  )
  expect_lt(
    regexpr('name="World"', tree, fixed = TRUE),
    regexpr('name="World (missing value)"', tree, fixed = TRUE)
  )
})

test_that("create_na_layer = FALSE ignores the missing values", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path, create_na_layer = FALSE)

  out <- read_qgs(path)
  expect_no_match(out, "(missing value)", fixed = TRUE)
  expect_no_match(out, "subset=", fixed = TRUE)
})

test_that("no NA layer when the data has no missing values", {
  skip_if_no_tmap()
  World <- tmap_data("World")
  World$complete <- seq_len(nrow(World))

  x <- tmap::tm_shape(World) + tmap::tm_polygons(fill = "complete")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  expect_no_match(read_qgs(path), "(missing value)", fixed = TRUE)
})

test_that("the exact continuous gradient filters missings out of the main layer", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) +
    tmap::tm_polygons(
      fill = "HPI",
      fill.scale = tmap::tm_scale_continuous(values = "viridis")
    )

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path, gradient_style = "continuous")

  out <- read_qgs(path)
  # The data-defined color expression would paint NULL attributes in the
  # static fallback color, so the main layer excludes them.
  expect_match(out, '|subset="HPI" IS NOT NULL<', fixed = TRUE)
  expect_match(out, 'name="World (missing value)"', fixed = TRUE)
})

test_that("a col-mapped NA drives the outline of the NA layer", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) +
    tmap::tm_polygons(fill = "gray", col = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'name="World (missing value)"', fixed = TRUE)
  # value.na (#BFBFBF) on the border of the NA symbol.
  expect_match(
    out,
    'name="outline_color" type="QString" value="191,191,191,255,rgb:',
    fixed = TRUE
  )
})

test_that("categorical scales keep the in-layer catch-all, no NA layer", {
  skip_if_no_tmap()
  World <- tmap_data("World")
  World$economy[c(1L, 2L)] <- NA

  x <- tmap::tm_shape(World) + tmap::tm_polygons(fill = "economy")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'value="NULL"', fixed = TRUE)
  expect_no_match(out, "(missing value)", fixed = TRUE)
})

test_that("a tmap categorical scale becomes a categorized style", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) + tmap::tm_polygons(fill = "economy")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'type="categorizedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="economy"', fixed = TRUE)
  # One category per level, referencing the raw data values.
  expect_length(
    regmatches(out, gregexpr("<category ", out, fixed = TRUE))[[1]],
    7L
  )
  expect_match(out, 'value="1. Developed region: G7"', fixed = TRUE)
  # A factor column stays a string-typed category.
  expect_match(out, 'type="string"', fixed = TRUE)
  # The color tmap assigned to the first level (#FF9D9A).
  expect_match(out, 'value="255,157,154,255,rgb:', fixed = TRUE)
})

test_that("a categorical scale on a numeric column keeps exact values", {
  skip_if_no_tmap()
  World <- tmap_data("World")
  World$big <- rep(c(2, 10, 1000000), length.out = nrow(World))

  x <- tmap::tm_shape(World) +
    tmap::tm_polygons(
      fill = "big",
      fill.scale = tmap::tm_scale_categorical()
    )

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  categories <- regmatches(
    out,
    regexpr("<categories>.*?</categories>", out)
  )
  # Values are written in plain notation (not "1e+06") so QGIS can match
  # them against the numeric field, in numeric order — and as typed
  # (double) categories: QGIS matches via the string form of the *typed*
  # value, and a REAL feature value 1000000 stringifies as "1e+06", which
  # a type="string" category would never equal.
  expect_match(categories, 'value="1000000"', fixed = TRUE)
  expect_no_match(categories, "1e+06", fixed = TRUE)
  expect_match(categories, 'type="double"', fixed = TRUE)
  expect_no_match(categories, 'type="string"', fixed = TRUE)
  expect_lt(
    regexpr('value="2"', categories, fixed = TRUE),
    regexpr('value="10"', categories, fixed = TRUE)
  )
  expect_lt(
    regexpr('value="10"', categories, fixed = TRUE),
    regexpr('value="1000000"', categories, fixed = TRUE)
  )
})

test_that("NA values in a categorical scale become the catch-all category", {
  skip_if_no_tmap()
  World <- tmap_data("World")
  World$economy[c(1L, 2L)] <- NA

  x <- tmap::tm_shape(World) + tmap::tm_polygons(fill = "economy")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'type="categorizedSymbol"', fixed = TRUE)
  expect_match(out, 'value="NULL"', fixed = TRUE)
})

test_that("an ordinal scale is categorized, too", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) +
    tmap::tm_polygons(
      fill = "income_grp",
      fill.scale = tmap::tm_scale_ordinal()
    )

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'type="categorizedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="income_grp"', fixed = TRUE)
})

test_that("a tmap continuous scale becomes a graduated style", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) +
    tmap::tm_polygons(
      fill = "HPI",
      fill.scale = tmap::tm_scale_continuous(values = "viridis")
    )

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="HPI"', fixed = TRUE)
  # Fine-grained classes, like the ggplot2 path.
  expect_length(
    regmatches(out, gregexpr("<range ", out, fixed = TRUE))[[1]],
    25L
  )
  # The viridis endpoints (#440154, #FDE725).
  expect_match(out, 'value="68,1,84,255,rgb:', fixed = TRUE)
  expect_match(out, 'value="253,231,37,255,rgb:', fixed = TRUE)
})

test_that("gradient_style = \"continuous\" uses a color ramp expression", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) +
    tmap::tm_polygons(
      fill = "HPI",
      fill.scale = tmap::tm_scale_continuous(values = "viridis")
    )

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path, gradient_style = "continuous")

  out <- read_qgs(path)
  expect_match(out, 'type="singleSymbol"', fixed = TRUE)
  expect_match(out, "ramp_color(create_ramp(map(", fixed = TRUE)
  expect_match(out, "&quot;HPI&quot;", fixed = TRUE)
})

test_that("a log continuous scale converts (piecewise-linear stops)", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) +
    tmap::tm_polygons(
      fill = "pop_est",
      fill.scale = tmap::tm_scale_continuous_log()
    )

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="pop_est"', fixed = TRUE)
})

test_that("constant tmap colors become a single symbol", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) + tmap::tm_polygons(fill = "red")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'type="singleSymbol"', fixed = TRUE)
  expect_match(out, 'value="255,0,0,255,rgb:', fixed = TRUE)
  # tmap's default polygon border: col = #404040, lwd = 1 (25.4/96 mm).
  expect_match(out, 'value="64,64,64,255,rgb:', fixed = TRUE)
  expect_match(out, 'name="outline_width" type="QString" value="0.2645833"',
    fixed = TRUE
  )
})

test_that("a varying col on polygons drives the stroke", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) +
    tmap::tm_polygons(fill = "gray", col = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="HPI"', fixed = TRUE)
  # The interior keeps the constant fill (gray, #BEBEBE).
  expect_match(out, 'name="color" type="QString" value="190,190,190,255,rgb:',
    fixed = TRUE
  )
})

test_that("tm_lines converts to a line layer", {
  skip_if_no_tmap()
  rivers <- tmap_data("World_rivers")

  x <- tmap::tm_shape(rivers) + tmap::tm_lines(col = "scalerank")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'geometry="LineString"', fixed = TRUE)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="scalerank"', fixed = TRUE)
  expect_match(out, 'class="SimpleLine"', fixed = TRUE)
})

test_that("tm_symbols / tm_dots convert to point layers", {
  skip_if_no_tmap()
  metro <- tmap_data("metro")

  x <- tmap::tm_shape(metro) + tmap::tm_symbols(fill = "pop2020")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'geometry="Point"', fixed = TRUE)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="pop2020"', fixed = TRUE)
  expect_match(out, 'class="SimpleMarker"', fixed = TRUE)

  x2 <- tmap::tm_shape(metro) + tmap::tm_dots()
  path2 <- file.path(dir, "proj2.qgs")
  write_qgs_quiet(x2, path2)
  out2 <- read_qgs(path2)
  expect_match(out2, 'geometry="Point"', fixed = TRUE)
  expect_match(out2, 'type="singleSymbol"', fixed = TRUE)
})

test_that("multiple tm_shape groups become stacked layers", {
  skip_if_no_tmap()
  World <- tmap_data("World")
  metro <- tmap_data("metro")

  x <- tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI") +
    tmap::tm_shape(metro) + tmap::tm_dots()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  expect_true(file.exists(file.path(dir, "proj_data", "World.gpkg")))
  expect_true(file.exists(file.path(dir, "proj_data", "metro.gpkg")))

  out <- read_qgs(path)
  # The layer tree lists the top-most layer (metro) first.
  tree <- regmatches(
    out,
    regexpr("<layer-tree-group>.*</layer-tree-group>", out)
  )
  expect_lt(
    regexpr('name="metro"', tree, fixed = TRUE),
    regexpr('name="World"', tree, fixed = TRUE)
  )
})

test_that("layer names can be overridden, collisions are suffixed", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  # Two separate tm_shape() calls: one GeoPackage per shape.
  x <- tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI") +
    tmap::tm_shape(World) + tmap::tm_borders()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)
  expect_true(file.exists(file.path(dir, "proj_data", "World.gpkg")))
  expect_true(file.exists(file.path(dir, "proj_data", "World_2.gpkg")))

  path2 <- file.path(dir, "proj2.qgs")
  write_qgs_quiet(x, path2, layer_names = c("fills", "borders"))
  expect_true(file.exists(file.path(dir, "proj2_data", "fills.gpkg")))
  expect_true(file.exists(file.path(dir, "proj2_data", "borders.gpkg")))
})

test_that("layers sharing one tm_shape share one GeoPackage", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  # One tm_shape() with two layers: the data is written once, and both
  # QGIS layers point at the same GeoPackage table.
  x <- tmap::tm_shape(World) + tmap::tm_fill(fill = "HPI") +
    tmap::tm_borders()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  expect_true(file.exists(file.path(dir, "proj_data", "World.gpkg")))
  expect_false(file.exists(file.path(dir, "proj_data", "World_2.gpkg")))

  out <- read_qgs(path)
  # Both layers reference the same table, under distinct display names.
  expect_length(
    regmatches(
      out,
      gregexpr("proj_data/World.gpkg|layername=World<", out, fixed = TRUE)
    )[[1]],
    2L
  )
  expect_match(out, 'name="World_2"', fixed = TRUE)
})

test_that("the GeoPackage keeps the raw attribute values", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  d <- sf::st_read(
    file.path(dir, "proj_data", "World.gpkg"),
    quiet = TRUE
  )
  expect_equal(nrow(d), nrow(World))
  expect_equal(d$HPI, World$HPI)
})

test_that("the project CRS defaults to the tmap display CRS", {
  skip_if_no_tmap()
  World <- tmap_data("World")
  africa <- World[World$continent == "Africa", ]

  x <- tmap::tm_shape(africa) + tmap::tm_polygons(fill = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  # No tm_crs()/tm_basemap(): tmap displays in the data CRS.
  block <- regmatches(out, regexpr("<projectCrs>.*?</projectCrs>", out))
  expect_match(block, "<authid>EPSG:4326</authid>", fixed = TRUE)
  # Zoomed to Africa, not the template's whole-world extent.
  expect_no_match(out, "<xmin>-20037508", fixed = TRUE)

  # use_plot_crs = FALSE forces the Web Mercator default.
  path2 <- file.path(dir, "proj2.qgs")
  write_qgs_quiet(x, path2, use_plot_crs = FALSE)
  block2 <- regmatches(
    read_qgs(path2),
    regexpr("<projectCrs>.*?</projectCrs>", read_qgs(path2))
  )
  expect_match(block2, "<authid>EPSG:3857</authid>", fixed = TRUE)
})

test_that("a basemap map opens in EPSG:3857, like tmap", {
  skip_if_no_tmap()
  testthat::skip_if_not_installed("maptiles")
  World <- tmap_data("World")

  # tmap itself switches the display CRS to 3857 when basemaps are used.
  x <- tmap::tm_basemap("OpenStreetMap") +
    tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  block <- regmatches(out, regexpr("<projectCrs>.*?</projectCrs>", out))
  expect_match(block, "<authid>EPSG:3857</authid>", fixed = TRUE)
})

test_that("a polar extent is clipped to the project CRS's valid area", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  # World reaches latitude -90; unclipped Web Mercator would blow the
  # extent up to y = -2.4e8.
  x <- tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path, use_plot_crs = FALSE)

  out <- read_qgs(path)
  extent <- regmatches(out, regexpr("<extent>.*?</extent>", out))
  ys <- as.numeric(regmatches(
    extent,
    gregexpr("-?[0-9.]+(?=</ymin>|</ymax>)", extent, perl = TRUE)
  )[[1]])
  expect_true(all(abs(ys) < 2.1e7))
})

test_that("use_plot_crs adopts the tmap CRS", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI") +
    tmap::tm_crs(3035)

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path, use_plot_crs = TRUE)

  out <- read_qgs(path)
  block <- regmatches(out, regexpr("<projectCrs>.*?</projectCrs>", out))
  expect_match(block, "<authid>EPSG:3035</authid>", fixed = TRUE)
  # The layer keeps its own CRS.
  expect_match(out, "<authid>EPSG:4326</authid>", fixed = TRUE)
})

test_that("qtm() output converts, too", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::qtm(World, fill = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="HPI"', fixed = TRUE)
})

test_that("tm_basemap resolves a provider name to an XYZ layer", {
  skip_if_no_tmap()
  testthat::skip_if_not_installed("maptiles")
  World <- tmap_data("World")

  x <- tmap::tm_basemap("OpenStreetMap") +
    tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, "type=xyz", fixed = TRUE)
  expect_match(out, "tile.openstreetmap.org", fixed = TRUE)
  expect_match(out, "zmax=17", fixed = TRUE)
  # The basemap draws below the vector layer.
  order <- regmatches(out, regexpr("<custom-order.*?</custom-order>", out))
  expect_lt(
    regexpr("OpenStreetMap", order, fixed = TRUE),
    regexpr("World", order, fixed = TRUE)
  )
})

test_that("subdomain and retina placeholders are normalized", {
  skip_if_no_tmap()
  testthat::skip_if_not_installed("maptiles")
  World <- tmap_data("World")

  x <- tmap::tm_basemap("CartoDB.Positron") +
    tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  # {s} -> first subdomain; {r} dropped ({} are percent-encoded).
  expect_match(out, "a.basemaps.cartocdn.com", fixed = TRUE)
  expect_match(out, "%7Bz%7D%2F%7Bx%7D%2F%7By%7D.png", fixed = TRUE)
  expect_no_match(out, "%7Br%7D", fixed = TRUE)
})

test_that("one tm_basemap with several providers writes all of them", {
  skip_if_no_tmap()
  testthat::skip_if_not_installed("maptiles")
  World <- tmap_data("World")

  x <- tmap::tm_basemap(c("OpenStreetMap", "CartoDB.Positron")) +
    tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, "tile.openstreetmap.org", fixed = TRUE)
  expect_match(out, "cartocdn.com", fixed = TRUE)
  tree <- regmatches(
    out,
    regexpr("<layer-tree-group>.*</layer-tree-group>", out)
  )
  entries <- regmatches(tree, gregexpr("<layer-tree-layer[^>]*>", tree))[[1]]
  expect_match(
    entries[grepl("openstreetmap", entries)],
    'checked="Qt::Checked"',
    fixed = TRUE
  )
  expect_match(
    entries[grepl("cartocdn", entries)],
    'checked="Qt::Unchecked"',
    fixed = TRUE
  )
})

test_that("a {s} placeholder in a URL template uses the first subdomain", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  # tm_basemap()'s default `sub` is the compact "abc" string.
  x <- tmap::tm_basemap("https://{s}.tile.example.com/{z}/{x}/{y}.png") +
    tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  expect_match(out, "a.tile.example.com", fixed = TRUE)
  expect_no_match(out, "%7Bs%7D", fixed = TRUE)
})

test_that("tm_basemap accepts a URL template directly", {
  skip_if_no_tmap()
  World <- tmap_data("World")

  x <- tmap::tm_basemap("https://tile.example.com/{z}/{x}/{y}.png") +
    tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  expect_match(read_qgs(path), "tile.example.com", fixed = TRUE)
})

test_that("multiple basemaps: only the first is checked", {
  skip_if_no_tmap()
  testthat::skip_if_not_installed("maptiles")
  World <- tmap_data("World")

  x <- tmap::tm_basemap("OpenStreetMap") +
    tmap::tm_basemap("CartoDB.Positron") +
    tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  write_qgs_quiet(x, path)

  out <- read_qgs(path)
  tree <- regmatches(
    out,
    regexpr("<layer-tree-group>.*</layer-tree-group>", out)
  )
  entries <- regmatches(
    tree,
    gregexpr("<layer-tree-layer[^>]*>", tree)
  )[[1]]
  expect_match(
    entries[grepl("openstreetmap", entries)],
    'checked="Qt::Checked"',
    fixed = TRUE
  )
  expect_match(
    entries[grepl("cartocdn", entries)],
    'checked="Qt::Unchecked"',
    fixed = TRUE
  )
})

test_that("tm_basemap overrides the basemap argument with a warning", {
  skip_if_no_tmap()
  testthat::skip_if_not_installed("maptiles")
  World <- tmap_data("World")

  x <- tmap::tm_basemap("CartoDB.Positron") +
    tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  expect_warning(
    write_qgs_quiet(x, path, basemap = "osm"),
    "tm_basemap"
  )

  out <- read_qgs(path)
  expect_match(out, "cartocdn.com", fixed = TRUE)
  expect_no_match(out, "tile.openstreetmap.org", fixed = TRUE)
})

test_that("an unknown basemap provider is an error", {
  skip_if_no_tmap()
  testthat::skip_if_not_installed("maptiles")
  World <- tmap_data("World")

  x <- tmap::tm_basemap("NoSuchProvider.Foo") +
    tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI")

  expect_error(
    write_qgs_quiet(x, tempfile(fileext = ".qgs")),
    "basemap"
  )
})

test_that("unsupported tmap features are errors", {
  skip_if_no_tmap()
  World <- tmap_data("World")
  metro <- tmap_data("metro")

  path <- tempfile(fileext = ".qgs")

  # Facets.
  expect_error(
    write_qgs_quiet(
      tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI") +
        tmap::tm_facets(by = "continent"),
      path
    ),
    "facet"
  )

  # Text layers.
  expect_error(
    write_qgs_quiet(
      tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI") +
        tmap::tm_text("iso_a3"),
      path
    ),
    "tm_text"
  )

  # Raster shapes.
  land <- tmap_data("land")
  expect_error(
    write_qgs_quiet(
      tmap::tm_shape(land) + tmap::tm_raster("elevation"),
      path
    ),
    "raster|sf object"
  )

  # Both fill and col mapped on the same layer.
  expect_error(
    write_qgs_quiet(
      tmap::tm_shape(World) + tmap::tm_polygons(fill = "HPI", col = "gender"),
      path
    ),
    "both `fill` and `col`"
  )

  # Non-color scales.
  expect_error(
    write_qgs_quiet(
      tmap::tm_shape(metro) + tmap::tm_symbols(size = "pop2020"),
      path
    ),
    "`size`"
  )

  # Rank scales silently remap the data; not representable in QGIS.
  expect_error(
    write_qgs_quiet(
      tmap::tm_shape(World) + tmap::tm_polygons(
        fill = "HPI",
        fill.scale = tmap::tm_scale_rank()
      ),
      path
    ),
    "scale"
  )
})
