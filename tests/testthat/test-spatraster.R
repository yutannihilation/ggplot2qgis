# tidyterra::geom_spatraster() layers -> GDAL raster layers.

read_volcano <- function() {
  terra::rast(system.file("extdata/volcano2.tif", package = "tidyterra"))
}

volcano_plot <- function(r = read_volcano()) {
  ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = r)
}

test_that("a geom_spatraster() layer becomes a GeoTIFF-backed raster layer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "volcano.qgs")
  write_qgs(volcano_plot(), path)

  # The SpatRaster is written as a GeoTIFF named after the band...
  tif <- file.path(dir, "volcano_data", "elevation.tif")
  expect_true(file.exists(tif))
  r <- terra::rast(tif)
  expect_equal(as.integer(terra::ncell(r)), 122L * 174L)

  out <- read_qgs(path)
  # ...referenced through the GDAL provider.
  expect_match(out, 'type="raster"', fixed = TRUE)
  expect_match(out, "<provider>gdal</provider>", fixed = TRUE)
  expect_match(out, 'providerKey="gdal"', fixed = TRUE)
  expect_match(
    out,
    "<datasource>volcano_data/elevation.tif</datasource>",
    fixed = TRUE
  )
  # NA cells rely on the GeoTIFF's own nodata value.
  expect_match(
    out,
    '<noDataList bandNo="1" useSrcNoData="1"/>',
    fixed = TRUE
  )

  # The renderer reproduces the trained fill scale: an interpolated
  # pseudocolor ramp between the scale limits (the volcano2 data range).
  expect_match(out, 'type="singlebandpseudocolor"', fixed = TRUE)
  expect_match(out, 'band="1"', fixed = TRUE)
  expect_match(out, 'classificationMin="76.2622222900391"', fixed = TRUE)
  expect_match(out, 'classificationMax="195.55419921875"', fixed = TRUE)
  expect_match(out, 'colorRampType="INTERPOLATED"', fixed = TRUE)

  # The endpoint items carry the colors ggplot2 maps the limits to.
  b <- ggplot2::ggplot_build(volcano_plot())
  scale <- b@plot@scales$get_scales("fill")
  limits <- scale$get_limits()
  ends <- grDevices::col2rgb(scale$map(limits))
  hex <- function(rgb) sprintf("#%02x%02x%02x", rgb[1], rgb[2], rgb[3])
  expect_match(out, paste0('color="', hex(ends[, 1L]), '"'), fixed = TRUE)
  expect_match(out, paste0('color="', hex(ends[, 2L]), '"'), fixed = TRUE)
})

test_that("the CRS helper layer of geom_spatraster() is not written", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  p <- volcano_plot()
  # geom_spatraster() returns the raster layer plus an sf layer with a
  # single empty point that only carries the CRS.
  expect_length(p@layers, 2L)

  dir <- local_out_dir()
  path <- file.path(dir, "volcano.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  expect_length(gregexpr("<maplayer", out, fixed = TRUE)[[1L]], 1L)
  expect_length(list.files(file.path(dir, "volcano_data")), 1L)
})

test_that("layer_names applies to the written layers only", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "volcano.qgs")
  # One name: the raster layer. The skipped helper layer does not count.
  write_qgs(volcano_plot(), path, layer_names = "auckland")

  expect_true(file.exists(file.path(dir, "volcano_data", "auckland.tif")))
  expect_match(read_qgs(path), 'name="auckland"', fixed = TRUE)
})

test_that("use_plot_crs = TRUE keeps the raster's CRS as the project CRS", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "volcano.qgs")
  write_qgs(volcano_plot(), path, use_plot_crs = TRUE)

  out <- read_qgs(path)
  # volcano2.tif is in NZGD2000 / New Zealand Transverse Mercator 2000.
  expect_match(
    out,
    "<projectCrs>\n    <spatialrefsys nativeFormat=\"Wkt\">\n      <wkt>PROJCRS[\"NZGD2000",
    fixed = TRUE
  )
})

test_that("a multi-band SpatRaster is an error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  r <- read_volcano()
  two <- c(r, r * 2)
  names(two) <- c("a", "b")
  expect_error(
    write_qgs(volcano_plot(two), file.path(local_out_dir(), "x.qgs")),
    "multi-band SpatRaster is not supported"
  )
})

test_that("a binned fill scale on a raster layer is an error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  p <- volcano_plot() + ggplot2::scale_fill_steps()
  expect_error(
    write_qgs(p, file.path(local_out_dir(), "x.qgs")),
    "only a continuous fill scale is supported"
  )
})

test_that("a non-default fill mapping on a raster layer is an error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(
      data = read_volcano(),
      ggplot2::aes(fill = ggplot2::after_stat(value * 2))
    )
  expect_error(
    write_qgs(p, file.path(local_out_dir(), "x.qgs")),
    "only the default `fill` mapping"
  )
})

test_that("a user-spelled default fill mapping is accepted", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(
      data = read_volcano(),
      ggplot2::aes(fill = ggplot2::after_stat(value))
    )
  dir <- local_out_dir()
  path <- file.path(dir, "volcano.qgs")
  write_qgs(p, path)
  expect_match(read_qgs(path), 'type="singlebandpseudocolor"', fixed = TRUE)
})

# tidyterra::geom_spatraster_rgb() layers -> multiband (true color)
# raster layers. cyl_tile.tif is a 3-band Byte tile in EPSG:3857 with
# band statistics 35..253 / 35..251 / 35..250.

read_tile <- function() {
  terra::rast(system.file("extdata/cyl_tile.tif", package = "tidyterra"))
}

test_that("a geom_spatraster_rgb() layer becomes a multiband raster layer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  tile <- read_tile()
  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster_rgb(data = tile)

  dir <- local_out_dir()
  path <- file.path(dir, "tile.qgs")
  write_qgs(p, path)

  # The layer is named after the data variable (the constructor's bare
  # `data` symbol) and written as a 3-band GeoTIFF.
  tif <- file.path(dir, "tile_data", "tile.tif")
  expect_true(file.exists(tif))
  r <- terra::rast(tif)
  expect_equal(as.integer(terra::nlyr(r)), 3L)

  out <- read_qgs(path)
  expect_match(out, "<provider>gdal</provider>", fixed = TRUE)
  expect_match(out, "<datasource>tile_data/tile.tif</datasource>", fixed = TRUE)

  # The renderer maps the written bands to the channels in order, with
  # QGIS's own defaults for a Byte RGB raster: NoEnhancement and the
  # band statistics as the recorded min/max.
  expect_match(out, 'type="multibandcolor"', fixed = TRUE)
  expect_match(out, 'redBand="1"', fixed = TRUE)
  expect_match(out, 'greenBand="2"', fixed = TRUE)
  expect_match(out, 'blueBand="3"', fixed = TRUE)
  expect_match(out, 'opacity="1"', fixed = TRUE)
  expect_match(out, "<limits>MinMax</limits>", fixed = TRUE)
  expect_match(out, "<algorithm>NoEnhancement</algorithm>", fixed = TRUE)
  expect_match(out, "<minValue>35</minValue>", fixed = TRUE)
  expect_match(out, "<maxValue>253</maxValue>", fixed = TRUE)
  expect_match(out, "<maxValue>251</maxValue>", fixed = TRUE)
  expect_match(out, "<maxValue>250</maxValue>", fixed = TRUE)

  # Missing cells stay transparent through the source nodata, one entry
  # per band.
  for (band in 1:3) {
    expect_match(
      out,
      sprintf('<noDataList bandNo="%d" useSrcNoData="1"/>', band),
      fixed = TRUE
    )
  }
})

test_that("the r/g/b band selection is materialized in the GeoTIFF", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  tile <- read_tile()
  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster_rgb(data = tile, r = 3, g = 2, b = 1)

  dir <- local_out_dir()
  path <- file.path(dir, "tile.qgs")
  write_qgs(p, path, layer_names = "swapped")

  # tidyterra reorders the bands at layer construction, so the written
  # tif is already in red-green-blue order and the renderer's band
  # mapping stays 1/2/3.
  r <- terra::rast(file.path(dir, "tile_data", "swapped.tif"))
  mm <- terra::minmax(r, compute = TRUE)
  expect_equal(as.vector(mm[2L, ]), c(250, 251, 253))

  out <- read_qgs(path)
  expect_match(out, 'redBand="1"', fixed = TRUE)
  expect_match(out, "<maxValue>250</maxValue>", fixed = TRUE)
})

test_that("alpha becomes the multiband renderer's opacity", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  tile <- read_tile()
  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster_rgb(data = tile, alpha = 0.5)

  dir <- local_out_dir()
  path <- file.path(dir, "tile.qgs")
  write_qgs(p, path)
  expect_match(read_qgs(path), 'opacity="0.5"', fixed = TRUE)
})

test_that("a non-default max_col_value becomes a linear stretch", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  tile <- read_tile()
  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster_rgb(data = tile, max_col_value = 500)

  dir <- local_out_dir()
  path <- file.path(dir, "tile.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  expect_match(
    out,
    "<algorithm>StretchToMinimumMaximum</algorithm>",
    fixed = TRUE
  )
  expect_match(out, "<limits>None</limits>", fixed = TRUE)
  expect_match(out, "<minValue>0</minValue>", fixed = TRUE)
  expect_match(out, "<maxValue>500</maxValue>", fixed = TRUE)
})

test_that("an all-NA band falls back to the stretch renderer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  # No band statistics to record, but the plot still renders (the NA
  # cells are transparent), so the project must still be written.
  r <- terra::rast(
    nrows = 2, ncols = 2, nlyrs = 3,
    vals = c(1:4, rep(NA, 4), 5:8),
    crs = "EPSG:4326"
  )
  p <- ggplot2::ggplot() + tidyterra::geom_spatraster_rgb(data = r)

  dir <- local_out_dir()
  path <- file.path(dir, "na_band.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  expect_match(
    out,
    "<algorithm>StretchToMinimumMaximum</algorithm>",
    fixed = TRUE
  )
  expect_match(out, "<maxValue>255</maxValue>", fixed = TRUE)
})

test_that("band values outside 0..255 use the stretch renderer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  # tidyterra clamps the values to [0, max_col_value] when drawing;
  # NoEnhancement's used-as-is assumption would not reproduce that, the
  # explicit stretch does.
  r <- terra::rast(
    nrows = 2, ncols = 2, nlyrs = 3,
    vals = seq(0, 1100, length.out = 12),
    crs = "EPSG:4326"
  )
  p <- ggplot2::ggplot() + tidyterra::geom_spatraster_rgb(data = r)

  dir <- local_out_dir()
  path <- file.path(dir, "wide.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  expect_match(
    out,
    "<algorithm>StretchToMinimumMaximum</algorithm>",
    fixed = TRUE
  )
  expect_match(out, "<limits>None</limits>", fixed = TRUE)
})

test_that("alpha = NA writes an opaque renderer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  # ggplot2 draws an NA alpha opaque, so it maps to opacity 1.
  r <- terra::rast(
    nrows = 2, ncols = 2, nlyrs = 3,
    vals = 1:12, crs = "EPSG:4326"
  )
  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster_rgb(data = r, alpha = NA)

  dir <- local_out_dir()
  path <- file.path(dir, "na_alpha.qgs")
  write_qgs(p, path)
  expect_match(read_qgs(path), 'opacity="1"', fixed = TRUE)
})

test_that("a SpatRaster without a CRS is a clear error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  r <- terra::rast(
    nrows = 2, ncols = 2, nlyrs = 3,
    vals = 1:12, crs = ""
  )
  p <- ggplot2::ggplot() + tidyterra::geom_spatraster_rgb(data = r)
  expect_error(
    write_qgs(p, file.path(local_out_dir(), "x.qgs")),
    "the data has no CRS"
  )
})

test_that("an RGB raster combines with vector layers", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  # The geom_spatraster_rgb() example of tidyterra: a SpatVector plot
  # (whose fortified sf mixes POLYGON and MULTIPOLYGON geometries) over
  # a true-color tile.
  tile <- read_tile()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  p <- ggplot2::ggplot(v) +
    tidyterra::geom_spatraster_rgb(data = tile) +
    tidyterra::geom_spatvector(fill = NA)

  dir <- local_out_dir()
  path <- file.path(dir, "combined.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  # The raster plus the vector layer (the CRS helper layer is skipped).
  expect_length(gregexpr("<maplayer", out, fixed = TRUE)[[1L]], 2L)
  expect_match(out, 'type="multibandcolor"', fixed = TRUE)
  expect_match(out, 'type="singleSymbol"', fixed = TRUE)
  expect_match(out, 'geometry="Polygon"', fixed = TRUE)
})
