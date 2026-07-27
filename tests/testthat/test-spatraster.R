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
