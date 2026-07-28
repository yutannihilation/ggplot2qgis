# tidyterra::geom_spatvector() layers.
#
# geom_spatvector() is a plain wrapper of ggplot2::geom_sf(), and
# tidyterra's fortify.SpatVector() method turns the SpatVector into an sf
# object when the plot (or the layer) is created — so such a layer is
# already an ordinary sf layer by the time write_qgs() sees it and needs
# no conversion of its own. These tests pin that down, plus the layer-name
# derivation, which the wrapper does affect (see qgs_derived_layer_name()).

read_cyl <- function() {
  terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
}

skip_if_no_tidyterra <- function() {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")
}

test_that("a geom_spatvector() layer becomes a GeoPackage-backed vector layer", {
  skip_if_no_tidyterra()

  cyl <- read_cyl()
  p <- ggplot2::ggplot(cyl) +
    tidyterra::geom_spatvector()

  dir <- local_out_dir()
  path <- file.path(dir, "cyl.qgs")
  write_qgs(p, path)

  expect_true(file.exists(file.path(dir, "cyl_data", "cyl.gpkg")))
  out <- read_qgs(path)
  expect_match(out, 'type="vector"', fixed = TRUE)
  expect_match(out, "<provider encoding=\"UTF-8\">ogr</provider>", fixed = TRUE)
  # terra::vect() gives a mixed POLYGON/MULTIPOLYGON geometry column, cast
  # to the MULTI variant by qgs_homogenize_geometry().
  expect_match(out, 'geometry="Polygon"', fixed = TRUE)

  # The features and their attributes are the ones the SpatVector held.
  written <- sf::st_read(
    file.path(dir, "cyl_data", "cyl.gpkg"),
    quiet = TRUE
  )
  expect_equal(nrow(written), nrow(cyl))
  expect_true(all(c("iso2", "cpro", "name") %in% names(written)))
  expect_equal(sf::st_crs(written), sf::st_crs(terra::crs(cyl)))
})

test_that("a geom_spatvector() layer is styled from the fill scale", {
  skip_if_no_tidyterra()

  cyl <- read_cyl()
  p <- ggplot2::ggplot(cyl) +
    tidyterra::geom_spatvector(ggplot2::aes(fill = name))

  path <- file.path(local_out_dir(), "cyl.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  # A character column is a discrete scale: a categorized renderer.
  expect_match(out, 'type="categorizedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="name"', fixed = TRUE)
  expect_match(out, 'value="Segovia"', fixed = TRUE)
})

test_that("point and line SpatVectors keep their geometry type", {
  skip_if_no_tidyterra()

  cyl <- read_cyl()
  dir <- local_out_dir()

  path <- file.path(dir, "points.qgs")
  write_qgs(
    ggplot2::ggplot() +
      tidyterra::geom_spatvector(data = terra::centroids(cyl)),
    path
  )
  expect_match(read_qgs(path), 'geometry="Point"', fixed = TRUE)

  path <- file.path(dir, "lines.qgs")
  write_qgs(
    ggplot2::ggplot() +
      tidyterra::geom_spatvector(data = terra::as.lines(cyl)),
    path
  )
  # Mixed LINESTRING/MULTILINESTRING, homogenized like the polygons above.
  expect_match(read_qgs(path), 'geometry="LineString"', fixed = TRUE)
})

test_that("geom_spatvector_text() becomes a labels-only layer", {
  skip_if_no_tidyterra()

  cyl <- read_cyl()
  p <- ggplot2::ggplot(cyl) +
    tidyterra::geom_spatvector(ggplot2::aes(fill = name)) +
    tidyterra::geom_spatvector_text(ggplot2::aes(label = iso2))

  dir <- local_out_dir()
  path <- file.path(dir, "cyl.qgs")
  write_qgs(p, path)

  # Two layers out of the one SpatVector: the fill layer and the labels.
  expect_true(file.exists(file.path(dir, "cyl_data", "cyl.gpkg")))
  expect_true(file.exists(file.path(dir, "cyl_data", "cyl_2.gpkg")))

  out <- read_qgs(path)
  expect_match(out, 'labelsEnabled="1"', fixed = TRUE)
  expect_match(out, '<labeling type="simple">', fixed = TRUE)
  expect_match(out, 'fieldName="iso2"', fixed = TRUE)
  # The labels layer itself draws nothing.
  expect_match(out, 'type="nullSymbol"', fixed = TRUE)
})

test_that("a SpatVector layer is named after its data variable", {
  skip_if_no_tidyterra()

  cyl <- read_cyl()

  # geom_spatvector() wraps geom_sf(data = data, ...), so the recorded
  # constructor names the wrapper's own parameter, not `cyl`: the name has
  # to come from the plot environment instead — where the binding still
  # holds the SpatVector, not the fortified sf object the layer got.
  layer_name <- function(p) {
    dir <- local_out_dir()
    path <- file.path(dir, "proj.qgs")
    write_qgs(p, path)
    tools::file_path_sans_ext(list.files(file.path(dir, "proj_data")))
  }

  expect_equal(
    layer_name(ggplot2::ggplot(cyl) + tidyterra::geom_spatvector()),
    "cyl"
  )
  expect_equal(
    layer_name(ggplot2::ggplot() + tidyterra::geom_spatvector(data = cyl)),
    "cyl"
  )
  # tidyterra advertises geom_sf() working directly on a SpatVector.
  expect_equal(
    layer_name(ggplot2::ggplot() + ggplot2::geom_sf(data = cyl)),
    "cyl"
  )
  expect_equal(
    layer_name(ggplot2::ggplot(cyl) + ggplot2::geom_sf()),
    "cyl"
  )
})

test_that("a SpatVector layer with an unnameable source falls back", {
  skip_if_no_tidyterra()

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  # No variable to find: the fallback is the wrapped geom's name, since a
  # wrapper's own name is not recorded anywhere in the layer.
  write_qgs(
    ggplot2::ggplot() +
      tidyterra::geom_spatvector(
        data = terra::vect(
          system.file("extdata/cyl.gpkg", package = "tidyterra")
        )
      ),
    path
  )

  expect_true(file.exists(file.path(dir, "proj_data", "geom_sf.gpkg")))
})

test_that("a SpatVector without a CRS is an error", {
  skip_if_no_tidyterra()

  cyl <- read_cyl()
  terra::crs(cyl) <- ""

  expect_error(
    write_qgs(
      ggplot2::ggplot() + tidyterra::geom_spatvector(data = cyl),
      tempfile(fileext = ".qgs")
    ),
    "no CRS"
  )
})
