test_that("geom_sf_text becomes a labels-only layer", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf() +
    ggplot2::geom_sf_text(ggplot2::aes(label = NAME))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  # stat_sf_coordinates warns about st_point_on_surface on lon/lat data
  suppressWarnings(write_qgs(p, path))

  # Two layers: the polygons and the labels-only layer on top.
  expect_true(file.exists(file.path(dir, "proj_data", "nc.gpkg")))
  expect_true(file.exists(file.path(dir, "proj_data", "nc_2.gpkg")))

  out <- read_qgs(path)
  expect_match(out, 'labelsEnabled="0"', fixed = TRUE)
  expect_match(out, 'labelsEnabled="1"', fixed = TRUE)
  expect_length(
    regmatches(out, gregexpr("<labeling ", out, fixed = TRUE))[[1]],
    1L
  )

  # The labels-only layer draws no features.
  expect_match(out, 'type="nullSymbol"', fixed = TRUE)

  expect_match(out, 'fieldName="NAME"', fixed = TRUE)
  # ggplot2's default text size (in mm, theme-derived) converted to points.
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  size_pt <- round(b@data[[2]]$size[[1]] * 72.27 / 25.4, 7)
  expect_match(out, paste0('fontSize="', num(size_pt), '"'), fixed = TRUE)
  expect_match(out, 'fontSizeUnit="Point"', fixed = TRUE)
  expect_match(out, 'textColor="0,0,0,255,rgb:0,0,0,1"', fixed = TRUE)
  # The default font family keeps the QGIS default font.
  expect_no_match(out, "fontFamily=", fixed = TRUE)

  # geom_sf_text has no background.
  expect_match(out, 'shapeDraw="0"', fixed = TRUE)

  # Polygon labels keep QGIS's polygon placement.
  expect_match(out, 'layerType="PolygonGeometry"', fixed = TRUE)
  expect_match(out, ' placement="0"', fixed = TRUE)
})

test_that("the labels-only gpkg keeps the labeled column", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf_text(ggplot2::aes(label = NAME))

  dir <- local_out_dir()
  # stat_sf_coordinates warns about st_point_on_surface on lon/lat data
  suppressWarnings(write_qgs(p, file.path(dir, "proj.qgs")))

  d <- sf::st_read(
    file.path(dir, "proj_data", "nc.gpkg"),
    layer = "nc",
    quiet = TRUE
  )
  expect_equal(nrow(d), nrow(nc))
  expect_equal(d$NAME, nc$NAME)
})

test_that("geom_sf_label draws a background in the layer's fill", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf_label(ggplot2::aes(label = NAME), fill = "lightblue")

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  # stat_sf_coordinates warns about st_point_on_surface on lon/lat data
  suppressWarnings(write_qgs(p, path))

  out <- read_qgs(path)
  expect_match(out, 'shapeDraw="1"', fixed = TRUE)
  # lightblue is 173,216,230; the rectangle is drawn with the fillSymbol,
  # so its color must carry the fill too.
  expect_match(out, 'shapeFillColor="173,216,230,255,rgb:', fixed = TRUE)
  expect_match(
    out,
    '<Option name="color" type="QString" value="173,216,230,255,rgb:',
    fixed = TRUE
  )
  # A plain rectangle.
  expect_match(out, 'shapeType="0"', fixed = TRUE)
})

test_that("geom_label's fill = NA disables the background", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf_label(ggplot2::aes(label = NAME), fill = NA)

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  # stat_sf_coordinates warns about st_point_on_surface on lon/lat data
  suppressWarnings(write_qgs(p, path))

  out <- read_qgs(path)
  expect_match(out, 'shapeDraw="0"', fixed = TRUE)
})

test_that("text size, family and color are carried over", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf_text(
      ggplot2::aes(label = NAME),
      size = 5,
      family = "Helvetica",
      colour = "red"
    )

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  # stat_sf_coordinates warns about st_point_on_surface on lon/lat data
  suppressWarnings(write_qgs(p, path))

  out <- read_qgs(path)
  # 5 mm * 72.27 / 25.4 pt per mm.
  expect_match(out, 'fontSize="14.226378"', fixed = TRUE)
  expect_match(out, 'fontFamily="Helvetica"', fixed = TRUE)
  expect_match(out, 'textColor="255,0,0,255,rgb:1,0,0,1"', fixed = TRUE)
})

test_that("line labels are placed on the line, polygon ones on the centroid", {
  nc <- read_nc()
  lines <- sf::st_cast(sf::st_geometry(nc)[1:3], "MULTILINESTRING")
  lines <- sf::st_sf(NAME = nc$NAME[1:3], geometry = lines)

  dir <- local_out_dir()
  suppressWarnings({
    write_qgs(
      ggplot2::ggplot(lines) +
        ggplot2::geom_sf_text(ggplot2::aes(label = NAME)),
      file.path(dir, "lines.qgs")
    )
    write_qgs(
      ggplot2::ggplot(nc) + ggplot2::geom_sf_text(ggplot2::aes(label = NAME)),
      file.path(dir, "poly.qgs")
    )
  })

  # ggplot2 centers the text on the geometry, so a line label goes on the
  # line (1 | 8) rather than above it (2 | 8, QGIS's default).
  out <- read_qgs(file.path(dir, "lines.qgs"))
  expect_match(out, 'layerType="LineGeometry"', fixed = TRUE)
  expect_match(out, 'placementFlags="9"', fixed = TRUE)
  # The flag does not apply to a polygon label, which keeps QGIS's default.
  out <- read_qgs(file.path(dir, "poly.qgs"))
  expect_match(out, 'layerType="PolygonGeometry"', fixed = TRUE)
  expect_match(out, 'placementFlags="10"', fixed = TRUE)
})

test_that("a labels-only layer masks nothing", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) + ggplot2::geom_sf_text(ggplot2::aes(label = NAME))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  suppressWarnings(write_qgs(p, path))

  out <- read_qgs(path)
  # Its features are not drawn, so there is nothing under the labels.
  expect_match(out, 'maskEnabled="0"', fixed = TRUE)
  expect_match(out, 'maskedSymbolLayers=""', fixed = TRUE)
  expect_equal(nrow(masked_symbol_layers(out)), 0L)
})

test_that("the text size is in the layer's size.unit", {
  nc <- read_nc()
  text_layer <- function(...) {
    ggplot2::ggplot(nc) +
      ggplot2::geom_sf_text(ggplot2::aes(label = NAME), size = 9, ...)
  }

  dir <- local_out_dir()
  # stat_sf_coordinates warns about st_point_on_surface on lon/lat data
  suppressWarnings({
    write_qgs(text_layer(size.unit = "pt"), file.path(dir, "pt.qgs"))
    write_qgs(text_layer(size.unit = "cm"), file.path(dir, "cm.qgs"))
  })

  # Already points, so no conversion; 9 cm is 10 times 9 mm.
  expect_match(read_qgs(file.path(dir, "pt.qgs")), 'fontSize="9"', fixed = TRUE)
  expect_match(
    read_qgs(file.path(dir, "cm.qgs")),
    paste0('fontSize="', num(round(9 * 10 * 72.27 / 25.4, 7)), '"'),
    fixed = TRUE
  )
})

test_that("an unknown size.unit is an error", {
  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf_text(ggplot2::aes(label = NAME), size.unit = "furlong")
  expect_error(
    suppressWarnings(write_qgs(p, file.path(local_out_dir(), "x.qgs"))),
    "unsupported `size.unit`: furlong"
  )
})

test_that("geom_text on a data.frame becomes labeled points", {
  d <- data.frame(
    lon = c(140.0, 141.0),
    lat = c(42.0, 43.0),
    name = c("a", "b")
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(lon, lat, label = name)) +
    ggplot2::geom_text() +
    ggplot2::coord_sf(crs = 4326)

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  # stat_sf_coordinates warns about st_point_on_surface on lon/lat data
  suppressWarnings(write_qgs(p, path))

  out <- read_qgs(path)
  expect_match(out, 'type="nullSymbol"', fixed = TRUE)
  expect_match(out, 'fieldName="name"', fixed = TRUE)
  # Point labels are drawn over the point, like ggplot2's centered text.
  expect_match(out, 'layerType="PointGeometry"', fixed = TRUE)
  expect_match(out, ' placement="1"', fixed = TRUE)

  # One point per row, all columns kept.
  gpkg <- sf::st_read(file.path(dir, "proj_data", "d.gpkg"), quiet = TRUE)
  expect_equal(nrow(gpkg), 2L)
  expect_equal(gpkg$name, d$name)
})

test_that("a text layer needs a bare-column label aesthetic", {
  nc <- read_nc()

  # A missing label aesthetic fails inside ggplot_build() already (label
  # is a required aesthetic of the text geoms).
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf_text()
  expect_error(
    suppressWarnings(write_qgs(p, tempfile(fileext = ".qgs"))),
    "label"
  )

  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf_text(ggplot2::aes(label = paste(NAME)))
  expect_error(
    suppressWarnings(write_qgs(p, tempfile(fileext = ".qgs"))),
    "only a bare column name is supported for `label`"
  )

  # A label mapped to a missing column fails inside ggplot_build()
  # (evaluating the aesthetics), before the bare-column check.
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf_text(ggplot2::aes(label = no_such))
  expect_error(
    suppressWarnings(write_qgs(p, tempfile(fileext = ".qgs"))),
    "no_such"
  )
})

test_that("nudging a text layer is an error", {
  nc <- read_nc()

  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf_text(ggplot2::aes(label = NAME), nudge_x = 1)
  expect_error(
    suppressWarnings(write_qgs(p, tempfile(fileext = ".qgs"))),
    "nudging"
  )

  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf_text(
      ggplot2::aes(label = NAME),
      position = ggplot2::position_nudge(y = 0.5)
    )
  expect_error(
    suppressWarnings(write_qgs(p, tempfile(fileext = ".qgs"))),
    "nudging"
  )

  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf_text(ggplot2::aes(label = NAME), position = "jitter")
  expect_error(
    suppressWarnings(write_qgs(p, tempfile(fileext = ".qgs"))),
    "nudging"
  )
})

test_that("the labeling block matches the QGIS-saved sample", {
  # samples/nc_text.qgs is the project written for this plot, re-saved by
  # QGIS 4.2 with the labels enabled and the text style set to what this
  # package writes (see the attribute assertions above for the ggplot2
  # defaults; the sample keeps QGIS's own defaults instead, so those
  # attributes are normalized before comparing).
  sample_path <- testthat::test_path("..", "..", "samples", "nc_text.qgs")
  skip_if(!file.exists(sample_path), "samples/nc_text.qgs not available")

  nc <- read_nc()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf() +
    ggplot2::geom_sf_text(ggplot2::aes(label = NAME))

  dir <- local_out_dir()
  path <- file.path(dir, "proj.qgs")
  # stat_sf_coordinates warns about st_point_on_surface on lon/lat data
  suppressWarnings(write_qgs(p, path))

  labeling_block <- function(s) {
    m <- regmatches(
      s,
      regexpr("(?s)<labeling .*?</labeling>", s, perl = TRUE)
    )
    # The callout line symbol embeds a random uuid.
    m <- gsub("\\{[0-9a-f-]{36}\\}", "{uuid}", m)
    m
  }

  ours <- labeling_block(read_qgs(path))
  # Normalize the ggplot2-derived text styles to the sample's QGIS
  # defaults; everything else must match the sample byte for byte.
  ours <- sub('fieldName="NAME" ', 'fieldName="NAME" fontFamily="Open Sans" ', ours)
  ours <- sub('fontSize="[0-9.]+"', 'fontSize="10"', ours)
  ours <- sub(
    'textColor="0,0,0,255,rgb:0,0,0,1"',
    'textColor="50,50,50,255,rgb:0.1960784,0.1960784,0.1960784,1"',
    ours
  )

  expect_equal(ours, labeling_block(read_qgs(sample_path)))
})
