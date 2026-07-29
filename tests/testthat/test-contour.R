# tidyterra::geom_spatraster_contour() layers -> GeoPackage-backed
# LineString layers, geom_spatraster_contour_filled() ones -> Polygon
# layers.

read_volcano_contour <- function() {
  terra::rast(system.file("extdata/volcano2.tif", package = "tidyterra"))
}

contour_plot <- function(r = read_volcano_contour(), ...) {
  ggplot2::ggplot() +
    tidyterra::geom_spatraster_contour(data = r, ...)
}

text_plot <- function(r = read_volcano_contour(), ...) {
  ggplot2::ggplot() +
    tidyterra::geom_spatraster_contour_text(data = r, ...)
}

filled_plot <- function(r = read_volcano_contour(), ...) {
  ggplot2::ggplot() +
    tidyterra::geom_spatraster_contour_filled(data = r, ...)
}

test_that("a geom_spatraster_contour() layer becomes a LineString layer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "volcano.qgs")
  write_qgs(contour_plot(), path)

  # Named after the band with a "_contour" suffix.
  gpkg <- file.path(dir, "volcano_data", "elevation_contour.gpkg")
  expect_true(file.exists(gpkg))

  d <- sf::st_read(gpkg, quiet = TRUE)
  expect_setequal(as.character(sf::st_geometry_type(d)), "LINESTRING")
  # The contour value and the band name are the attributes.
  expect_setequal(setdiff(names(d), attr(d, "sf_column")), c("level", "lyr"))
  expect_setequal(unique(d$lyr), "elevation")
  # volcano2 spans 76.3..195.6, which pretty() breaks every 10 m.
  expect_setequal(sort(unique(d$level)), seq(80, 190, by = 10))

  # One feature per contour piece, not one per level: the built data's
  # `group` is unique per level and per piece.
  b <- ggplot2::ggplot_build(contour_plot())
  expect_equal(nrow(d), length(unique(b@data[[1L]]$group)))

  out <- read_qgs(path)
  expect_match(out, 'geometry="LineString"', fixed = TRUE)
  expect_match(out, "<provider encoding=\"UTF-8\">ogr</provider>", fixed = TRUE)
  expect_match(
    out,
    "<datasource>volcano_data/elevation_contour.gpkg|layername=elevation_contour</datasource>",
    fixed = TRUE
  )
  # No colour mapping: a single symbol in the geom's default grey35 at
  # its default linewidth (0.2 ggplot2 units).
  expect_match(out, 'type="singleSymbol"', fixed = TRUE)
  expect_match(out, 'value="89,89,89,255,rgb:', fixed = TRUE)
  expect_match(
    out,
    '<Option name="line_width" type="QString" value="0.1505625"/>',
    fixed = TRUE
  )
})

test_that("the contour lines carry the geom's breaks", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "breaks.qgs")
  write_qgs(contour_plot(breaks = c(100, 150)), path)

  d <- sf::st_read(
    file.path(dir, "breaks_data", "elevation_contour.gpkg"),
    quiet = TRUE
  )
  expect_setequal(sort(unique(d$level)), c(100, 150))
})

test_that("the layer's CRS is the CRS the stat contoured in", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  # The stat reprojects the raster to the coord CRS before contouring, so
  # the lines are in the coord CRS, not in the raster's EPSG:2193.
  p <- contour_plot() + ggplot2::coord_sf(crs = 4326)

  dir <- local_out_dir()
  path <- file.path(dir, "wgs84.qgs")
  write_qgs(p, path)

  d <- sf::st_read(
    file.path(dir, "wgs84_data", "elevation_contour.gpkg"),
    quiet = TRUE
  )
  expect_true(sf::st_crs(d) == sf::st_crs(4326))
  expect_true(all(abs(sf::st_bbox(d)[c("xmin", "xmax")] - 174.8) < 0.5))
})

test_that("colour mapped to after_stat(level) becomes a graduated renderer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  p <- contour_plot(
    mapping = ggplot2::aes(colour = ggplot2::after_stat(level))
  )

  dir <- local_out_dir()
  path <- file.path(dir, "level.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="level"', fixed = TRUE)

  # The ramp endpoints are the colors ggplot2 maps the scale limits to,
  # on the line's own color (a line symbol has no separate fill).
  scale <- ggplot2::ggplot_build(p)@plot@scales$get_scales("colour")
  ends <- grDevices::col2rgb(scale$map(scale$get_limits()))
  for (j in 1:2) {
    expect_match(
      out,
      sprintf(
        '<Option name="line_color" type="QString" value="%d,%d,%d,255,rgb:',
        ends[1L, j], ends[2L, j], ends[3L, j]
      ),
      fixed = TRUE
    )
  }
})

test_that("gradient_style = \"continuous\" applies to a contour layer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  p <- contour_plot(
    mapping = ggplot2::aes(colour = ggplot2::after_stat(level))
  )

  dir <- local_out_dir()
  path <- file.path(dir, "cont.qgs")
  write_qgs(p, path, gradient_style = "continuous")

  out <- read_qgs(path)
  expect_match(out, 'type="singleSymbol"', fixed = TRUE)
  expect_match(out, "ramp_color(create_ramp(map(", fixed = TRUE)
  # The field reference is XML-escaped inside the attribute value.
  expect_match(out, "&quot;level&quot; - 80", fixed = TRUE)
})

test_that("the constant line aesthetics are carried over", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  p <- contour_plot(
    colour = "red", linewidth = 1, linetype = "dashed", alpha = 0.5
  )

  dir <- local_out_dir()
  path <- file.path(dir, "const.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  # alpha rides along with the line color (255 * 0.5, rounded).
  expect_match(
    out,
    '<Option name="line_color" type="QString" value="255,0,0,128,rgb:',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="line_style" type="QString" value="dash"/>',
    fixed = TRUE
  )
  # 1 ggplot2 linewidth unit is 72.27 / 96 mm.
  expect_match(
    out,
    '<Option name="line_width" type="QString" value="0.7528125"/>',
    fixed = TRUE
  )
})

test_that("contour lines over the raster become two named layers", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  # tidyterra's own example: the raster plus its contour lines. Both
  # layers derive their name from the same band, so the contour one takes
  # the "_contour" suffix instead of a collision number.
  r <- read_volcano_contour()
  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = r) +
    tidyterra::geom_spatraster_contour(data = r)

  dir <- local_out_dir()
  path <- file.path(dir, "both.qgs")
  write_qgs(p, path)

  expect_setequal(
    list.files(file.path(dir, "both_data")),
    c("elevation.tif", "elevation_contour.gpkg")
  )

  out <- read_qgs(path)
  # The two CRS helper layers are skipped.
  expect_length(gregexpr("<maplayer", out, fixed = TRUE)[[1L]], 2L)
  expect_match(out, 'name="elevation"', fixed = TRUE)
  expect_match(out, 'name="elevation_contour"', fixed = TRUE)
  expect_match(out, 'type="singlebandpseudocolor"', fixed = TRUE)
  expect_match(out, 'geometry="LineString"', fixed = TRUE)
})

test_that("layer_names overrides the contour layer name", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "named.qgs")
  write_qgs(contour_plot(), path, layer_names = "isolines")

  expect_true(file.exists(file.path(dir, "named_data", "isolines.gpkg")))
  expect_match(read_qgs(path), 'name="isolines"', fixed = TRUE)
})

test_that("a colour mapping other than after_stat(level) is an error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  p <- contour_plot(
    mapping = ggplot2::aes(colour = ggplot2::after_stat(nlevel))
  )
  expect_error(
    write_qgs(p, file.path(local_out_dir(), "x.qgs")),
    "only `after_stat\\(level\\)` is supported for `colour`"
  )
})

test_that("a fill mapping on a contour layer is an error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  p <- suppressWarnings(
    contour_plot(mapping = ggplot2::aes(fill = ggplot2::after_stat(level)))
  )
  expect_error(
    write_qgs(p, file.path(local_out_dir(), "x.qgs")),
    "draws lines, so a `fill` mapping is not supported"
  )
})

test_that("a multi-band SpatRaster contour is an error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  r <- read_volcano_contour()
  two <- c(r, r * 2)
  names(two) <- c("a", "b")
  # tidyterra itself warns about the overlapping bands while building.
  expect_error(
    suppressMessages(
      write_qgs(contour_plot(two), file.path(local_out_dir(), "x.qgs"))
    ),
    "multi-band SpatRaster is not supported"
  )
})

test_that("an unknown geom on a contour stat is an explicit error", {
  # It would otherwise reach the data.frame path, whose error blames the
  # wrong thing. Faked rather than built: every geom tidyterra puts on
  # these stats is supported.
  fake <- list(
    stat = structure(list(), class = c("StatTerraSpatRasterContour", "Stat")),
    geom = structure(list(), class = c("GeomPoint", "Geom"))
  )
  expect_error(
    qgs_check_contour_geom(fake, 3L),
    "layer 3: unsupported geom on a StatTerraSpatRasterContour layer: GeomPoint",
    fixed = TRUE
  )
})

test_that("a geom_spatraster_contour_text() layer is a labeled line layer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "volcano.qgs")
  write_qgs(text_plot(), path)

  # Named after the band with a "_contour_text" suffix.
  gpkg <- file.path(dir, "volcano_data", "elevation_contour_text.gpkg")
  expect_true(file.exists(gpkg))

  d <- sf::st_read(gpkg, quiet = TRUE)
  expect_setequal(as.character(sf::st_geometry_type(d)), "LINESTRING")
  # The isoline layer's attributes plus the text drawn along the line.
  expect_setequal(
    setdiff(names(d), attr(d, "sf_column")),
    c("level", "lyr", "label")
  )
  # The same lines as geom_spatraster_contour(), one feature per piece.
  write_qgs(contour_plot(), file.path(dir, "lines.qgs"))
  lines <- sf::st_read(
    file.path(dir, "lines_data", "elevation_contour.gpkg"),
    quiet = TRUE
  )
  expect_equal(sf::st_geometry(d), sf::st_geometry(lines))

  out <- read_qgs(path)
  expect_match(out, 'geometry="LineString"', fixed = TRUE)
  # The lines are drawn *and* labeled: the layer keeps its line symbol
  # instead of the null one a labels-only layer gets.
  expect_match(out, 'type="singleSymbol"', fixed = TRUE)
  expect_match(out, 'labelsEnabled="1"', fixed = TRUE)
  expect_match(out, 'fieldName="label"', fixed = TRUE)
  # Placed along the line (placement 2) and *on* it (placementFlags
  # 1 | 8 = OnLine | MapOrientation), where the geom writes the value.
  expect_match(out, 'layerType="LineGeometry"', fixed = TRUE)
  expect_match(out, ' placement="2"')
  expect_match(out, 'placementFlags="9"', fixed = TRUE)
})

test_that("the contour labels mask the lines they are written into", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "masked.qgs")
  write_qgs(text_plot(), path)
  out <- read_qgs(path)

  # The label erases the line under it, the way isolines_grob() breaks
  # each line where its label sits.
  expect_match(out, 'maskEnabled="1"', fixed = TRUE)
  # One <layer id>;<symbol layer id> pair, naming this layer's own single
  # symbol layer.
  expect_equal(
    masked_symbol_layers(out),
    data.frame(layer = layer_ids(out), symbol = symbol_layer_ids(out))
  )
})

test_that("a masking label names every class of a graduated renderer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "graduated.qgs")
  p <- text_plot(mapping = ggplot2::aes(colour = ggplot2::after_stat(level)))
  suppressWarnings(write_qgs(p, path))
  out <- read_qgs(path)

  # Every class draws its own line, so every class's symbol layer has to
  # be masked — but not the <source-symbol> template, which draws nothing.
  masked <- masked_symbol_layers(out)
  expect_equal(nrow(masked), 25L)
  expect_setequal(masked$layer, layer_ids(out))
  expect_true(all(masked$symbol %in% symbol_layer_ids(out)))
  expect_false(any(duplicated(masked$symbol)))
})

test_that("a contour text layer masks only its own symbol layers", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "both.qgs")
  # The bands are written first, so their symbol layers must not leak into
  # the mask of the layer above them.
  p <- filled_plot() + tidyterra::geom_spatraster_contour_text(
    data = read_volcano_contour()
  )
  write_qgs(p, path)
  out <- read_qgs(path)

  masked <- masked_symbol_layers(out)
  expect_equal(nrow(masked), 1L)
  expect_equal(masked$layer, layer_ids(out, "elevation_contour_text"))
})

test_that("the contour labels are the geom's formatted contour values", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  write_qgs(text_plot(), file.path(dir, "default.qgs"))
  d <- sf::st_read(
    file.path(dir, "default_data", "elevation_contour_text.gpkg"),
    quiet = TRUE
  )
  # scales::label_number(), the geom's default `label_format`.
  expect_equal(d$label, as.character(d$level))

  # A custom label_format is reproduced as it is.
  write_qgs(
    text_plot(label_format = scales::label_number(suffix = " m")),
    file.path(dir, "suffix.qgs")
  )
  d <- sf::st_read(
    file.path(dir, "suffix_data", "elevation_contour_text.gpkg"),
    quiet = TRUE
  )
  expect_equal(d$label, paste0(d$level, " m"))
})

test_that("a character label_format labels the levels positionally", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  write_qgs(
    text_plot(breaks = c(100, 150), label_format = c("low", "high")),
    file.path(dir, "labels.qgs")
  )
  d <- sf::st_read(
    file.path(dir, "labels_data", "elevation_contour_text.gpkg"),
    quiet = TRUE
  )
  expect_equal(d$label, ifelse(d$level == 100, "low", "high"))

  # One label per level, like the geom requires when it draws.
  expect_error(
    write_qgs(
      text_plot(breaks = c(100, 150), label_format = "only"),
      file.path(dir, "short.qgs")
    ),
    "one label per contour level \\(2\\), got 1"
  )
})

test_that("label_format = NULL leaves the contour lines unlabeled", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "bare.qgs")
  write_qgs(text_plot(label_format = NULL), path)

  # The geom places no labels at all, so there is nothing to label with.
  d <- sf::st_read(
    file.path(dir, "bare_data", "elevation_contour_text.gpkg"),
    quiet = TRUE
  )
  expect_false("label" %in% names(d))

  out <- read_qgs(path)
  expect_no_match(out, 'labelsEnabled="1"', fixed = TRUE)
  expect_no_match(out, "<labeling ", fixed = TRUE)
})

test_that("the contour label's text styles are the geom's", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "styled.qgs")
  write_qgs(
    text_plot(size = 5, family = "Helvetica", colour = "red"),
    path
  )

  out <- read_qgs(path)
  # 5 mm * 72.27 / 25.4 pt per mm, like a text layer's.
  expect_match(out, 'fontSize="14.226378"', fixed = TRUE)
  expect_match(out, 'fontFamily="Helvetica"', fixed = TRUE)
  # `colour` is the color of the labels and of the lines.
  expect_match(out, 'textColor="255,0,0,255,rgb:1,0,0,1"', fixed = TRUE)
  expect_match(
    out,
    '<Option name="line_color" type="QString" value="255,0,0,255,rgb:1,0,0,1"/>',
    fixed = TRUE
  )
})

test_that("the geom's `size.unit` is the unit of the label size", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "pt.qgs")
  write_qgs(text_plot(size = 9, size.unit = "pt"), path)

  # Already points, so no millimeter conversion.
  expect_match(read_qgs(path), 'fontSize="9"', fixed = TRUE)
})

test_that("the geom's default family leaves QGIS its own font", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "sans.qgs")
  write_qgs(text_plot(), path)

  # "sans" is R's device alias for the default sans-serif font, not the
  # name of one, so no fontFamily attribute is written.
  expect_no_match(read_qgs(path), "fontFamily=", fixed = TRUE)
})

test_that("a contour text layer's colour mapping colors the lines", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "colour.qgs")
  p <- text_plot(mapping = ggplot2::aes(colour = ggplot2::after_stat(level))) +
    ggplot2::scale_colour_viridis_c()
  # The lines follow the scale, but a QGIS labeling has one text color, so
  # the labels drop down to the first feature's like the other aesthetics
  # these renderers cannot vary.
  expect_warning(
    write_qgs(p, path),
    "the first feature's color is used for every label"
  )

  out <- read_qgs(path)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="level"', fixed = TRUE)
  # viridis starts at #440154, the color of the lowest level.
  expect_match(out, 'textColor="68,1,84,255,rgb:', fixed = TRUE)
})

test_that("an unlabeled contour text layer's colour mapping is silent", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "bare.qgs")
  p <- text_plot(
    mapping = ggplot2::aes(colour = ggplot2::after_stat(level)),
    label_format = NULL
  )
  # No labels, so there is nothing the renderer fails to color.
  expect_no_warning(write_qgs(p, path))

  out <- read_qgs(path)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_no_match(out, 'labelsEnabled="1"', fixed = TRUE)
})

test_that("only after_stat(level) may color a contour text layer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  p <- text_plot(mapping = ggplot2::aes(colour = ggplot2::after_stat(nlevel)))
  expect_error(
    write_qgs(p, file.path(dir, "nlevel.qgs")),
    "only `after_stat\\(level\\)` is supported for `colour`"
  )

  # The geom draws no fill at all.
  p <- suppressWarnings(
    text_plot(mapping = ggplot2::aes(fill = ggplot2::after_stat(level)))
  )
  expect_error(
    write_qgs(p, file.path(dir, "fill.qgs")),
    "so a `fill` mapping is not supported"
  )
})

test_that("a multi-band SpatRaster contour text layer is an error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  r <- read_volcano_contour()
  two <- c(r, r * 2)
  names(two) <- c("a", "b")
  expect_error(
    suppressMessages(
      write_qgs(text_plot(two), file.path(local_out_dir(), "x.qgs"))
    ),
    "multi-band SpatRaster is not supported"
  )
})

test_that("a geom_spatraster_contour_filled() layer becomes a Polygon layer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "volcano.qgs")
  write_qgs(filled_plot(), path)

  # Named after the band with a "_contour_filled" suffix.
  gpkg <- file.path(dir, "volcano_data", "elevation_contour_filled.gpkg")
  expect_true(file.exists(gpkg))

  d <- sf::st_read(gpkg, quiet = TRUE)
  expect_setequal(as.character(sf::st_geometry_type(d)), "MULTIPOLYGON")
  # The band label and the band name are the attributes.
  expect_setequal(setdiff(names(d), attr(d, "sf_column")), c("level", "lyr"))
  expect_setequal(unique(d$lyr), "elevation")

  # One feature per band, in the order and with the labels the stat's
  # `level` factor has.
  b <- ggplot2::ggplot_build(filled_plot())
  levels <- levels(droplevels(b@data[[1L]]$level))
  expect_equal(d$level, levels)

  out <- read_qgs(path)
  expect_match(out, 'geometry="Polygon"', fixed = TRUE)
  expect_match(
    out,
    paste0(
      "<datasource>volcano_data/elevation_contour_filled.gpkg",
      "|layername=elevation_contour_filled</datasource>"
    ),
    fixed = TRUE
  )
})

test_that("a contour band keeps the holes of the bands above it", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "holes.qgs")
  write_qgs(filled_plot(), path)

  d <- sf::st_read(
    file.path(dir, "holes_data", "elevation_contour_filled.gpkg"),
    quiet = TRUE
  )

  # The bands are the areas between two isolines, so a band enclosing a
  # higher one is a ring, not a disc: the lowest band of volcano2 covers
  # the raster's rim and has a hole punched by the (80, 90] contour.
  rings <- vapply(
    sf::st_geometry(d),
    function(g) sum(vapply(g, length, integer(1L))),
    integer(1L)
  )
  expect_gt(max(rings), 1L)
  # Bands do not overlap, so their areas add up to the contoured region
  # (the isobands stop half a cell short of the raster's edge).
  total <- sum(as.numeric(sf::st_area(d)))
  r <- read_volcano_contour()
  expect_lt(total, prod(dim(r)[1:2]) * prod(terra::res(r)))
  expect_gt(total, 0.9 * prod(dim(r)[1:2]) * prod(terra::res(r)))
})

test_that("the contour bands carry the geom's breaks", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "breaks.qgs")
  write_qgs(filled_plot(breaks = c(100, 150, 200)), path)

  d <- sf::st_read(
    file.path(dir, "breaks_data", "elevation_contour_filled.gpkg"),
    quiet = TRUE
  )
  expect_equal(d$level, c("(100, 150]", "(150, 200]"))
})

test_that("the filled bands become a categorized renderer on `level`", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  p <- filled_plot()

  dir <- local_out_dir()
  path <- file.path(dir, "cat.qgs")
  write_qgs(p, path)

  out <- read_qgs(path)
  expect_match(out, 'type="categorizedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="level"', fixed = TRUE)

  # One category per level of the trained fill scale, each in the color
  # ggplot2 maps that level to, on the polygon's fill.
  scale <- ggplot2::ggplot_build(p)@plot@scales$get_scales("fill")
  levels <- scale$get_breaks()
  rgb <- grDevices::col2rgb(scale$map(levels))
  for (j in seq_along(levels)) {
    expect_match(
      out,
      sprintf('value="%s"/>', levels[[j]]),
      fixed = TRUE
    )
    expect_match(
      out,
      sprintf(
        '<Option name="color" type="QString" value="%d,%d,%d,255,rgb:',
        rgb[1L, j], rgb[2L, j], rgb[3L, j]
      ),
      fixed = TRUE
    )
  }

  # The geom's default `colour` is NA, so the bands have no border.
  expect_match(
    out,
    '<Option name="outline_style" type="QString" value="no"/>',
    fixed = TRUE
  )
})

test_that("the constant polygon aesthetics of a filled layer are carried over", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "const.qgs")
  write_qgs(
    filled_plot(colour = "red", linewidth = 1, linetype = "dashed", alpha = 0.5),
    path
  )

  out <- read_qgs(path)
  expect_match(
    out,
    '<Option name="outline_color" type="QString" value="255,0,0,255,rgb:',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="outline_style" type="QString" value="dash"/>',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="outline_width" type="QString" value="0.7528125"/>',
    fixed = TRUE
  )
  # `alpha` applies to the interior only (the border above stays at 255),
  # as ggplot2 draws it: it rides along with each category's fill color.
  scale <- ggplot2::ggplot_build(filled_plot())@plot@scales$get_scales("fill")
  rgb <- grDevices::col2rgb(scale$map(scale$get_breaks()[[1L]]))
  expect_match(
    out,
    sprintf(
      '<Option name="color" type="QString" value="%d,%d,%d,128,rgb:',
      rgb[1L, 1L], rgb[2L, 1L], rgb[3L, 1L]
    ),
    fixed = TRUE
  )
})

test_that("the filled layer's CRS is the CRS the stat contoured in", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  dir <- local_out_dir()
  path <- file.path(dir, "wgs84.qgs")
  write_qgs(filled_plot() + ggplot2::coord_sf(crs = 4326), path)

  d <- sf::st_read(
    file.path(dir, "wgs84_data", "elevation_contour_filled.gpkg"),
    quiet = TRUE
  )
  expect_true(sf::st_crs(d) == sf::st_crs(4326))
  expect_true(all(abs(sf::st_bbox(d)[c("xmin", "xmax")] - 174.8) < 0.5))
})

test_that("the bands and the lines of one band become two named layers", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  # Both layers derive their name from the same band, so each takes its
  # own suffix instead of a collision number. (The raster itself cannot
  # join them: its continuous fill scale and the bands' discrete one
  # cannot coexist in one plot.)
  r <- read_volcano_contour()
  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster_contour_filled(data = r) +
    tidyterra::geom_spatraster_contour(data = r)

  dir <- local_out_dir()
  path <- file.path(dir, "both.qgs")
  write_qgs(p, path)

  expect_setequal(
    list.files(file.path(dir, "both_data")),
    c("elevation_contour_filled.gpkg", "elevation_contour.gpkg")
  )

  out <- read_qgs(path)
  expect_match(out, 'name="elevation_contour_filled"', fixed = TRUE)
  expect_match(out, 'name="elevation_contour"', fixed = TRUE)
  expect_match(out, 'geometry="Polygon"', fixed = TRUE)
  expect_match(out, 'geometry="LineString"', fixed = TRUE)
})

test_that("a fill mapping other than after_stat(level) is an error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  p <- filled_plot(
    mapping = ggplot2::aes(fill = ggplot2::after_stat(level_mid))
  )
  expect_error(
    write_qgs(p, file.path(local_out_dir(), "x.qgs")),
    "only `after_stat\\(level\\)` is supported for `fill`"
  )
})

test_that("a colour mapping on a filled layer is an error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  p <- filled_plot(
    mapping = ggplot2::aes(colour = ggplot2::after_stat(level))
  )
  expect_error(
    write_qgs(p, file.path(local_out_dir(), "x.qgs")),
    "varies its `fill`, so a `colour` mapping is not supported"
  )
})

test_that("a multi-band SpatRaster of filled bands is an error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("tidyterra")

  r <- read_volcano_contour()
  two <- c(r, r * 2)
  names(two) <- c("a", "b")
  expect_error(
    suppressMessages(
      write_qgs(filled_plot(two), file.path(local_out_dir(), "x.qgs"))
    ),
    "multi-band SpatRaster is not supported"
  )
})
