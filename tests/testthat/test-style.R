bw_stops <- function() {
  list(
    offsets = c(0, 1),
    colors = cbind(c(0L, 0L, 0L), c(255L, 255L, 255L))
  )
}

render <- function(geom, style) {
  w <- xml_writer(0L)
  write_renderer(w, geom, style)
  xw_finish(w)
}

test_that("a categorized renderer has categories, symbols and a catch-all", {
  style <- style_categorized(
    "NAME",
    c("Alamance", "Alexander"),
    cbind(c(255L, 255L, 255L), c(255L, 252L, 252L)),
    catch_all = c(255L, 0L, 0L)
  )
  out <- render("Polygon", style)

  expect_match(out, 'type="categorizedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="NAME"', fixed = TRUE)
  # Categories reference symbols by index...
  expect_match(
    out,
    '<category label="Alamance" render="true" symbol="0" type="string"',
    fixed = TRUE
  )
  expect_match(out, 'value="Alamance"', fixed = TRUE)
  expect_match(
    out,
    '<category label="Alexander" render="true" symbol="1" type="string"',
    fixed = TRUE
  )
  # ...and the catch-all is a NULL category with the next index.
  expect_match(
    out,
    '<category label="" render="true" symbol="2" type="NULL"',
    fixed = TRUE
  )
  expect_match(out, 'value="NULL"', fixed = TRUE)
  # Symbols are named with the same ids, colors included.
  expect_match(out, 'name="0" type="fill"', fixed = TRUE)
  expect_match(out, 'name="2" type="fill"', fixed = TRUE)
  expect_match(out, "255,255,255,255,rgb:1,1,1,1", fixed = TRUE)
  expect_match(out, "255,252,252,255,rgb:1,0.9882353,0.9882353,1", fixed = TRUE)
  expect_match(out, "255,0,0,255,rgb:1,0,0,1", fixed = TRUE)
})

test_that("a continuous renderer drives the color by an expression", {
  style <- style_continuous(
    "AREA", 0.042, 0.241,
    list(
      offsets = c(0, 0.5, 1),
      colors = cbind(c(19L, 43L, 67L), c(45L, 96L, 141L), c(86L, 177L, 247L))
    )
  )
  out <- render("Polygon", style)

  expect_match(out, 'type="singleSymbol"', fixed = TRUE)
  # The fill color is driven by a data-defined expression...
  expect_match(out, '<Option name="fillColor" type="Map">', fixed = TRUE)
  expect_match(out, 'name="active" type="bool" value="true"', fixed = TRUE)
  expect_match(out, 'name="type" type="int" value="3"', fixed = TRUE)
  # ...that interpolates the inline ramp over the rescaled attribute.
  expect_match(
    out,
    paste0(
      "ramp_color(create_ramp(map(0,'#132b43',0.5,'#2d608d',1,'#56b1f7')),",
      "(&quot;AREA&quot; - 0.042) / (0.241 - 0.042))"
    ),
    fixed = TRUE
  )
  # The static color is the middle of the ramp.
  expect_match(out, "45,96,141,255,rgb:", fixed = TRUE)
})

test_that("a continuous line color is a data-defined stroke", {
  style <- style_continuous("x", 0, 1, bw_stops())
  out <- render("LineString", style)
  # SimpleLine's color is its stroke, so the override targets
  # outlineColor rather than fillColor.
  expect_match(out, '<Option name="outlineColor" type="Map">', fixed = TRUE)
  expect_no_match(out, '<Option name="fillColor"', fixed = TRUE)
})

test_that("field names are escaped in expressions", {
  expect_equal(quote_field("AREA"), "\"AREA\"")
  expect_equal(quote_field("odd\"name"), "\"odd\"\"name\"")
})

test_that("narrow graduated classes get distinguishable labels", {
  # The nc AREA case: 25 classes over 0.042..0.241 used to produce
  # duplicate labels like "0.1 - 0.1".
  style <- style_graduated("AREA", 25, 0.042, 0.241, bw_stops())
  out <- render("Polygon", style)

  expect_match(out, 'label="0.042 - 0.05"', fixed = TRUE)
  expect_match(out, 'label="0.05 - 0.058"', fixed = TRUE)
  expect_match(out, 'labelprecision="3"', fixed = TRUE)
  # No range collapses into an empty "x - x" label.
  labels <- regmatches(out, gregexpr('label="[^"]*"', out))[[1]]
  labels <- labels[grepl(" - ", labels, fixed = TRUE)]
  expect_length(labels, 25L)
  for (label in labels) {
    bounds <- strsplit(sub('label="([^"]*)"', "\\1", label), " - ")[[1]]
    expect_false(identical(bounds[1], bounds[2]), label = label)
  }
})

test_that("a graduated renderer has ranges, symbols and a colorramp", {
  # samples/red.qgs: white -> red in 6 classes over 0..57.
  style <- style_graduated(
    "SID79", 6, 0, 57,
    list(
      offsets = c(0, 1),
      colors = cbind(c(255L, 255L, 255L), c(255L, 0L, 0L))
    )
  )
  out <- render("Polygon", style)

  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="SID79"', fixed = TRUE)
  expect_match(out, 'graduatedMethod="GraduatedColor"', fixed = TRUE)
  expect_length(regmatches(out, gregexpr("<range ", out, fixed = TRUE))[[1]], 6L)
  expect_match(out, 'label="0 - 9.5"', fixed = TRUE)
  expect_match(out, 'lower="0.000000000000000"', fixed = TRUE)
  expect_match(out, 'upper="9.500000000000000"', fixed = TRUE)
  # Interpolated class colors (see the sample_ramp tests).
  expect_match(out, "255,204,204,255,rgb:1,0.8,0.8,1", fixed = TRUE)
  # colorramp endpoints.
  expect_match(
    out,
    '<Option name="color1" type="QString" value="255,255,255,255,rgb:1,1,1,1"/>',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="color2" type="QString" value="255,0,0,255,rgb:1,0,0,1"/>',
    fixed = TRUE
  )
  # A two-stop ramp has no intermediate stops option.
  expect_no_match(out, '<Option name="stops"', fixed = TRUE)
  expect_match(out, '<classificationMethod id="Pretty">', fixed = TRUE)
})

test_that("intermediate ramp stops are serialized into the colorramp", {
  style <- style_graduated(
    "x", 2, 0, 1,
    list(
      offsets = c(0, 0.5, 1),
      colors = cbind(c(0L, 0L, 0L), c(255L, 0L, 0L), c(255L, 255L, 255L))
    )
  )
  out <- render("Polygon", style)
  expect_match(
    out,
    '<Option name="stops" type="QString" value="0.5;255,0,0,255,rgb:1,0,0,1;rgb;ccw"/>',
    fixed = TRUE
  )
})

test_that("a binned renderer has one explicit range per bin", {
  # scale_fill_steps(breaks = c(0.08, 0.12, 0.2)) on nc AREA: unequal bins
  # over 0.042..0.241 with the scale's exact bin colors.
  style <- style_binned(
    "AREA",
    c(0.042, 0.08, 0.12, 0.2, 0.241),
    cbind(
      c(25L, 54L, 82L), c(37L, 79L, 115L), c(57L, 119L, 169L),
      c(79L, 162L, 227L)
    )
  )
  out <- render("Polygon", style)

  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="AREA"', fixed = TRUE)
  expect_match(out, 'graduatedMethod="GraduatedColor"', fixed = TRUE)
  expect_length(regmatches(out, gregexpr("<range ", out, fixed = TRUE))[[1]], 4L)
  # Boundaries are the scale's exact bin edges, not equal intervals.
  expect_match(out, 'lower="0.042000000000000"', fixed = TRUE)
  expect_match(out, 'upper="0.080000000000000"', fixed = TRUE)
  expect_match(out, 'lower="0.120000000000000"', fixed = TRUE)
  expect_match(out, 'upper="0.241000000000000"', fixed = TRUE)
  expect_match(out, 'label="0.042 - 0.08"', fixed = TRUE)
  # Each bin keeps its exact color (no ramp interpolation).
  expect_match(out, "25,54,82,255,rgb:", fixed = TRUE)
  expect_match(out, "37,79,115,255,rgb:", fixed = TRUE)
  expect_match(out, "57,119,169,255,rgb:", fixed = TRUE)
  expect_match(out, "79,162,227,255,rgb:", fixed = TRUE)
  # colorramp endpoints are the first/last bin colors.
  expect_match(
    out,
    '<Option name="color1" type="QString" value="25,54,82,255,rgb:',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="color2" type="QString" value="79,162,227,255,rgb:',
    fixed = TRUE
  )
})

test_that("a single-bin style renders one range", {
  style <- style_binned("x", c(0, 1), cbind(c(10L, 20L, 30L)))
  out <- render("Polygon", style)
  expect_length(regmatches(out, gregexpr("<range ", out, fixed = TRUE))[[1]], 1L)
  expect_match(out, 'lower="0.000000000000000"', fixed = TRUE)
  expect_match(out, 'upper="1.000000000000000"', fixed = TRUE)
})

test_that("a stroke target moves the bin colors to the outline", {
  style <- style_binned(
    "AREA", c(0, 0.5, 1),
    cbind(c(10L, 20L, 30L), c(200L, 210L, 220L))
  )
  style <- style_set_stroke_target(style, c(229L, 229L, 229L))
  out <- render("Polygon", style)

  expect_match(
    out,
    '<Option name="outline_color" type="QString" value="10,20,30,255,rgb:',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="outline_color" type="QString" value="200,210,220,255,rgb:',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="color" type="QString" value="229,229,229,255,rgb:',
    fixed = TRUE
  )
  expect_no_match(
    out,
    '<Option name="color" type="QString" value="10,20,30,255,rgb:',
    fixed = TRUE
  )
})

test_that("invalid binned styles are errors", {
  expect_error(
    style_binned("x", 0, matrix(integer(), nrow = 3)),
    "at least 1 bin"
  )
  expect_error(
    style_binned("x", c(0, 1, 1), cbind(c(0L, 0L, 0L), c(1L, 1L, 1L))),
    "strictly ascending"
  )
  expect_error(
    style_binned("x", c(0, 0.5, 1), cbind(c(0L, 0L, 0L))),
    "matching lengths"
  )
})

test_that("set_outline applies to every variant", {
  style <- style_single(c(229L, 229L, 229L))
  style <- style_set_outline(style, c(89L, 89L, 89L), 0.1505625)
  out <- render("Polygon", style)
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

test_that("a NULL fill color renders the polygon fill as not drawn", {
  style <- style_single(NULL)
  style <- style_set_outline(style, c(0L, 0L, 0L), 0.2)
  out <- render("Polygon", style)

  expect_match(out, '<Option name="style" type="QString" value="no"/>',
    fixed = TRUE
  )
  # The ignored color value stays at the QGIS default.
  expect_match(
    out,
    '<Option name="color" type="QString" value="229,229,229,255,rgb:',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="outline_style" type="QString" value="solid"/>',
    fixed = TRUE
  )
})

test_that("a NULL outline color renders the outline as not drawn", {
  style <- style_single(c(51L, 51L, 51L))
  style <- style_set_outline(style, NULL, 0.2)

  for (geom in c("Polygon", "Point")) {
    out <- render(geom, style)
    expect_match(
      out,
      '<Option name="outline_style" type="QString" value="no"/>',
      fixed = TRUE
    )
    # The ignored color value stays at the QGIS default.
    expect_match(
      out,
      '<Option name="outline_color" type="QString" value="35,35,35,255,rgb:',
      fixed = TRUE
    )
    # The main color still draws.
    expect_match(
      out,
      '<Option name="color" type="QString" value="51,51,51,255,rgb:',
      fixed = TRUE
    )
  }
})

test_that("preset linetypes map to outline_style / line_style", {
  style <- style_set_linetype(style_single(c(51L, 51L, 51L)), "dash")
  for (geom in c("Polygon", "Point")) {
    out <- render(geom, style)
    expect_match(
      out,
      '<Option name="outline_style" type="QString" value="dash"/>',
      fixed = TRUE
    )
  }
  out <- render("LineString", style)
  expect_match(
    out,
    '<Option name="line_style" type="QString" value="dash"/>',
    fixed = TRUE
  )
  # The preset needs no custom dash; the placeholder stays inactive.
  expect_match(
    out,
    '<Option name="use_custom_dash" type="QString" value="0"/>',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="customdash" type="QString" value="5;2"/>',
    fixed = TRUE
  )
})

test_that("run-length linetypes become an exact custom dash on lines", {
  # longdash on a 0.5 mm line: 7 and 3 line widths.
  style <- style_single(c(0L, 0L, 0L))
  style <- style_set_outline(style, c(0L, 0L, 0L), 0.5)
  style <- style_set_linetype(style, c(7L, 3L))
  out <- render("LineString", style)

  expect_match(
    out,
    '<Option name="use_custom_dash" type="QString" value="1"/>',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="customdash" type="QString" value="3.5;1.5"/>',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="line_style" type="QString" value="solid"/>',
    fixed = TRUE
  )
})

test_that("run-length linetypes are approximated off lines", {
  expect_equal(linetype_preset(c(7L, 3L)), "dash") # longdash
  expect_equal(linetype_preset(c(2L, 2L, 6L, 2L)), "dash dot") # twodash
  expect_equal(linetype_preset(c(1L, 3L)), "dot")
  expect_equal(linetype_preset("dash dot"), "dash dot") # presets pass through

  # SimpleFill has no custom dash, so twodash lands on the nearest preset.
  style <- style_set_linetype(style_single(c(0L, 0L, 0L)), c(2L, 2L, 6L, 2L))
  out <- render("Polygon", style)
  expect_match(
    out,
    '<Option name="outline_style" type="QString" value="dash dot"/>',
    fixed = TRUE
  )
})

test_that("a zero line width falls back to the preset on lines", {
  style <- style_single(c(0L, 0L, 0L))
  style <- style_set_outline(style, c(0L, 0L, 0L), 0)
  style <- style_set_linetype(style, c(7L, 3L))
  out <- render("LineString", style)

  expect_match(
    out,
    '<Option name="line_style" type="QString" value="dash"/>',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="use_custom_dash" type="QString" value="0"/>',
    fixed = TRUE
  )
})

test_that("a blank linetype turns the outline off", {
  style <- style_set_linetype(style_single(c(51L, 51L, 51L)), "no")
  for (geom in c("Polygon", "Point")) {
    out <- render(geom, style)
    expect_match(
      out,
      '<Option name="outline_style" type="QString" value="no"/>',
      fixed = TRUE
    )
  }
})

test_that("the linetype applies to every symbol of a mapped style", {
  style <- style_graduated("x", 2, 0, 1, bw_stops())
  style <- style_set_linetype(style, "dash")
  out <- render("Polygon", style)
  # 2 class symbols + the source-symbol.
  expect_length(
    regmatches(
      out,
      gregexpr(
        '<Option name="outline_style" type="QString" value="dash"/>',
        out,
        fixed = TRUE
      )
    )[[1]],
    3L
  )
})

test_that("a stroke target moves the ramp to the outline", {
  style <- style_graduated(
    "AREA", 2, 0, 1,
    list(
      offsets = c(0, 1),
      colors = cbind(c(10L, 20L, 30L), c(200L, 210L, 220L))
    )
  )
  style <- style_set_stroke_target(style, c(229L, 229L, 229L))
  out <- render("Polygon", style)

  # The ramp endpoints land on the outline...
  expect_match(
    out,
    '<Option name="outline_color" type="QString" value="10,20,30,255,rgb:',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="outline_color" type="QString" value="200,210,220,255,rgb:',
    fixed = TRUE
  )
  # ...and every symbol's fill is the shared constant.
  expect_match(
    out,
    '<Option name="color" type="QString" value="229,229,229,255,rgb:',
    fixed = TRUE
  )
  expect_no_match(
    out,
    '<Option name="color" type="QString" value="10,20,30,255,rgb:',
    fixed = TRUE
  )
})

test_that("a stroke target switches the continuous dd property", {
  style <- style_continuous("AREA", 0, 1, bw_stops())
  style <- style_set_stroke_target(style, c(229L, 229L, 229L))
  out <- render("Polygon", style)

  expect_match(out, '<Option name="outlineColor" type="Map">', fixed = TRUE)
  expect_no_match(out, '<Option name="fillColor"', fixed = TRUE)
  expect_match(out, "ramp_color(create_ramp(", fixed = TRUE)
})

test_that("a stroke target on a single symbol is an error", {
  expect_error(
    style_set_stroke_target(style_single(c(0L, 0L, 0L)), c(229L, 229L, 229L)),
    "no varying color"
  )
})

test_that("invalid styles are errors", {
  stops <- bw_stops()
  expect_error(
    style_graduated("x", 1, 0, 1, stops),
    "at least 2 classes, got 1"
  )
  expect_error(
    style_graduated("x", 2, 1, 1, stops),
    "invalid range"
  )
  expect_error(
    style_continuous("x", 1, 1, stops),
    "invalid range"
  )
  expect_error(
    style_graduated("x", 2, 0, 1, stops_slice(stops, 1L)),
    "at least 2 color stops, got 1"
  )
  expect_error(
    style_continuous("x", 0, 1, stops_slice(stops, 1L)),
    "at least 2 color stops, got 1"
  )
  bad_start <- list(offsets = c(0.1, 1), colors = stops$colors)
  expect_error(
    style_graduated("x", 2, 0, 1, bad_start),
    "first color stop must be at offset 0.0 and last at 1.0"
  )
  non_ascending <- list(
    offsets = c(0, 0.5, 0.5, 1),
    colors = cbind(
      c(0L, 0L, 0L), c(255L, 255L, 255L), c(0L, 0L, 0L), c(255L, 255L, 255L)
    )
  )
  expect_error(
    style_graduated("x", 2, 0, 1, non_ascending),
    "ascending offset order"
  )
  expect_error(
    style_categorized("x", character(), matrix(integer(), nrow = 3)),
    "at least 1 category"
  )
})

test_that("a raster pseudocolor renderer interpolates the stops", {
  stops <- list(
    offsets = c(0, 0.5, 1),
    colors = cbind(c(0L, 0L, 0L), c(100L, 110L, 120L), c(255L, 255L, 255L))
  )
  style <- style_raster_pseudocolor(1L, 80, 200, stops)
  w <- xml_writer(0L)
  write_raster_renderer(w, style)
  out <- xw_finish(w)

  expect_match(out, 'type="singlebandpseudocolor"', fixed = TRUE)
  expect_match(out, 'band="1"', fixed = TRUE)
  expect_match(out, 'classificationMin="80"', fixed = TRUE)
  expect_match(out, 'classificationMax="200"', fixed = TRUE)
  expect_match(
    out,
    'classificationMode="1" clip="0" colorRampType="INTERPOLATED"',
    fixed = TRUE
  )
  expect_match(out, 'minimumValue="80"', fixed = TRUE)
  expect_match(out, 'maximumValue="200"', fixed = TRUE)
  # The colorramp holds the endpoints plus the intermediate stop.
  expect_match(
    out,
    '<Option name="color1" type="QString" value="0,0,0,255,rgb:0,0,0,1"/>',
    fixed = TRUE
  )
  expect_match(
    out,
    '<Option name="color2" type="QString" value="255,255,255,255,rgb:1,1,1,1"/>',
    fixed = TRUE
  )
  expect_match(
    out,
    'name="stops" type="QString" value="0.5;100,110,120,255,rgb:',
    fixed = TRUE
  )
  # One <item> per stop, at min + offset * (max - min).
  expect_match(
    out,
    '<item alpha="255" color="#000000" label="80" value="80"/>',
    fixed = TRUE
  )
  expect_match(
    out,
    '<item alpha="255" color="#646e78" label="140" value="140"/>',
    fixed = TRUE
  )
  expect_match(
    out,
    '<item alpha="255" color="#ffffff" label="200" value="200"/>',
    fixed = TRUE
  )
  expect_match(out, 'useContinuousLegend="1"', fixed = TRUE)
})

test_that("style_raster_pseudocolor() validates its inputs", {
  stops <- list(
    offsets = c(0, 1),
    colors = cbind(c(0L, 0L, 0L), c(255L, 255L, 255L))
  )
  expect_error(style_raster_pseudocolor(1L, 1, 1, stops), "invalid range")
  expect_error(
    style_raster_pseudocolor(1L, 0, 1, stops_slice(stops, 1L)),
    "at least 2 color stops, got 1"
  )
  expect_error(style_raster_pseudocolor(0L, 0, 1, stops), "1-based")
  expect_error(
    style_raster_pseudocolor(1L, 0, 1, stops, opacity = 2),
    "must be a single number in \\[0, 1\\]"
  )
  expect_error(
    style_raster_pseudocolor(1L, 0, 1, stops, nodata_color = c(1L, 2L, 3L)),
    "c\\(r, g, b, alpha\\)"
  )
})

test_that("a raster opacity and nodata color reach the renderer", {
  stops <- list(
    offsets = c(0, 1),
    colors = cbind(c(0L, 0L, 0L), c(255L, 255L, 255L))
  )
  style <- style_raster_pseudocolor(
    1L, 0, 1, stops,
    opacity = 0.5,
    nodata_color = c(255L, 0L, 0L, 255L)
  )
  w <- xml_writer(0L)
  write_raster_renderer(w, style)
  out <- xw_finish(w)

  expect_match(out, 'opacity="0.5"', fixed = TRUE)
  expect_match(out, 'nodataColor="255,0,0,255,rgb:1,0,0,1"', fixed = TRUE)

  # No nodata color leaves the attribute empty (QGIS's transparent default).
  w2 <- xml_writer(0L)
  write_raster_renderer(w2, style_raster_pseudocolor(1L, 0, 1, stops))
  expect_match(xw_finish(w2), 'nodataColor="" opacity="1"', fixed = TRUE)
})

test_that("a raster binned renderer writes a DISCRETE shader", {
  colors <- cbind(c(255L, 255L, 204L), c(253L, 141L, 60L), c(128L, 0L, 38L))
  style <- style_raster_binned(
    1L,
    boundaries = c(0, 3, 7, 15),
    colors = colors,
    labels = c("0 - 2", "3 - 6", "7 - 15")
  )
  w <- xml_writer(0L)
  write_raster_renderer(w, style)
  out <- xw_finish(w)

  expect_match(out, 'type="singlebandpseudocolor"', fixed = TRUE)
  expect_match(
    out,
    'classificationMode="1" clip="0" colorRampType="DISCRETE"',
    fixed = TRUE
  )
  expect_match(out, 'classificationMin="0"', fixed = TRUE)
  expect_match(out, 'classificationMax="15"', fixed = TRUE)
  # Each <item> value is its class's inclusive upper bound; the last
  # class is open-ended.
  expect_match(
    out,
    '<item alpha="255" color="#ffffcc" label="0 - 2" value="3"/>',
    fixed = TRUE
  )
  expect_match(
    out,
    '<item alpha="255" color="#fd8d3c" label="3 - 6" value="7"/>',
    fixed = TRUE
  )
  expect_match(
    out,
    '<item alpha="255" color="#800026" label="7 - 15" value="inf"/>',
    fixed = TRUE
  )
  # The informational ramp puts the interior bin color at its midpoint.
  expect_match(
    out,
    'name="stops" type="QString" value="0.333333;',
    fixed = TRUE
  )
})

test_that("style_raster_binned() validates its inputs", {
  colors <- cbind(c(0L, 0L, 0L), c(255L, 255L, 255L))
  expect_error(
    style_raster_binned(1L, c(0, 1, 2), colors, c("a", "b", "c")),
    "one label per class \\(expected 2, got 3\\)"
  )
  expect_error(
    style_raster_binned(1L, c(0, 2, 1), colors, c("a", "b")),
    "strictly ascending"
  )
  expect_error(
    style_raster_binned(1L, c(0, 1), colors, c("a", "b")),
    "matching lengths"
  )
  expect_error(
    style_raster_binned(1L, 0, matrix(integer(), nrow = 3), character()),
    "at least 1 bin"
  )
})

test_that("a raster paletted renderer writes one entry per value", {
  style <- style_raster_paletted(
    1L,
    values = c(1, 2, 3),
    colors = cbind(c(228L, 26L, 28L), c(55L, 126L, 184L), c(77L, 175L, 74L)),
    labels = c("forest", "water", "urban")
  )
  w <- xml_writer(0L)
  write_raster_renderer(w, style)
  out <- xw_finish(w)

  expect_match(out, 'type="paletted"', fixed = TRUE)
  expect_match(out, 'band="1"', fixed = TRUE)
  expect_match(
    out,
    '<paletteEntry alpha="255" color="#e41a1c" label="forest" value="1"/>',
    fixed = TRUE
  )
  expect_match(
    out,
    '<paletteEntry alpha="255" color="#4daf4a" label="urban" value="3"/>',
    fixed = TRUE
  )
  # A paletted renderer has no shader and no classification range.
  expect_false(grepl("rastershader", out, fixed = TRUE))
  expect_false(grepl("classificationMin", out, fixed = TRUE))
})

test_that("style_raster_paletted() validates its inputs", {
  colors <- cbind(c(0L, 0L, 0L), c(255L, 255L, 255L))
  expect_error(
    style_raster_paletted(
      1L, numeric(), matrix(integer(), nrow = 3), character()
    ),
    "at least 1 value"
  )
  expect_error(
    style_raster_paletted(1L, c(1, 1), colors, c("a", "b")),
    "must be unique"
  )
  expect_error(
    style_raster_paletted(1L, c(1, NA), colors, c("a", "b")),
    "non-missing numbers"
  )
  expect_error(
    style_raster_paletted(1L, c(1, 2, 3), colors, c("a", "b")),
    "matching lengths"
  )
})

test_that("a multiband renderer writes one enhancement per channel", {
  # samples/true-color.qgs of the generate-qgs crate.
  style <- style_raster_multiband(
    red = list(band = 1L, min = 35, max = 253),
    green = list(band = 2L, min = 35, max = 251),
    blue = list(band = 3L, min = 35, max = 250)
  )
  w <- xml_writer(0L)
  write_raster_renderer(w, style)
  out <- xw_finish(w)

  expect_match(out, 'type="multibandcolor"', fixed = TRUE)
  expect_match(out, 'redBand="1"', fixed = TRUE)
  expect_match(out, 'greenBand="2"', fixed = TRUE)
  expect_match(out, 'blueBand="3"', fixed = TRUE)
  expect_match(out, 'opacity="1"', fixed = TRUE)
  expect_match(out, "<limits>MinMax</limits>", fixed = TRUE)
  for (channel in list(
    c("redContrastEnhancement", "253"),
    c("greenContrastEnhancement", "251"),
    c("blueContrastEnhancement", "250")
  )) {
    expect_match(
      out,
      paste0(
        "<", channel[1L], ">\n    <minValue>35</minValue>\n",
        "    <maxValue>", channel[2L], "</maxValue>\n",
        "    <algorithm>NoEnhancement</algorithm>"
      ),
      fixed = TRUE
    )
  }
  # A multiband renderer has no shader.
  expect_no_match(out, "<rastershader>", fixed = TRUE)

  expect_equal(raster_style_bands(style), c(1L, 2L, 3L))
})

test_that("a multiband stretch writes the algorithm and user limits", {
  style <- style_raster_multiband(
    red = list(band = 1L, min = 0, max = 500),
    green = list(band = 2L, min = 0, max = 500),
    blue = list(band = 3L, min = 0, max = 500),
    algorithm = "StretchToMinimumMaximum",
    opacity = 0.5
  )
  w <- xml_writer(0L)
  write_raster_renderer(w, style)
  out <- xw_finish(w)

  expect_match(out, 'opacity="0.5"', fixed = TRUE)
  expect_match(out, "<limits>None</limits>", fixed = TRUE)
  expect_match(
    out,
    "<algorithm>StretchToMinimumMaximum</algorithm>",
    fixed = TRUE
  )
  expect_match(out, "<maxValue>500</maxValue>", fixed = TRUE)
})

test_that("style_raster_multiband() validates its inputs", {
  channel <- function(band, min = 0, max = 1) {
    list(band = band, min = min, max = max)
  }
  expect_error(
    style_raster_multiband(channel(0L), channel(2L), channel(3L)),
    "band numbers are 1-based, got 0"
  )
  expect_error(
    style_raster_multiband(channel(1.5), channel(2L), channel(3L)),
    "band numbers are 1-based, got 1.5"
  )
  expect_error(
    style_raster_multiband(list(band = 1L), channel(2L), channel(3L)),
    "a channel's `min` must be a single finite number"
  )
  expect_error(
    style_raster_multiband(
      channel(1L, 0, NaN), channel(2L), channel(3L)
    ),
    "a channel's `max` must be a single finite number"
  )
  expect_error(
    style_raster_multiband(channel(1L, 2, 1), channel(2L), channel(3L)),
    "invalid range"
  )
  # An equal min/max is allowed (a constant band).
  expect_no_error(
    style_raster_multiband(channel(1L, 1, 1), channel(2L), channel(3L))
  )
  expect_error(
    style_raster_multiband(
      channel(1L), channel(2L), channel(3L),
      opacity = 1.5
    ),
    "`opacity` must be a single number"
  )
  expect_error(
    style_raster_multiband(
      channel(1L), channel(2L), channel(3L),
      algorithm = "Nope"
    ),
    "'arg' should be one of"
  )
})
