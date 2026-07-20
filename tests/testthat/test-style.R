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
