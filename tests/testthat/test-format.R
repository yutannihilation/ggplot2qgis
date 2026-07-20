test_that("qgis_color matches QGIS's serialization", {
  expect_equal(
    qgis_color(c(232L, 113L, 141L)),
    "232,113,141,255,rgb:0.9098039,0.4431373,0.5529412,1"
  )
  expect_equal(
    qgis_color(c(35L, 35L, 35L)),
    "35,35,35,255,rgb:0.1372549,0.1372549,0.1372549,1"
  )
  expect_equal(qgis_color(c(255L, 0L, 0L)), "255,0,0,255,rgb:1,0,0,1")
  expect_equal(
    qgis_color(c(255L, 204L, 204L)),
    "255,204,204,255,rgb:1,0.8,0.8,1"
  )
})

test_that("channels are formatted from the float32 value like QGIS", {
  # 8/255 rounds differently as a double (0.0313725) than as the float32
  # QGIS uses (0.0313726).
  expect_equal(
    qgis_color(c(8L, 40L, 228L)),
    "8,40,228,255,rgb:0.0313726,0.1568628,0.8941177,1"
  )
})

test_that("color_hex formats as #rrggbb", {
  expect_equal(color_hex(c(19L, 43L, 67L)), "#132b43")
  expect_equal(color_hex(c(255L, 255L, 255L)), "#ffffff")
  expect_equal(color_hex(c(0L, 0L, 0L)), "#000000")
})

test_that("ramp interpolation matches the red sample", {
  # samples/red.qgs: white -> red in 6 classes
  ramp <- list(
    offsets = c(0, 1),
    colors = cbind(c(255L, 255L, 255L), c(255L, 0L, 0L))
  )
  expected <- list(
    c(255L, 255L, 255L),
    c(255L, 204L, 204L),
    c(255L, 153L, 153L),
    c(255L, 102L, 102L),
    c(255L, 51L, 51L),
    c(255L, 0L, 0L)
  )
  for (i in seq_along(expected)) {
    expect_equal(
      unname(sample_ramp(ramp, (i - 1) / 5)),
      expected[[i]],
      info = paste("class", i)
    )
  }
})

test_that("multi-stop interpolation matches the magma sample", {
  # Class colors in samples/magma.qgs (6 classes), and the magma control
  # points adjacent to the class positions. Guards the half-away-from-zero
  # rounding of rgb_lerp().
  ramp <- list(
    offsets = c(
      0, 0.196078, 0.215686, 0.392157, 0.411765,
      0.588235, 0.607843, 0.784314, 0.803922, 1
    ),
    colors = cbind(
      c(0L, 0L, 4L),
      c(57L, 15L, 110L),
      c(66L, 15L, 117L),
      c(137L, 40L, 129L),
      c(145L, 43L, 129L),
      c(217L, 70L, 107L),
      c(224L, 76L, 103L),
      c(253L, 152L, 105L),
      c(254L, 161L, 110L),
      c(252L, 253L, 191L)
    )
  )
  expected <- list(
    c(0L, 0L, 4L),
    c(59L, 15L, 111L),
    c(140L, 41L, 129L),
    c(221L, 74L, 105L),
    c(254L, 159L, 109L),
    c(252L, 253L, 191L)
  )
  for (i in seq_along(expected)) {
    expect_equal(
      unname(sample_ramp(ramp, (i - 1) / 5)),
      expected[[i]],
      info = paste("class", i)
    )
  }
})

test_that("g6 formats offsets like C's %g", {
  expect_equal(g6(0), "0")
  expect_equal(g6(0.5), "0.5")
  expect_equal(g6(1), "1")
  expect_equal(g6(1 / 51), "0.0196078")
  expect_equal(g6(0.509804), "0.509804")
  expect_equal(g6(0.980392), "0.980392")
  expect_equal(g6(0.0000123457), "1.23457e-05")
})

test_that("num never uses scientific notation", {
  expect_equal(num(0.26), "0.26")
  expect_equal(num(1), "1")
  expect_equal(num(57), "57")
  expect_equal(num(0.1505625), "0.1505625")
  expect_equal(num(1e-05), "0.00001")
  expect_equal(num(1e+08), "100000000")
})

test_that("num ignores the OutDec option", {
  withr::local_options(OutDec = ",")
  expect_equal(num(0.26), "0.26")
  expect_equal(num(0.042), "0.042")
})

test_that("range labels match the labelFormat convention", {
  expect_equal(range_label(0, 10, 1L), "0 - 10")
  expect_equal(range_label(9.5, 19, 1L), "9.5 - 19")
  expect_equal(range_label(0.042, 0.04996, 3L), "0.042 - 0.05")
})

test_that("label precision scales with the class width", {
  # Wide classes keep the QGIS default of one decimal.
  expect_equal(label_precision(9.5), 1L)
  expect_equal(label_precision(2), 1L)
  expect_equal(label_precision(0.5), 1L)
  # Narrow classes get enough decimals to stay distinguishable.
  expect_equal(label_precision(0.05), 2L)
  expect_equal(label_precision(0.00796), 3L)
  expect_equal(label_precision(0.0005), 4L)
  # Degenerate steps fall back to the default.
  expect_equal(label_precision(0), 1L)
  expect_equal(label_precision(NaN), 1L)
  expect_equal(label_precision(NA_real_), 1L)
})

test_that("exact label precision reproduces every boundary", {
  expect_equal(exact_label_precision(c(0.042, 0.08, 0.12, 0.2, 0.241)), 3L)
  expect_equal(exact_label_precision(c(0, 0.05, 0.1)), 2L)
  # Integer boundaries keep the QGIS default of one decimal.
  expect_equal(exact_label_precision(c(0, 10, 57)), 1L)
  # Float noise is absorbed by num()'s 15-significant-digit form.
  expect_equal(exact_label_precision(c(0, 0.30000000000000004, 1)), 1L)
})

test_that("qgs_uuid looks like a UUID and is unique", {
  id <- qgs_uuid()
  expect_equal(nchar(id), 36L)
  expect_match(
    id,
    "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"
  )
  ids <- vapply(1:1000, function(i) qgs_uuid(), character(1))
  expect_equal(length(unique(ids)), 1000L)
})

test_that("layer ids sanitize the name to ASCII alphanumerics", {
  id <- qgs_layer_id("nc")
  expect_match(id, "^nc_")
  expect_no_match(id, "-", fixed = TRUE)

  id <- qgs_layer_id("地理院タイル（標準地図）")
  expect_match(id, "^_")
})

test_that("the timestamp is ISO-8601 without a zone suffix", {
  expect_match(
    qgs_now_iso8601(),
    "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}$"
  )
})
