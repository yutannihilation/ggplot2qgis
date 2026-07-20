single_style <- function(r = 0L, g = 0L, b = 0L) {
  style_single(c(r, g, b))
}

test_that("an empty project keeps the template anchors", {
  out <- qgs_build(list())
  expect_type(out, "character")
  expect_match(out, '<custom-order enabled="0"/>', fixed = TRUE)
  expect_match(out, '<legend updateDrawingOrder="true"/>', fixed = TRUE)
  expect_match(out, "<projectlayers/>", fixed = TRUE)
  expect_match(out, "<layerorder/>", fixed = TRUE)
  expect_no_match(out, "{{SAVE_DATETIME}}", fixed = TRUE)
})

test_that("layers appear in all four places", {
  layers <- list(
    xyz_tile_layer(
      "地理院タイル（標準地図）",
      "https://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png",
      0, 18
    ),
    vector_layer(
      "../tmp/nc.gpkg", "nc", 4267, "Polygon",
      style_graduated(
        "SID79", 6, 0, 57,
        list(
          offsets = c(0, 1),
          colors = cbind(c(255L, 255L, 255L), c(255L, 0L, 0L))
        )
      )
    )
  )
  out <- qgs_build(layers)
  expect_match(out, "<layer-tree-layer", fixed = TRUE)
  expect_match(out, "<legendlayer", fixed = TRUE)
  expect_match(out, "<maplayer", fixed = TRUE)
  expect_match(out, "<layer id=", fixed = TRUE)
  expect_match(out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(out, 'attr="SID79"', fixed = TRUE)
  expect_match(out, "../tmp/nc.gpkg|layername=nc", fixed = TRUE)
  expect_match(out, "type=xyz", fixed = TRUE)
  # The vector layer's SRS, and the XYZ tile layer's one (always 3857).
  expect_match(out, "<authid>EPSG:4267</authid>", fixed = TRUE)
  expect_match(out, "<authid>EPSG:3857</authid>", fixed = TRUE)
})

test_that("the layer tree lists the top-most layer first", {
  layers <- list(
    vector_layer("a.gpkg", "bottom", 4326, "Polygon", single_style()),
    vector_layer("b.gpkg", "top", 4326, "Point", single_style())
  )
  out <- qgs_build(layers)
  tree <- regmatches(
    out,
    regexpr("<layer-tree-group>.*</layer-tree-group>", out)
  )
  expect_lt(
    regexpr('name="top"', tree, fixed = TRUE),
    regexpr('name="bottom"', tree, fixed = TRUE)
  )
  # The drawing order (<layerorder>) is bottom-most first.
  layerorder <- regmatches(out, regexpr("<layerorder>.*</layerorder>", out))
  bottom_id <- regexpr(
    layers[[1]]$id,
    layerorder,
    fixed = TRUE
  )
  top_id <- regexpr(layers[[2]]$id, layerorder, fixed = TRUE)
  expect_lt(bottom_id, top_id)
})

test_that("the XYZ datasource matches the sample", {
  layer <- xyz_tile_layer(
    "地理院タイル（標準地図）",
    "https://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png",
    0, 18
  )
  expect_equal(
    layer_datasource(layer),
    paste0(
      "crs=EPSG%3A3857&format&type=xyz&url=",
      "https%3A%2F%2Fcyberjapandata.gsi.go.jp%2Fxyz%2Fstd%2F",
      "%7Bz%7D%2F%7Bx%7D%2F%7By%7D.png&zmax=18&zmin=0&http-header:referer="
    )
  )
})

test_that("percent encoding keeps only RFC 3986 unreserved characters", {
  expect_equal(
    percent_encode("https://example.com/{z}/{x}/{y}.png"),
    "https%3A%2F%2Fexample.com%2F%7Bz%7D%2F%7Bx%7D%2F%7By%7D.png"
  )
  expect_equal(percent_encode("Az09-_.~"), "Az09-_.~")
  expect_equal(percent_encode("a b"), "a%20b")
  # Multibyte characters encode their UTF-8 bytes, without warnings.
  expect_no_warning(
    expect_equal(percent_encode("std日"), "std%E6%97%A5")
  )
})

test_that("the default project CRS is the template's EPSG:3857", {
  out <- qgs_build(list())
  block <- regmatches(out, regexpr("<projectCrs>.*?</projectCrs>", out))
  expect_match(block, "<authid>EPSG:3857</authid>", fixed = TRUE)
})

test_that("the project CRS can be set", {
  out <- qgs_build(list(), project_srs = resolve_srs(4267))
  block <- regmatches(out, regexpr("<projectCrs>.*?</projectCrs>", out))
  expect_match(block, "<authid>EPSG:4267</authid>", fixed = TRUE)
  expect_match(block, "<description>NAD27</description>", fixed = TRUE)
  expect_no_match(block, "3857", fixed = TRUE)
})

test_that("srs can be a WKT2 string", {
  wkt <- paste0(
    'GEOGCRS["WGS 84",DATUM["World Geodetic System 1984",',
    'ELLIPSOID["WGS 84",6378137,298.257223563,LENGTHUNIT["metre",1]]],',
    'CS[ellipsoidal,2],AXIS["geodetic latitude (Lat)",north,ORDER[1]],',
    'AXIS["geodetic longitude (Lon)",east,ORDER[2]],',
    'ANGLEUNIT["degree",0.0174532925199433],ID["EPSG",4326]]'
  )
  layer <- vector_layer("points.gpkg", "stations", wkt, "Point", single_style())
  out <- qgs_build(list(layer))
  expect_match(out, "<authid>EPSG:4326</authid>", fixed = TRUE)
})

test_that("an invalid geometry type is an error", {
  expect_error(
    vector_layer("nc.gpkg", "nc", 4267, "polygon", single_style()),
    "'arg' should be one of"
  )
})

test_that("special characters are escaped", {
  layer <- vector_layer("a&b<.gpkg", "x\"y", 4326, "Point", single_style())
  out <- qgs_build(list(layer))
  expect_match(out, "a&amp;b&lt;.gpkg", fixed = TRUE)
  expect_match(out, "x&quot;y", fixed = TRUE)
})

test_that("qgs_write() writes the built content byte for byte", {
  layers <- list(
    xyz_tile_layer(
      "osm", "https://tile.openstreetmap.org/{z}/{x}/{y}.png", 0, 19
    )
  )
  f <- withr::local_tempfile(fileext = ".qgs")
  qgs_write(layers, f)

  expect_true(file.exists(f))
  written <- readBin(f, "raw", file.size(f))
  # Byte-identical modulo the saveDateTime and generated ids, which change
  # between the two builds; compare a rebuilt copy after normalizing them.
  normalize <- function(s) {
    s <- gsub('saveDateTime="[^"]*"', 'saveDateTime=""', s)
    gsub("_[0-9a-f]{8}_[0-9a-f]{4}_[0-9a-f]{4}_[0-9a-f]{4}_[0-9a-f]{12}", "_id", s)
  }
  expect_identical(
    normalize(rawToChar(written)),
    normalize(qgs_build(layers))
  )
  # No CR bytes even on Windows: written in binary mode.
  expect_false(any(written == charToRaw("\r")))
})

test_that("qgs_write() fails on a non-writable path", {
  expect_error(
    suppressWarnings(
      qgs_write(list(), file.path(tempdir(), "no-such-dir", "x.qgs"))
    )
  )
})

test_that("a missing template anchor is a loud error", {
  expect_error(
    splice("<qgis></qgis>", "\n  <projectlayers/>", "x"),
    "template anchor not found"
  )
})

test_that("the generated XML is well-formed", {
  skip_if_not_installed("xml2")
  layers <- list(
    xyz_tile_layer(
      "地理院タイル（標準地図）",
      "https://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png",
      0, 18
    ),
    vector_layer(
      "a&b.gpkg", "nc", 4267, "Polygon",
      style_categorized(
        "NAME",
        c("Alamance", "Alexander"),
        cbind(c(255L, 255L, 255L), c(255L, 252L, 252L)),
        catch_all = c(255L, 0L, 0L)
      )
    ),
    vector_layer(
      "c.gpkg", "lines", 4326, "LineString",
      style_continuous(
        "x", 0, 1,
        list(
          offsets = c(0, 1),
          colors = cbind(c(0L, 0L, 0L), c(255L, 255L, 255L))
        )
      )
    )
  )
  out <- qgs_build(layers, project_srs = resolve_srs(4326))
  expect_no_error(xml2::read_xml(out))
})
