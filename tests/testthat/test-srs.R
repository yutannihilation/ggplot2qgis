test_that("EPSG codes resolve to wkt/name/geographic", {
  srs <- resolve_srs(4326)
  expect_equal(srs$epsg, 4326)
  expect_equal(srs$name, "WGS 84")
  expect_true(srs$geographic)
  expect_match(srs$wkt, "^GEOGCRS")

  srs <- resolve_srs(3857)
  expect_equal(srs$epsg, 3857)
  expect_equal(srs$name, "WGS 84 / Pseudo-Mercator")
  expect_false(srs$geographic)
  expect_match(srs$wkt, "^PROJCRS")
})

test_that("a WKT2 string round-trips through resolve_srs", {
  wkt <- resolve_srs(4267)$wkt
  srs <- resolve_srs(wkt)
  expect_equal(srs$epsg, 4267)
  expect_equal(srs$name, "NAD27")
  expect_true(srs$geographic)
})

test_that("an sf crs object is accepted directly", {
  srs <- resolve_srs(sf::st_crs(4326))
  expect_equal(srs$epsg, 4326)
})

test_that("an unresolvable CRS is an error", {
  expect_error(resolve_srs(sf::st_crs(NA)), "cannot resolve the CRS")
})

test_that("the spatialrefsys block has the QGIS structure", {
  w <- xml_writer(0L)
  write_spatialrefsys(w, resolve_srs(4267))
  out <- xw_finish(w)

  expect_match(out, "<spatialrefsys nativeFormat=\"Wkt\">", fixed = TRUE)
  expect_match(out, "<wkt>GEOGCRS", fixed = TRUE)
  expect_match(out, "<srsid>4267</srsid>", fixed = TRUE)
  expect_match(out, "<srid>4267</srid>", fixed = TRUE)
  expect_match(out, "<authid>EPSG:4267</authid>", fixed = TRUE)
  expect_match(out, "<description>NAD27</description>", fixed = TRUE)
  expect_match(
    out,
    "<projectionacronym></projectionacronym>",
    fixed = TRUE
  )
  expect_match(out, "<ellipsoidacronym></ellipsoidacronym>", fixed = TRUE)
  expect_match(out, "<geographicflag>true</geographicflag>", fixed = TRUE)
  # proj4 is deliberately not emitted.
  expect_no_match(out, "proj4", fixed = TRUE)
})

test_that("a CRS without an EPSG code gets srid 0 and an empty authid", {
  srs <- resolve_srs(4326)
  srs$epsg <- NA_integer_
  w <- xml_writer(0L)
  write_spatialrefsys(w, srs)
  out <- xw_finish(w)
  expect_match(out, "<srsid>0</srsid>", fixed = TRUE)
  expect_match(out, "<srid>0</srid>", fixed = TRUE)
  expect_match(out, "<authid></authid>", fixed = TRUE)
})
