# A small point/line-ish data set: two groups of two points each, a
# varying value and a group-constant label.
read_pts <- function() {
  data.frame(
    lon = c(139.70, 139.80, 139.75, 139.90),
    lat = c(35.60, 35.70, 35.65, 35.80),
    f = c("a", "a", "b", "b"),
    v = c(1, 2, 3, 4)
  )
}

# Two triangles (one per group), with a group-constant value.
read_tri <- function() {
  data.frame(
    lon = c(0, 1, 0.5, 2, 3, 2.5),
    lat = c(0, 0, 1, 2, 2, 3),
    g = c("a", "a", "a", "b", "b", "b"),
    v = c(10, 10, 10, 20, 20, 20)
  )
}

write_df_qgs <- function(p, env = parent.frame()) {
  dir <- local_out_dir(env = env)
  path <- file.path(dir, "proj.qgs")
  write_qgs(p, path)
  list(dir = dir, path = path, out = read_qgs(path))
}

test_that("geom_point on a data.frame becomes a point layer", {
  pts <- read_pts()
  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_point(ggplot2::aes(lon, lat)) +
    ggplot2::coord_sf(crs = 4326)

  r <- write_df_qgs(p)

  expect_match(r$out, 'geometry="Point"', fixed = TRUE)
  expect_match(r$out, 'type="singleSymbol"', fixed = TRUE)
  # geom_point's constant color is black.
  expect_match(
    r$out,
    '<Option name="color" type="QString" value="0,0,0,255,rgb:',
    fixed = TRUE
  )

  d <- sf::st_read(
    file.path(r$dir, "proj_data", "pts.gpkg"),
    layer = "pts",
    quiet = TRUE
  )
  expect_equal(nrow(d), nrow(pts))
  expect_equal(sf::st_crs(d)$epsg, 4326L)
  expect_true(all(sf::st_geometry_type(d) == "POINT"))
  # All raw columns are kept, including the coordinate columns.
  expect_equal(d$lon, pts$lon)
  expect_equal(d$lat, pts$lat)
  expect_equal(d$f, pts$f)
  expect_equal(d$v, pts$v)
  expect_equal(
    unname(sf::st_coordinates(d)),
    unname(cbind(pts$lon, pts$lat))
  )
})

test_that("a data.frame layer takes the CRS of the first sf layer", {
  nc <- read_nc()
  pts <- read_pts()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf() +
    ggplot2::geom_point(data = pts, ggplot2::aes(lon, lat))

  r <- write_df_qgs(p)

  d <- sf::st_read(
    file.path(r$dir, "proj_data", "pts.gpkg"),
    layer = "pts",
    quiet = TRUE
  )
  # nc.shp is in NAD27.
  expect_equal(sf::st_crs(d)$epsg, 4267L)
})

test_that("coord_sf(crs = ) sets the CRS of a data.frame layer", {
  pts <- read_pts()
  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_point(ggplot2::aes(lon, lat)) +
    ggplot2::coord_sf(crs = 3857)

  r <- write_df_qgs(p)

  d <- sf::st_read(
    file.path(r$dir, "proj_data", "pts.gpkg"),
    layer = "pts",
    quiet = TRUE
  )
  expect_equal(sf::st_crs(d)$epsg, 3857L)
})

test_that("an unresolvable CRS is an error", {
  pts <- read_pts()
  path <- file.path(local_out_dir(), "proj.qgs")

  # No coord_sf() at all.
  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_point(ggplot2::aes(lon, lat))
  expect_error(write_qgs(p, path), "requires coord_sf")

  # coord_sf() without a crs and no sf layer to take one from.
  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_point(ggplot2::aes(lon, lat)) +
    ggplot2::coord_sf()
  expect_error(write_qgs(p, path), "cannot determine the CRS")
})

test_that("coord_sf(default_crs = ) is an error", {
  pts <- read_pts()
  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_point(ggplot2::aes(lon, lat)) +
    ggplot2::coord_sf(crs = 3857, default_crs = 4326)

  path <- file.path(local_out_dir(), "proj.qgs")
  expect_error(write_qgs(p, path), "default_crs.*not supported")
})

test_that("a mapped colour on a data.frame point layer is categorized", {
  pts <- read_pts()
  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_point(ggplot2::aes(lon, lat, colour = f)) +
    ggplot2::coord_sf(crs = 4326)

  r <- write_df_qgs(p)

  expect_match(r$out, 'type="categorizedSymbol"', fixed = TRUE)
  expect_match(r$out, 'attr="f"', fixed = TRUE)
})

test_that("geom_path becomes one linestring per group, in data order", {
  pts <- read_pts()
  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_path(ggplot2::aes(lon, lat, group = f)) +
    ggplot2::coord_sf(crs = 4326)

  r <- write_df_qgs(p)

  expect_match(r$out, 'geometry="LineString"', fixed = TRUE)
  # geom_path/line's constant linewidth is 0.5.
  expect_match(
    r$out,
    sprintf(
      '<Option name="line_width" type="QString" value="%s"/>',
      ggplot2qgis:::num(round(0.5 * 72.27 / 96, 7))
    ),
    fixed = TRUE
  )

  d <- sf::st_read(
    file.path(r$dir, "proj_data", "pts.gpkg"),
    layer = "pts",
    quiet = TRUE
  )
  expect_equal(nrow(d), 2L)
  expect_true(all(sf::st_geometry_type(d) == "LINESTRING"))
  # The group-constant column is kept; the varying ones are dropped.
  expect_equal(d$f, c("a", "b"))
  expect_false("v" %in% names(d))
  expect_false("lon" %in% names(d))
  # Vertices in data order.
  coords <- sf::st_coordinates(d[1L, ])
  expect_equal(unname(coords[, c("X", "Y")]), cbind(c(139.7, 139.8), c(35.6, 35.7)))
})

test_that("geom_line sorts each group by x", {
  d0 <- data.frame(
    x = c(3, 1, 2),
    y = c(30, 10, 20)
  )
  p <- ggplot2::ggplot(d0) +
    ggplot2::geom_line(ggplot2::aes(x, y)) +
    ggplot2::coord_sf(crs = 3857)

  r <- write_df_qgs(p)

  d <- sf::st_read(
    file.path(r$dir, "proj_data", "d0.gpkg"),
    layer = "d0",
    quiet = TRUE
  )
  expect_equal(nrow(d), 1L)
  coords <- sf::st_coordinates(d)
  expect_equal(unname(coords[, "X"]), c(1, 2, 3))
  expect_equal(unname(coords[, "Y"]), c(10, 20, 30))
})

test_that("a discrete colour drives the default grouping of geom_line", {
  pts <- read_pts()
  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_line(ggplot2::aes(lon, lat, colour = f)) +
    ggplot2::coord_sf(crs = 4326)

  r <- write_df_qgs(p)

  expect_match(r$out, 'type="categorizedSymbol"', fixed = TRUE)
  expect_match(r$out, 'attr="f"', fixed = TRUE)

  d <- sf::st_read(
    file.path(r$dir, "proj_data", "pts.gpkg"),
    layer = "pts",
    quiet = TRUE
  )
  expect_equal(d$f, c("a", "b"))
})

test_that("geom_polygon closes the ring and drops its NA outline", {
  tri <- read_tri()
  p <- ggplot2::ggplot(tri) +
    ggplot2::geom_polygon(ggplot2::aes(lon, lat, group = g)) +
    ggplot2::coord_sf(crs = 3857)

  r <- write_df_qgs(p)

  expect_match(r$out, 'geometry="Polygon"', fixed = TRUE)
  # geom_polygon's constant fill is #333333; its default colour is NA, so
  # the outline is not drawn.
  expect_match(
    r$out,
    '<Option name="color" type="QString" value="51,51,51,255,rgb:',
    fixed = TRUE
  )
  expect_match(
    r$out,
    '<Option name="outline_style" type="QString" value="no"/>',
    fixed = TRUE
  )

  d <- sf::st_read(
    file.path(r$dir, "proj_data", "tri.gpkg"),
    layer = "tri",
    quiet = TRUE
  )
  expect_equal(nrow(d), 2L)
  expect_true(all(sf::st_geometry_type(d) == "POLYGON"))
  coords <- sf::st_coordinates(d[1L, ])
  # 3 vertices + the closing repeat of the first.
  expect_equal(nrow(coords), 4L)
  expect_equal(coords[1L, c("X", "Y")], coords[4L, c("X", "Y")])
})

test_that("a constant polygon outline is drawn solid", {
  tri <- read_tri()
  p <- ggplot2::ggplot(tri) +
    ggplot2::geom_polygon(ggplot2::aes(lon, lat, group = g), colour = "black") +
    ggplot2::coord_sf(crs = 3857)

  r <- write_df_qgs(p)

  expect_match(
    r$out,
    '<Option name="outline_style" type="QString" value="solid"/>',
    fixed = TRUE
  )
  expect_match(
    r$out,
    '<Option name="outline_color" type="QString" value="0,0,0,255,rgb:',
    fixed = TRUE
  )
})

test_that("fill = NA renders the polygon fill as not drawn", {
  tri <- read_tri()
  p <- ggplot2::ggplot(tri) +
    ggplot2::geom_polygon(
      ggplot2::aes(lon, lat, group = g),
      fill = NA, colour = "black"
    ) +
    ggplot2::coord_sf(crs = 3857)

  r <- write_df_qgs(p)

  expect_match(
    r$out,
    '<Option name="style" type="QString" value="no"/>',
    fixed = TRUE
  )
})

test_that("a layer that would draw nothing is an error", {
  pts <- read_pts()
  path <- file.path(local_out_dir(), "proj.qgs")

  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_point(ggplot2::aes(lon, lat), colour = NA) +
    ggplot2::coord_sf(crs = 4326)
  expect_error(write_qgs(p, path), "would not be drawn")

  tri <- read_tri()
  p <- ggplot2::ggplot(tri) +
    ggplot2::geom_polygon(ggplot2::aes(lon, lat, group = g), fill = NA) +
    ggplot2::coord_sf(crs = 3857)
  expect_error(write_qgs(p, path), "would not be drawn")
})

test_that("a mapped column varying within a group is an error", {
  tri <- read_tri()
  tri$v[1L] <- 99
  p <- ggplot2::ggplot(tri) +
    ggplot2::geom_polygon(ggplot2::aes(lon, lat, group = g, fill = v)) +
    ggplot2::coord_sf(crs = 3857)

  path <- file.path(local_out_dir(), "proj.qgs")
  expect_error(write_qgs(p, path), "must be constant within each group")
})

test_that("non-identity stats and positions are errors", {
  pts <- read_pts()
  path <- file.path(local_out_dir(), "proj.qgs")

  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_point(ggplot2::aes(lon, lat), position = "jitter") +
    ggplot2::coord_sf(crs = 4326)
  expect_error(write_qgs(p, path), "only the identity position")

  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_point(ggplot2::aes(lon, lat), stat = "unique") +
    ggplot2::coord_sf(crs = 4326)
  expect_error(write_qgs(p, path), "only the identity stat")
})

test_that("a flipped orientation is an error", {
  pts <- read_pts()
  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_line(ggplot2::aes(lon, lat), orientation = "y") +
    ggplot2::coord_sf(crs = 4326)

  path <- file.path(local_out_dir(), "proj.qgs")
  expect_error(write_qgs(p, path), "orientation")
})

test_that("invalid x/y aesthetics are errors", {
  pts <- read_pts()
  path <- file.path(local_out_dir(), "proj.qgs")

  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_point(ggplot2::aes(lon + 1, lat)) +
    ggplot2::coord_sf(crs = 4326)
  expect_error(write_qgs(p, path), "only a bare column name is supported for `x`")

  # A discrete column already fails inside ggplot_build(); a Date column
  # builds but is not numeric raw data.
  pts$day <- as.Date("2026-01-01") + seq_len(nrow(pts))
  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_point(ggplot2::aes(day, lat)) +
    ggplot2::coord_sf(crs = 4326)
  # The date values make an implausible longitude range, which sf warns
  # about while the panel is built; only the error matters here.
  expect_error(suppressWarnings(write_qgs(p, path)), "must be numeric")

  pts$lon[2L] <- NA
  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_point(ggplot2::aes(lon, lat)) +
    ggplot2::coord_sf(crs = 4326)
  expect_error(write_qgs(p, path), "must not contain NA")
})

test_that("a transformed scale is an error", {
  pts <- read_pts()
  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_point(ggplot2::aes(lon, lat)) +
    ggplot2::scale_x_log10() +
    ggplot2::coord_sf(crs = 4326)

  path <- file.path(local_out_dir(), "proj.qgs")
  expect_error(write_qgs(p, path), "transformed")
})

test_that("unsupported geoms on data.frame data are errors", {
  pts <- read_pts()
  path <- file.path(local_out_dir(), "proj.qgs")

  # geom_step inherits GeomPath but is not a plain path.
  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_step(ggplot2::aes(lon, lat)) +
    ggplot2::coord_sf(crs = 4326)
  expect_error(write_qgs(p, path), "unsupported geom.*GeomStep")

  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_tile(ggplot2::aes(lon, lat)) +
    ggplot2::coord_sf(crs = 4326)
  expect_error(write_qgs(p, path), "unsupported geom.*GeomTile")
})

test_that("the subgroup aesthetic is an error", {
  tri <- read_tri()
  p <- ggplot2::ggplot(tri) +
    ggplot2::geom_polygon(ggplot2::aes(lon, lat, group = g, subgroup = v)) +
    ggplot2::coord_sf(crs = 3857)

  path <- file.path(local_out_dir(), "proj.qgs")
  expect_error(write_qgs(p, path), "subgroup")
})

test_that("degenerate groups are errors", {
  path <- file.path(local_out_dir(), "proj.qgs")

  d1 <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3), g = c("a", "a", "b"))
  p <- ggplot2::ggplot(d1) +
    ggplot2::geom_path(ggplot2::aes(x, y, group = g)) +
    ggplot2::coord_sf(crs = 3857)
  expect_error(write_qgs(p, path), "at least 2 points per group")

  p <- ggplot2::ggplot(d1) +
    ggplot2::geom_polygon(ggplot2::aes(x, y, group = g)) +
    ggplot2::coord_sf(crs = 3857)
  expect_error(write_qgs(p, path), "at least 3 points per group")
})

test_that("facets split features by panel", {
  pts <- read_pts()
  p <- ggplot2::ggplot(pts) +
    ggplot2::geom_path(ggplot2::aes(lon, lat)) +
    ggplot2::facet_wrap(ggplot2::vars(f)) +
    ggplot2::coord_sf(crs = 4326)

  r <- write_df_qgs(p)

  d <- sf::st_read(
    file.path(r$dir, "proj_data", "pts.gpkg"),
    layer = "pts",
    quiet = TRUE
  )
  # One feature per panel, not one line connecting all rows.
  expect_equal(nrow(d), 2L)
})

test_that("sf and data.frame layers mix in one project", {
  nc <- read_nc()
  pts <- read_pts()
  p <- ggplot2::ggplot(nc) +
    ggplot2::geom_sf(ggplot2::aes(fill = AREA)) +
    ggplot2::geom_point(data = pts, ggplot2::aes(lon, lat))

  r <- write_df_qgs(p)

  expect_true(file.exists(file.path(r$dir, "proj_data", "nc.gpkg")))
  expect_true(file.exists(file.path(r$dir, "proj_data", "pts.gpkg")))
  # The sf layer keeps its graduated styling.
  expect_match(r$out, 'type="graduatedSymbol"', fixed = TRUE)
  expect_match(r$out, 'attr="AREA"', fixed = TRUE)
  expect_match(r$out, 'geometry="Point"', fixed = TRUE)
})
