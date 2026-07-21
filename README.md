# ggplot2qgis

<!-- badges: start -->
[![R-CMD-check](https://github.com/yutannihilation/ggplot2qgis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yutannihilation/ggplot2qgis/actions/workflows/R-CMD-check.yaml)
[![ggplot2qgis status badge](https://yutannihilation.r-universe.dev/ggplot2qgis/badges/version)](https://yutannihilation.r-universe.dev/ggplot2qgis)
<!-- badges: end -->

Export a [ggplot2](https://ggplot2.tidyverse.org/) map plot (e.g. `geom_sf()`)
as a [QGIS](https://qgis.org/) project (`.qgs`) file.

`write_qgs()` takes a ggplot2 plot whose layers are backed by
[sf](https://r-spatial.github.io/sf/) objects and writes a QGIS project. The
data of each layer is saved as a GeoPackage alongside the `.qgs`, and each
layer is styled after the plot's trained color scale:

- a continuous `fill`/`colour` scale becomes a graduated renderer (or a
  continuously interpolated color, see `gradient_style`),
- a discrete scale becomes a categorized renderer,
- a layer with no `fill`/`colour` mapping becomes a single symbol with the
  color ggplot2 would have used.

## Installation

You can install ggplot2qgis via [R-universe](https://yutannihilation.r-universe.dev/ggplot2qgis):

``` r
install.packages("ggplot2qgis", repos = c("https://yutannihilation.r-universe.dev", "https://cloud.r-project.org"))
```

## Usage

``` r
library(ggplot2)
library(ggplot2qgis)

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

p <- ggplot(nc) +
  geom_sf(aes(fill = AREA))

write_qgs(p, "nc.qgs")
```

Open `nc.qgs` in QGIS: the polygons are rendered with the same fill gradient
as the ggplot2 plot, and the data lives in `nc_data/`.

To add an XYZ tile basemap below the layers, pass `basemap` a predefined key
or an XYZ URL template:

``` r
write_qgs(p, "nc.qgs", basemap = "osm")
```

See `?write_qgs` for the full set of options (`use_plot_crs`,
`gradient_style`, `basemap`).

## Notes

- Each layer must be backed by sf data, or be a `geom_point()`, `geom_path()`,
  `geom_line()` or `geom_polygon()` layer drawn from a plain data frame. A
  data frame layer is converted to sf (one point per row, or one
  linestring/polygon per group); the plot must use `coord_sf()`, and the
  `x`/`y` values are taken to be coordinates in the panel CRS (`coord_sf()`'s
  `crs`, or the CRS of the first sf layer). See `?write_qgs` for the details
  and constraints.
- Only a bare column name is supported for the `fill`/`colour` (and a data
  frame layer's `x`/`y`) aesthetics; a constant or a computed expression
  (e.g. `aes(fill = AREA * 2)`) is an error.
- Mapping both `fill` and `colour` on the same layer is not supported.

## Related

This R package is a port of the Rust crate
[generate-qgs](https://github.com/yutannihilation/generate-qgs), which
generates `.qgs` files programmatically. The `.qgs` file format itself is
documented in [`docs/qgs-format.md`](docs/qgs-format.md).

## License

MIT © Hiroaki Yutani
