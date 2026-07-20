# ggplot2qgis

<!-- badges: start -->
[![R-CMD-check](https://github.com/yutannihilation/ggplot2qgis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yutannihilation/ggplot2qgis/actions/workflows/R-CMD-check.yaml)
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

You can install the development version from
[GitHub](https://github.com/yutannihilation/ggplot2qgis) with:

``` r
# install.packages("pak")
pak::pak("yutannihilation/ggplot2qgis")
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

See `?write_qgs` for the full set of options (`use_plot_crs`,
`gradient_style`).

## Notes

- All layers must be backed by sf data.
- Only a bare column name is supported for the `fill`/`colour` aesthetics; a
  constant or a computed expression (e.g. `aes(fill = AREA * 2)`) is an error.
- Mapping both `fill` and `colour` on the same layer is not supported.

## Related

This R package is a port of the Rust crate
[generate-qgs](https://github.com/yutannihilation/generate-qgs), which
generates `.qgs` files programmatically. The `.qgs` file format itself is
documented in [`docs/qgs-format.md`](docs/qgs-format.md).

## License

MIT © Hiroaki Yutani
