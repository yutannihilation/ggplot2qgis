# ggplot2qgis

<!-- badges: start -->
[![R-CMD-check](https://github.com/yutannihilation/ggplot2qgis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yutannihilation/ggplot2qgis/actions/workflows/R-CMD-check.yaml)
[![ggplot2qgis status badge](https://yutannihilation.r-universe.dev/ggplot2qgis/badges/version)](https://yutannihilation.r-universe.dev/ggplot2qgis)
<!-- badges: end -->

Export a [ggplot2](https://ggplot2.tidyverse.org/) map plot (e.g. `geom_sf()`)
or a [tmap](https://r-tmap.github.io/tmap/) map as a
[QGIS](https://qgis.org/) project (`.qgs`) file.

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

![A ggplot2 map (front) and the exported QGIS project (back) showing the same North Carolina counties with the same fill colors over an OpenStreetMap basemap](man/figures/screenshot.png)

See `?write_qgs` for the full set of options (`use_plot_crs`,
`gradient_style`, `basemap`).

### tmap

A [tmap](https://r-tmap.github.io/tmap/) (>= 4.4) object with vector layers
works the same way, reproducing tmap's own trained color scales
(`tm_scale_intervals()` with its exact class boundaries,
`tm_scale_categorical()`, `tm_scale_continuous()`) and converting
`tm_basemap()` to an XYZ tile layer:

``` r
library(tmap)

x <- tm_basemap("OpenStreetMap") +
  tm_shape(nc) +
  tm_polygons(fill = "AREA")

write_qgs(x, "nc.qgs")
```

## TODOs

- [ ] Support labels
- [ ] Support tidyterra
  - [ ] vector
  - [ ] raster
- [x] Support tmap (vector only)
  - [ ] raster
  - [ ] symbol size / alpha / line type constants
  - [ ] `tm_symbols()` on polygons (centroids)
