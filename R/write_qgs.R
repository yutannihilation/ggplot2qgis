# Number of equal-interval classes of a graduated renderer. QGIS classifies
# the attribute into this many ranges; the colors are interpolated from the
# gradient stops. High enough to approximate ggplot2's continuous gradient,
# at the cost of a long legend. (style_continuous() would reproduce the
# gradient exactly, but QGIS shows no color ramp in the legend for it.)
QGS_GRADUATED_CLASSES <- 25L

# Number of gradient stops sampled from a continuous scale. QGIS
# interpolates between stops in RGB space while ggplot2 interpolates in Lab
# space, so sample densely enough that the difference is invisible.
QGS_GRADIENT_STOPS <- 21L

# Millimeters per lwd unit: an R line width of 1 is 1/96 inch.
QGS_MM_PER_LWD <- 25.4 / 96

# Millimeters per ggplot2 linewidth unit: 1 linewidth is .pt (72.27 / 25.4)
# lwd units of 1/96 inch each, i.e. 72.27 / 96 mm.
QGS_MM_PER_LINEWIDTH <- 72.27 / 96

# The size of a ggplot2 point symbol, in millimeters per unit of `size`
# and per unit of `stroke`. Both geoms draw the symbol with pointsGrob()
# at its default size of one "char", i.e. the gpar fontsize, which
# gg_par() sets to `size * .pt + stroke * .stroke / 2` points
# (.pt = 72.27 / 25.4, .stroke = 96 / 25.4). A grid point is 1/72 inch,
# which makes the two contributions 72.27/72 mm and exactly 2/3 mm.
QGS_MM_PER_POINT_SIZE <- 25.4 / 72 * (72.27 / 25.4)
QGS_MM_PER_POINT_STROKE <- 25.4 / 72 * (96 / 25.4) / 2

# The lwd a ggplot2 point symbol's ring is drawn at, per unit of `stroke`
# (gg_par()'s `.stroke / 2`). No point geom has a `linewidth`.
QGS_LWD_PER_STROKE <- (96 / 25.4) / 2

# Characters a layer name cannot contain: the name becomes the GeoPackage
# file name (so path separators and the characters Windows forbids in file
# names are out) and the `|layername=` part of the ogr datasource URI (so
# `|`, its delimiter, is out too).
QGS_LAYER_NAME_FORBIDDEN <- "[/\\\\|:*?\"<>[:cntrl:]]"

# Predefined XYZ basemaps `basemap` accepts by key. Each is the display
# name, the {z}/{x}/{y} URL template, and the tile set's zoom range.
QGS_BASEMAPS <- list(
  osm = list(
    name = "OpenStreetMap",
    url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    zmin = 0L, zmax = 19L
  ),
  gsi_standard = list(
    # GSI standard map, name escaped so the R source stays ASCII (R CMD
    # check warns on non-ASCII characters in code).
    name = "\u5730\u7406\u9662\u30bf\u30a4\u30eb\uff08\u6a19\u6e96\u5730\u56f3\uff09",
    url = "https://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png",
    zmin = 0L, zmax = 18L
  ),
  gsi_pale = list(
    # GSI pale map (name is the escaped literal below).
    name = "\u5730\u7406\u9662\u30bf\u30a4\u30eb\uff08\u6de1\u8272\u5730\u56f3\uff09",
    url = "https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png",
    zmin = 0L, zmax = 18L
  )
)

#' Write a ggplot2 or tmap map as a QGIS project
#'
#' Converts a ggplot2 plot whose layers are drawn from sf objects (or, for
#' [ggplot2::geom_point()], [ggplot2::geom_path()], [ggplot2::geom_line()]
#' and [ggplot2::geom_polygon()], from plain data frames, or from a
#' SpatRaster via [tidyterra::geom_spatraster()],
#' [tidyterra::geom_spatraster_rgb()],
#' [tidyterra::geom_spatraster_contour()],
#' [tidyterra::geom_spatraster_contour_text()] or
#' [tidyterra::geom_spatraster_contour_filled()]) into a QGIS project
#' (`.qgs`) file. A SpatVector layer, drawn with
#' [tidyterra::geom_spatvector()] or with `geom_sf()` itself, counts as an
#' sf layer (see *SpatVector layers*). The data of each layer is saved as
#' a GeoPackage (a GeoTIFF for raster layers) under
#' `<path minus extension>_data/`, and the layer is styled after the
#' plot's trained color scale:
#'
#' - a continuous `fill`/`colour` scale becomes a graduated renderer with
#'   fine-grained equal-interval classes (or a continuously interpolated
#'   color, see `gradient_style`),
#' - a binned one (e.g. [ggplot2::scale_fill_steps()]) becomes a graduated
#'   renderer with one class per bin, using the scale's exact bin
#'   boundaries and colors,
#' - a discrete one becomes a categorized renderer,
#' - a layer with no `fill`/`colour` mapping becomes a single symbol with
#'   the color ggplot2 would have used.
#'
#' Following ggplot2's semantics for polygons, `fill` is the interior and
#' `colour` is the border: a `colour` scale on a polygon layer colors the
#' outlines while the interior keeps the constant fill. Constant outline
#' colors and widths are taken from the plot as well, as are the marker
#' constants of a point layer (see *Point symbols*) and `alpha` (see
#' *Opacity*). Mapping both `fill` and `colour` on the same layer is not
#' supported.
#'
#' A constant `fill`/`colour` of `NA` renders as "not drawn", the way
#' ggplot2 draws it: an unfilled polygon keeps its border, a `colour = NA`
#' one loses it, and a layer left with nothing to draw at all (a polygon
#' with neither color, a line without its own) is an error.
#'
#' Only a bare column name is supported for the `fill`/`colour` aesthetics;
#' a constant or a computed expression (e.g. `aes(fill = AREA * 2)`) is an
#' error.
#'
#' # Line types
#'
#' A constant `linetype` (tmap: `lty`) is carried over to the symbol's
#' stroke — the body of a line, the border of a polygon, the ring around
#' a point marker. `"solid"`, `"dashed"`, `"dotted"` and `"dotdash"`
#' (1-4 in numeric form) map to the matching QGIS pen styles.
#' `"longdash"`, `"twodash"` and the hex on/off patterns (e.g. `"1343"`)
#' have no QGIS preset: on a line layer they become the equivalent custom
#' dash pattern (scaled by the line width, like in R), while polygon
#' borders and marker rings, which only support the preset pen styles,
#' get the nearest preset instead. `"blank"` (0) turns a polygon border
#' or marker ring off; a blank line layer would draw nothing and is an
#' error. A linetype that varies by feature (a mapped `linetype`/`lty`)
#' cannot be represented by these renderers: the symbols are drawn with
#' solid lines, with a warning.
#'
#' # Point symbols
#'
#' A point layer's `shape` becomes the QGIS marker of the same outline:
#' pch 0/15/22 a `square`, 1/16/19/20/21 a `circle`, 5/18/23 a `diamond`,
#' 2/17/24 an `equilateral_triangle` (6/25 the same, rotated 180 degrees),
#' 3 a `cross` and 4 a `cross2`; ggplot2's shape names (`"circle
#' filled"`, ...) are those same symbols. The composite symbols
#' (pch 7-14), R's thin asterisk (8), a single-character shape (which
#' draws a text glyph) and tmap's grob shapes have no QGIS counterpart and
#' are errors rather than a silently different marker.
#'
#' How R colors the shape decides where the layer's colors go: pch 0-6 are
#' stroke-only, so the marker gets a transparent interior and its
#' `colour`; pch 15-20 are filled with a single color (ggplot2's `colour`,
#' tmap's `fill`, see *tmap symbol constants*) and get no distinct border;
#' pch 21-25 fill with `fill` and stroke with `colour`. Mapping a color to
#' the slot the shape does not draw is an error.
#'
#' The marker's size is the symbol's extent, which each package computes
#' its own way: ggplot2 spans `size` millimeters plus two thirds of
#' `stroke`, tmap a multiple of the text line height (see *tmap symbol
#' constants*). QGIS is then given the width the shape's ink actually
#' spans — three quarters of that extent for a circle — so a default
#' ggplot2 point (`size = 1.5`, `stroke = 0.5`, pch 19) is a 1.38 mm
#' circle. Its ring is `stroke` wide in R's line-width units, i.e.
#' 0.25 mm by default; `linewidth`, which the point geoms do not have,
#' plays no part.
#'
#' A `size`, `shape` or `stroke` that varies by feature (a mapped
#' aesthetic) is drawn with the first feature's value, with a warning: the
#' QGIS renderers vary the color and nothing else.
#'
#' # Opacity
#'
#' A constant `alpha` (tmap: `fill_alpha` and `col_alpha`) becomes the
#' alpha component of the colors it applies to, rather than QGIS's
#' symbol-wide opacity — which could not render a translucent fill under
#' an opaque border. Which colors it applies to is what the plotting
#' package draws with it: ggplot2 gives a polygon's interior its `alpha`
#' and leaves the border opaque, applies it to a line's own color, and to
#' both colors of a point marker; tmap's two alphas belong to `fill` and
#' `col` respectively. Like the other constants, an `alpha` that varies by
#' feature is dropped down to the first feature's value, with a warning.
#'
#' An alpha of 0 means "not drawn", like an NA color, and a layer with
#' nothing left to draw is an error. The classes of a graduated or
#' categorized renderer carry the alpha too, as does the data-defined
#' color expression of `gradient_style = "continuous"`, but the
#' renderer's `[source]` color ramp stays opaque: it is the palette rather
#' than the rendering opacity, so re-classifying the layer in QGIS drops
#' the transparency. An alpha a color carries itself
#' (`colour = "#FF000080"`) is not carried over — only its RGB part is.
#'
#' # Text and labels
#'
#' A [ggplot2::geom_sf_text()] or [ggplot2::geom_sf_label()] layer becomes
#' a separate labels-only QGIS layer: its features are not drawn (a
#' null-symbol renderer) and the `label` aesthetic — a bare column name,
#' like `fill`/`colour` — becomes the labeled field, rendered by QGIS's
#' own labeling engine. [ggplot2::geom_text()] and [ggplot2::geom_label()]
#' work the same way for plain data frames, under the *Data frame layers*
#' rules (the plot must use [ggplot2::coord_sf()], `x`/`y` must be bare
#' untransformed columns, identity stat and position — so nudging is not
#' supported).
#'
#' The carried-over styles are the text size (in the layer's `size.unit`),
#' the font family and the text color and, for the label geoms, the
#' background fill color (`geom_label()`'s `fill`; drawn as a plain
#' rectangle, `fill = NA` disables it). R's default sans-serif family
#' (`""` or `"sans"`) is a device alias rather than a font name, so it
#' leaves QGIS its own default font. Everything else (fontface, rounded
#' corners, hjust/vjust, alpha, ...) keeps the QGIS labeling defaults, and
#' the label placement is QGIS's: point labels are drawn over the point,
#' line labels along and *on* the line (rather than QGIS's default of
#' above it, since ggplot2 centers its text on the geometry), polygon
#' labels around the centroid.
#'
#' A layer whose features are drawn under its own labels — that is
#' `geom_spatraster_contour_text()`, see *SpatRaster layers* — masks
#' them, so the text is not overdrawn by the line it labels. The
#' labels-only layers of the text/label geoms have nothing to mask.
#'
#' # Data frame layers
#'
#' A `geom_point()`, `geom_path()`, `geom_line()` or `geom_polygon()` layer
#' drawn from a plain data frame is converted to an sf layer: one point per
#' row, or one linestring/polygon per group (ggplot2's grouping — an
#' explicit `group` aesthetic or the interaction of the discrete
#' aesthetics; `geom_line()` orders each line by `x` like ggplot2 does,
#' and polygon rings are closed). The plot must use [ggplot2::coord_sf()],
#' and the `x`/`y` values are taken to be coordinates in the panel CRS:
#' `coord_sf()`'s `crs` argument if given, otherwise the CRS of the first
#' sf layer (`coord_sf(default_crs = )` is not supported). Like
#' `fill`/`colour`, the `x`/`y` aesthetics must be bare column names, and
#' the layer must use the identity stat and position.
#'
#' A point layer keeps every column of the data frame as attributes; a
#' line/polygon layer keeps the columns that are constant within every
#' group, one feature per group (a mapped `fill`/`colour` column must be
#' constant within each group). `geom_polygon()`'s default `colour` is
#' `NA`, so such a layer is written without a border. A
#' `geom_point()` layer's `size`, `shape`, `stroke` and `alpha` are
#' carried over like a `geom_sf()` point layer's (see *Point symbols*
#' and *Opacity*).
#'
#' The project opens zoomed to the plot's displayed range (the panel range,
#' including the default expansion and any [ggplot2::coord_sf()] `xlim`/
#' `ylim`), reprojected to the project CRS, rather than the whole world.
#'
#' # SpatRaster layers
#'
#' A [tidyterra::geom_spatraster()] layer becomes a QGIS raster layer:
#' the SpatRaster is written as a GeoTIFF (`<layer name>.tif`, named
#' after the band by default) next to the GeoPackages, and rendered by a
#' single-band pseudocolor renderer reproducing the plot's continuous
#' `fill` scale as an interpolated color ramp. Unlike for vector layers
#' the ramp is exact *and* the legend shows it as a continuous ramp, so
#' the `gradient_style` option does not apply to raster layers. Cells
#' with missing values are transparent (the GeoTIFF's nodata value).
#'
#' A [tidyterra::geom_spatraster_rgb()] layer becomes a QGIS raster
#' layer with a multiband (true color) renderer. The written GeoTIFF
#' holds the three bands in red-green-blue order, with the layer's
#' `r`/`g`/`b` band selection, `zlim`/`stretch` rescaling and the
#' constant `alpha` (the renderer's opacity) carried over. A
#' `max_col_value` other than 255 becomes a linear contrast stretch
#' from `0..max_col_value`, matching how tidyterra scales the channel
#' values.
#'
#' What is written is the data the plot draws: a raster larger than
#' the geom's `maxcell` argument (500,000 cells by default)
#' has already been downsampled by tidyterra when the layer was created
#' and the original is not recoverable from the plot object — so the
#' GeoTIFF stays small, but is not the full-resolution source. For
#' `geom_spatraster()`, only a single-band SpatRaster with a continuous
#' (not binned) `fill` scale and the default `fill` mapping (the band
#' value) is supported for now; tidyterra's color tables
#' (`scale_fill_coltab()`) are not.
#'
#' The contour geoms are the exception among the SpatRaster geoms: they
#' draw vector shapes, so they become ordinary GeoPackage-backed layers.
#' [tidyterra::geom_spatraster_contour()] becomes a LineString layer with
#' one feature per contour line (per level and per piece), and
#' [tidyterra::geom_spatraster_contour_filled()] a Polygon layer with one
#' MULTIPOLYGON feature per band, the holes punched by the bands above it
#' kept as holes. Both keep the band name as a `lyr` attribute and the
#' contour value as a `level` one — a number for the lines, the band's
#' label (e.g. `"(70, 80]"`) for the filled bands. The shapes are the ones
#' the layer's stat computed, so the geom's `breaks`/`bins`/`binwidth` are
#' reproduced as they are — and, since the stat reprojects the raster to
#' the plot's CRS before contouring, the layer's CRS is the plot's, not
#' the raster's.
#'
#' [tidyterra::geom_spatraster_contour_text()] draws the same isolines with
#' their value written along them, so it becomes that same LineString
#' layer with QGIS labeling enabled (see *Text and labels*). The text is
#' written as a `label` attribute — the `label` aesthetic (the contour
#' value by default) run through the geom's `label_format`, so a custom
#' format or a vector of labels is reproduced as it is; `label_format =
#' NULL`, which tells the geom to place no labels, leaves the layer
#' unlabeled. The labels mask the lines they are written into, which is
#' how the gap ggplot2 breaks in each line under its label is reproduced.
#'
#' On a `geom_spatraster_contour()` layer, `colour` may be mapped to
#' `after_stat(level)`, which becomes a renderer on the `level` attribute
#' under the usual `gradient_style` rules. A
#' `geom_spatraster_contour_filled()` layer always varies its `fill` (the
#' stat maps it to `after_stat(level)`), which becomes a categorized
#' renderer, one category per level of the fill scale. `level` is the only
#' computed value written, so any other `colour`/`fill` expression is an
#' error, as is a mapping the geom does not draw (`fill` on the lines and
#' on the text, `colour` on the bands). On the text geom, `colour` colors
#' the labels as well as the lines, and a QGIS labeling carries a single
#' text color: the lines follow the scale and every label is drawn in the
#' first feature's color, with a warning (silently when the layer has no
#' labels to begin with). The constant `colour`, `linewidth`, `linetype`
#' and `alpha` are carried over like any other line or polygon layer's. A
#' multi-band SpatRaster is not supported.
#'
#' The layers are named `"<band>_contour"`, `"<band>_contour_text"` and
#' `"<band>_contour_filled"` by default, so overlaying the contours on the
#' raster itself gives distinguishable layers.
#'
#' `geom_spatraster()` appends an invisible helper layer (a single empty
#' point carrying the raster's CRS to `coord_sf()`). Such a layer draws
#' nothing, so it is not written to the project: in general, an sf layer
#' whose geometries are all empty is skipped and does not count for
#' `layer_names`.
#'
#' # SpatVector layers
#'
#' [tidyterra::geom_spatvector()], [tidyterra::geom_spatvector_text()] and
#' [tidyterra::geom_spatvector_label()] are wrappers of
#' [ggplot2::geom_sf()], [ggplot2::geom_sf_text()] and
#' [ggplot2::geom_sf_label()], and tidyterra registers a `fortify()` method
#' that turns a `SpatVector` into an sf object when the plot or the layer is
#' created. Such a layer is therefore an ordinary sf layer here, converted
#' by all the rules above, and so is a plain `geom_sf()` layer given a
#' `SpatVector` as its data. `terra::vect()` typically produces a geometry
#' column mixing the single and `MULTI` variants of one type (e.g.
#' `POLYGON` and `MULTIPOLYGON`), which is cast to the `MULTI` variant
#' since a GeoPackage table holds a single geometry type.
#'
#' One consequence of the wrapping shows up in the derived layer names:
#' ggplot2 records the *wrapped* `geom_sf()` call as the layer's
#' constructor, so the name comes from the variable the `SpatVector` is
#' bound to rather than from the call. When there is no such variable (e.g.
#' `geom_spatvector(data = terra::vect(path))`), the geom fallback names
#' the layer `geom_sf`, not `geom_spatvector` — the wrapper's own name is
#' not recorded anywhere in the layer. Pass `layer_names` for full control.
#'
#' # tmap plots
#'
#' A tmap (>= 4.4) object is converted the same way:
#' `tm_polygons()`/`tm_fill()`/`tm_borders()`, `tm_lines()`, and
#' `tm_symbols()`/`tm_dots()`/`tm_bubbles()`/`tm_squares()` (on point data)
#' are supported, including [tmap::qtm()] maps, as is [tmap::tm_raster()]
#' on a raster shape (see *tmap raster layers*). The color scales are
#' reproduced from tmap's own trained scales:
#'
#' - [tmap::tm_scale_intervals()] (any classification style) becomes a
#'   graduated renderer with tmap's exact break boundaries and colors
#'   (zero-width bins from tied breaks are collapsed; the continuous-style
#'   legend variants, `label.style`, are not supported),
#' - [tmap::tm_scale_categorical()] and [tmap::tm_scale_ordinal()] become a
#'   categorized renderer keyed by the raw data values (missing values
#'   become the "all other values" category); tmap's formatted legend
#'   labels are not carried over,
#' - [tmap::tm_scale_continuous()] (including the transformed variants,
#'   approximated with piecewise-linear color stops) becomes a graduated
#'   renderer with 25 equal-interval classes, or an exact continuous
#'   gradient with `gradient_style = "continuous"`.
#'
#' A layer maps either `fill` or `col` to a data column (not both). The
#' constants tmap computed for the layer — `lwd`, `lty` (see *Line
#' types*), `size` and `shape` (see *Point symbols* and *tmap symbol
#' constants*), `fill_alpha` and `col_alpha` (see *Opacity*) — are carried
#' over; a constant that varies between features is dropped down to its
#' first value, with a warning.
#' Layers sharing one [tmap::tm_shape()] share one GeoPackage: the data is
#' written once and every layer of the shape references the same table.
#'
#' QGIS's graduated renderers have no missing-value class, so for an
#' intervals/continuous scale the features whose value is missing go to a
#' separate layer named `"<layer> (missing value)"`, drawn in tmap's
#' `value.na` color directly below its parent layer (the same GeoPackage
#' table, filtered with the QGIS provider filter `"column" IS NULL`). Set
#' `create_na_layer = FALSE` to leave the missing features undrawn
#' instead. Categorical scales render missing values within their own
#' layer (the "all other values" category), so they never get the extra
#' layer. [tmap::tm_basemap()] layers become XYZ tile layers
#' (overriding the `basemap` argument): a URL template is used as is, a
#' provider name is resolved via [maptiles::get_providers()], and with
#' several basemaps only the first one is checked (visible) in the layer
#' tree. Facets, `tm_text()` and the other scale types are errors.
#'
#' Map decorations are dropped: [tmap::tm_graticules()]/[tmap::tm_grid()],
#' and the components tmap draws around the map
#' ([tmap::tm_compass()], [tmap::tm_scalebar()], [tmap::tm_title()],
#' [tmap::tm_credits()], ...). They carry no data of their own, and QGIS
#' offers its own equivalents. [tmap::tm_tiles()] is *not* dropped but an
#' error: it draws content that would silently go missing.
#'
#' A caveat on the bin edges: tmap's interval bins are left-closed
#' (`[a, b)`) while every QGIS classed renderer is right-closed
#' (`(a, b]`), so a feature or cell whose value falls exactly on an
#' interior break renders one class lower in QGIS than in tmap.
#'
#' The project CRS defaults to the tmap display CRS (`use_plot_crs = TRUE`
#' for tmap objects): [tmap::tm_crs()] or the main shape's CRS — or
#' EPSG:3857 when the map has basemaps, which is how tmap itself resolves
#' it — so the project opens looking like the tmap plot, zoomed to the
#' main shape's bounding box. `use_plot_crs = FALSE` forces EPSG:3857.
#'
#' The conversion relies on tmap internals that are not part of its public
#' API, so a tmap version older than 4.4 is rejected.
#'
#' # tmap symbol constants
#'
#' What *Point symbols* leaves to each package is the symbol's extent:
#' tmap's `size` is a multiple of the text line height, which is 5.08 mm
#' on an R device with the default 12 pt font, so a default
#' `tm_symbols()` marker is a 3.81 mm circle (three quarters of it) and a
#' `tm_dots()` one 1.143 mm. A device opened with a different `pointsize`
#' would draw tmap's symbols at a different physical size; the conversion
#' assumes the default. [tmap::tm_layout()]'s `scale` is already part of
#' the size tmap computes, so it carries over.
#'
#' tmap moves `col` into `fill` before drawing a pch 15-20 symbol (R fills
#' those with `col`), so the single color of such a marker is tmap's
#' `fill` — which is also why mapping `col` on one is an error.
#'
#' # tmap raster layers
#'
#' [tmap::tm_raster()] on a raster shape (a stars object, a SpatRaster or
#' a RasterLayer passed to [tmap::tm_shape()]) becomes a QGIS raster
#' layer, written as a single-band GeoTIFF next to the project. The
#' renderer comes from the same trained scale as for vector layers:
#'
#' - [tmap::tm_scale_intervals()] (the default for a numeric variable)
#'   becomes a `singlebandpseudocolor` renderer with a DISCRETE color-ramp
#'   shader holding tmap's exact breaks, colors and bin labels,
#' - [tmap::tm_scale_categorical()] and [tmap::tm_scale_ordinal()] become
#'   a `paletted` renderer with one entry per value. A factor variable is
#'   written as its integer codes and labeled with the level names;
#'   values that no cell has are dropped,
#' - [tmap::tm_scale_continuous()] becomes a `singlebandpseudocolor`
#'   renderer with an INTERPOLATED shader, which reproduces the gradient
#'   exactly and shows a continuous legend ramp — so `gradient_style` does
#'   not apply to raster layers.
#'
#' What is written is the grid tmap *draws*: tmap downsamples a raster
#' beyond `tmap_options(raster.max_cells =)` and reprojects it to the
#' display CRS before the scales are trained, and the original is not
#' recoverable from the plot object.
#'
#' Missing cells become the GeoTIFF's nodata value and are painted in
#' tmap's `value.na` color through the renderer's `nodataColor` — exactly,
#' so raster layers never get the separate `"(missing value)"` layer that
#' vector layers do and `create_na_layer` does not apply to them. A
#' constant `col_alpha` becomes the layer opacity; a per-cell one is an
#' error. Unlike vector layers, raster layers of one [tmap::tm_shape()] do
#' *not* share a file: each writes its own single-band GeoTIFF, since two
#' variables can differ in value type and nodata value.
#'
#' [tmap::tm_rgb()]/[tmap::tm_rgba()], [tmap::tm_scale_discrete()], and
#' curvilinear, rotated or irregularly spaced grids are errors.
#'
#' @param plot A ggplot object whose layers are backed by sf data (or
#'   SpatVector data, see *SpatVector layers*), one of the supported
#'   data.frame geoms (see *Data frame layers*),
#'   [tidyterra::geom_spatraster()], [tidyterra::geom_spatraster_rgb()],
#'   [tidyterra::geom_spatraster_contour()],
#'   [tidyterra::geom_spatraster_contour_text()] or
#'   [tidyterra::geom_spatraster_contour_filled()] (see *SpatRaster
#'   layers*), or a tmap object (see *tmap plots* and *tmap raster
#'   layers*).
#' @param path Path of the `.qgs` file to write. Tilde paths (e.g. `~/x.qgs`)
#'   are expanded.
#' @param use_plot_crs If `TRUE`, the project (map canvas) CRS is the plot's
#'   display CRS: for a ggplot, resolved the way [ggplot2::coord_sf()] does
#'   (its `crs` argument if specified, otherwise the CRS of the first layer
#'   that defines one); for a tmap object, tmap's own display CRS (see
#'   *tmap plots*). If `FALSE`, the project CRS is EPSG:3857 (Web
#'   Mercator). The default is `FALSE` for ggplot plots and `TRUE` for tmap
#'   objects. Either way the layers keep the CRS of their data; QGIS
#'   reprojects them on the fly.
#' @param gradient_style How a continuous `fill`/`colour` scale is rendered:
#'
#'   - `"graduated"` (the default): a graduated renderer with 25
#'     equal-interval classes. The gradient is slightly banded, but the
#'     legend shows the classes with their value ranges.
#'   - `"continuous"`: the exact ggplot2 look. The color is interpolated
#'     per feature by a data-defined expression on the symbol color
#'     (`ramp_color(create_ramp(...), ...)`). Caveats: QGIS cannot display
#'     a color ramp in the legend for a data-defined color, so the legend
#'     is a single swatch without any value labels, and the gradient is
#'     only discoverable in the layer styling panel behind the
#'     data-defined override of the symbol color, not in the renderer
#'     dropdown.
#'
#'   Binned scales are unaffected: their bins are exact in a graduated
#'   renderer, so there is nothing to trade off. Requesting `"continuous"`
#'   for a layer with a binned scale keeps the bins, with a warning.
#'   Raster layers are unaffected too: their shader is exact *and*
#'   legend-friendly, so there is no trade-off to make.
#' @param basemap An XYZ tile layer to add below the vector layers, or `NULL`
#'   (the default) for none. Either a predefined key or an arbitrary XYZ URL
#'   template (a string containing the `{z}`, `{x}` and `{y}` placeholders,
#'   e.g. `"https://tile.openstreetmap.org/{z}/{x}/{y}.png"`). The predefined
#'   keys are:
#'
#'   - `"osm"`: OpenStreetMap.
#'   - `"gsi_standard"`: the GSI standard map
#'     (<https://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png>).
#'   - `"gsi_pale"`: the GSI pale map
#'     (<https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png>).
#'
#'   XYZ tiles are in EPSG:3857; QGIS reprojects them to the project CRS on
#'   the fly.
#' @param create_na_layer tmap vector layers only. If `TRUE` (the default),
#'   features whose mapped value is missing become a separate
#'   `"<layer> (missing value)"` layer in tmap's `value.na` color (see
#'   *tmap plots*). If `FALSE`, they are not drawn. Raster layers express
#'   missing cells exactly and ignore this (see *tmap raster layers*).
#' @param overwrite If `FALSE` (the default), writing to a `path` that already
#'   exists is an error. Set to `TRUE` to overwrite it.
#' @param layer_names Names for the layers, used for the GeoPackage (or
#'   GeoTIFF) files and in the QGIS layer tree: a character vector with
#'   one name per layer, bottom-most first (layers that are skipped
#'   because they draw nothing — see *SpatRaster layers* — do not
#'   count). `/`, `\`, `|`, `:`, `*`, `?`, `"`, `<`, `>`
#'   and control characters cannot be used (the name becomes a file name).
#'   If `NULL` (the default), each layer is named after the first of these
#'   that applies:
#'
#'   - the layer's own name (e.g. `geom_sf(name = "counties")`),
#'   - the variable its data came from (e.g. `nc` for
#'     `geom_sf(data = nc)`, or for `ggplot(nc)` when the variable is
#'     unambiguous),
#'   - the geom (e.g. `geom_sf`),
#'
#'   with a numbered suffix (`nc_2`) on collision.
#' @param ... Passed on to the methods.
#' @returns `path`, invisibly.
#' @examples
#' library(ggplot2)
#'
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' p <- ggplot(nc) +
#'   geom_sf(aes(fill = AREA))
#'
#' write_qgs(p, tempfile(fileext = ".qgs"))
#'
#' # tmap objects work the same way
#' if (requireNamespace("tmap", quietly = TRUE)) {
#'   x <- tmap::tm_shape(nc) + tmap::tm_polygons(fill = "AREA")
#'   write_qgs(x, tempfile(fileext = ".qgs"))
#' }
#' @importFrom rlang %||%
#' @export
write_qgs <- function(plot, path, ...) {
  UseMethod("write_qgs")
}

#' @export
write_qgs.default <- function(plot, path, ...) {
  stop(
    "`plot` must be a ggplot or tmap object, got ", class(plot)[1],
    call. = FALSE
  )
}

#' @rdname write_qgs
#' @export
write_qgs.ggplot <- function(plot, path, use_plot_crs = FALSE,
                             gradient_style = c("graduated", "continuous"),
                             overwrite = FALSE, layer_names = NULL,
                             basemap = NULL, ...) {
  rlang::check_dots_empty()
  layers <- plot@layers
  if (length(layers) == 0L) {
    stop("`plot` must have at least one layer", call. = FALSE)
  }
  if (!isTRUE(use_plot_crs) && !isFALSE(use_plot_crs)) {
    stop("`use_plot_crs` must be TRUE or FALSE", call. = FALSE)
  }
  if (!isTRUE(overwrite) && !isFALSE(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE", call. = FALSE)
  }
  gradient_style <- match.arg(gradient_style)
  basemap_layer <- qgs_basemap_layer(basemap)

  path <- path.expand(path)

  if (!overwrite && file.exists(path)) {
    stop(
      "`path` already exists: ", path,
      "\nSet `overwrite = TRUE` to overwrite it.",
      call. = FALSE
    )
  }

  # The raw data of each layer, validated before ggplot_build() so a bad
  # layer fails with a specific error, not somewhere inside the build.
  layer_data <- lapply(seq_along(layers), function(i) {
    qgs_layer_data(plot, layers[[i]], i)
  })

  # Layers that draw nothing (see qgs_skip_layer()) are not written; they
  # do not count for `layer_names` either.
  keep <- !vapply(layer_data, qgs_skip_layer, logical(1L))
  if (!any(keep)) {
    stop("`plot` has no layers to write", call. = FALSE)
  }
  layer_names <- qgs_layer_names(plot, layer_names, layer_data, keep)

  # Build the plot first so that the scales are trained by the data.
  built <- ggplot2::ggplot_build(plot)

  data_dir_name <- paste0(tools::file_path_sans_ext(basename(path)), "_data")
  data_dir <- file.path(dirname(path), data_dir_name)
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  qgs_layers <- vector("list", length(layers))

  # coord_sf() uses its crs argument if specified, otherwise the CRS of
  # the first layer that defines one; the built plot carries the result
  # (as given, so e.g. a bare EPSG code needs normalization).
  plot_crs <- NULL
  if (use_plot_crs) {
    plot_crs <- built@layout$panel_params[[1]]$crs
    if (!is.null(plot_crs)) {
      plot_crs <- sf::st_crs(plot_crs)
    }
  }

  # ggplot2's first layer is the bottom-most one, which is also the order
  # qgs_build() expects.
  for (i in seq_along(layers)) {
    if (!keep[[i]]) {
      next
    }
    layer <- layers[[i]]

    d <- layer_data[[i]]
    is_rgb <- qgs_is_spatraster_rgb_layer(layer)
    is_raster <- is_rgb || qgs_is_spatraster_layer(layer)
    # NULL for anything but a contour layer, else which kind it is (see
    # contour.R): they differ in their geometry and in how their varying
    # color is resolved.
    contour <- if (qgs_is_spatraster_contour_layer(layer)) {
      "lines"
    } else if (qgs_is_spatraster_contour_text_layer(layer)) {
      "text"
    } else if (qgs_is_spatraster_contour_filled_layer(layer)) {
      "filled"
    }
    sf_data <- inherits(d, "sf")
    is_text <- qgs_is_text_layer(layer)
    label <- NULL
    spat <- NULL
    if (is_raster) {
      # geom_spatraster()/geom_spatraster_rgb(): the layer carries the
      # SpatRaster itself, which is written as a GeoTIFF below instead
      # of a GeoPackage (see spatraster.R).
      spat <- if (is_rgb) {
        qgs_spatraster_rgb(layer, i)
      } else {
        qgs_spatraster(layer, i)
      }
      # A missing CRS is "" here (st_crs("") would error instead of
      # returning the NA CRS the check below expects).
      spat_crs <- terra::crs(spat)
      crs <- if (nzchar(spat_crs)) {
        sf::st_crs(spat_crs)
      } else {
        sf::st_crs(NA_integer_)
      }
    } else if (is_text) {
      # geom_sf_text()/geom_sf_label()/geom_text()/geom_label(): a
      # labels-only layer — QGIS labeling on a layer whose features are
      # not drawn (see labeling.R).
      label <- qgs_label_spec(plot, built, layer, i, d)
      qgs_check_text_position(layer, i)
      if (!inherits(layer$stat, "StatSfCoordinates")) {
        d <- qgs_text_df_sf(plot, built, layer, i, d)
      }
    } else if (identical(contour, "lines")) {
      # geom_spatraster_contour(): the isolines the stat computed become a
      # LineString layer (see contour.R).
      d <- qgs_contour_sf(built, layer, i)
    } else if (identical(contour, "text")) {
      # geom_spatraster_contour_text(): the same LineString layer, with
      # the text drawn along the lines written as a `label` attribute and
      # rendered by QGIS's labeling engine. Unlike the text geoms the
      # features themselves are drawn too, so the layer keeps its style.
      d <- qgs_contour_sf(built, layer, i, text = TRUE)
      label <- qgs_contour_text_label_spec(built, layer, i, d)
    } else if (identical(contour, "filled")) {
      # geom_spatraster_contour_filled(): the bands the stat computed
      # become a Polygon layer (see contour.R).
      d <- qgs_contour_filled_sf(built, layer, i)
    } else if (!sf_data) {
      qgs_check_contour_geom(layer, i)
      d <- qgs_df_layer_sf(plot, built, layer, i, d)
    }

    if (!is_raster) {
      crs <- sf::st_crs(d)
    }
    if (is.na(crs)) {
      stop("layer ", i, ": the data has no CRS", call. = FALSE)
    }
    if (use_plot_crs && (is.null(plot_crs) || is.na(plot_crs))) {
      plot_crs <- crs
    }

    layer_name <- layer_names[[i]]

    if (is_raster) {
      tif_file <- paste0(layer_name, ".tif")
      tif_path <- file.path(data_dir, tif_file)
      if (file.exists(tif_path)) {
        unlink(tif_path)
      }
      terra::writeRaster(spat, tif_path)
      qgs_layers[[i]] <- raster_layer(
        # relative to the project file
        paste0(data_dir_name, "/", tif_file),
        layer_name,
        crs,
        if (is_rgb) {
          qgs_spatraster_rgb_style(layer, spat, i)
        } else {
          qgs_spatraster_style(built, i)
        }
      )
      next
    }

    gpkg_file <- paste0(layer_name, ".gpkg")
    gpkg_path <- file.path(data_dir, gpkg_file)
    if (file.exists(gpkg_path)) {
      unlink(gpkg_path)
    }
    d <- qgs_homogenize_geometry(d)
    sf::st_write(d, gpkg_path, layer = layer_name, quiet = TRUE)

    geometry <- qgs_geometry_type(d, i)
    qgs_layers[[i]] <- vector_layer(
      # relative to the project file
      paste0(data_dir_name, "/", gpkg_file),
      layer_name,
      crs,
      geometry,
      if (is_text) {
        style_none()
      } else {
        qgs_vector_style(
          plot, built, layer, i, d, gradient_style, geometry, contour
        )
      },
      label = label
    )
  }

  # Drop the slots of the skipped layers.
  qgs_layers <- qgs_layers[keep]

  # Open the project zoomed to the plot's displayed range instead of the
  # template's whole-world extent.
  panel <- built@layout$panel_params[[1]]
  project_crs <- if (use_plot_crs) sf::st_crs(plot_crs) else sf::st_crs(3857L)
  extent <- qgs_canvas_extent(panel, project_crs)

  # The basemap draws under every vector layer, so it is the bottom-most
  # entry (qgs_build() expects bottom-most first).
  if (!is.null(basemap_layer)) {
    qgs_layers <- c(list(basemap_layer), qgs_layers)
  }

  project_srs <- if (use_plot_crs) resolve_srs(plot_crs)
  qgs_write(qgs_layers, path, project_srs, extent)

  invisible(path)
}

# The raw data of a layer (its own, or inherited from ggplot()) — not the
# computed data of ggplot_build(), which no longer has the original
# values. Must be an sf object or a data.frame with at least one row.
qgs_layer_data <- function(plot, layer, i) {
  d <- layer$data
  if (is.null(d) || inherits(d, "waiver")) {
    d <- plot@data
  }
  if (is.null(d) || inherits(d, "waiver")) {
    stop("layer ", i, " has no data", call. = FALSE)
  }
  if (!is.data.frame(d)) {
    stop(
      "layer ", i, ": the layer data must be an sf object or a ",
      "data.frame, got ", class(d)[1],
      call. = FALSE
    )
  }
  if (nrow(d) == 0L) {
    stop("layer ", i, ": the data has no rows", call. = FALSE)
  }
  d
}

# A geom parameter of a built layer (`size.unit`, `label_format`, ...), or
# `default` when the geom has no such parameter. Read by name rather than
# with `%||%`, since a parameter is allowed to *be* NULL (which is how
# geom_spatraster_contour_text() spells "place no labels").
qgs_geom_param <- function(layer, name, default = NULL) {
  params <- layer$computed_geom_params
  if (!name %in% names(params)) {
    # Not built yet (computed_geom_params is filled in by ggplot_build()).
    params <- layer$geom_params
  }
  if (!name %in% names(params)) {
    return(default)
  }
  params[[name]]
}

# A layer that would draw nothing and is skipped instead of written: an
# sf layer whose geometries are all empty. geom_spatraster() appends such
# a layer (a single empty point in the raster's CRS) just to carry the
# CRS to coord_sf().
qgs_skip_layer <- function(d) {
  inherits(d, "sf") && all(sf::st_is_empty(sf::st_geometry(d)))
}

# Resolves the `basemap` argument to an xyz_tile_layer(), or NULL when no
# basemap was requested. A predefined key (see QGS_BASEMAPS) wins; otherwise
# the string must be an XYZ URL template with the {z}/{x}/{y} placeholders.
qgs_basemap_layer <- function(basemap) {
  if (is.null(basemap)) {
    return(NULL)
  }
  if (!is.character(basemap) || length(basemap) != 1L || is.na(basemap)) {
    stop("`basemap` must be a single string or NULL", call. = FALSE)
  }

  spec <- QGS_BASEMAPS[[basemap]]
  if (!is.null(spec)) {
    return(xyz_tile_layer(spec$name, spec$url, spec$zmin, spec$zmax))
  }

  # Not a known key: treat it as a URL template. Require the placeholders so
  # a mistyped key fails loudly instead of producing a broken tile source.
  if (!qgs_is_xyz_template(basemap)) {
    stop(
      "`basemap` must be one of ",
      paste0('"', names(QGS_BASEMAPS), '"', collapse = ", "),
      ", or an XYZ URL template containing {z}, {x} and {y}; got: ", basemap,
      call. = FALSE
    )
  }
  xyz_tile_layer("basemap", basemap, 0L, 19L)
}

# Whether a string is an XYZ tile URL template (shared by the `basemap`
# argument and tm_basemap() resolution).
qgs_is_xyz_template <- function(url) {
  all(vapply(c("{z}", "{x}", "{y}"), grepl, logical(1L), url, fixed = TRUE))
}

# The name of every layer, bottom-most first, "" for the layers that are
# not written (see qgs_skip_layer()). `layer_names` is the user-supplied
# override (already documented in write_qgs()), one name per kept layer;
# NULL means derive a name per layer:
#
# 1. the ggplot2 layer name (user-set, so a forbidden character is an
#    error, like in `layer_names`),
# 2. the band name for a tidyterra raster-derived layer (see
#    qgs_tidyterra_layer_name()),
# 3. the variable the data came from,
# 4. the geom that created the layer,
# 5. "layer<i>",
#
# where 2.-5. are derived, so they are silently sanitized instead. A name
# colliding with an earlier one gets a "_2", "_3", ... suffix.
qgs_layer_names <- function(plot, layer_names, layer_data, keep) {
  layers <- plot@layers
  out <- character(length(layers))
  idx <- which(keep)

  if (!is.null(layer_names)) {
    out[idx] <- qgs_validate_layer_names(layer_names, length(idx))
    return(out)
  }

  for (i in idx) {
    name <- layers[[i]]$name
    band_name <- qgs_tidyterra_layer_name(layers[[i]], layer_data[[i]])
    if (!is.null(name)) {
      qgs_check_layer_name(name, paste0("layer ", i, ": the layer name"))
    } else if (!is.null(band_name)) {
      name <- qgs_sanitize_layer_name(band_name, i)
    } else {
      name <- qgs_derived_layer_name(plot, layers[[i]], i)
    }
    out[[i]] <- qgs_uncollide_name(name, out[seq_len(i - 1L)])
  }
  out
}

# The band name a tidyterra raster-derived layer is named after: its
# constructor is a bare ggplot2::layer() call and its data an internal
# tibble, so the derivations in qgs_derived_layer_name() would never find
# anything useful. The contour layers get a "_contour" / "_contour_filled"
# suffix: overlaying the contour lines on the raster itself (tidyterra's
# own example) would otherwise name both after the same band and leave the
# collision suffix ("elevation_2") to say which is which. NULL for any
# other layer.
qgs_tidyterra_layer_name <- function(layer, d) {
  band <- d[["lyr"]]
  if (length(band) == 0L) {
    return(NULL)
  }
  if (qgs_is_spatraster_layer(layer)) {
    as.character(band[[1L]])
  } else if (qgs_is_spatraster_contour_layer(layer)) {
    paste0(as.character(band[[1L]]), "_contour")
  } else if (qgs_is_spatraster_contour_text_layer(layer)) {
    paste0(as.character(band[[1L]]), "_contour_text")
  } else if (qgs_is_spatraster_contour_filled_layer(layer)) {
    paste0(as.character(band[[1L]]), "_contour_filled")
  }
}

# Validates a user-supplied `layer_names` override (shared by the ggplot
# and tmap methods): one non-empty, unique, file-name-safe name per layer.
qgs_validate_layer_names <- function(layer_names, n) {
  if (!is.character(layer_names) || length(layer_names) != n) {
    stop(
      "`layer_names` must be a character vector with one name per layer (",
      n, ")",
      call. = FALSE
    )
  }
  if (anyNA(layer_names) || !all(nzchar(layer_names))) {
    stop("`layer_names` must not contain NA or empty names", call. = FALSE)
  }
  if (anyDuplicated(layer_names)) {
    stop("`layer_names` must be unique", call. = FALSE)
  }
  qgs_check_layer_name(layer_names, "`layer_names`")
  layer_names
}

qgs_check_layer_name <- function(names, what) {
  bad <- grepl(QGS_LAYER_NAME_FORBIDDEN, names)
  if (any(bad)) {
    stop(
      what, " cannot contain any of /\\|:*?\"<> or control characters: ",
      names[bad][1L],
      call. = FALSE
    )
  }
}

qgs_derived_layer_name <- function(plot, layer, i) {
  d <- layer$data
  if (is.null(d) || inherits(d, "waiver")) {
    d <- plot@data
  }

  # The layer's constructor is the geom call as the user wrote it, so a
  # bare symbol passed as its `data` argument is the variable name (`data`
  # is never positional: `mapping` comes first in every geom). The symbol
  # must name data, though: for a geom wrapping another one (e.g.
  # tidyterra::geom_spatvector(), which calls geom_sf()) ggplot2 records
  # the inner call, whose `data` argument is the wrapper's own parameter
  # name rather than anything the user wrote. Such a symbol falls through
  # to the rules below.
  cons <- layer$constructor
  if (is.call(cons)) {
    data_arg <- rlang::call_args(cons)[["data"]]
    if (rlang::is_symbol(data_arg) &&
      qgs_symbol_is_data(data_arg, plot@plot_env)) {
      return(qgs_sanitize_layer_name(rlang::as_string(data_arg), i))
    }
  }

  # The variable the layer's data (own or inherited from ggplot()) is
  # bound to in the environment the plot was created in, e.g. `nc` for
  # ggplot(nc). Only when the match is unambiguous; a guess is worse
  # than the geom fallback.
  name <- qgs_data_binding_name(d, plot@plot_env)
  if (!is.null(name)) {
    return(qgs_sanitize_layer_name(name, i))
  }

  if (is.call(cons)) {
    fn <- rlang::call_name(cons)
    if (!is.null(fn)) {
      return(qgs_sanitize_layer_name(fn, i))
    }
  }

  paste0("layer", i)
}

# The single variable in `env` (not its parents) holding `d`, or NULL if
# there is none or more than one. Bindings that cannot be read (e.g. an
# active binding that errors) are skipped.
qgs_data_binding_name <- function(d, env) {
  if (!is.data.frame(d) || !is.environment(env)) {
    return(NULL)
  }
  hit <- NULL
  for (name in ls(env, sorted = TRUE)) {
    obj <- tryCatch(
      get(name, envir = env, inherits = FALSE),
      error = function(e) NULL
    )
    if (qgs_is_layer_data(obj, d)) {
      if (!is.null(hit)) {
        return(NULL)
      }
      hit <- name
    }
  }
  hit
}

# Whether the variable `sym` names could be a layer's data: a data frame,
# or a terra Spat* object, which the tidyterra geoms turn into one. It is
# looked up from the environment the plot was created in and its parents
# (a plot built inside a function commonly refers to data from the
# caller).
#
# This is what tells a user-written `data` argument from the one a geom
# wrapping another geom leaves in the recorded call: with
# tidyterra::geom_spatvector(), which calls geom_sf(data = data, ...),
# the symbol is the wrapper's own parameter name and resolves to
# utils::data(), a function.
qgs_symbol_is_data <- function(sym, env) {
  if (!is.environment(env)) {
    return(FALSE)
  }
  obj <- tryCatch(
    get(rlang::as_string(sym), envir = env, inherits = TRUE),
    error = function(e) NULL
  )
  is.data.frame(obj) || inherits(obj, c("SpatVector", "SpatRaster"))
}

# Whether `obj` is the object the layer's data `d` came from. ggplot2
# fortifies data when the plot and the layer are created, so an object
# with a fortify() method is no longer identical to what the layer holds:
# a SpatVector (tidyterra registers fortify.SpatVector(), which is all
# that makes geom_spatvector() work — it is geom_sf()) has already become
# an sf object by the time write_qgs() sees the plot. nrow() is compared
# first so a large SpatVector is only converted when it could match at
# all.
qgs_is_layer_data <- function(obj, d) {
  if (identical(obj, d)) {
    return(TRUE)
  }
  if (!inherits(obj, "SpatVector") || !is.data.frame(d) ||
    nrow(obj) != nrow(d)) {
    return(FALSE)
  }
  fortified <- tryCatch(ggplot2::fortify(obj), error = function(e) NULL)
  identical(fortified, d)
}

qgs_sanitize_layer_name <- function(name, i) {
  name <- gsub(QGS_LAYER_NAME_FORBIDDEN, "_", name)
  # A leading dot would hide the .gpkg file; Windows forbids a trailing one.
  name <- gsub("^[ .]+|[ .]+$", "", name)
  if (nzchar(name)) name else paste0("layer", i)
}

qgs_uncollide_name <- function(name, taken) {
  if (!name %in% taken) {
    return(name)
  }
  k <- 2L
  while (paste0(name, "_", k) %in% taken) {
    k <- k + 1L
  }
  paste0(name, "_", k)
}

# The initial map-canvas extent reproducing what the plot displays.
# ggplot2's panel range (`x_range`/`y_range`, already including the default
# expansion and any coord_sf() xlim/ylim) is in the panel CRS; QGIS wants
# the extent in the project CRS. When they differ, the rectangle is
# densified before transforming so a curved reprojected edge is bounded by
# its whole arc, not just the four corners. Returns c(xmin, ymin, xmax,
# ymax), or NULL if the range is unavailable (keeping the world default).
qgs_canvas_extent <- function(panel, project_crs) {
  x_range <- panel$x_range
  y_range <- panel$y_range
  if (is.null(x_range) || is.null(y_range) ||
      anyNA(x_range) || anyNA(y_range)) {
    return(NULL)
  }

  src_crs <- sf::st_crs(panel$crs)
  qgs_reproject_extent(
    c(x_range[1L], y_range[1L], x_range[2L], y_range[2L]),
    src_crs,
    project_crs
  )
}

# Reprojects a c(xmin, ymin, xmax, ymax) extent, densifying the rectangle
# before transforming so a curved reprojected edge is bounded by its
# whole arc, not just the four corners. The rectangle is clipped to the
# destination CRS's area of use first, so e.g. a pole-touching extent
# does not blow up in Web Mercator (whose domain ends at about +-85
# degrees). Same-CRS (or unknown-CRS) extents pass through unchanged.
qgs_reproject_extent <- function(extent, src_crs, dst_crs) {
  if (is.na(src_crs) || is.na(dst_crs) || src_crs == dst_crs) {
    return(extent)
  }

  n <- 100L
  xs <- seq(extent[1L], extent[3L], length.out = n)
  ys <- seq(extent[2L], extent[4L], length.out = n)
  ring <- rbind(
    cbind(xs, extent[2L]),
    cbind(extent[3L], ys),
    cbind(rev(xs), extent[4L]),
    cbind(extent[1L], rev(ys))
  )
  ring <- rbind(ring, ring[1L, , drop = FALSE])
  poly <- sf::st_sfc(sf::st_polygon(list(ring)), crs = src_crs)

  aou <- qgs_crs_area_of_use(dst_crs)
  if (!is.null(aou)) {
    # The area of use is stated in longitude/latitude: clamp the ring
    # there, then transform on to the destination.
    lonlat <- sf::st_crs(4326L)
    coords <- sf::st_coordinates(sf::st_transform(poly, lonlat))[, 1:2]
    coords[, 1L] <- pmin(pmax(coords[, 1L], aou$lon[1L]), aou$lon[2L])
    coords[, 2L] <- pmin(pmax(coords[, 2L], aou$lat[1L]), aou$lat[2L])
    poly <- sf::st_sfc(sf::st_polygon(list(coords)), crs = lonlat)
  }

  bbox <- sf::st_bbox(sf::st_transform(poly, dst_crs))
  as.numeric(bbox[c("xmin", "ymin", "xmax", "ymax")])
}

# The area of use of a CRS as list(lon = c(min, max), lat = c(min, max)),
# parsed from the USAGE BBOX of its WKT (stated as south, west, north,
# east in degrees); NULL when the WKT carries none.
qgs_crs_area_of_use <- function(crs) {
  m <- regmatches(crs$wkt, regexpr("BBOX\\[[^]]+\\]", crs$wkt))
  if (length(m) == 0L) {
    return(NULL)
  }
  v <- suppressWarnings(
    as.numeric(strsplit(substr(m, 6L, nchar(m) - 1L), ",", fixed = TRUE)[[1L]])
  )
  if (length(v) != 4L || anyNA(v)) {
    return(NULL)
  }
  list(lon = c(v[2L], v[4L]), lat = c(v[1L], v[3L]))
}

qgs_geometry_type <- function(d, i) {
  type <- as.character(sf::st_geometry_type(d, by_geometry = FALSE))
  switch(type,
    POINT = ,
    MULTIPOINT = "Point",
    LINESTRING = ,
    MULTILINESTRING = "LineString",
    POLYGON = ,
    MULTIPOLYGON = "Polygon",
    stop("layer ", i, ": unsupported geometry type ", type, call. = FALSE)
  )
}

# A geometry column mixing the single and MULTI variants of one family
# (e.g. POLYGON + MULTIPOLYGON, which a terra::vect()-backed layer
# produces) has the generic GEOMETRY type, which has no QGIS layer
# geometry and no concrete GeoPackage geometry type — cast it to the
# MULTI variant. Any other mix is returned as is and fails in
# qgs_geometry_type().
qgs_homogenize_geometry <- function(d) {
  if (!inherits(d, "sf") ||
    as.character(sf::st_geometry_type(d, by_geometry = FALSE)) != "GEOMETRY") {
    return(d)
  }
  types <- as.character(unique(sf::st_geometry_type(d)))
  if (length(types) == 0L) {
    # No features, no family to pick (and all() below would be
    # vacuously TRUE for the first one tried).
    return(d)
  }
  for (family in c("POINT", "LINESTRING", "POLYGON")) {
    multi <- paste0("MULTI", family)
    if (all(types %in% c(family, multi))) {
      return(sf::st_cast(d, multi))
    }
  }
  d
}

# Resolves which of `fill`/`colour` drives the varying color of a layer:
# NULL when neither is mapped, else list(aes =, attribute =). The layer's
# mapping takes precedence over the plot's, following how ggplot2 itself
# resolves aesthetics. Only a bare column name is supported (the raw data
# is what's written to the GeoPackage). Shared between the style
# resolution below and the data.frame conversion (df_layer.R), which must
# know the styled column to keep it in the per-group attributes.
qgs_style_attribute <- function(plot, layer, i, d) {
  # aes() normalizes `color` to `colour`, so only these two keys exist.
  fill <- layer$mapping[["fill"]] %||% plot@mapping[["fill"]]
  colour <- layer$mapping[["colour"]] %||% plot@mapping[["colour"]]
  if (!is.null(fill) && !is.null(colour)) {
    stop(
      "layer ", i,
      ": mapping both `fill` and `colour` on the same layer is not supported",
      call. = FALSE
    )
  }
  if (is.null(fill) && is.null(colour)) {
    return(NULL)
  }

  aes_name <- if (is.null(fill)) "colour" else "fill"
  quo <- fill %||% colour

  if (!(rlang::is_quosure(quo) && rlang::quo_is_symbol(quo))) {
    stop(
      "layer ", i, ": only a bare column name is supported for `", aes_name,
      "`, got `", rlang::as_label(quo), "`",
      call. = FALSE
    )
  }
  attribute <- rlang::as_string(rlang::quo_get_expr(quo))
  if (!attribute %in% names(d)) {
    stop(
      "layer ", i, ": column `", attribute, "` not found in the layer data",
      call. = FALSE
    )
  }
  list(aes = aes_name, attribute = attribute)
}

# Resolves the aesthetics of a layer into the matching style. A contour
# layer resolves its varying color differently (its data has no user
# columns, see contour.R); everything after that is shared. `contour` is
# NULL, "lines", "text" or "filled".
qgs_vector_style <- function(plot, built, layer, i, d, gradient_style,
                             geometry, contour = NULL) {
  mapped <- switch(contour %||% "",
    lines = qgs_contour_style_attribute(plot, layer, i),
    text = qgs_contour_text_style_attribute(plot, layer, i),
    filled = qgs_contour_filled_style_attribute(plot, layer, i),
    qgs_style_attribute(plot, layer, i, d)
  )

  const <- qgs_layer_constants(built@data[[i]], geometry, i)
  is_point <- geometry == "Point"

  marker <- if (is_point) qgs_point_marker(const, i)
  # Rounded so binary float noise (0.15056250000000002) stays out of the
  # project file.
  outline_width <- round(
    if (is_point) {
      # The ring around a marker is ggplot2's `stroke`, not `linewidth`.
      const$stroke * QGS_LWD_PER_STROKE * QGS_MM_PER_LWD
    } else {
      const$linewidth * QGS_MM_PER_LINEWIDTH
    },
    7
  )

  linetype <- qgs_linetype(const$linetype, i)
  if (geometry == "LineString" && identical(linetype, "no")) {
    stop(
      "layer ", i, ": the layer would not be drawn (`linetype` is blank)",
      call. = FALSE
    )
  }

  # Which constant color reaches which slot of the symbol, and at what
  # opacity: a marker's answer depends on its shape (see QGS_PCH).
  slots <- qgs_color_slots(geometry, const, marker, mapped$aes, i)

  if (is.null(mapped)) {
    return(qgs_apply_constants(
      qgs_single_style(geometry, slots$fill, slots$col, outline_width, i),
      linetype, marker, slots
    ))
  }
  aes_name <- mapped$aes
  attribute <- mapped$attribute

  scale <- built@plot@scales$get_scales(aes_name)
  # ScaleBinned must be checked before is_discrete(): a binned scale is not
  # discrete, but falling through to the gradient paths would smooth away
  # the steps.
  style <- if (inherits(scale, "ScaleBinned")) {
    if (gradient_style == "continuous") {
      # Per layer, not per plot: a mixed plot can have a continuous scale
      # on another layer that the option legitimately applies to.
      warning(
        "layer ", i, ": `gradient_style = \"continuous\"` does not apply ",
        "to a binned scale; the exact bins are kept",
        call. = FALSE
      )
    }
    qgs_binned_style(scale, attribute, i)
  } else if (scale$is_discrete()) {
    qgs_categorized_style(scale, attribute, i)
  } else if (gradient_style == "continuous") {
    qgs_continuous_style(scale, attribute, i)
  } else {
    qgs_graduated_style(scale, attribute, i)
  }

  if (aes_name == "fill" || (is_point && marker$band == "solid")) {
    # The varying color is the interior — a polygon's fill, or the single
    # color R fills a pch 15-20 marker with, which leaves that marker no
    # border at all (qgs_color_slots() dropped its color) — with the
    # constant stroke around it.
    style <- style_set_outline(style, qgs_rgb(slots$col), outline_width)
  } else if (geometry == "LineString") {
    # The line color is the varying one; only the width is constant.
    style <- style_set_outline(style, qgs_rgb(slots$fill), outline_width)
  } else {
    # ggplot2 draws a colour aesthetic on polygons as the border color and
    # on a marker as the ring around it; the interior keeps the constant
    # fill (which an open marker does not draw at all). The outline color
    # is ignored for a stroke target, only its width applies.
    style <- style_set_stroke_target(style, qgs_rgb(slots$fill))
    style <- style_set_outline(style, qgs_rgb(slots$fill), outline_width)
  }

  qgs_apply_constants(style, linetype, marker, slots)
}

# The layer's constant visual values, applied to a style the same way for
# a ggplot2 layer and for a tmap one (and for the latter's missing-value
# companion, ADR 0002).
qgs_apply_constants <- function(style, linetype, marker, slots) {
  style <- style_set_linetype(style, linetype)
  style <- style_set_alpha(style, slots$fill_alpha, slots$stroke_alpha)
  if (!is.null(marker)) {
    style <- style_set_marker(style, marker$name, marker$size, marker$angle)
  }
  style
}

# The QGIS marker of a point layer, as ggplot2 draws it: `shape` picks the
# marker and the symbol spans `size * .pt + stroke * .stroke / 2` points
# (see QGS_MM_PER_POINT_SIZE). The defaults are only reached when the
# built layer has no such column at all, which the point geoms always
# have; an NA is the user's own "do not draw this".
qgs_point_marker <- function(const, i) {
  size <- const$size %||% 1.5
  stroke <- const$stroke %||% 0.5
  if (is.na(stroke)) {
    stroke <- 0 # what gg_par() does with a missing `stroke`
  }
  qgs_marker(
    const$shape %||% 19,
    size * QGS_MM_PER_POINT_SIZE + stroke * QGS_MM_PER_POINT_STROKE,
    i
  )
}

# Where a layer's constant `fill`/`colour` go, and how opaque each slot
# is, following how ggplot2 draws them: `alpha` applies to a polygon's
# interior only (its border stays opaque), to a line's own color, and to
# both colors of a point marker. Which slots a marker draws at all depends
# on its shape (QGS_PCH$band): R draws pch 0-6 with `colour` alone and
# fills pch 15-20 with it, using `fill` only for pch 21-25 — so a `fill`
# mapped to a shape that does not draw it would render something ggplot2
# does not, which is an error. An alpha of 0 makes the slot "not drawn",
# the same as an NA color.
qgs_color_slots <- function(geometry, const, marker, mapped_aes, i) {
  alpha <- qgs_alpha(const$alpha)
  fill <- const$fill
  col <- const$colour
  fill_alpha <- QGS_OPAQUE
  stroke_alpha <- QGS_OPAQUE
  if (geometry == "Polygon") {
    fill_alpha <- alpha
  } else if (geometry == "LineString") {
    stroke_alpha <- alpha
  } else {
    fill_alpha <- alpha
    stroke_alpha <- alpha
    if (marker$band != "bordered") {
      if (identical(mapped_aes, "fill")) {
        stop(
          "layer ", i, ": `fill` is mapped to data but shape ",
          num(marker$shape), " does not draw it",
          call. = FALSE
        )
      }
      # The interior of an open shape is never drawn, and a solid one is
      # filled with `colour`.
      fill <- if (marker$band == "solid") col
      if (marker$band == "open") {
        fill_alpha <- QGS_OPAQUE
      } else if (!is.null(mapped_aes)) {
        # The one color of a solid shape varies per feature, and a QGIS
        # symbol varies one color property: the marker loses its border.
        col <- NULL
      }
    }
  }
  if (fill_alpha == 0L) {
    fill <- NULL
  }
  if (stroke_alpha == 0L) {
    col <- NULL
  }
  list(
    fill = fill,
    col = col,
    fill_alpha = fill_alpha,
    stroke_alpha = stroke_alpha
  )
}

# An 0..1 alpha (ggplot2's `alpha`, tmap's `fill_alpha`/`col_alpha`) as
# QGIS's 0..255. NA means "no alpha given", which draws the color as it is.
qgs_alpha <- function(alpha) {
  if (is.null(alpha) || is.na(alpha)) {
    return(QGS_OPAQUE)
  }
  as.integer(round(alpha * 255))
}

# The constant aesthetics ggplot2 computed for a layer, taken from its
# first feature (only meaningful for aesthetics that are not mapped).
# ggplot2 (>= 4.0, which this package requires) resolves every geom's
# defaults against the theme during the build — GeomSf's per-geometry ones
# included — so a color is concrete here unless the user asked for NA,
# which means "not drawn" and becomes NULL. A missing `linewidth` keeps
# the 0.2 default; the point geoms have none, but they draw their ring
# with `stroke` instead.
qgs_layer_constants <- function(computed, geometry, i) {
  first_or <- function(name, default) {
    v <- computed[[name]]
    if (length(v) == 0L || is.na(v[[1L]])) default else v[[1L]]
  }
  is_point <- geometry == "Point"
  # The marker constants and the opacity cannot vary per feature (see
  # qgs_constant()); the marker ones are only read where they are drawn,
  # so an aesthetic ggplot2 itself ignores (`size` on a polygon layer)
  # does not warn.
  point_constant <- function(name) {
    if (is_point) qgs_constant(computed[[name]], name, i)
  }
  list(
    colour = first_or("colour", NULL),
    fill = first_or("fill", NULL),
    linewidth = first_or("linewidth", 0.2),
    linetype = qgs_constant_linetype(computed[["linetype"]], i),
    size = point_constant("size"),
    shape = point_constant("shape"),
    stroke = point_constant("stroke"),
    alpha = qgs_constant(computed[["alpha"]], "alpha", i)
  )
}

# The value of a visual constant, taken from the layer's first feature: the
# renderers vary the color and nothing else, so a value that differs
# between features (a mapped `size`/`shape`/`alpha`, tmap's
# `lwd = c(1, 2)`) loses all but the first, with a warning. NULL when the
# aesthetic has no column at all (e.g. `size` of a polygon layer).
qgs_constant <- function(values, name, i) {
  if (length(values) == 0L) {
    return(NULL)
  }
  if (length(unique(values)) > 1L) {
    warning(
      "layer ", i, ": a varying `", name, "` is not supported; the first ",
      "value is used for every feature",
      call. = FALSE
    )
  }
  values[[1L]]
}

# The layer's constant linetype, or solid with a warning when it varies
# by feature (a mapped `linetype` aesthetic): the renderers only vary the
# color, and silently drawing the first feature's linetype everywhere
# would misrepresent the plot. Shared with the tmap path (`lty`).
qgs_constant_linetype <- function(values, i, what = "linetype") {
  v <- unique(values)
  if (length(v) > 1L) {
    warning(
      "layer ", i, ": a `", what, "` that varies by feature is not ",
      "supported; the symbols are drawn with solid lines",
      call. = FALSE
    )
    return("solid")
  }
  if (length(v) == 0L || is.na(v[[1L]])) "solid" else v[[1L]]
}

# For a layer without a varying color: the constant colors, already routed
# to their slots (see qgs_color_slots() and its tmap counterpart). `fill`
# is the interior — a polygon's, a marker's — and `col` the outline, or a
# line's own color. NULL means "not drawn" (an NA color, or an alpha of
# 0), which every slot but a line's color can express: a polygon and a
# marker are still drawn by their outline alone (R's open pch are). A
# layer with nothing left to draw is an error.
qgs_single_style <- function(geometry, fill, col, outline_width, i) {
  is_line <- geometry == "LineString"
  main_rgb <- qgs_rgb(if (is_line) col else fill)
  col_rgb <- qgs_rgb(col)
  if (is.null(main_rgb) && (is_line || is.null(col_rgb))) {
    stop(
      "layer ", i, ": the layer would not be drawn (the colors are NA or ",
      "fully transparent)",
      call. = FALSE
    )
  }
  style_set_outline(style_single(main_rgb), col_rgb, outline_width)
}

# The gradient of a trained continuous scale, sampled at evenly spaced
# points so QGIS reproduces ggplot2's gradient regardless of the scale's
# palette.
qgs_gradient_ramp <- function(scale, attribute, i) {
  limits <- scale$get_limits()
  if (anyNA(limits) || limits[2L] <= limits[1L]) {
    stop(
      "layer ", i, ": cannot map `", attribute,
      "` to a color gradient (the scale's limits are degenerate)",
      call. = FALSE
    )
  }

  offsets <- seq(0, 1, length.out = QGS_GRADIENT_STOPS)
  values <- limits[1L] + offsets * (limits[2L] - limits[1L])
  list(
    limits = limits,
    offsets = offsets,
    colors = grDevices::col2rgb(scale$map(values))
  )
}

qgs_graduated_style <- function(scale, attribute, i) {
  ramp <- qgs_gradient_ramp(scale, attribute, i)

  style_graduated(
    attribute,
    classes = QGS_GRADUATED_CLASSES,
    min = ramp$limits[1L],
    max = ramp$limits[2L],
    stops = list(offsets = ramp$offsets, colors = ramp$colors)
  )
}

qgs_continuous_style <- function(scale, attribute, i) {
  ramp <- qgs_gradient_ramp(scale, attribute, i)

  style_continuous(
    attribute,
    min = ramp$limits[1L],
    max = ramp$limits[2L],
    stops = list(offsets = ramp$offsets, colors = ramp$colors)
  )
}

# A trained binned scale (scale_fill_steps() etc.) as explicit bins: the
# boundaries are the scale limits plus the inner breaks, and each bin's
# color is what ggplot2 maps its midpoint to (constant within a bin).
qgs_binned_style <- function(scale, attribute, i) {
  limits <- scale$get_limits()
  if (anyNA(limits) || limits[2L] <= limits[1L]) {
    stop(
      "layer ", i, ": cannot map `", attribute,
      "` to binned colors (the scale's limits are degenerate)",
      call. = FALSE
    )
  }

  breaks <- scale$get_breaks()
  # ggplot2 NA-s out-of-bounds breaks; also drop breaks sitting exactly on
  # a limit so no zero-width bin is emitted.
  breaks <- breaks[is.finite(breaks) & breaks > limits[1L] & breaks < limits[2L]]
  boundaries <- c(limits[1L], sort(breaks), limits[2L])

  mids <- (boundaries[-length(boundaries)] + boundaries[-1L]) / 2
  style_binned(attribute, boundaries, grDevices::col2rgb(scale$map(mids)))
}

qgs_categorized_style <- function(scale, attribute, i) {
  values <- scale$get_breaks()
  values <- values[!is.na(values)]
  if (length(values) == 0L) {
    stop(
      "layer ", i, ": the scale of `", attribute, "` has no values",
      call. = FALSE
    )
  }
  colors <- grDevices::col2rgb(scale$map(values))

  style_categorized(attribute, as.character(values), colors)
}

# NULL (or NA, its data.frame-layer source) stays NULL: "not drawn".
qgs_rgb <- function(color) {
  if (is.null(color) || is.na(color[[1L]])) {
    return(NULL)
  }
  grDevices::col2rgb(color)[, 1L]
}

# The names of R's numeric linetypes 0..6 (see ?par, "lty").
QGS_LINETYPE_NAMES <- c(
  "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"
)

# Normalizes an R linetype — numeric 0..6, a name, or a string of 2-8 hex
# digits of on/off run lengths — into what the style layer consumes
# (see style_set_linetype()): the equivalent QGIS pen-style preset where
# one exists, or the integer run lengths for longdash ("73"), twodash
# ("2262") and hex patterns, which have none. The run unit is the line
# width both in R (1/96 inch times lwd) and in Qt's presets, so the
# numbers carry over as-is. NA is R's "use the default": solid.
qgs_linetype <- function(linetype, i) {
  if (length(linetype) == 1L && is.na(linetype)) {
    return("solid")
  }
  if (length(linetype) != 1L ||
      !(is.character(linetype) || is.numeric(linetype))) {
    stop(
      "layer ", i, ": `linetype` must be a single number in 0..6 or a ",
      "linetype name",
      call. = FALSE
    )
  }
  if (is.numeric(linetype)) {
    if (!linetype %in% 0:6) {
      stop(
        "layer ", i, ": invalid `linetype`: ", num(linetype),
        " (must be in 0..6)",
        call. = FALSE
      )
    }
    linetype <- QGS_LINETYPE_NAMES[[linetype + 1L]]
  }
  switch(linetype,
    blank = "no",
    solid = "solid",
    dashed = "dash",
    dotted = "dot",
    dotdash = "dash dot",
    longdash = c(7L, 3L),
    twodash = c(2L, 2L, 6L, 2L),
    {
      # The hex on/off patterns, e.g. "44" or "1343". R requires full
      # on/off pairs of nonzero runs.
      if (!grepl("^([1-9A-Fa-f]{2}){1,4}$", linetype)) {
        stop(
          "layer ", i, ": invalid `linetype`: \"", linetype, "\"",
          call. = FALSE
        )
      }
      strtoi(strsplit(linetype, "", fixed = TRUE)[[1L]], base = 16L)
    }
  )
}
