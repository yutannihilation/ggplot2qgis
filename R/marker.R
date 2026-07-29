# R's plotting symbols (pch) as QGIS markers, shared by the ggplot2 and
# tmap paths. Both draw their symbols with grid's pointsGrob(), which
# hands the pch to R's graphics engine at a "size" the caller picks; only
# how that size is computed differs (ggplot2: `size`/`stroke`, tmap: a
# multiple of the text line height), so everything below takes the size
# in millimeters.

# The QGIS marker each R plotting symbol becomes.
#
# `factor` turns one millimeter of the symbol's size unit into the QGIS
# marker size: R draws a pch spanning `2 * RADIUS * <shape constant>` size
# units wide (grDevices' engine.c), and QGIS sizes a marker by its width —
# except `equilateral_triangle`, which is inscribed in the circle of that
# diameter and so comes out sqrt(3)/2 as wide, which the factor
# compensates for. Both sets of numbers were checked by rendering the
# symbols and measuring the ink (see
# .tmp/research/20260729_tmap_symbol_constants.md and
# .tmp/research/20260729_ggplot2_symbol_constants.md).
#
# `band` is how R colors the symbol, which decides which color reaches
# which slot: "open" shapes are stroke-only, "solid" ones are filled with
# a single color and have no distinct border, "bordered" ones fill with
# `fill` and stroke with `col`. Which of the caller's colors that single
# color is differs (ggplot2 passes it as `colour`, tmap has already
# swapped it into `fill`), so the callers route it.
#
# The composite pch (7-14) and R's thin asterisk (8) have no QGIS
# counterpart, and neither do tmap's grob shapes (> 999); they are
# errors rather than a silently different marker.
QGS_PCH <- local({
  radius <- 0.375 # engine.c RADIUS
  sqrc <- 0.88623 # sqrt(pi / 4)
  dmd <- 1.25331 # sqrt(pi / 4) * sqrt(2)
  trc1 <- 1.34677 # half-width of the triangles
  tri <- sqrt(3) / 2 # QGIS's equilateral_triangle width per size
  spec <- list(
    list(pch = 0, name = "square", factor = 2 * radius, band = "open"),
    list(pch = 15, name = "square", factor = 2 * radius, band = "solid"),
    list(pch = 22, name = "square", factor = 2 * radius * sqrc,
         band = "bordered"),
    list(pch = 1, name = "circle", factor = 2 * radius, band = "open"),
    list(pch = c(16, 19), name = "circle", factor = 2 * radius,
         band = "solid"),
    list(pch = 20, name = "circle", factor = 2 * radius * 2 / 3,
         band = "solid"),
    list(pch = 21, name = "circle", factor = 2 * radius, band = "bordered"),
    list(pch = 5, name = "diamond", factor = 2 * radius * sqrt(2),
         band = "open"),
    list(pch = 18, name = "diamond", factor = 2 * radius, band = "solid"),
    list(pch = 23, name = "diamond", factor = 2 * radius * dmd,
         band = "bordered"),
    list(pch = 2, name = "equilateral_triangle",
         factor = 2 * radius * trc1 / tri, band = "open"),
    list(pch = 6, name = "equilateral_triangle",
         factor = 2 * radius * trc1 / tri, band = "open", angle = 180),
    list(pch = 17, name = "equilateral_triangle",
         factor = 2 * radius * trc1 / tri, band = "solid"),
    list(pch = 24, name = "equilateral_triangle",
         factor = 2 * radius * trc1 / tri, band = "bordered"),
    list(pch = 25, name = "equilateral_triangle",
         factor = 2 * radius * trc1 / tri, band = "bordered", angle = 180),
    list(pch = 3, name = "cross", factor = 2 * radius * sqrt(2),
         band = "open"),
    list(pch = 4, name = "cross2", factor = 2 * radius, band = "open")
  )
  out <- list()
  for (s in spec) {
    for (pch in s$pch) {
      out[[as.character(pch)]] <- list(
        name = s$name,
        factor = s$factor,
        band = s$band,
        angle = s$angle %||% 0
      )
    }
  }
  out
})

# ggplot2's shape names, as ggplot2::translate_shape_string() maps them to
# pch. It is unexported, and a `shape` string is still untranslated in the
# built data (the geoms translate while drawing), so the table is
# reproduced here — including the partial matching, so a plot that
# ggplot2 accepts converts.
QGS_SHAPE_NAMES <- c(
  "square open" = 0L, "circle open" = 1L, "triangle open" = 2L, "plus" = 3L,
  "cross" = 4L, "diamond open" = 5L, "triangle down open" = 6L,
  "square cross" = 7L, "asterisk" = 8L, "diamond plus" = 9L,
  "circle plus" = 10L, "star" = 11L, "square plus" = 12L,
  "circle cross" = 13L, "square triangle" = 14L, "triangle square" = 14L,
  "square" = 15L, "circle small" = 16L, "triangle" = 17L, "diamond" = 18L,
  "circle" = 19L, "bullet" = 20L, "circle filled" = 21L,
  "square filled" = 22L, "diamond filled" = 23L, "triangle filled" = 24L,
  "triangle down filled" = 25L
)

# The pch a `shape` string names. A single character is R's "draw this
# glyph" shape, which no QGIS marker reproduces.
qgs_shape_pch <- function(shape, i) {
  if (nchar(shape) <= 1L) {
    stop(
      "layer ", i, ": a single-character `shape` (\"", shape, "\") draws a ",
      "text glyph, which QGIS has no marker for",
      call. = FALSE
    )
  }
  match <- charmatch(shape, names(QGS_SHAPE_NAMES))
  if (is.na(match) || match == 0L) {
    stop(
      "layer ", i, ": unsupported symbol shape (\"", shape, "\")",
      call. = FALSE
    )
  }
  QGS_SHAPE_NAMES[[match]]
}

# The QGIS marker drawing R's plotting symbol `shape` at `size_mm`, the
# span of the symbol's size unit in millimeters:
# list(name, shape, size, angle, band). A shape or size R itself would not
# draw (NA) means an empty layer, which is an error like an all-NA color.
qgs_marker <- function(shape, size_mm, i) {
  if (is.character(shape) && !is.na(shape)) {
    shape <- qgs_shape_pch(shape, i)
  }
  if (is.na(shape) || is.na(size_mm) || size_mm <= 0) {
    stop(
      "layer ", i, ": the layer would not be drawn (`",
      if (is.na(shape)) "shape" else "size", "` is ",
      if (is.na(size_mm) || is.na(shape)) "NA" else num(size_mm), ")",
      call. = FALSE
    )
  }
  pch <- QGS_PCH[[as.character(shape)]]
  if (is.null(pch)) {
    stop(
      "layer ", i, ": unsupported symbol shape (", num(shape),
      "); QGIS has no marker drawing it",
      call. = FALSE
    )
  }
  list(
    name = pch$name,
    shape = shape,
    # Rounded so binary float noise stays out of the project file.
    size = round(size_mm * pch$factor, 7),
    angle = pch$angle,
    band = pch$band
  )
}
