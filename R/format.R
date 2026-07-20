# Formatting helpers reproducing QGIS's serialization conventions.
#
# Ported from the reference Rust implementation (src/style.rs, src/ids.rs,
# src/time.rs of the generate-qgs crate). The expected strings are pinned
# down by the tests, which were derived from QGIS-saved sample projects.

# Strips trailing zeros (and a then-trailing dot) from a fixed-point number.
# Every input must contain a decimal point (i.e. come from sprintf("%.Nf")
# with N >= 1), so integers like "10" are never touched.
strip_zeros <- function(s) {
  sub("\\.$", "", sub("0+$", "", s))
}

# One color channel as a float in 0..1 the way QGIS writes it: %.7f with
# trailing zeros stripped (e.g. "0.9098039", "0.682353"). QGIS computes the
# value as a 32-bit float, whose 7th decimal differs from the double result
# for 25 of the 256 channel values, so quantize through an IEEE754 single
# before formatting.
fmt_channel <- function(v) {
  f32 <- readBin(writeBin(v / 255, raw(), size = 4L), "double", size = 4L)
  strip_zeros(sprintf("%.7f", f32))
}

# QGIS color serialization: "R,G,B,255,rgb:r,g,b,1", e.g.
# "232,113,141,255,rgb:0.9098039,0.4431373,0.5529412,1".
# `rgb` is an integer vector c(r, g, b) in 0..255.
qgis_color <- function(rgb) {
  sprintf(
    "%d,%d,%d,255,rgb:%s,%s,%s,1",
    rgb[1L], rgb[2L], rgb[3L],
    fmt_channel(rgb[1L]), fmt_channel(rgb[2L]), fmt_channel(rgb[3L])
  )
}

# "#rrggbb", used by color ramp expressions.
color_hex <- function(rgb) {
  sprintf("#%02x%02x%02x", rgb[1L], rgb[2L], rgb[3L])
}

# Gradient stop offsets as QGIS writes them: C's %g with 6 significant
# digits (e.g. "0.0196078", "0.509804", "0.5", "1"). R's sprintf() delegates
# to the C library, so "%g" is exactly that.
g6 <- function(v) {
  sprintf("%g", v)
}

# A number the way QGIS writes widths and classification limits ("0.26",
# "1", ...): the shortest plain-decimal form, never scientific notation
# (mirrors Rust's f64::Display up to 15 significant digits). decimal.mark
# is forced to "." — unlike format(), which honors getOption("OutDec")
# and would corrupt the output for European-decimal setups.
num <- function(v) {
  trimws(formatC(v, format = "fg", digits = 15, decimal.mark = "."))
}

# Number of label decimals needed to tell adjacent class bounds apart: the
# smallest p with 10^-p <= step / 2, at least 1 (the QGIS default, kept for
# wide classes so the output matches the samples).
label_precision <- function(step) {
  if (!is.finite(step) || step <= 0) {
    return(1L)
  }
  as.integer(min(max(ceiling(log10(2 / step)), 1), 15))
}

# Label for a graduated range, following the
# <labelFormat format="%1 - %2" labelprecision="N" trimtrailingzeroes="1"/>
# convention (e.g. "0 - 10", "9.5 - 19", "0.042 - 0.05").
range_label <- function(lower, upper, precision) {
  bound <- function(v) strip_zeros(sprintf("%.*f", precision, v))
  paste0(bound(lower), " - ", bound(upper))
}

# Linear interpolation in RGB space (matches QGIS color ramps). Rounds half
# away from zero like Rust's f64::round(), not half to even like round().
rgb_lerp <- function(a, b, t) {
  as.integer(floor(a + (b - a) * t + 0.5))
}

# Samples the color ramp at t (in 0..1) by piecewise linear interpolation
# between consecutive control points. `stops` is
# list(offsets = <numeric n>, colors = <3 x n integer matrix>).
sample_ramp <- function(stops, t) {
  offsets <- stops$offsets
  colors <- stops$colors
  n <- length(offsets)
  if (t <= offsets[1L]) {
    return(colors[, 1L])
  }
  if (t >= offsets[n]) {
    return(colors[, n])
  }
  i <- which(offsets > t)[1L]
  f <- (t - offsets[i - 1L]) / (offsets[i] - offsets[i - 1L])
  rgb_lerp(colors[, i - 1L], colors[, i], f)
}

# A random-looking UUIDv4 string, e.g.
# "0b31b699-73f7-4f89-bb3b-2ddb939863a3". QGIS only needs document-local
# uniqueness. Note this consumes the user's RNG stream; QGIS ids are
# cosmetic, so that is acceptable.
qgs_uuid <- function() {
  bytes <- sample.int(256L, 16L, replace = TRUE) - 1L
  bytes[7L] <- bitwOr(bitwAnd(bytes[7L], 0x0fL), 0x40L) # version 4
  bytes[9L] <- bitwOr(bitwAnd(bytes[9L], 0x3fL), 0x80L) # variant
  hex <- sprintf("%02x", bytes)
  paste0(
    paste0(hex[1:4], collapse = ""), "-",
    paste0(hex[5:6], collapse = ""), "-",
    paste0(hex[7:8], collapse = ""), "-",
    paste0(hex[9:10], collapse = ""), "-",
    paste0(hex[11:16], collapse = "")
  )
}

# Layer id as QGIS generates it: the layer name stripped to ASCII
# alphanumerics, followed by "_" and a UUID with dashes turned into
# underscores, e.g. "nc_b1079259_f0a1_4ebf_8df3_a22a440d836b". If the name
# has no ASCII alphanumerics (e.g. Japanese), the id starts with "_".
qgs_layer_id <- function(name) {
  # Explicit ranges, not [:alnum:], which is locale-dependent and would
  # keep non-ASCII letters.
  prefix <- gsub("[^A-Za-z0-9]", "", name)
  paste0(prefix, "_", gsub("-", "_", qgs_uuid(), fixed = TRUE))
}

# Current UTC time as "YYYY-MM-DDTHH:MM:SS" for the saveDateTime attribute.
qgs_now_iso8601 <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
}
