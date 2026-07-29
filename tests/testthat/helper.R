# Helpers shared by the write_qgs()-level test files.

read_nc <- function() {
  sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
}

local_out_dir <- function(env = parent.frame()) {
  dir <- tempfile("write_qgs_test")
  dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE), envir = env)
  dir
}

read_qgs <- function(path) {
  readChar(path, file.size(path), useBytes = TRUE)
}

# The `maskedSymbolLayers` of the project's labeling, as a data frame of
# the `<layer id>;<symbol layer id>` pairs it flattens (see
# symbol_layer_references()). Empty when no layer is labeled.
masked_symbol_layers <- function(out) {
  value <- regmatches(
    out,
    gregexpr('(?<=maskedSymbolLayers=")[^"]*', out, perl = TRUE)
  )[[1]]
  ids <- unlist(strsplit(value[nzchar(value)], ";", fixed = TRUE))
  data.frame(
    layer = ids[c(TRUE, FALSE)],
    symbol = ids[c(FALSE, TRUE)]
  )
}

# The ids of the project's map layers, or of the one named `name`.
layer_ids <- function(out, name = NULL) {
  pattern <- if (is.null(name)) {
    "(?s)<maplayer.*?</maplayer>"
  } else {
    paste0("(?s)<maplayer(?:(?!</maplayer>).)*<layername>", name, "</layername>")
  }
  layers <- regmatches(out, gregexpr(pattern, out, perl = TRUE))[[1]]
  vapply(
    layers,
    function(l) regmatches(l, regexpr("(?<=<id>)[^<]*", l, perl = TRUE)),
    character(1L),
    USE.NAMES = FALSE
  )
}

# The ids of every symbol layer of every map layer's renderer, including
# the <source-symbol> templates a graduated renderer carries.
symbol_layer_ids <- function(out) {
  regmatches(
    out,
    gregexpr('(?<=<layer class="SimpleLine" enabled="1" id=")[^"]*', out,
      perl = TRUE
    )
  )[[1]]
}

# The marker options of the first map layer's symbol (the project template
# carries symbols of its own before the layers).
marker_option <- function(out, name) {
  # (?s) so the patterns span the pretty-printed XML's newlines.
  layers <- sub("(?s).*?<maplayer", "<maplayer", out, perl = TRUE)
  block <- regmatches(
    layers,
    regexpr('(?s)<layer class="SimpleMarker".*?</layer>', layers, perl = TRUE)
  )
  pattern <- paste0('<Option name="', name, '" type="QString" value="[^"]*"/>')
  option <- regmatches(block, regexpr(pattern, block))
  if (length(option) == 0L) {
    return(NA_character_)
  }
  sub('.*value="([^"]*)"/>', "\\1", option)
}
