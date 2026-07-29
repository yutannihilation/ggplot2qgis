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
