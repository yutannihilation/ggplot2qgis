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
