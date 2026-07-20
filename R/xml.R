# Minimal XML writer with proper escaping and indentation.
#
# Not a general-purpose XML library; just enough to build QGIS project
# files. Every element starts on its own line, indented by its depth.
# Ported from src/xml.rs of the generate-qgs crate.

# Builds an XML fragment. `base_depth` is the indentation depth (2 spaces
# per level) at which the first element is written, so fragments can be
# spliced into the project template at the right indentation.
#
# The writer is a mutable environment; the frame stack is kept as parallel
# vectors (tag / open / has_children / has_text).
xml_writer <- function(base_depth = 0L) {
  w <- new.env(parent = emptyenv())
  w$buf <- vector("list", 64L)
  w$n <- 0L
  w$tags <- character()
  w$open <- logical()
  w$has_children <- logical()
  w$has_text <- logical()
  w$base_depth <- as.integer(base_depth)
  w
}

xw_chunk <- function(w, s) {
  w$n <- w$n + 1L
  if (w$n > length(w$buf)) {
    length(w$buf) <- 2L * length(w$buf)
  }
  w$buf[[w$n]] <- s
}

xw_newline_indent <- function(w) {
  xw_chunk(w, paste0("\n", strrep("  ", w$base_depth + length(w$tags))))
}

# Close the pending start tag of the current element, if any.
xw_close_start_tag <- function(w) {
  d <- length(w$tags)
  if (d > 0L && w$open[d]) {
    xw_chunk(w, ">")
    w$open[d] <- FALSE
  }
}

# Start a new element. Attributes can be added with xw_attr() until any
# other content is written.
xw_start <- function(w, tag) {
  xw_close_start_tag(w)
  d <- length(w$tags)
  if (d > 0L) {
    w$has_children[d] <- TRUE
  }
  xw_newline_indent(w)
  xw_chunk(w, paste0("<", tag))
  w$tags <- c(w$tags, tag)
  w$open <- c(w$open, TRUE)
  w$has_children <- c(w$has_children, FALSE)
  w$has_text <- c(w$has_text, FALSE)
  invisible(w)
}

# Add an attribute to the most recently started element.
xw_attr <- function(w, key, value) {
  xw_chunk(w, paste0(" ", key, "=\"", escape_attr(as.character(value)), "\""))
  invisible(w)
}

# Write escaped text content into the current element.
xw_text <- function(w, text) {
  xw_close_start_tag(w)
  d <- length(w$tags)
  if (d > 0L) {
    w$has_text[d] <- TRUE
  }
  xw_chunk(w, escape_text(as.character(text)))
  invisible(w)
}

# End the current element.
xw_end <- function(w) {
  d <- length(w$tags)
  if (d == 0L) {
    stop("xw_end() without xw_start()")
  }
  tag <- w$tags[d]
  open <- w$open[d]
  has_children <- w$has_children[d]
  has_text <- w$has_text[d]
  w$tags <- w$tags[-d]
  w$open <- w$open[-d]
  w$has_children <- w$has_children[-d]
  w$has_text <- w$has_text[-d]
  if (open) {
    xw_chunk(w, "/>")
  } else if (has_text && !has_children) {
    xw_chunk(w, paste0("</", tag, ">"))
  } else {
    xw_newline_indent(w)
    xw_chunk(w, paste0("</", tag, ">"))
  }
  invisible(w)
}

# Write <tag>text</tag> in one call.
xw_elem <- function(w, tag, text) {
  xw_start(w, tag)
  xw_text(w, text)
  xw_end(w)
}

# Write an empty element with the given attributes (a named character
# vector or named list, possibly empty or NULL).
xw_empty <- function(w, tag, attrs = NULL) {
  xw_start(w, tag)
  for (key in names(attrs)) {
    xw_attr(w, key, attrs[[key]])
  }
  xw_end(w)
}

# Return the fragment.
xw_finish <- function(w) {
  if (length(w$tags) > 0L) {
    stop("unclosed XML elements: ", paste(w$tags, collapse = ", "))
  }
  paste0(unlist(w$buf[seq_len(w$n)]), collapse = "")
}

# QGIS (QDom) escapes `&`, `<` and `"` in attribute values but NOT `>`;
# text content escapes `&`, `<` and `>`. The `&` replacement must come
# first in both.
escape_attr <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  gsub("\"", "&quot;", s, fixed = TRUE)
}

escape_text <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  gsub(">", "&gt;", s, fixed = TRUE)
}
