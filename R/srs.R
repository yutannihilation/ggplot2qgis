# Spatial reference systems, written into <srs>/<spatialrefsys> nodes.
#
# A layer's SRS is written as a <spatialrefsys> block whose <wkt> element
# holds the WKT2 definition. QGIS re-resolves the CRS from it (and from
# <authid>) on load, so the other fields only need to be plausible;
# <proj4> is not needed and is not emitted.
#
# Ported from src/srs.rs of the generate-qgs crate, with sf::st_crs()
# (PROJ) replacing the epsg-utils lookup. sf's WKT2 text differs from
# epsg-utils' formatting, which is fine: QGIS re-resolves the CRS anyway.

# Resolves an SRS specification into everything the <spatialrefsys> block
# needs: list(wkt, epsg, name, geographic). `x` can be an sf crs object,
# an EPSG code, or a WKT2 string (anything sf::st_crs() accepts).
resolve_srs <- function(x) {
  crs <- sf::st_crs(x)
  if (is.na(crs)) {
    stop("cannot resolve the CRS", call. = FALSE)
  }
  list(
    wkt = crs$wkt,
    epsg = crs$epsg, # NA if not identifiable
    name = crs$Name,
    geographic = isTRUE(crs$IsGeographic)
  )
}

# Writes a <spatialrefsys nativeFormat="Wkt">...</spatialrefsys> block.
write_spatialrefsys <- function(w, srs) {
  if (is.na(srs$epsg)) {
    srid <- "0"
    authid <- ""
  } else {
    srid <- as.character(srs$epsg)
    authid <- paste0("EPSG:", srs$epsg)
  }
  xw_start(w, "spatialrefsys")
  xw_attr(w, "nativeFormat", "Wkt")
  xw_elem(w, "wkt", srs$wkt)
  # srsid is QGIS's internal database id; it is re-resolved on load, so
  # the SRID itself is a good enough placeholder.
  xw_elem(w, "srsid", srid)
  xw_elem(w, "srid", srid)
  xw_elem(w, "authid", authid)
  xw_elem(w, "description", srs$name)
  xw_elem(w, "projectionacronym", "")
  xw_elem(w, "ellipsoidacronym", "")
  xw_elem(w, "geographicflag", if (srs$geographic) "true" else "false")
  xw_end(w)
}
