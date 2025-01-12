#' Buffer a Multipolygon by a Fraction of its Area
#'
#' @description
#' This function takes an `sf` object with a MULTIPOLYGON geometry and 
#' returns a new `sfc` object where each multipolygon is buffered by a 
#' distance calculated as a fraction of its original area.
#'
#' @param geom An `sfc` object with a MULTIPOLYGON geometry type. It can be
#' a GEOMETRYCOLLECTION type, but it will be converted to MULTIPOLYGON
#' @param buffer_frac A numeric value representing the fraction of the 
#' original area to use as the buffer distance.
#'
#' @details
#' The function first checks if the input `buffer_frac` is valid (>= 0). Then
#' it checks if the input geometry is a MULTIPOLYGON. If it's a
#' GEOMETRYCOLLECTION, it will attempt to convert it to MULTIPOLYGON. If it is
#' neither, it will throw an error.
#' 
#' The original CRS (Coordinate Reference System) of the input geometry is 
#' preserved.
#' 
#' The buffer distance is calculated for each multipolygon individually as:
#' `dist = original_area * buffer_frac`
#' 
#' The `sf::st_buffer` function is used to perform the buffering.
#'
#' @return An `sfc` object with the buffered MULTIPOLYGON geometry and the 
#' original CRS.
#'
#' @noRd
st_buffer_frac <- function(geom, buffer_frac) {

  if (buffer_frac == 0) return(geom)

  if (buffer_frac < 0) {
    stop("Buffer fraction must be greater than or equal to 0")
  }

  if (!all(sf::st_geometry_type(geom) == "MULTIPOLYGON"))
    geom <- sf::st_cast(geom, to = "MULTIPOLYGON")
  
  if (all(sf::st_geometry_type(geom) != "MULTIPOLYGON")) {
    stop("Input must be a multipolygon")
  }
  
  orig_crs <- sf::st_crs(geom)

  orig_geom_area <- sf::st_area(geom) |> as.numeric()
  
  buffered_geom <- sf::st_buffer(geom, dist = orig_geom_area * buffer_frac) |>
    sf::st_cast(to = "MULTIPOLYGON")
  
  sf::st_sfc(buffered_geom, crs = orig_crs)
}
