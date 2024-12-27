#' Check Polygon Intersections with US States
#'
#' This function checks if the geometries in a given `sf` object intersect with US states.
#'
#' @param geometry An `sf` object with geometries of class
#'   `sfc_MULTIPOLYGON`. The object must have a column named
#'   'geometry'.
#' @return An integer vector, where 1 indicates that the corresponding
#'   polygon intersects with a US state, and 0 indicates no
#'   intersection.
#' @details The function first verifies that the input has the correct
#'   geometry type (`sfc_MULTIPOLYGON`). It then transforms the input
#'   geometry to CRS 5070 for accurate spatial operations, converts
#'   the geometry to a `data.table`, and checks for intersections with
#'   US state geometries.
#' @examples
#' stopifnot(
#'  polygon_intersects_with_us_state(geolinkr::us_states_sf$geometry) == rep(1L, 51)
#' )
#' 
#' stopifnot(
#'  polygon_intersects_with_us_state(geolinkr::puerto_rico_sf$geometry) == 0L
#' )
#' @import data.table
#' @export
polygon_intersects_with_us_state <- function(geometry) {

  # For NSE notes in R CMD check
  . <- NULL

  if ("sfc_MULTIPOLYGON" %notin% class(geometry)) {
    stop("The input must have a column named 'geometry' of class 'sfc_MULTIPOLYGON'")
  }

  geometry <- sf::st_transform(geometry, crs = 5070)
  dt_sf <- sf::st_as_sf(geometry) |> 
    as.data.table() %>%
    .[, rowid := .I]

  us_states_sf <- geolinkr::us_states_sf |> 
    sf::st_transform(crs = 5070) 
  
  dt_is_state_idx <- sf::st_intersects(geometry, us_states_sf$geometry) |> 
    as.data.frame() 

  fifelse(dt_sf$rowid %in% dt_is_state_idx$row.id, 1L, 0L)
  
}


#' Identify Alaska Polygons
#'
#' This function checks if the geometries in a given `sf` object intersect with the state of Alaska.
#'
#' @param geometry An `sf` object with geometries of class `sfc_MULTIPOLYGON`. The object must have a column named 'geometry'.
#' @return An integer vector, where 1 indicates that the corresponding polygon intersects with Alaska, and 0 indicates no intersection.
#' @examples
#' \dontrun{
#' # Identify Alaska polygons in US states
#' library(data.table); library(sf)
#' us_states <- geolinkr::us_states_sf %>%
#'   as.data.table() %>%
#'   .[, is_ak_st_code := fifelse(stusps == "AK", 1L, 0L)] %>%
#'   .[, is_ak := polygon_is_ak(geometry)]
#' stopifnot(us_states[, all(is_ak_st_code == is_ak)])
#' }
#' @import data.table
#' @export
polygon_is_ak <- function(geometry) {

  # For NSE notes in R CMD check
  statefp <- NULL
  
  if ("sfc_MULTIPOLYGON" %notin% class(geometry)) {
    stop("The input must have a column named 'geometry' of class 'sfc_MULTIPOLYGON'")
  }

  geometry <- sf::st_transform(geometry, crs = 5070)
  dt_sf <- as.data.table(geometry) |> 
    _[, rowid := .I]

  dt_ak <- geolinkr::us_states_sf |>
    as.data.table() |> 
    _[statefp == "02"] |> 
    _[, geometry := sf::st_cast(geometry, to = "MULTIPOLYGON")] |> 
    _[, geometry := sf::st_transform(geometry, crs = 5070)]

  dt_is_ak_idx <- sf::st_intersects(dt_sf$geometry, dt_ak$geometry) |> 
    as.data.frame() |> 
    as.data.table()

  fifelse(dt_sf$rowid %in% dt_is_ak_idx$row.id, 1L, 0L)

}


#' Identify Hawaii Polygons
#'
#' This function checks if the geometries in a given `sf` object intersect with the state of Hawaii.
#'
#' @param geometry An `sf` object with geometries of class `sfc_MULTIPOLYGON`. The object must have a column named 'geometry'.
#' @return An integer vector, where 1 indicates that the corresponding polygon intersects with Hawaii, and 0 indicates no intersection.
#' @examples
#' \dontrun{
#' # Identify Hawaii polygons in US states
#' library(data.table); library(sf)
#' us_states <- geolinkr::us_states_sf %>%
#'   as.data.table() %>%
#'   .[, is_hi_st_code := fifelse(stusps == "HI", 1L, 0L)] %>%
#'   .[, is_hi := polygon_is_hi(geometry)]
#' stopifnot(us_states[, all(is_hi_st_code == is_hi)])
#' }
#' @import data.table
#' @export
polygon_is_hi <- function(geometry) {

  # For NSE notes in R CMD check
  statefp <- NULL
  
  if ("sfc_MULTIPOLYGON" %notin% class(geometry)) {
    stop("The input must have a column named 'geometry' of class 'sfc_MULTIPOLYGON'")
  }

  geometry <- sf::st_transform(geometry, crs = 5070)
  dt_sf <- as.data.table(geometry) |>
    _[, rowid := .I]

  dt_hi <- geolinkr::us_states_sf |>
    as.data.table() |>
    _[statefp == "15"] |>
    _[, geometry := sf::st_cast(geometry, to = "MULTIPOLYGON")] |>
    _[, geometry := sf::st_transform(geometry, crs = 5070)]

  dt_is_hi_idx <- sf::st_intersects(dt_sf$geometry, dt_hi$geometry) |>
    as.data.frame() |>
    as.data.table()

  fifelse(dt_sf$rowid %in% dt_is_hi_idx$row.id, 1L, 0L)
}



#' Convert Geometry Collection to Multipolygon
#'
#' This function converts an `sf` geometry collection object to a multipolygon.
#' It extracts all polygons (an only polygons) from the geometry collection and combines
#' them into a single multipolygon. If the geometry collection contains no polygons,
#' it returns an empty multipolygon.
#'
#' @param geom_collection An `sf` geometry collection object.
#'
#' @details
#' This function is useful for simplifying geometries and ensuring compatibility
#' with functions or data structures that expect multipolygons. It handles
#' cases where a geometry collection might contain a mix of different geometry
#' types by focusing only on the polygons.
#'
#' @return An `sf` multipolygon object.
#'
#' @examples
#' # Create a geometry collection with polygons and points
#' library(sf)
#' geom_collection <- st_as_sfc(
#'   c("GEOMETRYCOLLECTION(POLYGON((0 0, 0 1, 1 1, 1 0, 0 0)), POINT(2 2))")
#' )
#'
#' # Convert to multipolygon
#' multipolygon <- convert_goemcollection_to_multipolygon(geom_collection)
#'
#' # Check the geometry type
#' st_geometry_type(multipolygon) 
#'
#' @noRd
convert_goemcollection_to_multipolygon <- function(geom_collection) {
  if (sf::st_geometry_type(geom_collection) != "GEOMETRYCOLLECTION") 
    stop("Input must be a geometrycollection")

  orig_crs <- sf::st_crs(geom_collection)
  
  # Extract all polygons from the geometrycollection
  polygons <- try(
    suppressWarnings(
      sf::st_collection_extract(geom_collection, type = "POLYGON")
    ),
    silent = TRUE
  )

  if (inherits(polygons, "try-error")) {
    # Add a point and try again 
    geom_collection <- c(
      geom_collection,
      sf::st_as_sfc(c("GEOMETRYCOLLECTION(POINT(2 2))"),
                    crs = sf::st_crs(geom_collection))
    )
    polygons <- try(
      suppressWarnings(
        sf::st_collection_extract(geom_collection, type = "POLYGON")
      ),
      silent = TRUE
    )
  }

  if (inherits(polygons, "try-error")) {
    # Return an empty MULTIPOLYGON
    return(sf::st_multipolygon())
  }

  # If there are any polygons, create a single multipolygon
  if (length(polygons) > 0) {
    multipolygon <- sf::st_union(polygons, by_feature = FALSE) %>%
      sf::st_cast(to = "MULTIPOLYGON") |> sf::st_sfc(crs = orig_crs)
    return(multipolygon)
  } else {
    # Return an empty MULTIPOLYGON
    return(
      sf::st_multipolygon() |> sf::st_sfc(crs = orig_crs)
    )
    
    
  }
}


#' Generate Non-Overlapping Square Polygons
#'
#' This function generates a set of non-overlapping square polygons arranged in a grid-like pattern.
#' The squares are stacked in rows, with the option to specify the number of squares per row and the 
#' starting position of the grid.
#'
#' @param num_squares The total number of squares to generate.
#' @param side_length The length of the sides of each square.
#' @param squares_per_row The maximum number of squares to place in a
#'   horizontal row before starting a new row.
#' @param crs An integer representing the coordinate reference system
#'   (CRS) for the generated polygons.  If NULL (default), no CRS is
#'   set.
#' @param start_at_xy A numeric vector of length 2 specifying the x
#'   and y coordinates of the bottom-left corner of the starting
#'   square. Default is c(0, 0).
#'
#' @return An sf object containing the generated non-overlapping
#'   square polygons as MULTIPOLYGONs.
#'
#' @details The function calculates the position of each square based
#'   on the `side_length`, `squares_per_row`, and `start_at_xy`
#'   parameters. The `crs` argument allows you to set a coordinate
#'   reference system for the generated polygons.
#'
#' @examples
#' # Generate 12 squares with a side length of 2, arranged in rows of 4,
#' # starting at (0, 0)
#' squares_1 <- gen_nonoverlapping_square_polygons(
#'   num_squares = 12, 
#'   side_length = 2, 
#'   squares_per_row = 4
#' )
#'
#' # Generate 8 squares with a side length of 1, arranged in rows of 3, 
#' # starting at (5, 10), and using WGS 84 (EPSG:4326)
#' squares_2 <- gen_nonoverlapping_square_polygons(
#'   num_squares = 8, 
#'   side_length = 1, 
#'   squares_per_row = 3, 
#'   crs = 4326, 
#'   start_at_xy = c(5, 10)
#' )
#' @export
gen_nonoverlapping_square_polygons <- function(num_squares, side_length, 
                                               squares_per_row, crs = NULL,
                                               start_at_xy = c(0, 0)) {

  i <- 1:num_squares
  x <- start_at_xy[1] + ((i - 1) %% squares_per_row) * side_length
  y <- start_at_xy[2] + floor((i - 1) / squares_per_row) * side_length

  # Create a list of matrices with coordinates for each square
  square_coords_list <- lapply(1:num_squares, function(i) {
    matrix(c(
      x[i], y[i],
      x[i] + side_length, y[i],
      x[i] + side_length, y[i] + side_length,
      x[i], y[i] + side_length,
      x[i], y[i] 
    ), ncol = 2, byrow = TRUE)
  })

  # Create polygons from the list of matrices
  sfc_out <- lapply(square_coords_list, \(x) sf::st_polygon(list(x))) |>
    sf::st_sfc() |>
    sf::st_cast(to = "MULTIPOLYGON")

  if (!is.null(crs))
    sfc_out <- sf::st_set_crs(sfc_out, crs)

  return(sfc_out)
}
