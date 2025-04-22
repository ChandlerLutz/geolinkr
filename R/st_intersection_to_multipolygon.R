#' Spatial Intersection with Multipolygon Output
#'
#' This function performs a spatial intersection between two data.tables 
#' containing geometries. It ensures that the output always contains 
#' multipolygon geometries, even if the intersection results in other 
#' geometry types.
#'
#' @param dt_x A data.table containing geometries in an sf column named 'geometry'.
#' @param dt_y A data.table containing geometries in an sf column named 'geometry'.
#'
#' @details
#' The function performs the following steps:
#' 1. **Input Validation:** Checks if the inputs are data.tables and contain the required 'geometry' column.
#' 2. **Setting spatial aggregation:** Sets the spatial aggregation to "constant" assuming non-geometry columns are constant across space.
#' 3. **Intersection:** Performs the spatial intersection using `sf::st_intersection()`.
#' 4. **Geometry Type Handling:** 
#'     - Extracts the geometry type of the intersection result.
#'     - Filters out point, multipoint, linestring, and multilinestring geometries.
#'     - Converts geometry collections to multipolygons.
#'     - Casts polygons to multipolygons.
#' 6. **Empty Geometry Removal:**  Removes any empty geometries.
#'
#' @return A data.table with a column named 'geometry' containing the intersecting multipolygon geometries.
#'
#' @import data.table
#' @noRd
st_intersection_to_multipolygon <- function(dt_x, dt_y) {
  ## Intersect each polygon in dt_x with each polygon in dt_y
  ## If there is no intersection, return an empty MULTIPOLYGON
  
  # For NSE notes in R CMD check
  . <- geom_type <- geometry <- geomcollection_has_no_polygons <- NULL

  if (!inherits(dt_x, "data.table")) 
    stop("The `dt_x` input must be a data.table object")
  if (!inherits(dt_y, "data.table"))
    stop("The `dt_y` input must be a data.table object")

  if ("geometry" %notin% names(dt_x))
    stop("The `dt_x` input must have a column named 'geometry'")
  if ("geometry" %notin% names(dt_y))
    stop("The `dt_y` input must have a column named 'geometry'")

  st_x <- sf::st_as_sf(dt_x)
  st_y <- sf::st_as_sf(dt_y)

  if (sf::st_crs(st_x) != sf::st_crs(st_y))
    stop("The Coordinate Reference Systems (CRS) of `dt_x` and `dt_y` must match")

  orig_crs <- sf::st_crs(st_x)

  ## Assume that non-geometry columns are constant across space
  st_x <- sf::st_set_agr(st_x, "constant")
  st_y <- sf::st_set_agr(st_y, "constant")

  dt_intersection <- sf::st_intersection(st_x, st_y) |> 
    as.data.table()

  geom_types <- sf::st_geometry_type(dt_intersection$geometry) |> as.character() |>
    unique()

  if (all(geom_types %chin% c("POLYGON", "MULTIPOLYGON"))) {
    dt_intersection <- dt_intersection |>
      sf::st_as_sf() |>
      sf::st_cast(to = "MULTIPOLYGON") |>
      as.data.table()
    return(dt_intersection)
  }

  if (nrow(dt_intersection) == 0) 
    return(dt_intersection)

  dt_intersection <- dt_intersection |>  
    _[, geom_type := sf::st_geometry_type(geometry)] |>
    _[, geom_type := as.character(geom_type)] |> 
    _[geom_type %notin% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING")]

  geom_types <- sf::st_geometry_type(dt_intersection$geometry) |> as.character() |>
    unique()

  if (all(geom_types %chin% c("POLYGON", "MULTIPOLYGON"))) {
    dt_intersection <- dt_intersection |>
      sf::st_as_sf() |>
      sf::st_cast(to = "MULTIPOLYGON") |>
      as.data.table() |>
      _[, geom_type := NULL]
    return(dt_intersection)
  }

  ## Handle cases where the intersection results in a geomcollection with no
  ## polygons
  dt_intersection <- dt_intersection |>
    _[, geomcollection_has_no_polygons := 0L] |>
    _[geom_type == "GEOMETRYCOLLECTION" & "POLYGON" %notin% attr(geometry, "classes"),
      geomcollection_has_no_polygons := 1L] |>
    _[geomcollection_has_no_polygons == 0L] |>
    _[, geomcollection_has_no_polygons := NULL]

  if (nrow(dt_intersection) == 0)
    return(dt_intersection)

  if (nrow(dt_intersection) == 1) {
    dt_intersection <- dt_intersection |>
      _[geom_type == "GEOMETRYCOLLECTION",
        geometry := convert_goemcollection_to_multipolygon(geometry)] |>
      _[geom_type == "POLYGON",
        geometry := list(sf::st_cast(geometry, to = "MULTIPOLYGON"))]
  } else if (nrow(dt_intersection) > 1) {

    dt_intersection <- dt_intersection |> 
      ## Make a list of simple feature collections (sfc) to
      ## easily manipulate each individual geometry
      _[, geometry := lapply(geometry, sf::st_sfc, crs = orig_crs)] |>
      _[geom_type == "GEOMETRYCOLLECTION",
        geometry := lapply(geometry, convert_goemcollection_to_multipolygon)] |>
      _[geom_type == "POLYGON",
        geometry := lapply(geometry, sf::st_cast, to = "MULTIPOLYGON")] |>
      _[!sapply(geometry, sf::st_is_empty)] |>
      _[, geometry := lapply(geometry, sf::st_sfc, crs = orig_crs)] |>
      ## Combine the list of sfc objects into a single sfc
      _[, geometry := do.call(what = "c", args = geometry)]

  } else {
    stop("Unexpected error occurred in st_intersection_to_multipolygon()")
  }
  
  dt_intersection <- dt_intersection |>
    _[, geom_type := NULL]


  return(dt_intersection)
}
