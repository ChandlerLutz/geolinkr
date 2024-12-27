
#' Create Crosswalk between Two Sets of Spatial Features
#'
#' This function generates a crosswalk between two sets of spatial features 
#' (`from_sf` and `to_sf`) using a third set of spatial features (`wts_sf`) 
#' for weighting. The crosswalk maps `from_sf` to `to_sf` based on the 
#' intersection with `wts_sf`, providing weights for how much of `from_sf` 
#' intersects with `to_sf` relative to `wts_sf`.
#'
#' @param from_sf An sf object representing the source features. Must
#'   have columns named "geoid" and "geometry" with geometry of class
#'   'sfc_MULTIPOLYGON'.
#' @param to_sf An sf object representing the target features. Must
#'   have columns named "geoid" and "geometry" with geometry of class
#'   'sfc_MULTIPOLYGON'.
#' @param wts_sf An sf object representing the weight features. Must
#'   have columns named "geoid" and "geometry" with geometry of class
#'   'sfc_MULTIPOLYGON'. It must have a column used for weighting. The
#'   name for this weighting column must be passed to
#'   `wt_var_name`. For area-based weighting, use the area of the
#'   features in `wts_sf`.
#' @param check_that_wts_cover_from_and_to A logical value indicating
#'  whether to check that `wts_sf` completely covers both `from_sf` and
#'  `to_sf` (default is TRUE).
#' @param wt_var_name A character string specifying the name of the
#'   column in `wts_sf` to be used for weighting.
#' @param check_for_ak A logical value indicating whether to check for
#'   and handle Alaska specifically (default is TRUE).
#' @param check_for_hi A logical value indicating whether to check for
#'   and handle Hawaii specifically (default is TRUE).
#'
#' @details 
#' The function performs the following steps:
#' 1. **Input Validation:** Checks the classes and column names of the input sf 
#'    objects.
#' 2. **Data Preparation:** Converts sf objects to data.tables and renames 
#'    columns for consistency.
#' 3. **Alaska and Hawaii Handling:** If `check_for_ak` or `check_for_hi` is 
#'    TRUE, the function identifies and transforms the coordinate reference 
#'    systems (CRS) of Alaska and Hawaii polygons for accurate calculations.
#' 4. **Crosswalk Computation:** 
#'    * Identifies 1:1 mappings where `to_sf` completely covers `from_sf`.
#'    * For the remaining `from_sf` polygons, it finds the `wts_sf` polygons 
#'      covered by both `from_sf` and `to_sf`.
#'    * Calculates the intersection of the relevant polygons from `from_sf`, 
#'      `to_sf`, and `wts_sf`.
#'    * Determines the share of the `wt_var` for each `wts_sf` polygon that 
#'      relates to the intersection with `from_sf` and `to_sf`.
#' 5. **Output:** Returns a data.table containing the crosswalk with the 
#'    following columns:
#'    * `from_geoid`: The "geoid" from `from_sf`.
#'    * `to_geoid`: The "geoid" from `to_sf`.
#'    * `wt_var_name`: The weight variable from `wts_sf` (renamed from 
#'       `wt_var_name`).
#'    * `afact`: The allocation factor representing the share of the `wt_var` 
#'       that connects `from_geoid` to `to_geoid`.
#'
#' @return A data.table containing the crosswalk between `from_sf` and `to_sf`.
#'
#' @examples
#' \dontrun{
#' # Assuming you have sf objects named from_sf, to_sf, and wts_sf
#' # with a weight variable named "population" in wts_sf
#' 
#' cw <- create_cw(from_sf, to_sf, wts_sf, "population")
#' }
#'
#' @import data.table
#' @export
create_cw <- function(from_sf, to_sf, wts_sf, wt_var_name,
                      check_that_wts_cover_from_and_to = TRUE, 
                      check_for_ak = TRUE, check_for_hi = TRUE) {

  # For R CMD check
  . <- geometry <- is_ak <- is_hi <- id_x <- id_y <- NULL
  from_geoid <- to_geoid <- NULL

  if (!inherits(from_sf, "sf")) 
    stop("The `from_sf` input for create_cw() must be an sf object. Provided object is of class: ", class(from_sf))
  if (!inherits(to_sf, "sf")) 
    stop("The `to_sf` input for create_cw() must be an sf object. Provided object is of class: ", class(to_sf))
  if (!inherits(wts_sf, "sf")) 
    stop("The `wts_sf` input for create_cw() must be an sf object. Provided object object is of class: ", class(wts_sf))
  
  if (!is.character(wt_var_name)) 
    stop("The `wt_var_name` input must be a character string")
  if (length(wt_var_name) != 1)
    stop("The `wt_var_name` input must have length 1")
  if (wt_var_name %notin% names(wts_sf))
    stop("The `wt_var_name` input must be a column name in `wts_sf`")
  if ("wt_var" %chin% names(wts_sf))
    stop("The `wts_sf` input must not have a column named 'wt_var' as it will be created by the function")

  if ("geoid" %notin% names(from_sf)) 
    stop("The `from_sf` input must have a column named 'geoid'")
  if ("geoid" %notin% names(to_sf)) 
    stop("The `to_sf` input must have a column named 'geoid'")
  if ("geoid" %notin% names(wts_sf))
    stop("The `wts_sf` input must have a column named 'geoid'")

  dt_from <- as.data.table(from_sf)
  dt_to <- as.data.table(to_sf)
  dt_wts <- as.data.table(wts_sf) |> 
    setnames(wt_var_name, "wt_var")

  if ("geometry" %notin% names(dt_from))
    stop("The `from_sf` input must have a column named 'geometry'")
  if ("geometry" %notin% names(dt_to))
    stop("The `to_sf` input must have a column named 'geometry'")
  if ("geometry" %notin% names(dt_wts))
    stop("The `wts_sf` input must have a column named 'geometry'")

  if ("sfc_MULTIPOLYGON" %notin% class(dt_from$geometry))
    stop("The `from_sf` input must have a column named 'geometry' of class 'sfc_MULTIPOLYGON'")
  if ("sfc_MULTIPOLYGON" %notin% class(dt_to$geometry))
    stop("The `to_sf` input must have a column named 'geometry' of class 'sfc_MULTIPOLYGON'")
  if ("sfc_MULTIPOLYGON" %notin% class(dt_wts$geometry))
    stop("The `wts_sf` input must have a column named 'geometry' of class 'sfc_MULTIPOLYGON'")

  if (any(duplicated(dt_from$geoid)))
    stop("Error: the `geoid` variable must be unique in `dt_from`")
  if (any(duplicated(dt_to$geoid)))
    stop("Error: the `geoid` variable must be unique in `dt_to`")
  if (any(duplicated(dt_wts$geoid)))
    stop("Error: the `geoid` variable must be unique in `dt_wts`")

  if (check_that_wts_cover_from_and_to == TRUE) {
    from_union <- sf::st_union(from_sf)
    to_union <- sf::st_union(to_sf)
    wts_union <- sf::st_union(wts_sf)

    if (!sf::st_covered_by(from_union, wts_union, sparse = FALSE)[1, 1]) {
      stop_custom(
        .subclass = "wts_not_covering_from_error",
        message = "In create_cw(), `wts_sf` does not cover `from_sf`"
      )
    }
    
    if (!sf::st_covered_by(to_union, wts_union, sparse = FALSE)[1, 1]) 
      stop_custom(
        .subclass = "wts_not_covering_to_error",
        message = "In create_cw(), `wts_sf` does not cover `from_sf`"
      )

    rm(from_union, to_union, wts_union)
    
  }

  cw_out <- as.data.table(list(
    from_geoid = character(),
    to_geoid = character(),
    wt_var = numeric(), 
    afact = numeric()
  ))

  dt_from <- dt_from |> 
    setnames("geoid", "from_geoid")
  dt_to <- dt_to |> 
    setnames("geoid", "to_geoid")
  dt_wts <- dt_wts |> 
    setnames("geoid", "wts_geoid")

  ## Identify Alaska
  if (check_for_ak == FALSE) {
    dt_from_ak <- data.table()
    dt_to_ak <- data.table()
    dt_wts_ak <- data.table()

    cw_ak <- data.table()
    
  } else if (check_for_ak == TRUE) {
    dt_from <- dt_from[, is_ak := polygon_is_ak(geometry)]
    dt_to <- dt_to[, is_ak := polygon_is_ak(geometry)]
    dt_wts <- dt_wts[, is_ak := polygon_is_ak(geometry)]

    dt_from_ak <- dt_from[is_ak == 1L] |> 
      _[, is_ak := NULL] |> 
      # Transform to Alaska Albers projection (EPSG:3338)
      _[, geometry := sf::st_transform(geometry, crs = 3338)]
    dt_to_ak <- dt_to[is_ak == 1L] |> 
      _[, is_ak := NULL] |> 
      _[, geometry := sf::st_transform(geometry, crs = 3338)]
    dt_wts_ak <- dt_wts[is_ak == 1L] |> 
      _[, is_ak := NULL] |> 
      _[, geometry := sf::st_transform(geometry, crs = 3338)]

    if (nrow(dt_from_ak) > 0 && nrow(dt_to_ak) > 0 && nrow(dt_wts_ak) > 0) {
      cw_ak <- create_cw_worker(
        dt_from = dt_from_ak, dt_to = dt_to_ak, dt_wts = dt_wts_ak
      )
    } else {
      cw_ak <- data.table()
    }
    
    rm(dt_from_ak, dt_to_ak, dt_wts_ak)

    dt_from <- dt_from[is_ak == 0L] |> 
      _[, is_ak := NULL]
    if (sf::st_crs(dt_from$geometry)$epsg != 5070)
      dt_from <- dt_from[, geometry := sf::st_transform(geometry, crs = 5070)]
    
    dt_to <- dt_to[is_ak == 0L] |> 
      _[, is_ak := NULL]
    if (sf::st_crs(dt_to$geometry)$epsg != 5070)
      dt_to <- dt_to[, geometry := sf::st_transform(geometry, crs = 5070)]
    
    dt_wts <- dt_wts[is_ak == 0L] |> 
      _[, is_ak := NULL]
    if (sf::st_crs(dt_wts$geometry)$epsg != 5070)
      dt_wts <- dt_wts[, geometry := sf::st_transform(geometry, crs = 5070)]
  }

  if (check_for_hi == FALSE) {
    
    dt_from_ak <- data.table()
    dt_to_ak <- data.table()
    dt_wts_ak <- data.table()

    cw_hi <- data.table()
    
  } else if (check_for_hi == TRUE) {
    dt_from <- dt_from[, is_hi := polygon_is_hi(geometry)]
    dt_to <- dt_to[, is_hi := polygon_is_hi(geometry)]
    dt_wts <- dt_wts[, is_hi := polygon_is_hi(geometry)]

    dt_from_hi <- dt_from[is_hi == 1L] |> 
      _[, is_hi := NULL] |> 
      # Transform to Hawaii EPSG (EPSG:3563)
      _[, geometry := sf::st_transform(geometry, crs = 3563)]
    dt_to_hi <- dt_to[is_hi == 1L] |> 
      _[, is_hi := NULL] |> 
      _[, geometry := sf::st_transform(geometry, crs = 3563)]
    dt_wts_hi <- dt_wts[is_hi == 1L] |> 
      _[, is_hi := NULL] |> 
      _[, geometry := sf::st_transform(geometry, crs = 3563)]

    if (nrow(dt_from_hi) > 0 && nrow(dt_to_hi) > 0 && nrow(dt_wts_hi) > 0) {
      cw_hi <- create_cw_worker(
        dt_from = dt_from_hi, dt_to = dt_to_hi, dt_wts = dt_wts_hi
      )
    } else {
      cw_hi <- data.table()
    }
    
    rm(dt_from_hi, dt_to_hi, dt_wts_hi)

    dt_from <- dt_from[is_hi == 0L] |> 
      _[, is_hi := NULL]
    if (sf::st_crs(dt_from$geometry)$epsg != 5070)
      dt_from <- dt_from[, geometry := sf::st_transform(geometry, crs = 5070)]
    
    dt_to <- dt_to[is_hi == 0L] |> 
      _[, is_hi := NULL]
    if (sf::st_crs(dt_to$geometry)$epsg != 5070)
      dt_to <- dt_to[, geometry := sf::st_transform(geometry, crs = 5070)]

    dt_wts <- dt_wts[is_hi == 0L] |> 
      _[, is_hi := NULL]
    if (sf::st_crs(dt_wts$geometry)$epsg != 5070)
      dt_wts <- dt_wts[, geometry := sf::st_transform(geometry, crs = 5070)]
  }

  if (nrow(dt_from) != 0 && nrow(dt_to) != 0 && nrow(dt_wts) != 0) {
    cw_out <- create_cw_worker(
      dt_from = dt_from, dt_to = dt_to, dt_wts = dt_wts
    )
  } else {
    cw_out <- data.table()
  }

  cw_out <- rbind(
    cw_out, cw_ak, cw_hi, use.names = TRUE, fill = TRUE
  ) |> 
    setkey(from_geoid, to_geoid) |>
    _[order(from_geoid, to_geoid)] |>
    setnames("wt_var", wt_var_name)
  
  return(cw_out)

}
