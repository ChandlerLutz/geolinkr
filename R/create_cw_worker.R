#' Create Crosswalk (Worker Function)
#'
#' This function is a worker function used to create a crosswalk between two sets of 
#' polygons (`dt_from` and `dt_to`) using a third set of polygons (`dt_wts`) for 
#' weighting. It handles different intersection scenarios and calculates weights based on 
#' the areas of intersection.
#'
#' @param dt_from A data.table containing the "from" polygons with columns:
#'   - `from_geoid`: Character vector representing the unique IDs of the "from" polygons.
#'   - `geometry`: An sf geometry column containing the "from" polygons.
#' @param dt_to A data.table containing the "to" polygons with columns:
#'   - `to_geoid`: Character vector representing the unique IDs of the "to" polygons.
#'   - `geometry`: An sf geometry column containing the "to" polygons.
#' @param dt_wts A data.table containing the weight polygons with columns:
#'   - `wts_geoid`: Character vector representing the unique IDs of the weight polygons.
#'   - `wt_var`: Numeric vector representing the weight variable associated with each weight polygon.
#'   - `geometry`: An sf geometry column containing the weight polygons.
#'
#' @details 
#' This function performs the following steps:
#' 
#' 1. **Input Validation:** Checks if the inputs are data.tables and contain the required columns.
#' 2. **CRS Check:** Ensures that all input data.tables have the same Coordinate Reference System (CRS).
#' 3. **Empty Geometry Handling:**  Removes any empty geometries from the input data.tables.
#' 4. **1:1 Mapping:** Identifies cases where `dt_to` polygons completely cover `dt_from` polygons and creates a 1:1 mapping.
#' 5. **Weighted Crosswalk:** For the remaining polygons, calculates the intersection between `dt_from`, `dt_to`, and `dt_wts` 
#'    and determines the weights based on the area of intersection relative to the weight polygons.
#'
#' @return A data.table with the following columns representing the crosswalk:
#'   - `from_geoid`: Character vector of "from" polygon IDs.
#'   - `to_geoid`: Character vector of "to" polygon IDs.
#'   - `wt_var`: Numeric vector representing the weight variable.
#'   - `afact`: Numeric vector representing the area factor (normalized weights).
#'
#' @keywords internal
#' @noRd 
create_cw_worker <- function(dt_from, dt_to, dt_wts) {

  . <- from_geoid <- to_geoid <- wts_geoid <- wt_var <- geometry <- NULL
  afact <- wts_geom_area <- intersect3_area <- from_wts_intersect_area <- NULL
  allocated_area <- unallocated_area <- NULL
  

  # Check that each input is a data.table 
  if (!inherits(dt_from, "data.table")) 
    stop("The `dt_from` input must be a data.table object")
  if (!inherits(dt_to, "data.table"))
    stop("The `dt_to` input must be a data.table object")
  if (!inherits(dt_wts, "data.table"))
    stop("The `dt_wts` input must be a data.table object")

  # Check that each input has the required columns
  if ("from_geoid" %notin% names(dt_from)) 
    stop("The `dt_from` input must have a column named 'from_geoid'")
  if ("to_geoid" %notin% names(dt_to))
    stop("The `dt_to` input must have a column named 'to_geoid'")
  if ("wts_geoid" %notin% names(dt_wts))
    stop("The `dt_wts` input must have a column named 'wts_geoid'")
  if ("wt_var" %notin% names(dt_wts))
    stop("The `dt_wts` input must have a column named 'wt_var'")
  if ("geometry" %notin% names(dt_from))
    stop("The `dt_from` input must have a column named 'geometry'")
  if ("geometry" %notin% names(dt_to))
    stop("The `dt_to` input must have a column named 'geometry'")
  if ("geometry" %notin% names(dt_wts))
    stop("The `dt_wts` input must have a column named 'geometry'")

  # Check for matching CRS
  if (!identical(sf::st_crs(dt_from$geometry), sf::st_crs(dt_to$geometry))) 
    stop("The `dt_from` and `dt_to` objects must have the same CRS")
  if (!identical(sf::st_crs(dt_from$geometry), sf::st_crs(dt_wts$geometry))) 
    stop("The `dt_from` and `dt_wts` objects must have the same CRS")

  # Check for empty geometries
  dt_from <- dt_from[!sf::st_is_empty(geometry)]
  dt_to <- dt_to[!sf::st_is_empty(geometry)]
  dt_wts <- dt_wts[!sf::st_is_empty(geometry)]

  if (nrow(dt_from) == 0 || nrow(dt_to) == 0 || nrow(dt_wts) == 0)
    stop("In create_cw_worker(), `dt_from`, `dt_to`, or `dt_wts` only contain empty geometries.")

  cw_out <- data.table(
    from_geoid = character(),
    to_geoid = character(),
    wt_var = numeric(),
    afact = numeric()
  )

  ## 1:1 mapping when dt_to covers dt_from
  cw_one_to_one <- get_one_to_one_cw(dt_from = dt_from, dt_to = dt_to)
  if (nrow(cw_one_to_one) > 0) {
    cw_out <- rbind(cw_out, cw_one_to_one, use.names = TRUE, fill = TRUE)
    if (all(dt_from$from_geoid %chin% cw_out$from_geoid)) {
      cw_out <- cw_out |>
        _[order(from_geoid, to_geoid)]
      return(cw_out)
    }
    
    wts_in_from_and_to <- sf::st_covered_by(
      dt_wts$geometry, dt_from[from_geoid %chin% cw_one_to_one$from_geoid, geometry]
    ) |> 
      lengths() |> as.logical() |> sapply(isTRUE) %>% dt_wts[., wts_geoid]

    dt_wts <- dt_wts[wts_geoid %notin% c(wts_in_from_and_to)]
    
    dt_from <- dt_from[!from_geoid %in% cw_one_to_one$from_geoid]
  }
  
  ## For dt_from polygons not covered by or equal to dt_from, create the crosswalk
  ## Two steps:
  ## 1. Find the polygons in dt_wts covered by dt_from AND dt_to. These
  ##    polygons yield a 1:1 mapping between dt_from and dt_to
  ## 2. For the polygons in dt_wts not in (1), calculate the intersection of 
  ##    relevant polygons from dt_from, dt_to, and dt_wts. Evaluate this
  ##    intersection relative to weight polygon. That is the share of the `wt_var`
  ##    for the polygon in dt_wts that relates dt_from and dt_to

  ## -- Step 1 -- ##

  dt_wts_in_from_not_overlapping_with_to <- get_wts_in_from_not_overlapping_with_to(
    dt_from, dt_to, dt_wts
  )
  if (nrow(dt_wts_in_from_not_overlapping_with_to) > 0
      && dt_wts_in_from_not_overlapping_with_to[, !all(is.na(to_geoid))]) {
    
    dt_wts <- dt_wts |>
      _[wts_geoid %notin% dt_wts_in_from_not_overlapping_with_to$wts_geoid]
    
    dt_wts_in_from_not_overlapping_with_to <- dt_wts_in_from_not_overlapping_with_to |>
      _[, .(wt_var = sum(wt_var)), by = .(from_geoid, to_geoid)]
    
    if (any(is.na(dt_wts_in_from_not_overlapping_with_to$wt_var))) {
      stop("The weight variable `wt_var` contains missing values. Check the input data.")
    }
    
    cw_out <- rbind(
      cw_out, dt_wts_in_from_not_overlapping_with_to, use.names = TRUE, fill = TRUE
    )

    if (nrow(dt_wts) == 0 && all(dt_from$from_geoid %chin% cw_out$from_geoid)) {
      cw_out <- cw_out |> 
        _[is.na(afact), afact := wt_var / sum(wt_var), by = .(from_geoid)] |>
        setkey(from_geoid, to_geoid) |>
        _[order(from_geoid, to_geoid)]
      return(cw_out)
    }
    
  }

  ## -- Step 2 -- ##

  ## For the remaining polygons in dt_wts, calculate the intersection between
  ## dt_from, dt_to, and dt_wts. Evaluate the intersection relative to the weight
  ## polygon. That is, the share of the `wt_var` for the polygon in dt_wts that
  ## relates dt_from and dt_to. Area in dt_from that does not overlap with dt_to will
  ## be unallocated in the crosswalk.
  
  dt_from_wts_intersection <- st_intersection_to_multipolygon(
    dt_from[, .(from_geoid, geometry)], dt_wts[, .(wts_geoid, geometry)]
  )

  dt_wts <- dt_wts |>
    _[, wts_geom_area := as.numeric(sf::st_area(geometry))] |>
    _[, geometry := NULL]

  dt_triple_intersection <- st_intersection_to_multipolygon(
    dt_from_wts_intersection, dt_to[, .(to_geoid, geometry)]
  ) |> 
    _[, intersect3_area := as.numeric(sf::st_area(geometry))] |>
    _[, geometry := NULL]

  dt_from_wts_intersection <- dt_from_wts_intersection |>
    _[, from_wts_intersect_area := as.numeric(sf::st_area(geometry))] |>
    _[, geometry := NULL]

  dt_allocated_area <- dt_triple_intersection |>
    _[, .(allocated_area = sum(intersect3_area)), by = .(from_geoid, wts_geoid)]

  dt_step2_unallocated <- dt_from_wts_intersection |>
    merge(dt_allocated_area, by = c("from_geoid", "wts_geoid"), all.x = TRUE) |>
    _[is.na(allocated_area), allocated_area := 0] |>
    _[, unallocated_area := from_wts_intersect_area - allocated_area] |>
    # For rounding errors. Negative values are set to 0
    _[unallocated_area < 0, unallocated_area := 0] |>
    _[, c("from_wts_intersect_area", "allocated_area") := NULL] |>
    merge(dt_wts, by = "wts_geoid", all.x = TRUE) |>
    _[, .(wt_var = sum(wt_var * (unallocated_area / wts_geom_area))),
      keyby = .(from_geoid)] |>
    _[, to_geoid := NA_character_] |>
    setcolorder(c("from_geoid", "to_geoid", "wt_var"))

  dt_step2_allocated <- dt_triple_intersection |>
    merge(dt_wts, by = "wts_geoid", all.x = TRUE) |>
    _[, .(wt_var = sum(wt_var * (intersect3_area / wts_geom_area))),
      keyby = .(from_geoid, to_geoid)]
  
  cw_out <- rbind(cw_out, dt_step2_allocated, dt_step2_unallocated,
                  use.names = TRUE, fill = TRUE)

  f_sum <- function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    } else {
      return(sum(x, na.rm = FALSE))
    }
  }

  cw_out <- cw_out |>
    _[, .(wt_var = f_sum(wt_var)), keyby = .(from_geoid, to_geoid)] |>
    _[, afact := wt_var / sum(wt_var), by = .(from_geoid)] |>
    _[is.na(afact) | afact > 1e-06] |>
    _[, afact := wt_var / sum(wt_var), by = .(from_geoid)] |>
    _[is.na(afact), afact := 1L] |>
    _[order(from_geoid, to_geoid)]

  return(cw_out)

}
