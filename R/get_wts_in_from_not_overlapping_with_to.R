#' Get weights in from not overlapping with to
#'
#' This function is used to get weights that 
#' are in `dt_from` but not overlapping with `dt_to`.
#'
#' @param dt_from A data.table containing the from geography.
#' @param dt_to A data.table containing the to geography.
#' @param dt_wts A data.table containing the weights.
#'
#' @return A data.table containing the weights that are in 
#'   `dt_from` but not overlapping with `dt_to`.
#' 
#' @details This function identifies weights located within 
#'   the geometries defined in `dt_from` that do not 
#'   intersect with any geometries in `dt_to`. 
#'   It achieves this by:
#'   
#'   1. **Ensuring valid inputs:** It checks if the inputs 
#'     are data.tables with the required columns 
#'     (`from_geoid`, `to_geoid`, `wts_geoid`, `geometry`) 
#'     and of the correct class (`sfc_MULTIPOLYGON`).
#'   2. **Removing empty geometries:** It removes any empty 
#'     geometries from the input data.tables.
#'   3. **Finding weights within 'from' geography:** 
#'     It uses the `get_one_to_one_cw` function to identify 
#'     weights that are completely within the `dt_from` geometries.
#'   4. **Finding weights within 'to' geography:** 
#'     Similarly, it identifies weights that are completely 
#'     within the `dt_to` geometries.
#'   5. **Identifying non-overlapping weights:** 
#'     It uses `sf::st_overlaps` to find weights that do not 
#'     overlap with any `dt_to` geometries.
#'   6. **Filtering and merging:** It filters the weights within 
#'     `dt_from` to include only those not overlapping with 
#'     `dt_to`, merges this result with the weights within 
#'     `dt_to`, and finally merges with the original weight 
#'     data.table to include the weight variable (`wt_var`).
#'   7. **Ordering the output:** The final output data.table 
#'      is ordered by `from_geoid`, `to_geoid`, and `wts_geoid`.
#'
#' @keywords internal
#' @noRd 
get_wts_in_from_not_overlapping_with_to <- function(dt_from, dt_to, dt_wts) {

  # For NSE notes in R CMD check
  . <- from_geoid <- to_geoid <- wts_geoid <- geometry <- wt_var <- NULL
  from_index <- to_index <- wts_index <- afact <- NULL

  if (!inherits(dt_from, "data.table")) 
    stop("The `dt_from` input must be a data.table object")
  if (!inherits(dt_to, "data.table")) 
    stop("The `dt_to` input must be a data.table object")
  if (!inherits(dt_wts, "data.table")) 
    stop("The `dt_wts` input must be a data.table object")

  if ("from_geoid" %notin% names(dt_from)) 
    stop("The `dt_from` input must have a column named 'from_geoid'")
  if ("to_geoid" %notin% names(dt_to)) 
    stop("The `dt_to` input must have a column named 'to_geoid'")
  if ("wts_geoid" %notin% names(dt_wts)) 
    stop("The `dt_wts` input must have a column named 'wts_geoid'")

  if ("geometry" %notin% names(dt_from)) 
    stop("The `dt_from` input must have a column named 'geometry'")
  if ("geometry" %notin% names(dt_to)) 
    stop("The `dt_to` input must have a column named 'geometry'")
  if ("geometry" %notin% names(dt_wts)) 
    stop("The `dt_wts` input must have a column named 'geometry'")

  if ("sfc_MULTIPOLYGON" %notin% class(dt_from$geometry)) 
    stop("The `dt_from` input must have a column named 'geometry' of class 'sfc_MULTIPOLYGON'")
  if ("sfc_MULTIPOLYGON" %notin% class(dt_to$geometry)) 
    stop("The `dt_to` input must have a column named 'geometry' of class 'sfc_MULTIPOLYGON'")
  if ("sfc_MULTIPOLYGON" %notin% class(dt_wts$geometry)) 
    stop("The `dt_wts` input must have a column named 'geometry' of class 'sfc_MULTIPOLYGON'")

  dt_from <- dt_from[!sf::st_is_empty(geometry)]
  dt_to <- dt_to[!sf::st_is_empty(geometry)]
  dt_wts <- dt_wts[!sf::st_is_empty(geometry)]

  if (nrow(dt_from) == 0 || nrow(dt_to) == 0 || nrow(dt_wts) == 0)
    stop("In get_wts_in_from_and_to(), `dt_from`, `dt_to`, or `dt_wts` only contain empty geometries.")
  
  dt_from_covers_wts <- get_one_to_one_cw(
    dt_from = dt_wts[, .(from_geoid = wts_geoid, geometry)],
    dt_to = dt_from[, .(to_geoid = from_geoid, geometry)]
  ) |> 
    setnames("from_geoid", "wts_geoid") |> 
    setnames("to_geoid", "from_geoid") |> 
    _[, afact := NULL] %>%
    setcolorder(c("from_geoid", "wts_geoid"))
  
  dt_to_covers_wts <- get_one_to_one_cw(
    dt_from = dt_wts[, .(from_geoid = wts_geoid, geometry)],
    dt_to
  ) |> 
    setnames("from_geoid", "wts_geoid") |> 
    _[, afact := NULL] |>
    setcolorder(c("to_geoid", "wts_geoid"))

  wts_not_overlapping_with_to <- sf::st_overlaps(
    dt_wts$geometry, dt_to$geometry
  ) |> 
    lengths() |> as.logical() |> sapply(isFALSE) %>% 
    dt_wts[., wts_geoid]

  dt_wts_in_from_not_overlapping_with_to <- dt_from_covers_wts |>
    _[wts_geoid %chin% c(wts_not_overlapping_with_to)] |>
    merge(dt_to_covers_wts, by = "wts_geoid", all.x = TRUE) |>
    merge(dt_wts[, .(wts_geoid, wt_var)], by = "wts_geoid") |> 
    setcolorder(c("from_geoid", "to_geoid", "wts_geoid", "wt_var")) |> 
    _[order(from_geoid, to_geoid, wts_geoid)] 

  dt_wts_in_from_not_overlapping_with_to

}
