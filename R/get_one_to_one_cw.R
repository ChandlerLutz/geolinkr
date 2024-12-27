
#' Generate One-to-One Correspondence Weights between Spatial Datasets
#'
#' This function creates a one-to-one correspondence table (crosswalk) between geometries in two spatial datasets (`dt_from` and `dt_to`). The function uses specified spatial matching functions to determine which geometries in `dt_from` correspond to geometries in `dt_to`.
#'
#' @param dt_from A `data.table` containing `sfc_MULTIPOLYGON` geometries with columns `from_geoid` and `geometry`.
#' @param dt_to A `data.table` containing `sfc_MULTIPOLYGON` geometries with columns `to_geoid` and `geometry`.
#' @param funs A list of spatial predicate functions (default is `list(st_covered_by)`) used to match geometries between `dt_from` and `dt_to`.
#' @return A `data.table` with columns:
#' - `from_geoid`: Identifier from `dt_from`.
#' - `to_geoid`: Identifier from `dt_to`.
#' - `afact`: Assignment factor, set to 1 for all matches.
#' @details
#' The function checks that `dt_from` and `dt_to` are valid `data.table` objects with `sfc_MULTIPOLYGON` geometries and columns named `from_geoid` and `to_geoid`, respectively. It then applies the spatial functions specified in `funs` to establish one-to-one matches between the geometries in `dt_from` and `dt_to`. 
#'
#' For each spatial predicate in `funs`, if multiple geometries in `dt_from` correspond to a single geometry in `dt_to`, the function will stop with an error. Only unique matches (one-to-one) are accepted.
#'
#' @examples
#' library(tigris); library(sf); library(data.table); library(magrittr)
#'
#' options(tigris_use_cache = TRUE)
#' 
#' # get_one_to_one_cw() allocates all CT counties to CT state
#' 
#' ct_cnty22_sf <- tigris::counties(state = "CT", year = 2022) |>
#'  st_transform(crs = 5070) |>
#'  as.data.table() |>
#'  _[, .(geoid = GEOID, geometry)] |>
#'  sf::st_as_sf()
#' ct_st <- geolinkr::us_states_sf
#' ct_st <- ct_st[ct_st$statefp == "09", ]
#' 
#' covered_by_check <- sf::st_covered_by(ct_cnty22_sf, ct_st) %>%
#'   as.data.frame %>% as.data.table
#' 
#' stopifnot(nrow(covered_by_check) == nrow(ct_cnty22_sf))
#' 
#' dt_ct_cnty22 <- ct_cnty22_sf %>%
#'   as.data.table %>%
#'   .[, .(from_geoid = geoid, geometry)]
#' 
#' dt_ct_st <- ct_st %>%
#'   as.data.table %>%
#'   .[, .(to_geoid = statefp, geometry)]
#' 
#' cw <- get_one_to_one_cw(dt_from = dt_ct_cnty22,
#'                         dt_to = dt_ct_st)
#' print(cw)
#' 
#' stopifnot(nrow(cw) == nrow(ct_cnty22_sf))
#' stopifnot(sort(cw$from_geoid) == sort(ct_cnty22_sf$geoid))
#' stopifnot(cw[, unique(to_geoid)] == "09")
#' stopifnot(cw[, sum(afact)] == nrow(ct_cnty22_sf))
#' @import data.table 
#' @export
get_one_to_one_cw <- function(dt_from, dt_to, funs = list(sf::st_covered_by)) {

  # For NSE notes in R CMD check
  . <- geometry <- from_geoid <- to_geoid <- afact <- from_index <- to_index <- NULL

  if (!inherits(dt_from, "data.table")) 
    stop("The `dt_from` input must be a data.table object")
  if (!inherits(dt_to, "data.table")) 
    stop("The `dt_to` input must be a data.table object")

  if (!is.list(funs)) 
    stop("The `funs` input must be a list of spatial functions")

  if ("from_geoid" %notin% names(dt_from)) 
    stop("The `dt_from` input must have a column named 'from_geoid'")
  if ("to_geoid" %notin% names(dt_to)) 
    stop("The `dt_to` input must have a column named 'to_geoid'")

  if ("geometry" %notin% names(dt_from)) 
    stop("The `dt_from` input must have a column named 'geometry'")
  if ("geometry" %notin% names(dt_to)) 
    stop("The `dt_to` input must have a column named 'geometry'")

  if ("sfc_MULTIPOLYGON" %notin% class(dt_from$geometry)) 
    stop("The `dt_from` input must have a column named 'geometry' of class 'sfc_MULTIPOLYGON'")
  if ("sfc_MULTIPOLYGON" %notin% class(dt_to$geometry)) 
    stop("The `dt_to` input must have a column named 'geometry' of class 'sfc_MULTIPOLYGON'")

  dt_from <- dt_from |>
    _[!sf::st_is_empty(geometry)] |>
    _[!duplicated(geometry)]
  
  dt_to <- dt_to |>
    _[!sf::st_is_empty(geometry)] |>
    _[!duplicated(geometry)]

  if (nrow(dt_from) == 0 || nrow(dt_to) == 0)
    stop("In get_one_to_one_cw(), `dt_from` or `dt_to` only contain empty geometries.")

  from_geom <- dt_from[, geometry]
  to_geom <- dt_to[, geometry]

  dt_from <- dt_from[, .(from_index = .I, from_geoid)] %>%
    data.table::setkey(from_index)
  dt_to <- dt_to[, .(to_index = .I, to_geoid)] %>%
    data.table::setkey(to_index)

  cw <- as.data.table(
    list(
      from_geoid = character(),
      to_geoid = character(),
      afact = numeric()
    )
  )
  
  for (fun in funs) {

    dt_idx <- fun(from_geom, to_geom) %>%
      as.data.frame %>% as.data.table %>%
      setnames(names(.), c("from_index", "to_index"))

    if (nrow(dt_idx) > 0) {
        
      if (dt_idx[duplicated(from_index), .N] > 0)
        stop("In cw_one_to_one: `from` at least one `from` polygon matches with more than one `to` polygon for a given function in `funs`. Check to make sure that polygons in `dt_from` do not overlap with each other.")

      cw_fun <- merge(
        dt_idx, dt_from, by = "from_index"
      ) %>%
        merge(dt_to, by = "to_index") %>%
        .[, afact := 1] %>%
        .[, c("to_index", "from_index") := NULL]

      cw <- rbind(cw, cw_fun, use.names = TRUE)
  
    }
  }
  cw
}
