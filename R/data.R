#' US States Geometries
#'
#' A spatial dataset containing the boundaries of the 51 US states (including the District of Columbia).
#'
#' @format A simple feature collection (`sf` object) with 51 rows and 2 columns:
#' - `statefp`: A character vector representing the FIPS code for each state.
#' - `stusps`: A character vector with the two-letter USPS abbreviation for each state.
#' - `geometry`: A `MULTIPOLYGON` geometry column representing the boundaries of each state.
#'
#' @details
#' The dataset is projected in **NAD83 / Conus Albers** (EPSG:5070) for accurate spatial analysis across the contiguous US. It includes 51 features (50 states plus the District of Columbia) with geometries of class `MULTIPOLYGON`.
#'
#'
#' @source US Census Bureau.
"us_states_sf"

#' Puerto Rico Geometry
#'
#' A spatial dataset containing the boundary of Puerto Rico.
#'
#' @format A simple feature collection (`sf` object) with 1 row and 2 columns:
#' - `statefp`: A character vector representing the FIPS code for Puerto Rico.
#' - `stusps`: A character vector with the USPS abbreviation for Puerto Rico.
#' - `geometry`: A `MULTIPOLYGON` geometry column representing the boundary of Puerto Rico.
#'
#' @details
#' The dataset is projected in **NAD83 / Conus Albers** (EPSG:5070) for consistent spatial analysis with other US territories. It includes a single feature with a `MULTIPOLYGON` geometry representing the boundary of Puerto Rico.
#'
#'
#' @source US Census Bureau.
"puerto_rico_sf"


#' Connecticut County Visual Crosswalk (2020 - 2022)
#'
#' A dataset providing a visual crosswalk for Connecticut counties between 2020 and 2022.
#'
#' @format A data frame with 18 rows and 2 variables:
#' - `cntyfp2020`: County FIPS code for 2020.
#' - `cntyfp2022`: County FIPS code for 2022
#'
#' @source Chandler Lutz. This dataset is for illustrative purposes only and does not represent actual changes in Connecticut county FIPS codes.
#'
#' @examples
#' data(ct_visual_cw_2020_2022)
#' head(ct_visual_cw_2020_2022)
"ct_visual_cw_2020_2022" 
