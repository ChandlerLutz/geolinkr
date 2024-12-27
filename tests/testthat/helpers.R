## tests/test-helpers.R

library(testthat)

# Note: in `testthat::test()` and `devtools::check()` the working directory
# for this file will be `tests/testthat`
# We need to use `file.path()` for this to work:
# https://github.com/r-lib/testthat/issues/1270

if (!is_testing() && !is_checking()) {
  dir.create(here::here("tests/testthat/external-testdata"), showWarnings = FALSE)
} else {
  dir.create(test_path("external-testdata"), showWarnings = FALSE)
}

ex_test_data_path <- function(file) {

  if (is_testing() || is_checking()) {
    return(test_path("external-testdata", file))
  } else {
    return(here::here("tests/testthat/external-testdata", file))
  }
}

f_get_geocorr_test_data <- function(file) {

  if (file.exists(ex_test_data_path(file))) {
    return(fread(ex_test_data_path(file), colClasses = "character"))
  } else {
    out <- try({
      fread(
        sprintf(          "https://github.com/ChandlerLutz/missouri-geocorr-test-data/blob/main/%s?raw=1",
                file),
        colClasses = "character"
      )
    })
    if (inherits(out, "try-error")) {
      stop(sprintf("Error: could not download file %s", file))
    }
    fwrite(out, ex_test_data_path(file))
    return(out)
  }
}


f_get_ct20_shp <- function(geo) {

  if (geo == "tracts") {
    if (!file.exists(ex_test_data_path("ct_tracts20.rds"))) {
      download.file(
        url = "https://github.com/ChandlerLutz/ct-shps/blob/main/ct_tracts20_sf.rds?raw=1",
        destfile = ex_test_data_path("ct_tracts20.rds")
      )
    }
    return(readRDS(ex_test_data_path("ct_tracts20.rds")))
  } else if (geo == "blkgrp") {
    if (!file.exists(ex_test_data_path("ct_blkgrp20.rds"))) {
      download.file(
        url = "https://github.com/ChandlerLutz/ct-shps/blob/main/ct_blkgrp20_sf.rds?raw=1",
        destfile = ex_test_data_path("ct_blkgrp20.rds")
      )
    }
    return(readRDS(ex_test_data_path("ct_blkgrp20.rds")))
  } else if (geo == "blocks") {
    if (!file.exists(ex_test_data_path("ct_blocks20.rds"))) {
      download.file(
        url = "https://github.com/ChandlerLutz/ct-shps/blob/main/ct_blocks20_sf.rds?raw=1",
        destfile = ex_test_data_path("ct_blocks20.rds")
      )
    }
    return(readRDS(ex_test_data_path("ct_blocks20.rds")))
  } else {
    stop("Error: in `f_get_ct20_shp()`, `geo` must be one of 'tracts', 'blkgrp', or 'blocks'")
  }
  
}

f_get_ak20_shp <- function(geo) {
  
  if (geo == "tracts") {
    if (!file.exists(ex_test_data_path("ak_tracts20.rds"))) {
      download.file(
        url = "https://github.com/ChandlerLutz/ak-hi-shps/blob/main/ak_tracts20_sf.rds?raw=1",
        destfile = ex_test_data_path("ak_tracts20.rds")
      )
    }
    return(readRDS(ex_test_data_path("ak_tracts20.rds")))
  } else if (geo == "blocks") {
    if (!file.exists(ex_test_data_path("ak_blocks20.rds"))) {
      download.file(
        url = "https://github.com/ChandlerLutz/ak-hi-shps/blob/main/ak_blocks20_sf.rds?raw=1",
        destfile = ex_test_data_path("ak_blocks20.rds")
      )
    }
    return(readRDS(ex_test_data_path("ak_blocks20.rds")))
  } else {
    stop("Error: in `f_get_ak20_shp()`, `geo` must be 'tracts' or 'blocks'")
  }
  
}

f_get_hi20_shp <- function(geo) {

  if (geo == "tracts") {
    if (!file.exists(ex_test_data_path("hi_tracts20.rds"))) {
      download.file(
        url = "https://github.com/ChandlerLutz/ak-hi-shps/blob/main/hi_tracts20_sf.rds?raw=1",
        destfile = ex_test_data_path("hi_tracts20.rds")
      )
    } 
    return(readRDS(ex_test_data_path("hi_tracts20.rds")))
  } else if (geo == "blocks") {
    if (!file.exists(ex_test_data_path("hi_blocks20.rds"))) {
      download.file(
        url = "https://github.com/ChandlerLutz/ak-hi-shps/blob/main/hi_blocks20_sf.rds?raw=1",
        destfile = ex_test_data_path("hi_blocks20.rds")
      )
      return(readRDS(ex_test_data_path("hi_blocks20.rds")))
    }
    return(readRDS(ex_test_data_path("hi_blocks20.rds")))
  } else {
    stop("Error: in `f_get_hi20_shp()`, `geo` must be 'blocks' or 'tracts'")
  }
}
