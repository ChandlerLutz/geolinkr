library(sf); library(data.table); library(tigris); library(tidycensus); library(ggplot2);

options(tigris_use_cache = TRUE)

if (!is_checking() || !is_testing())
  source(here::here("tests/testthat/helpers.R"))

test_that("create_cw_worker() works for 2020 to 2022 CT counties, weighting by census blocks", {

  ct_blocks20 <- f_get_ct20_shp("blocks") %>%
    as.data.table() %>%
    setnames("GEOID", "wts_geoid") %>%
    setnames("hh2020", "wt_var")

  ct_cnty20 <- tigris::counties(state = "CT", year = 2020) |>
    st_transform(crs = 5070) %>%
    as.data.table() %>%
    .[, .(GEOID, geometry)] %>%
    setnames("GEOID", "from_geoid")
  
  ct_cnty22 <- tigris::counties(state = "CT", year = 2022) |>
    st_transform(crs = 5070) %>%
    as.data.table() %>%
    .[, .(GEOID, geometry)] %>%
    setnames("GEOID", "to_geoid")

  result <- create_cw_worker(dt_from = ct_cnty20,
                             dt_to = ct_cnty22,
                             dt_wts = ct_blocks20)

  ## Check that from_geoid should have all of the counties in the source data
  expect_equal(result[, sort(unique(from_geoid))], sort(unique(ct_cnty20$from_geoid)))

  dt_visual_cw <- geolinkr::ct_visual_cw_2020_2022

  for (cnty20 in sort(ct_cnty20$from_geoid)) {

    visual_cw_to_cntyfps <- dt_visual_cw[cntyfp2020 == c(cnty20), cntyfp2022]
    
    ## For cnty20, check that the target 2022 counties in the visual cw are in the result
    expect_true(
      all(
        visual_cw_to_cntyfps %chin% result[from_geoid == c(cnty20), to_geoid]
      ),
      info = paste("cnty20:", cnty20)
    )
    
    ## For cnty20, check that result has an afact bigger than 0.5% for the counties
    ## in the visual cw
    expect_true(all(result[from_geoid == c(cnty20)
                           & to_geoid %chin% visual_cw_to_cntyfps,
                           afact] > 0.005),
                info = paste("cnty20:", cnty20))

    ## For cnty20, check that result has an afact smaller than 0.01% for the counties
    ## not in the visual cw
    expect_true(all(result[from_geoid == c(cnty20)
                           & to_geoid %notin% visual_cw_to_cntyfps,
                           afact] < 0.0001),
                info = paste("cnty20:", cnty20))

  }
  
})

test_that("create_cw_worker() works for 2022 to 2020 CT counties, weighting by census blocks", {

  ct_blocks20 <- f_get_ct20_shp("blocks") %>%
    as.data.table() %>%
    setnames("GEOID", "wts_geoid") %>%
    setnames("hh2020", "wt_var")

  ct_cnty20 <- tigris::counties(state = "CT", year = 2020) |>
    st_transform(crs = 5070) %>%
    as.data.table() %>%
    .[, .(GEOID, geometry)] %>%
    setnames("GEOID", "to_geoid")

  ct_cnty22 <- tigris::counties(state = "CT", year = 2022) |>
    st_transform(crs = 5070) %>%
    as.data.table() %>%
    .[, .(GEOID, geometry)] %>%
    setnames("GEOID", "from_geoid")

  result <- create_cw_worker(dt_from = ct_cnty22,
                             dt_to =, ct_cnty20, 
                             dt_wts = ct_blocks20)

  ## Check that from_geoid should have all of the counties in the source data
  expect_equal(result[, sort(unique(from_geoid))], sort(unique(ct_cnty22$from_geoid)))

  dt_visual_cw <- geolinkr::ct_visual_cw_2020_2022

  for (cnty22 in sort(ct_cnty22$from_geoid)) {

    visual_cw_to_cntyfps <- dt_visual_cw[cntyfp2022 == c(cnty22), cntyfp2020]
    
    ## For cnty22, check that the target 2020 counties in the visual cw are in the result
    expect_true(
      all(
        visual_cw_to_cntyfps %chin% result[from_geoid == c(cnty22), to_geoid]
      ),
      info = paste("cnty22:", cnty22)
    )
    
    ## For cnty20, check that result has an afact bigger than 0.5% for the counties
    ## in the visual cw
    expect_true(all(result[from_geoid == c(cnty22)
                           & to_geoid %chin% visual_cw_to_cntyfps,
                           afact] > 0.005),
                info = paste("cnty22:", cnty22))

  }

})
