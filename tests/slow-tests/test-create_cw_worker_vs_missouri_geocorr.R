library(sf); library(data.table); library(tigris); library(tidycensus); library(ggplot2);

options(tigris_use_cache = TRUE)

if (!is_checking() || !is_testing())
  source(here::here("tests/testthat/helpers.R"))

test_that("create_cw_worker() has similar weights to Missouri goecorr for 2020 CT tracts to county subdivisions", {

  ct_tracts20 <- tigris::tracts(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    st_cast(to = "MULTIPOLYGON") |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "from_geoid")

  ct_cntysub20 <- tigris::county_subdivisions(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "to_geoid")

  ct_blocks20 <- f_get_ct20_shp("blocks") |>
    as.data.table() |>
    setnames("GEOID", "wts_geoid") |>
    setnames("hh2020", "wt_var")

  result <- create_cw_worker(dt_from = ct_tracts20,
                             dt_to = ct_cntysub20,
                             dt_wts = ct_blocks20)


  geocorr_cw_ct20_tracts_to_cntysub <-
    f_get_geocorr_test_data("ct20_cw_tract_to_cntysub.csv") |>
    _[-1] |>
    _[, .(tract_geocorr = paste0(county, tract),
          cntysub_geocorr = paste0(county, cousub20),
          wt_var_geocorr = as.integer(hus20), afact_geocorr = as.numeric(afact))] |>
    _[, tract_geocorr := gsub("\\.", "", tract_geocorr)]

  test_data <- merge(result[afact > 0.000001], geocorr_cw_ct20_tracts_to_cntysub,
                     by.x = c("from_geoid", "to_geoid"),
                     by.y = c("tract_geocorr", "cntysub_geocorr"),
                     all = TRUE)

  ## Check that the only missing record from create_cw_worker() relative to the
  ## Missouri geocorr is the from_geoid = "09009347200" and to_geoid = "0900949950".
  ## The missouri geocorr has an intersection between these regions, but the shapefiles
  ## do not intersect. See the below plot. 

  ## ggplot() + geom_sf(data = ct_cntysub20[to_geoid == "0900949950", geometry],
  ##                    color = "blue", fill = NA, linewidth = 3) +
  ##   geom_sf(data = ct_tracts20[from_geoid == "09009347200", geometry],
  ##           color = "red", fill = NA)

  expect_equal(nrow(test_data[is.na(afact)]), 1)
  expect_true(
    test_data[is.na(afact),
              from_geoid == "09009347200" & to_geoid == "0900949950"]
  )

  

  ## Check that cases where afact_geocorr is missing but afact is not missing that
  ## `wt_var` has zero output. See the plots.
  test_data_na_afact_geocorr <- test_data[is.na(afact_geocorr)]

  ## ggplot() + geom_sf(
  ##   data = ct_cntysub20[to_geoid %chin% test_data_na_afact_geocorr$to_geoid, geometry],
  ##   color = "blue", fill = NA, linewidth = 1.1
  ## ) +
  ##   geom_sf(
  ##     data = ct_tracts20[from_geoid %chin% test_data_na_afact_geocorr$from_geoid,
  ##                        geometry],
  ##     color = "red", fill = NA
  ##   )

  expect_true(test_data_na_afact_geocorr[, sum(wt_var, na.rm = TRUE) == 0])

  ## Check that afact is similar between create_cw_worker() and the Missouri geocorr
  expect_true(test_data[!is.na(afact_geocorr) & !is.na(afact),
                        abs(afact - afact_geocorr) < 0.01] |> all())

  expect_true(test_data[!is.na(afact_geocorr) & !is.na(afact),
                        cor(afact, afact_geocorr) > 0.999])
  
})

test_that("create_cw_worker() has similar weights to Missouri goecorr for 2020 CT county subdivisions to census tracts", {
  
  ct_cntysub20 <- tigris::county_subdivisions(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "from_geoid")
  
  ct_tracts20 <- tigris::tracts(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    st_cast(to = "MULTIPOLYGON") |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "to_geoid")
  
  ct_blocks20 <- f_get_ct20_shp("blocks") |>
    as.data.table() |>
    setnames("GEOID", "wts_geoid") |>
    setnames("hh2020", "wt_var")
  
  result <- create_cw_worker(dt_from = ct_cntysub20,
                             dt_to = ct_tracts20,
                             dt_wts = ct_blocks20)
  
  geocorr_cw_ct20_cntysub_to_tracts <- 
    f_get_geocorr_test_data("ct20_cw_cntysub_to_tract.csv") |>
    _[-1] |>
    _[, .(cntysub_geocorr = paste0(county, cousub20),
          tract_geocorr = paste0(county, tract),
          wt_var_geocorr = as.integer(hus20), afact_geocorr = as.numeric(afact))] |>
    _[, tract_geocorr := gsub("\\.", "", tract_geocorr)]
  
  test_data <- merge(result[afact > 0.000001], geocorr_cw_ct20_cntysub_to_tracts,
                     by.x = c("from_geoid", "to_geoid"),
                     by.y = c("cntysub_geocorr", "tract_geocorr"),
                     all = TRUE)

  ## Check that the only missing record from create_cw_worker() relative to the
  ## Missouri geocorr is the from_geoid = "0900949950" and to_geoid = "09009347200".
  ## The missouri geocorr has an intersection between these regions, but the shapefiles
  ## do not intersect. See the below plot.

  ## ggplot() + geom_sf(data = ct_tracts20[to_geoid == "09009347200", geometry],
  ##                    color = "blue", fill = NA, linewidth = 3) +
  ##   geom_sf(data = ct_cntysub20[from_geoid == "0900949950", geometry],
  ##           color = "red", fill = NA)

  expect_true(
    test_data[is.na(afact), from_geoid == "0900949950" & to_geoid == "09009347200"]
  )

  ## Check that cases where afact_geocorr is missing but afact is not missing that
  ## the tract (from_geoid) is undefined
  expect_true(
    test_data[is.na(afact_geocorr), substr(from_geoid, 6, 10) == "00000"] |>
      all()
  )

  ## The only notable difference in afact between the result and the Missouri geocorr
  ## is tract == "0900914160" and countysub == "09009343101". The difference in
  ## allocation factors is still less than 0.03.
  expect_true(
    test_data[
      abs(afact - afact_geocorr) > 0.01 & abs(afact - afact_geocorr) < 0.03,
      from_geoid == "0900914160" & to_geoid == "09009343101"
    ]
  )
  
  ## Check that afact is otherwise similar between create_cw_worker() and
  ## the Missouri geocorr
  expect_true(
    test_data[
      !is.na(afact_geocorr) & !is.na(afact)
      & !(from_geoid == "0900914160" & to_geoid == "09009343101"), 
      abs(afact - afact_geocorr) < 0.01
    ] |>
      all()
  )
  
  expect_true(
    test_data[
      !is.na(afact_geocorr) & !is.na(afact), cor(afact, afact_geocorr) > 0.999
    ]
  )

})


test_that("create_cw_worker() has similar weights to Missouri geocorr for 2020 CT zcta to county subdivisions", {

  ct_zip20 <- tigris::zctas(starts_with = "06", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(ZCTA5CE20, geometry)] |>
    setnames("ZCTA5CE20", "from_geoid")

  ct_cntysub20 <- tigris::county_subdivisions(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "to_geoid")

  ct_blocks20 <- f_get_ct20_shp("blocks") |>
    as.data.table() |>
    setnames("GEOID", "wts_geoid") |>
    setnames("hh2020", "wt_var")

  result <- create_cw_worker(dt_from = ct_zip20,
                             dt_to = ct_cntysub20,
                             dt_wts = ct_blocks20)

  geocorr_cw_ct20_zip_to_cntysub <-
    f_get_geocorr_test_data("ct20_cw_zcta_to_cntysub.csv") |>
    _[-1] |>
    _[, .(zcta_geocorr = zcta, cntysub_geocorr = paste0(county, cousub20),
          wt_var_geocorr = as.integer(hus20), afact_geocorr = as.numeric(afact))]

  test_data <- merge(result[afact > 0.000001], geocorr_cw_ct20_zip_to_cntysub,
                     by.x = c("from_geoid", "to_geoid"),
                     by.y = c("zcta_geocorr", "cntysub_geocorr"),
                     all = TRUE)

  ## Check that the only difference between create_cw_worker() and the Missouri geocorr
  ## is the from_geoid = "06770" and to_geoid = "0900962290" that is below the
  ## tolerance of the missouri geocorr
  expect_equal(nrow(test_data[is.na(afact_geocorr) | is.na(afact)]), 1)

  ## Check that afact is similar between create_cw_worker() and the Missouri geocorr
  expect_true(test_data[!is.na(afact_geocorr) & !is.na(afact),
                        abs(afact - afact_geocorr) < 0.01] |> all())

  expect_true(test_data[!is.na(afact_geocorr) & !is.na(afact),
                        cor(afact, afact_geocorr) > 0.999])


})

test_that("create_cw_worker() has similar weights to Missouri geocorr for 2020 CT county subdivisions to zcta", {

  ct_cntysub20 <- tigris::county_subdivisions(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "from_geoid")

  ct_zip20 <- tigris::zctas(starts_with = "06", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(ZCTA5CE20, geometry)] |>
    setnames("ZCTA5CE20", "to_geoid")

  ct_blocks20 <- f_get_ct20_shp("blocks") |>
    as.data.table() |>
    setnames("GEOID", "wts_geoid") |>
    setnames("hh2020", "wt_var")

  result <- create_cw_worker(dt_from = ct_cntysub20, 
                             dt_to = ct_zip20,
                             dt_wts = ct_blocks20) |>
    _[order(from_geoid, to_geoid)]

  geocorr_cw_ct20_cntysub_to_zip <- 
    f_get_geocorr_test_data("ct20_cw_cntysub_to_zcta.csv") |>
    _[-1] |>
    _[, .(cntysub_geocorr = paste0(county, cousub20),
          zcta_geocorr = zcta, wt_var_geocorr = as.integer(hus20),
          afact_geocorr = as.numeric(afact))] |>
    _[order(cntysub_geocorr, zcta_geocorr)]

  test_data <- merge(result[afact > 0.000001], geocorr_cw_ct20_cntysub_to_zip,
                     by.x = c("from_geoid", "to_geoid"),
                     by.y = c("cntysub_geocorr", "zcta_geocorr"),
                     all = TRUE)

  ## `result` has a record for every entry in `geocorr_cw_ct20_cntysub_to_zip`
  expect_equal(nrow(test_data[is.na(afact)]), 0)
  ## `result` has undefined county subdivisions as they are in the shape file,
  ## but these are not in the Missouri geocorr
  expect_true(
    test_data[
      is.na(afact_geocorr) & substr(from_geoid, 6, 10) == "00000",
      from_geoid
    ] %chin%
      ct_cntysub20[
        substr(from_geoid, 6, 10) == "00000", from_geoid
      ] |>
      all()
  )

  ## The only result that does not match the Missouri geocorr is
  ## countysub20 = 0900962290 and zcta = 06770
  expect_true(
    test_data[
      is.na(afact_geocorr) & !is.na(afact) & substr(from_geoid, 6, 10) != "00000",
      from_geoid == "0900962290" & to_geoid == "06770"
    ]
  )

  ## Check that afact is similar between create_cw_worker() and the Missouri geocorr
  expect_true(test_data[!is.na(afact_geocorr) & !is.na(afact),
                        abs(afact - afact_geocorr) < 0.01] |> all())

  expect_true(test_data[!is.na(afact_geocorr) & !is.na(afact),
                        cor(afact, afact_geocorr) > 0.999])


})



test_that("create_cw_worker() has similar weights to Missouri geocorr for 2020 CT places to county subdivisions", {

  ct_places20 <- tigris::places(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "from_geoid")

  ct_cntysub20 <- tigris::county_subdivisions(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "to_geoid")

  ct_blocks20 <- f_get_ct20_shp("blocks") |>
    as.data.table() |>
    setnames("GEOID", "wts_geoid") |>
    setnames("hh2020", "wt_var")

  ggplot() + geom_sf(data = ct_cntysub20$geometry, color = "blue", fill = NA) +
    geom_sf(data = ct_places20$geometry, color = "red", fill = NA) 

  result <- create_cw_worker(dt_from = ct_places20,
                             dt_to = ct_cntysub20,
                             dt_wts = ct_blocks20)

  geocorr_cw_ct20_places_to_cntysub <-
    f_get_geocorr_test_data("ct20_cw_place_to_cntysub.csv") |>
    _[-1] |>
    _[, .(place_geocorr = paste0(state, place),
          cntysub_geocorr = paste0(county, cousub20),
          wt_var_geocorr = as.integer(hus20), afact_geocorr = as.numeric(afact))]

  test_data <- merge(result[afact > 0.000001], geocorr_cw_ct20_places_to_cntysub,
                     by.x = c("from_geoid", "to_geoid"),
                     by.y = c("place_geocorr", "cntysub_geocorr"),
                     all = TRUE)

  ## For unallocated places in the Missouri geocorr, check that afact is missing
  expect_true(test_data[from_geoid == "0999999", all(is.na(afact))])

  ## Check that the result and the Missouri geocorr have the same number or records
  ## for places
  expect_equal(nrow(test_data[from_geoid != "0999999"][is.na(afact)]), 0)

  ## Check that afact is similar between create_cw_worker() and the Missouri geocorr
  expect_true(test_data[!is.na(afact_geocorr) & !is.na(afact),
                        abs(afact - afact_geocorr) < 0.01] |> all())

  expect_true(test_data[!is.na(afact_geocorr) & !is.na(afact),
                        cor(afact, afact_geocorr) > 0.999])


})


test_that("create_cw_worker() has similar weights to Missouri geocorr for 2020 CT county subdivisions to places", {

  ct_cntysub20 <- tigris::county_subdivisions(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "from_geoid")

  ct_places20 <- tigris::places(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "to_geoid")

  ct_blocks20 <- f_get_ct20_shp("blocks") |>
    as.data.table() |>
    setnames("GEOID", "wts_geoid") |>
    setnames("hh2020", "wt_var")

  result <- create_cw_worker(dt_from = ct_cntysub20,
                             dt_to = ct_places20,
                             dt_wts = ct_blocks20)

  geocorr_cw_ct20_cntysub_to_places <- 
    f_get_geocorr_test_data("ct20_cw_cntysub_to_place.csv") |>
    _[-1] |>
    _[, .(cntysub_geocorr = paste0(county, cousub20),
          place_geocorr = paste0(state, place),
          wt_var_geocorr = as.integer(hus20), afact_geocorr = as.numeric(afact))] |>
    ## For unallocated places in the Missouri geocorr, set afact to NA like in
    ## the result
    _[place_geocorr == "0999999", place_geocorr := NA_character_]

  test_data <- merge(result[afact > 0.000001], geocorr_cw_ct20_cntysub_to_places,
                     by.x = c("from_geoid", "to_geoid"),
                     by.y = c("cntysub_geocorr", "place_geocorr"),
                     all = TRUE)

  ## `result` has a record for every entry in `geocorr_cw_ct20_cntysub_to_places`,
  ## except for countysub geoid "0900949950" to an unallocated place, which the
  ## Missouri geocorr has a small weight 
  expect_true(test_data[is.na(afact), from_geoid == "0900949950" & afact_geocorr < 0.01])

  ## `result` has undefined county subdivisions as they are in the shape file,
  ## but these are not in the Missouri geocorr
  expect_true(
    test_data[
      is.na(afact_geocorr) & substr(from_geoid, 6, 10) == "00000",
      from_geoid
    ] %chin%
      ct_cntysub20[
        substr(from_geoid, 6, 10) == "00000", from_geoid
      ] |>
      all()
  )

  ## Check that afact is similar between create_cw_worker() and the Missouri geocorr
  expect_true(test_data[!is.na(afact_geocorr) & !is.na(afact),
                        abs(afact - afact_geocorr) < 0.01] |> all())

  expect_true(test_data[!is.na(afact_geocorr) & !is.na(afact),
                        cor(afact, afact_geocorr) > 0.999])

})

test_that("create_cw_worker() has similar weights to Missouri geocorr for 2020 CT zctas to unified school districts", {

  ct_zip20 <- tigris::zctas(starts_with = "06", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(ZCTA5CE20, geometry)] |>
    setnames("ZCTA5CE20", "from_geoid")

  ct_unified20 <- tigris::school_districts(
    state = "CT", type = "unified", year = 2020
  ) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "to_geoid")

  ct_blocks20 <- f_get_ct20_shp("blocks") |>
    as.data.table() |>
    setnames("GEOID", "wts_geoid") |>
    setnames("hh2020", "wt_var")

  result <- create_cw_worker(dt_from = ct_zip20,
                             dt_to = ct_unified20,
                             dt_wts = ct_blocks20)


  geocorr_cw_ct20_zip_to_unified <- 
    f_get_geocorr_test_data("ct20_cw_zcta_to_unified_schl_district.csv") |>
    _[-1] |>
    _[, .(zcta_geocorr = zcta, unified_geocorr = paste0(state, sduni20),
          wt_var_geocorr = as.integer(hus20), afact_geocorr = as.numeric(afact))]

  test_data <- merge(result[afact > 0.000001], geocorr_cw_ct20_zip_to_unified,
                     by.x = c("from_geoid", "to_geoid"),
                     by.y = c("zcta_geocorr", "unified_geocorr"),
                     all = TRUE)

  ## When afact is missing, test that the only difference between the result
  ## and the Missouri geocorr is either unallocated unified school districts
  ## or when zcta == "06712" and unified == "0902640". The Missouri geocorr
  ## has a small weight for this intersection, but the shapefiles do not intersect
  ## (see the below plot).
  
  expect_true(
    test_data[is.na(afact) & to_geoid != "0902640", to_geoid == "0999999"] |>
      all()
  )
  expect_true(
    test_data[
      is.na(afact) & to_geoid != "0999999",
      from_geoid == "06712" & to_geoid == "0902640"
    ]
  )

  ## ggplot() +
  ##   geom_sf(data = ct_unified20[to_geoid == "0902640", geometry],
  ##           color = "blue", fill = NA) +
  ##   geom_sf(data = ct_zip20[from_geoid == "06712", geometry], color = "red", fill = NA)

  ## Check that when afact_geocorr is missing but afact is not missing that
  ## the school district (from_geoid) is undefined
  expect_true(
    test_data[is.na(afact_geocorr), is.na(to_geoid)] |>
      all()
  )

  ## Check that afact is similar between create_cw_worker() and the Missouri geocorr
  ## Except when from_geoid == "06712" and to_geoid == "0903538",
  ## from_geoid == "06770" and to_geoid == "0902640",
  ## and from_geoid == "06770" and to_geoid == "0903538"
  ## - These are just minor exceptions and related to the shapefiles not intersecting
  ##   as documented above
  expect_true(
    test_data[!is.na(afact_geocorr) & !is.na(afact) 
              & !(from_geoid == "06712" & to_geoid == "0903538") 
              & !(from_geoid == "06770" & to_geoid == "0902640") 
              & !(from_geoid == "06770" & to_geoid == "0903538"),
              abs(afact - afact_geocorr) < 0.01] |> all()
  )

  expect_true(
    test_data[!is.na(afact_geocorr) & !is.na(afact), 
              cor(afact, afact_geocorr) > 0.999]
  )

})

test_that("create_cw_worker() has similar weights to Missouri geocorr for 2020 CT  unified school districts to zctas", {

  
  ct_unified20 <- tigris::school_districts(
    state = "CT", type = "unified", year = 2020
  ) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "from_geoid")

  ct_zip20 <- tigris::zctas(starts_with = "06", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(ZCTA5CE20, geometry)] |>
    setnames("ZCTA5CE20", "to_geoid")


  ct_blocks20 <- f_get_ct20_shp("blocks") |>
    as.data.table() |>
    setnames("GEOID", "wts_geoid") |>
    setnames("hh2020", "wt_var")

  result <- create_cw_worker(dt_from = ct_unified20, 
                             dt_to = ct_zip20,
                             dt_wts = ct_blocks20)

  geocorr_cw_ct20_unified_to_zip <-
    f_get_geocorr_test_data("ct20_cw_unified_schl_district_to_zcta.csv") |>
    _[-1] |>
    _[, .(unified_geocorr = paste0(state, sduni20),
          zcta_geocorr = zcta, wt_var_geocorr = as.integer(hus20),
          afact_geocorr = as.numeric(afact))]

  test_data <- merge(result[afact > 0.000001], geocorr_cw_ct20_unified_to_zip,
                     by.x = c("from_geoid", "to_geoid"),
                     by.y = c("unified_geocorr", "zcta_geocorr"),
                     all = TRUE)

  ## Check that when afact is missing, the only difference between the result
  ## and the Missouri geocorr is the from_geoid = "0902640" and to_geoid = "06712"
  ## see the plot below 
  expect_true(
    test_data[is.na(afact) & !(from_geoid == "0902640" & to_geoid == "06712"),
              from_geoid == "0999999"] |> all()
  )

  ## ggplot() + geom_sf(data = ct_unified20[from_geoid == "0902640", geometry],
  ##                    color = "blue", fill = NA) +
  ##   geom_sf(data = ct_zip20[to_geoid == "06712", geometry], color = "red", fill = NA)

  ## Check that when afact_geocorr is missing but afact is not missing that
  ## the school district (from_geoid) is undefined ("0999997"). See the below plot.
  expect_true(
    test_data[is.na(afact_geocorr), from_geoid == "0999997"] |>
      all()
  )
  
  ## ggplot() + geom_sf(data = ct_unified20[from_geoid == "0999997", geometry],
  ##                    color = "blue", fill = NA) +
  ##   geom_sf(data = ct_zip20[to_geoid %chin% c("06437", "06443", "06460", "06512",
  ##                                             "06516", "06519"),
  ##                           geometry], color = "red", fill = NA)

  ## Check that afact is similar between create_cw_worker() and the Missouri geocorr
  ## Except when from_geoid == "0902640" and to_geoid == "06770"
  ## and from_geoid == "0903538" and to_geoid == "06712"
  ## and from_geoid == "0903538" and to_geoid == "06770"
  ## - These are just minor exceptions and related to the shapefiles differing from the
  ##   Missouri Geocorr as documented above. 
  expect_true(
    test_data[!is.na(afact_geocorr) & !is.na(afact)
              & !(from_geoid == "0902640" & to_geoid == "06770")
              & !(from_geoid == "0903538" & to_geoid == "06712")
              & !(from_geoid == "0903538" & to_geoid == "06770"),
              abs(afact - afact_geocorr) < 0.01] |> all()
  )

  expect_true(
    test_data[!is.na(afact_geocorr) & !is.na(afact), 
              cor(afact, afact_geocorr) > 0.999]
  )

})

test_that("create_cw_worker() has similar weights to Missouri geocorr for 2020 CT tracts to counties, weighting by tracts", {

  ct_tracts20 <- tigris::tracts(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    st_cast(to = "MULTIPOLYGON") |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "from_geoid")

  ct_cntys20 <- tigris::counties(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "to_geoid")

  ct_tracts20_wts <- f_get_ct20_shp("tracts") |>
    as.data.table() |>
    setnames("GEOID", "wts_geoid") |>
    setnames("hh2020", "wt_var")

  result <- create_cw_worker(dt_from = ct_tracts20,
                             dt_to = ct_cntys20,
                             dt_wts = ct_tracts20_wts)

  geocorr_cw_ct20_tracts_to_cntys <-
    f_get_geocorr_test_data("ct20_cw_tract_to_cnty.csv") |>
    _[-1] |>
    _[, .(tract_geocorr = paste0(county, tract),
          cnty_geocorr = county,
          wt_var_geocorr = as.integer(hus20), afact_geocorr = as.numeric(afact))] |>
    _[, tract_geocorr := gsub("\\.", "", tract_geocorr)]

  test_data <- merge(result[afact > 0.000001], geocorr_cw_ct20_tracts_to_cntys,
                     by.x = c("from_geoid", "to_geoid"),
                     by.y = c("tract_geocorr", "cnty_geocorr"),
                     all = TRUE)

  ## Check that the results from create_cw_worker() and the Missouri geocorr
  ## are similar
  expect_true(test_data[!is.na(afact_geocorr) & !is.na(afact),
                        abs(afact - afact_geocorr) < 0.01] |> all())
  
})

test_that("create_cw_worker() has similar weights to Missouri geocorr for 2020 CT tracts to ztcas, weighting by tracts", {
  
  ct_zip20 <- tigris::zctas(starts_with = "06", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(ZCTA5CE20, geometry)] |>
    setnames("ZCTA5CE20", "to_geoid")

  ct_tracts20 <- f_get_ct20_shp("tracts") |>
    as.data.table() |>
    setnames("GEOID", "from_geoid") |>
    setnames("hh2020", "wt_var")

  result <- create_cw_worker(
    dt_from = ct_tracts20,
    dt_to = ct_zip20,
    dt_wts = ct_tracts20[, .(wts_geoid = from_geoid, wt_var = wt_var, geometry)]
  )

  geocorr_cw_ct20_blocks_to_zcta <-
    f_get_geocorr_test_data("ct20_cw_tract_to_zcta.csv") |>
    _[-1] |>
    _[, .(tract_geocorr = paste0(county, tract), zcta_geocorr = zcta,
          wt_var_geocorr = as.integer(hus20), afact_geocorr = as.numeric(afact))] |>
    _[, tract_geocorr := gsub("\\.", "", tract_geocorr)]

  test_data <- merge(result[afact > 0.000001], geocorr_cw_ct20_blocks_to_zcta,
                     by.x = c("from_geoid", "to_geoid"),
                     by.y = c("tract_geocorr", "zcta_geocorr"),
                     all = TRUE)

  ## There are cases where result has a record but the Missouri geocorr does not.
  ## In these cases, the shapefiles intersect but there is no weight in the Missouri
  ## geocorr. See the below plot.
  
  ## ggplot() + geom_sf(data = ct_tracts20[from_geoid == "09001073800", geometry],
  ##                    color = "blue", fill = NA) +
  ##   geom_sf(data = ct_zip20[to_geoid == "06610", geometry], color = "red", fill = NA)

  expect_true(test_data[is.na(afact_geocorr), .N] > 0)

  ## Check that the result is not missing any records that are in the Missouri geocorr
  expect_true(test_data[is.na(afact), .N] == 0)

  ## Check that the results from create_cw_worker() and the Missouri geocorr
  ## are similar
  expect_true(test_data[!is.na(afact_geocorr) & !is.na(afact),
                        cor(afact, afact_geocorr) > 0.95])

})
