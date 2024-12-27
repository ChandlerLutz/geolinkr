library(sf); library(data.table); library(tigris); library(tidycensus); library(ggplot2);

options(tigris_use_cache = TRUE)

if (!is_checking() || !is_testing())
  source(here::here("tests/testthat/helpers.R"))


test_that("create_cw() matches for create_cw_worker() for 2020 CT zcta to county subdivisions", {

  ct_zip20 <- tigris::zctas(starts_with = "06", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(ZCTA5CE20, geometry)] |>
    setnames("ZCTA5CE20", "geoid") |>
    sf::st_as_sf()

  ct_cnty_sub20 <- tigris::county_subdivisions(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "geoid") |>
    sf::st_as_sf()

  ct_blocks20 <- f_get_ct20_shp("blocks") |>
    as.data.table() |>
    setnames("GEOID", "geoid") |>
    sf::st_as_sf()

  # Note: The CT20 blocks don't cover the entire state and the CT20 zips.
  ## ggplot() +
  ##   geom_sf(data = st_union(ct_blocks20$geometry), color = "blue", fill = NA) +
  ##   geom_sf(data = st_union(ct_zip20$geometry), color = "red", fill = NA)

  result_worker <- create_cw_worker(
    dt_from = ct_zip20 |> as.data.table() |> setnames("geoid", "from_geoid"),
    dt_to = ct_cnty_sub20 |> as.data.table() |> setnames("geoid", "to_geoid"),
    dt_wts = ct_blocks20 |> as.data.table() |> setnames("geoid", "wts_geoid") |>
      setnames("hh2020", "wt_var")
  ) |>
    setnames("wt_var", "hh2020")
    

  result <- create_cw(
    from_sf = ct_zip20,
    to_sf = ct_cnty_sub20,
    wts_sf = ct_blocks20,
    wt_var_name = "hh2020",
    check_that_wts_cover_from_and_to = FALSE
  )

  expect_equal(result, result_worker)
  
})

test_that("create_cw() matches for create_cw_worker() for 2020 CT county subdivisions to zcta", {
  
  ct_cnty_sub20 <- tigris::county_subdivisions(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "geoid") |>
    sf::st_as_sf()

  ct_zip20 <- tigris::zctas(starts_with = "06", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(ZCTA5CE20, geometry)] |>
    setnames("ZCTA5CE20", "geoid") |>
    sf::st_as_sf()

  ct_blocks20 <- f_get_ct20_shp("blocks") |>
    as.data.table() |>
    setnames("GEOID", "geoid") |>
    sf::st_as_sf()

  ## st_covered_by(st_union(ct_cnty_sub20), st_union(ct_blocks20), sparse = FALSE)
  ## st_equals(st_union(ct_cnty_sub20), st_union(ct_blocks20), sparse = FALSE)

  # Note: The CT20 blocks don't cover the entire state and the CT20 zips.
  ## ggplot() +
  ##   geom_sf(data = st_union(ct_blocks20$geometry), color = "blue", fill = NA) +
  ##   geom_sf(data = st_union(ct_cnty_sub20$geometry), color = "red", fill = NA)


  result_worker <- create_cw_worker(
    dt_from = ct_cnty_sub20 |> as.data.table() |> setnames("geoid", "from_geoid"),
    dt_to = ct_zip20 |> as.data.table() |> setnames("geoid", "to_geoid"),
    dt_wts = ct_blocks20 |> as.data.table() |> setnames("geoid", "wts_geoid") |>
      setnames("hh2020", "wt_var")
  ) |>
    setnames("wt_var", "hh2020")

  result <- create_cw(
    from_sf = ct_cnty_sub20,
    to_sf = ct_zip20,
    wts_sf = ct_blocks20,
    wt_var_name = "hh2020",
    check_that_wts_cover_from_and_to = FALSE
  )

  expect_equal(result, result_worker)
  
})

test_that("create_cw() matches create_cw_worker() for 2020 AK,CT,HI zcta to county subdivisions", {

  ct_zip20 <- tigris::zctas(starts_with = "06", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(ZCTA5CE20, geometry)] |>
    setnames("ZCTA5CE20", "geoid")

  ct_cnty_sub20 <- tigris::county_subdivisions(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "geoid")

  ct_blocks20 <- f_get_ct20_shp("blocks") |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    setnames("GEOID", "geoid")
    

  ak_zip20 <- tigris::zctas(starts_with = "99", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(ZCTA5CE20, geometry)] |>
    setnames("ZCTA5CE20", "geoid")

  ak_cnty_sub20 <- tigris::county_subdivisions(state = "AK", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "geoid")

  ak_blocks20 <- f_get_ak20_shp("blocks") |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    setnames("GEOID", "geoid")

  hi_zip20 <- tigris::zctas(starts_with = "96", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(ZCTA5CE20, geometry)] |>
    setnames("ZCTA5CE20", "geoid")

  hi_cnty_sub20 <- tigris::county_subdivisions(state = "HI", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "geoid")

  hi_blocks20 <- f_get_hi20_shp("blocks") |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    setnames("GEOID", "geoid")


  result_worker_ct <- create_cw_worker(
    dt_from = ct_zip20 |> copy() |> setnames("geoid", "from_geoid"),
    dt_to = ct_cnty_sub20 |> copy() |> setnames("geoid", "to_geoid"),
    dt_wts = ct_blocks20 |> copy() |> setnames("geoid", "wts_geoid") |>
      setnames("hh2020", "wt_var")
  )

  result_worker_ak <- create_cw_worker(
    dt_from = ak_zip20 |> copy() |>
      _[, geometry := st_transform(geometry, crs = 3338)] |> 
      setnames("geoid", "from_geoid"),
    dt_to = ak_cnty_sub20 |> copy() |>
      _[, geometry := st_transform(geometry, crs = 3338)] |>
      setnames("geoid", "to_geoid"),
    dt_wts = ak_blocks20 |> copy() |> 
      _[, geometry := st_transform(geometry, crs = 3338)] |>
      setnames("geoid", "wts_geoid") |> setnames("hh2020", "wt_var")
  )

  result_worker_hi <- create_cw_worker(
    dt_from = hi_zip20 |> copy() |>
      _[, geometry := st_transform(geometry, crs = 3563)] |> 
      setnames("geoid", "from_geoid"),
    dt_to = hi_cnty_sub20 |> copy() |>
      _[, geometry := st_transform(geometry, crs = 3563)] |> 
      setnames("geoid", "to_geoid"),
    dt_wts = hi_blocks20 |> copy() |>
      _[, geometry := st_transform(geometry, crs = 3563)] |> 
      setnames("geoid", "wts_geoid") |> setnames("hh2020", "wt_var")
  )

  result_worker <- rbind(result_worker_ct, result_worker_ak, result_worker_hi) |>
    setnames("wt_var", "hh2020") |>
    setkeyv(c("from_geoid", "to_geoid")) |>
    _[order(from_geoid, to_geoid)]

  result <- create_cw(
    from_sf = rbind(ct_zip20, ak_zip20, hi_zip20) |> st_as_sf(),
    to_sf = rbind(ct_cnty_sub20, ak_cnty_sub20, hi_cnty_sub20) |> st_as_sf(),
    wts_sf = rbind(ct_blocks20, ak_blocks20, hi_blocks20) |> st_as_sf(),
    wt_var_name = "hh2020",
    check_that_wts_cover_from_and_to = FALSE
  )

  expect_equal(result, result_worker)
  
})
