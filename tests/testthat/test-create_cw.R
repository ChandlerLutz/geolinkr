library(sf); library(data.table); library(ggplot2)

options(tigris_use_cache = TRUE)

if (!is_checking() || !is_testing())
  source(here::here("tests/testthat/helpers.R"))

test_that("create_cw() requires each sf object to be of type sf", {

  from_sf <- st_sf(
    geoid = c("a", "b", "c"),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 3, side_length = 2, squares_per_row = 1
    )
  )
  to_sf <- st_sf(
    geoid = c("d", "e", "f"),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 3, side_length = 2, squares_per_row = 1
    )
  )
  wts_sf <- st_sf(
    geoid = c("w1", "w2", "w3"),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 3, side_length = 2, squares_per_row = 1
    )
  )

  expect_error(create_cw(as.data.table(from_sf), to_sf, wts_sf, "wt_col"))
  expect_error(create_cw(from_sf, as.data.table(to_sf), wts_sf, "wt_col"))
  expect_error(create_cw(from_sf, to_sf, as.data.table(wts_sf), "wt_col"))
})

test_that("create_cw() requires a unique geoid for each sf input", {

  from_sf <- st_sf(
    geoid = c("a", "b", "c"),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 3, side_length = 2, squares_per_row = 1
    )
  )
  to_sf <- st_sf(
    geoid = c("d", "e", "f"),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 3, side_length = 2, squares_per_row = 1
    )
  )
  wts_sf <- st_sf(
    geoid = c("w1", "w2", "w3"),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 3, side_length = 2, squares_per_row = 1
    )
  )

  f_dup_geoid <- function(sf) {
    sf$geoid[2] <- sf$geoid[1]
    return(sf)
  }

  expect_true(any(duplicated(f_dup_geoid(from_sf)$geoid)))
  
  expect_error(create_cw(f_dup_geoid(from_sf), to_sf, wts_sf, "wt_col"))
  expect_error(create_cw(from_sf, f_dup_geoid(to_sf), wts_sf, "wt_col"))
  expect_error(create_cw(from_sf, to_sf, f_dup_geoid(wts_sf), "wt_col"))
})

test_that("create_cw() raises an error if `check_that_wts_cover_from_and_to == TRUE` and wts_sf does not cover from_sf", {
  
  wts_sf <- st_sf(
    geoid = sprintf("w%02.f", 1:8),
    wt_col = 1, 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 8, side_length = 1, squares_per_row = 4, crs = 5070
    )
  )

  ## wts_sf does not cover from_sf
  from_sf <- st_sf(
    geoid = sprintf("from_geoid%02.f", 1:10), 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 10, side_length = 1, squares_per_row = 4, crs = 5070
    )
  )
  to_sf <- st_sf(
    geoid = sprintf("to_geoid%02.f", 1:8),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 8, side_length = 1, squares_per_row = 4, crs = 5070
    )
  )

  ## ggplot() + geom_sf(data = from_sf, color = "blue", fill = NA, linewidth = 3) +
  ##   geom_sf(data = to_sf, color = "red", fill = NA, linewidth = 1.1) +
  ##   geom_sf(data = wts_sf, color = "black", fill = NA)
  
  expect_error(
    create_cw(from_sf = from_sf, to_sf = to_sf, wts_sf = wts_sf,
              wt_var_name = "wt_col",
              check_that_wts_cover_from_and_to = TRUE),
    class = "wts_not_covering_from_error"
  )

})

test_that("create_cw() raises an error if `check_that_wts_cover_from_and_to == TRUE` and wts_sf does not cover to_sf", {

  wts_sf <- st_sf(
    geoid = sprintf("w%02.f", 1:8),
    wt_col = 1, 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 8, side_length = 1, squares_per_row = 4, crs = 5070
    )
  )

  
  ## wts_sf does not cover to_sf
  from_sf <- st_sf(
    geoid = sprintf("from_geoid%02.f", 1:8), 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 8, side_length = 1, squares_per_row = 4, crs = 5070
    )
  )
  to_sf <- st_sf(
    geoid = sprintf("to_geoid%02.f", 1:10),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 10, side_length = 1, squares_per_row = 4, crs = 5070
    )
  )

  ## ggplot() + geom_sf(data = from_sf, color = "blue", fill = NA, linewidth = 3) +
  ##   geom_sf(data = to_sf, color = "red", fill = NA, linewidth = 1.1) +
  ##   geom_sf(data = wts_sf, color = "black", fill = NA)

  expect_error(
    create_cw(from_sf = from_sf, to_sf = to_sf, wts_sf = wts_sf,
              wt_var_name = "wt_col",
              check_that_wts_cover_from_and_to = TRUE),
    class = "wts_not_covering_to_error"
  )


})

test_that("create_cw() works with `check_that_wts_cover_from_and_to == TRUE` and wts_sf equal to `from_sf` and `to_sf`", {

  from_sf <- st_sf(
    geoid = sprintf("geoid%02.f", 1:8), 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 8, side_length = 1, squares_per_row = 4, crs = 5070
    )
  )

  to_sf <- from_sf
  
  wts_sf <- from_sf |>
    as.data.table() |>
    _[, wt_col := 1] |>
    st_as_sf()

  expect_true(st_equals(st_union(from_sf), st_union(to_sf), sparse = FALSE)[1, 1])
  
  result <- create_cw(from_sf = from_sf, to_sf = to_sf, wts_sf = wts_sf,
                      wt_var_name = "wt_col",
                      check_that_wts_cover_from_and_to = TRUE)
  
  expect_equal(result$from_geoid, from_sf$geoid)
  expect_equal(result$to_geoid, to_sf$geoid)
  expect_true(result[, all(afact == 1)])
    
})

test_that("create_cw() returns the expected results from create_worker() for a basic case", {
  
  from_sf <- st_sf(
    geoid = paste0("from_geoid", 1:8), 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 8, side_length = 2, squares_per_row = 2, crs = 5070
    )
  )
  to_sf <- st_sf(
    geoid = sprintf("to_geoid%02.f", 1:16),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 16, side_length = 1, squares_per_row = 4, crs = 5070
    )
  )
  wts_sf <- st_sf(
    geoid = sprintf("w%02.f", 1:128),
    wt_col = 1, 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 128, side_length = 0.5, squares_per_row = 8, crs = 5070
    )
  )

  ## ggplot() + geom_sf(data = from_sf, color = "blue", fill = NA, linewidth = 3) +
  ##   geom_sf(data = to_sf, color = "red", fill = NA, linewidth = 1.1) +
  ##   geom_sf(data = wts_sf, color = "black", fill = NA)

  dt_from <- as.data.table(from_sf) |>
    setnames("geoid", "from_geoid")
  dt_to <- as.data.table(to_sf) |>
    setnames("geoid", "to_geoid")
  dt_wts <- as.data.table(wts_sf) |>
    setnames("geoid", "wts_geoid") |>
    setnames("wt_col", "wt_var")
    
  result_worker <- create_cw_worker(dt_from, dt_to, dt_wts) %>%
    setnames("wt_var", "wt_col")

  result1 <- create_cw(from_sf = from_sf, to_sf = to_sf, wts_sf = wts_sf,
                       wt_var_name = "wt_col")
  result2 <- create_cw(from_sf = from_sf, to_sf = to_sf, wts_sf = wts_sf,
                       wt_var_name = "wt_col",
                       check_that_wts_cover_from_and_to = FALSE)
  result3 <- create_cw(from_sf = from_sf, to_sf = to_sf, wts_sf = wts_sf,
                       wt_var_name = "wt_col",
                       check_that_wts_cover_from_and_to = TRUE)
  result4 <- create_cw(from_sf = from_sf, to_sf = to_sf, wts_sf = wts_sf,
                       wt_var_name = "wt_col",
                       check_that_wts_cover_from_and_to = FALSE,
                       check_for_ak = FALSE)
  result5 <- create_cw(from_sf = from_sf, to_sf = to_sf, wts_sf = wts_sf,
                       wt_var_name = "wt_col",
                       check_that_wts_cover_from_and_to = FALSE,
                       check_for_hi = FALSE)
  result6 <- create_cw(from_sf = from_sf, to_sf = to_sf, wts_sf = wts_sf,
                       wt_var_name = "wt_col",
                       check_that_wts_cover_from_and_to = FALSE,
                       check_for_ak = FALSE, check_for_hi = FALSE)
  

  expect_equal(result1, result_worker)
  expect_equal(result2, result_worker)
  expect_equal(result3, result_worker)
  expect_equal(result4, result_worker)
  expect_equal(result5, result_worker)
  expect_equal(result6, result_worker)
  
})

test_that("create_cw() returns the expected results from create_worker() for AK", {

  from_sf <- tigris::county_subdivisions(state = "AK", year = 2020) |>
    as.data.table() |>
    _[, .(geoid = GEOID, geometry)] |>
    _[, geometry := st_transform(geometry, crs = 5070)] |>
    st_as_sf()

  to_sf <- tigris::counties(state = "AK", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(geoid = GEOID, geometry)] |>
    st_as_sf()

  print(getwd())
  wts_sf <- f_get_ak20_shp("tracts") |>
    setnames("GEOID", "geoid") |>
    st_as_sf()

  dt_from <- from_sf |> st_transform(crs = 3338) |> as.data.table() |>
    setnames("geoid", "from_geoid")
  dt_to <- to_sf |> st_transform(crs = 3338) |> as.data.table() |>
    setnames("geoid", "to_geoid")
  dt_wts <- wts_sf |> st_transform(crs = 3338) |> as.data.table() |>
    setnames("geoid", "wts_geoid") |>
    setnames("hh2020", "wt_var")

  result_worker <- create_cw_worker(
    dt_from = dt_from,
    dt_to = dt_to,
    dt_wts = dt_wts
  ) |>
    setnames("wt_var", "hh2020") |> 
    setkey(from_geoid, to_geoid) |>
    _[order(from_geoid, to_geoid)]

  result <- create_cw(from_sf = from_sf, to_sf = to_sf, wts_sf = wts_sf,
                      wt_var_name = "hh2020",
                      check_that_wts_cover_from_and_to = FALSE,
                      check_for_ak = TRUE, 
                      check_for_hi = FALSE)

  expect_equal(result, result_worker)
  
})

test_that("create_cw() returns the expected results from create_worker() for HI", {

  from_sf <- tigris::county_subdivisions(state = "HI", year = 2020) |>
    as.data.table() |>
    _[, .(geoid = GEOID, geometry)] |>
    _[, geometry := st_transform(geometry, crs = 5070)] |>
    st_as_sf()

  to_sf <- tigris::counties(state = "HI", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(geoid = GEOID, geometry)] |>
    st_as_sf()

  wts_sf <- f_get_hi20_shp("tracts") |>
    setnames("GEOID", "geoid") |>
    st_as_sf()

  dt_from <- from_sf |> st_transform(crs = 3563) |> as.data.table() |>
    setnames("geoid", "from_geoid")
  dt_to <- to_sf |> st_transform(crs = 3563) |> as.data.table() |>
    setnames("geoid", "to_geoid")
  dt_wts <- wts_sf |> st_transform(crs = 3563) |> as.data.table() |>
    setnames("geoid", "wts_geoid") |>
    setnames("hh2020", "wt_var")

  result_worker <- create_cw_worker(
    dt_from = dt_from,
    dt_to = dt_to,
    dt_wts = dt_wts
  ) |>
    setnames("wt_var", "hh2020") |>
    setkey(from_geoid, to_geoid) |>
    _[order(from_geoid, to_geoid)]

  result <- create_cw(from_sf = from_sf, to_sf = to_sf, wts_sf = wts_sf,
                      wt_var_name = "hh2020",
                      check_that_wts_cover_from_and_to = FALSE,
                      check_for_ak = FALSE,
                      check_for_hi = TRUE)

  expect_equal(result, result_worker)
  
})


test_that("create_cw() identifies AK vs CT", {

  from_sf <- rbind(
    tigris::county_subdivisions(state = "AK", year = 2020) |>
      as.data.table(),
    tigris::county_subdivisions(state = "CT", year = 2020) |>
      as.data.table()
  ) |> 
    _[, .(geoid = GEOID, geometry = st_transform(geometry, 5070))] |>
    st_as_sf() 

  to_sf <- rbind(
    tigris::counties(state = "AK", year = 2020) |>
      as.data.table(),
    tigris::counties(state = "CT", year = 2020) |>
      as.data.table()
  ) |>
    _[, .(geoid = GEOID, geometry = st_transform(geometry, 5070))] |>
    st_as_sf()
  
  wts_sf <- rbind(
    as.data.table(f_get_ak20_shp("tracts")), 
    as.data.table(f_get_ct20_shp("tracts"))
  ) |>
    _[, .(geoid = GEOID, hh2020, geometry = st_transform(geometry, 5070))] |>
    st_as_sf()

  result_worker_ak <- create_cw_worker(
    dt_from = from_sf |> st_transform(3338) |> as.data.table() |>
      _[substr(geoid, 1, 2) == "02"] |> 
      setnames("geoid", "from_geoid"),
    dt_to = to_sf |> st_transform(3338) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "02"] |>
      setnames("geoid", "to_geoid"),
    dt_wts = wts_sf |> st_transform(3338) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "02"] |>
      setnames("geoid", "wts_geoid") |>
      setnames("hh2020", "wt_var")
  ) |>
    setnames("wt_var", "hh2020") |>
    setkey(from_geoid, to_geoid) |>
    _[order(from_geoid, to_geoid)]

  result_worker_ct <- create_cw_worker(
    dt_from = from_sf |> st_transform(5070) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "09"] |> 
      setnames("geoid", "from_geoid"),
    dt_to = to_sf |> st_transform(5070) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "09"] |>
      setnames("geoid", "to_geoid"),
    dt_wts = wts_sf |> st_transform(5070) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "09"] |>
      setnames("geoid", "wts_geoid") |>
      setnames("hh2020", "wt_var")
  ) |>
    setnames("wt_var", "hh2020") |>
    setkey(from_geoid, to_geoid) |>
    _[order(from_geoid, to_geoid)]

  result_worker <- rbind(result_worker_ak, result_worker_ct) |>
    setkey(from_geoid, to_geoid) |>
    _[order(from_geoid, to_geoid)]

  result <- create_cw(from_sf = from_sf, to_sf = to_sf, wts_sf = wts_sf,
                      wt_var_name = "hh2020",
                      check_that_wts_cover_from_and_to = TRUE,
                      check_for_ak = TRUE,
                      check_for_hi = TRUE)

  expect_equal(result, result_worker)
  
})

test_that("create_cw() identifies CT vs HI", {
  
  from_sf <- rbind(
    tigris::county_subdivisions(state = "CT", year = 2020) |>
      as.data.table(),
    tigris::county_subdivisions(state = "HI", year = 2020) |>
      as.data.table()
  ) |> 
    _[, .(geoid = GEOID, geometry = st_transform(geometry, 5070))] |>
    st_as_sf() 

  to_sf <- rbind(
    tigris::counties(state = "CT", year = 2020) |>
      as.data.table(),
    tigris::counties(state = "HI", year = 2020) |>
      as.data.table()
  ) |>
    _[, .(geoid = GEOID, geometry = st_transform(geometry, 5070))] |>
    st_as_sf()
  
  wts_sf <- rbind(
    as.data.table(f_get_ct20_shp("tracts")), 
    as.data.table(f_get_hi20_shp("tracts"))
  ) |>
    _[, .(geoid = GEOID, hh2020, geometry = st_transform(geometry, 5070))] |>
    st_as_sf()

  result_worker_ct <- create_cw_worker(
    dt_from = from_sf |> st_transform(5070) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "09"] |> 
      setnames("geoid", "from_geoid"),
    dt_to = to_sf |> st_transform(5070) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "09"] |>
      setnames("geoid", "to_geoid"),
    dt_wts = wts_sf |> st_transform(5070) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "09"] |>
      setnames("geoid", "wts_geoid") |>
      setnames("hh2020", "wt_var")
  ) 

  result_worker_hi <- create_cw_worker(
    dt_from = from_sf |> st_transform(3563) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "15"] |> 
      setnames("geoid", "from_geoid"),
    dt_to = to_sf |> st_transform(3563) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "15"] |>
      setnames("geoid", "to_geoid"),
    dt_wts = wts_sf |> st_transform(3563) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "15"] |>
      setnames("geoid", "wts_geoid") |>
      setnames("hh2020", "wt_var")
  )

  result_worker <- rbind(result_worker_ct, result_worker_hi) |>
    setnames("wt_var", "hh2020") |>
    setkey(from_geoid, to_geoid) |>
    _[order(from_geoid, to_geoid)]

  result <- create_cw(from_sf = from_sf, to_sf = to_sf, wts_sf = wts_sf,
                      wt_var_name = "hh2020",
                      check_that_wts_cover_from_and_to = TRUE,
                      check_for_ak = TRUE,
                      check_for_hi = TRUE)

  expect_equal(result, result_worker)
  
})

test_that("create_cw() returns the expected results from create_worker() for CT + AK + HI", {

  from_sf <- rbind(
    tigris::county_subdivisions(state = "CT", year = 2020) |>
      as.data.table(),
    tigris::county_subdivisions(state = "AK", year = 2020) |>
      as.data.table(),
    tigris::county_subdivisions(state = "HI", year = 2020) |>
      as.data.table()
  ) |> 
    _[, .(geoid = GEOID, geometry = st_transform(geometry, 5070))] |>
    st_as_sf() 

  to_sf <- rbind(
    tigris::counties(state = "CT", year = 2020) |>
      as.data.table(),
    tigris::counties(state = "AK", year = 2020) |>
      as.data.table(),
    tigris::counties(state = "HI", year = 2020) |>
      as.data.table()
  ) |>
    _[, .(geoid = GEOID, geometry = st_transform(geometry, 5070))] |>
    st_as_sf()
  
  wts_sf <- rbind(
    as.data.table(f_get_ct20_shp("tracts")), 
    as.data.table(f_get_ak20_shp("tracts")),
    as.data.table(f_get_hi20_shp("tracts"))
  ) |>
    _[, .(geoid = GEOID, hh2020, geometry = st_transform(geometry, 5070))] |>
    st_as_sf()

  result_worker_ct <- create_cw_worker(
    dt_from = from_sf |> st_transform(5070) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "09"] |> 
      setnames("geoid", "from_geoid"),
    dt_to = to_sf |> st_transform(5070) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "09"] |>
      setnames("geoid", "to_geoid"),
    dt_wts = wts_sf |> st_transform(5070) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "09"] |>
      setnames("geoid", "wts_geoid") |>
      setnames("hh2020", "wt_var")
  )

  result_worker_ak <- create_cw_worker(
    dt_from = from_sf |> st_transform(3338) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "02"] |> 
      setnames("geoid", "from_geoid"),
    dt_to = to_sf |> st_transform(3338) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "02"] |>
      setnames("geoid", "to_geoid"),
    dt_wts = wts_sf |> st_transform(3338) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "02"] |>
      setnames("geoid", "wts_geoid") |>
      setnames("hh2020", "wt_var")
  )

  result_worker_hi <- create_cw_worker(
    dt_from = from_sf |> st_transform(3563) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "15"] |> 
      setnames("geoid", "from_geoid"),
    dt_to = to_sf |> st_transform(3563) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "15"] |>
      setnames("geoid", "to_geoid"),
    dt_wts = wts_sf |> st_transform(3563) |> as.data.table() |>
      _[substr(geoid, 1, 2)  == "15"] |>
      setnames("geoid", "wts_geoid") |>
      setnames("hh2020", "wt_var")
  )

  result_worker <- rbind(result_worker_ct, result_worker_ak, result_worker_hi) |>
    setnames("wt_var", "hh2020") |>
    setkey(from_geoid, to_geoid) |>
    _[order(from_geoid, to_geoid)]


  result <- create_cw(from_sf = from_sf, to_sf = to_sf, wts_sf = wts_sf,
                      wt_var_name = "hh2020",
                      check_that_wts_cover_from_and_to = TRUE,
                      check_for_ak = TRUE,
                      check_for_hi = TRUE)

  expect_equal(result, result_worker)

})
