library(sf); library(data.table); library(tigris); library(tidycensus); library(ggplot2);

options(tigris_use_cache = TRUE)

if (!is_testing() && !is_checking())
  source(here::here("tests/testthat/helpers.R"))

test_that("create_cw_worker() works with no overlap between from_sf and to_sf but with overlap between from_sf and wts_sf", {
  
  from_sf <- st_sf(
    from_geoid = paste0("from_", c("a", "b", "c")),  
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 3, side_length = 2, squares_per_row = 1, crs = 5070,
      start_at_xy = c(0, 0)
    )
  )
  to_sf <- st_sf(
    to_geoid = paste0("to_", c("d", "e", "f")),  
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 3, side_length = 2, squares_per_row = 1,
      crs = 5070, 
      start_at_xy = c(3, 0)
    ) 
  )

  wts_sf <- st_sf(
    wts_geoid = paste0("w", 1:40),  
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 40, side_length = 1, squares_per_row = 6, crs = 5070, 
      start_at_xy = c(0, 0)
    ),
    wt_var = 1  
  )

  ## ggplot() +
  ##   geom_sf(data = from_sf, aes(fill = from_geoid), alpha = 0.4) + 
  ##   geom_sf(data = to_sf, aes(fill = to_geoid), alpha = 0.1) + 
  ##   geom_sf(data = wts_sf, color = "green", fill = NA) +
  ##   geom_sf_text(data = wts_sf, aes(label = wts_geoid), color = "black")

  dt_from <- data.table(from_sf)
  dt_to <- data.table(to_sf)
  dt_wts <- data.table(wts_sf)

  result <- create_cw_worker(dt_from, dt_to, dt_wts)

  # Expected output (no overlap, so no crosswalk entries)
  expected_result <- data.table(
    from_geoid = dt_from$from_geoid,
    to_geoid = NA_character_,
    wt_var = 4,  # 4 wts per from_geoid
    afact = 1
  ) |>
    setkeyv(c("from_geoid", "to_geoid"))

  expect_equal(result, expected_result)
})

test_that("create_cw_worker() works when dt_to covers dt_from", {

  from_sf <- st_sf(
    from_geoid = paste0("from_", c("a", "b", "c", "d")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 4, side_length = 2, squares_per_row = 4
    ),
    crs = 5070
  )
  to_sf <- st_sf(
    to_geoid = paste0("to_", c("a", "b")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 2, side_length = 4,  # Larger side length
      squares_per_row = 2
    ),
    crs = 5070
  )
  wts_sf <- st_sf(
    wts_geoid = paste0("w", 1:32), 
    wt_var = 1:32, 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 32, side_length = 1,  
      squares_per_row = 8, start_at_xy = c(0, 0)
    ),
    crs = 5070
  )

  ## ggplot() + 
  ##   geom_sf(data = from_sf, aes(fill = from_geoid)) +
  ##   geom_sf(data = to_sf, aes(fill = to_geoid), alpha = 0.1) + 
  ##   geom_sf(data = wts_sf, color = "green", fill = NA) 

  dt_from <- data.table(from_sf)
  dt_to <- data.table(to_sf)
  dt_wts <- data.table(wts_sf)

  result <- create_cw_worker(dt_from, dt_to, dt_wts)

  expect_equal(sort(result$from_geoid), sort(dt_from$from_geoid))
  expect_true(all(result$afact == 1))
  expect_equal(result[from_geoid == "from_a", to_geoid], "to_a")
  expect_equal(result[from_geoid == "from_b", to_geoid], "to_a")
  expect_equal(result[from_geoid == "from_c", to_geoid], "to_b")
  expect_equal(result[from_geoid == "from_d", to_geoid], "to_b")
  # The wt var should be NA, since it is not used and it would be best
  # for the user to determine the wt from `dt_from` rather than slicing
  # `dt_wts` to get the wt_var
  expect_true(all(is.na(result$wt_var)))

  expected_result <- data.table(
    from_geoid = c("from_a", "from_b", "from_c", "from_d"),
    to_geoid = c("to_a", "to_a", "to_b", "to_b"),
    wt_var = as.numeric(NA), 
    afact = c(1, 1, 1, 1)
  )

  expect_equal(result, expected_result)
    
})


test_that("create_cw_worker() works when from_sf covers to_sf but to_sf does not fill all of the space in from_sf", {

  from_sf <- st_sf(
    from_geoid = paste0("from_", c("a", "b")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 2, side_length = 4,  # Larger side length
      squares_per_row = 2
    ),
    crs = 5070
  )
  
  to_sf <- st_sf(
    to_geoid = paste0("to_", c("a", "b", "c", "d")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 4, side_length = 2, squares_per_row = 4
    ),
    crs = 5070
  )
  
  wts_sf <- st_sf(
    wts_geoid = paste0("w", 1:32), 
    wt_var = 1, 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 32, side_length = 1,  
      squares_per_row = 8, start_at_xy = c(0, 0)
    ),
    crs = 5070
  )

  ## ggplot() + 
  ##   geom_sf(data = from_sf, aes(fill = from_geoid), alpha = 0.1) +
  ##   geom_sf(data = to_sf, aes(fill = to_geoid)) + 
  ##   geom_sf(data = wts_sf, color = "green", fill = NA) +
  ##   geom_sf_text(data = wts_sf, aes(label = wts_geoid), color = "black")

  dt_from <- data.table(from_sf)
  dt_to <- data.table(to_sf)
  dt_wts <- data.table(wts_sf)

  result <- create_cw_worker(dt_from, dt_to, dt_wts) 

  expected_result <- rbind(
    data.table(
      from_geoid = "from_a",
      to_geoid = "to_a",
      wt_var = 4, 
      afact = 0.25
    ),
    data.table(
      from_geoid = "from_a",
      to_geoid = "to_b",
      wt_var = 4, 
      afact = 0.25
    ),
    data.table(
      from_geoid = "from_a",
      to_geoid = NA_character_, 
      wt_var = 8, 
      afact = 0.5
    ),
    data.table(
      from_geoid = "from_b",
      to_geoid = "to_c",
      wt_var = 4, 
      afact = 0.25
    ),
    data.table(
      from_geoid = "from_b",
      to_geoid = "to_d",
      wt_var = 4, 
      afact = 0.25
    ),
    data.table(
      from_geoid = "from_b",
      to_geoid = NA_character_, 
      wt_var = 8, 
      afact = 0.5
    )
  ) |>
    setkeyv(c("from_geoid", "to_geoid")) |>
    _[order(from_geoid, to_geoid)]
  
    
  expect_equal(result, expected_result)
    
})

test_that("create_cw_worker() works when there is a 1:1 mapping from `dt_from` to `dt_to` and a partial mapping from `dt_from` to `dt_to` but `dt_from` covers all wts", {

  from_sf <- st_sf(
    from_geoid = paste0("from_", c("a", "b", "c", "d")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 4, side_length = 4,  # Larger side length
      squares_per_row = 2
    ),
    crs = 5070
  )
  
  to_sf <- st_sf(
    to_geoid = paste0("to_", c("a", "b")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 2, side_length = 4, squares_per_row = 2
    ),
    crs = 5070
  )
  
  wts_sf <- st_sf(
    wts_geoid = paste0("w", 1:64), 
    wt_var = 1, 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 64, side_length = 1,  
      squares_per_row = 8, start_at_xy = c(0, 0)
    ),
    crs = 5070
  )

  # Make sure that the first 2 geoms in from_sf equal to 2 geoms in to_sf
  expect_equal(lengths(st_equals(from_sf, to_sf)),
               c(1, 1, 0, 0))

  ## ggplot() + 
  ##   geom_sf(data = from_sf, aes(fill = from_geoid), alpha = 0.1) +
  ##   geom_sf(data = to_sf, aes(fill = to_geoid)) + 
  ##   geom_sf(data = wts_sf, color = "green", fill = NA) +
  ##   geom_sf_text(data = wts_sf, aes(label = wts_geoid), color = "black")

  dt_from <- data.table(from_sf)
  dt_to <- data.table(to_sf)
  dt_wts <- data.table(wts_sf)
  
  result <- create_cw_worker(dt_from, dt_to, dt_wts)
  
  expected_result <- data.table(
    from_geoid = c("from_a", "from_b", "from_c", "from_d"),
    to_geoid = c("to_a", "to_b", NA_character_, NA_character_),
    wt_var = c(NA, NA, 16, 16),
    afact = 1
  ) |>
    setkeyv(c("from_geoid", "to_geoid")) |>
    _[order(from_geoid, to_geoid)]

  expect_equal(result, expected_result)

})

test_that("create_cw_worker() works when with partial intersection where all from_sf and to_sf cover all wts", {
  
  from_sf <- st_sf(
    from_geoid = paste0("from_", c("a", "b")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 2, side_length = 2,  
      squares_per_row = 2
    ),
    crs = 5070
  )
  
  to_sf <- st_sf(
    to_geoid = paste0("to_", c("a", "b")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 2, side_length = 2, squares_per_row = 2, start_at_xy = c(1, 1)
    ),
    crs = 5070
  )
  
  wts_sf <- rbind(
    st_sf(
      wts_geoid = paste0("w", 1:4), 
      wt_var = 1, 
      geometry = gen_nonoverlapping_square_polygons(
        num_squares = 4, side_length = 1,  
        squares_per_row = 4, start_at_xy = c(0, 0)
      ),
      crs = 5070
    ),
    st_sf(
      wts_geoid = paste0("w", 5:9), 
      wt_var = 1, 
      geometry = gen_nonoverlapping_square_polygons(
        num_squares = 5, side_length = 1,  
        squares_per_row = 5, start_at_xy = c(0, 1)
      ),
      crs = 5070
    ),
    st_sf(
      wts_geoid = paste0("w", 10:13), 
      wt_var = 1, 
      geometry = gen_nonoverlapping_square_polygons(
        num_squares = 4, side_length = 1,  
        squares_per_row = 4, start_at_xy = c(1, 2)
      ),
      crs = 5070
    )
  )

  ## ggplot() + 
  ##   geom_sf(data = from_sf, aes(fill = from_geoid), alpha = 0.25) +
  ##   geom_sf(data = to_sf, aes(fill = to_geoid), alpha = 0.25) + 
  ##   geom_sf(data = wts_sf, color = "green", fill = NA) +
  ##   geom_sf_text(data = wts_sf, aes(label = wts_geoid), color = "black")

  dt_from <- data.table(from_sf)
  dt_to <- data.table(to_sf)
  dt_wts <- data.table(wts_sf)

  result <- create_cw_worker(dt_from, dt_to, dt_wts)

  expected_result <- data.table(
    from_geoid = c("from_a", "from_a", "from_b", "from_b", "from_b"),
    to_geoid = c("to_a", NA_character_, "to_a", "to_b", NA_character_),
    wt_var = as.numeric(NA),
    afact = as.numeric(NA)
  ) |>
    _[from_geoid == "from_a" & to_geoid == "to_a",
      `:=`(wt_var = 1, afact = 0.25)] |>
    _[from_geoid == "from_a" & is.na(to_geoid),
      `:=`(wt_var = 3, afact = 0.75)] |>
    _[from_geoid == "from_b" & to_geoid == "to_a",
      `:=`(wt_var = 1, afact = 0.25)] |>
    _[from_geoid == "from_b" & to_geoid == "to_b",
      `:=`(wt_var = 1, afact = 0.25)] |>
    _[from_geoid == "from_b" & is.na(to_geoid),
      `:=`(wt_var = 2, afact = 0.5)] |>
    _[order(from_geoid, to_geoid)]

  expect_equal(result, expected_result)
  
})


test_that("create_cw_worker() works with partial intersection where wts expand outside of from_sf and to_sf", {
  
  from_sf <- st_sf(
    from_geoid = paste0("from_", c("a", "b")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 2, side_length = 2,  
      squares_per_row = 2
    ),
    crs = 5070
  )
  
  to_sf <- st_sf(
    to_geoid = paste0("to_", c("a", "b")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 2, side_length = 2, squares_per_row = 2, start_at_xy = c(1, 1)
    ),
    crs = 5070
  )
  
  wts_sf <- st_sf(
    wts_geoid = paste0("w", 1:32), 
    wt_var = 1, 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 32, side_length = 1,  
      squares_per_row = 8, start_at_xy = c(0, 0)
    ),
    crs = 5070
  )

  ## ggplot() + 
  ##   geom_sf(data = from_sf, aes(fill = from_geoid), alpha = 0.25) +
  ##   geom_sf(data = to_sf, aes(fill = to_geoid), alpha = 0.25) + 
  ##   geom_sf(data = wts_sf, color = "green", fill = NA) +
  ##   geom_sf_text(data = wts_sf, aes(label = wts_geoid), color = "black")

  dt_from <- data.table(from_sf)
  dt_to <- data.table(to_sf)
  dt_wts <- data.table(wts_sf)

  result <- create_cw_worker(dt_from, dt_to, dt_wts)

  expected_result <- data.table(
    from_geoid = c("from_a", "from_a", "from_b", "from_b", "from_b"),
    to_geoid = c("to_a", NA_character_, "to_a", "to_b", NA_character_),
    wt_var = as.numeric(NA),
    afact = as.numeric(NA)
  ) |>
    _[from_geoid == "from_a" & to_geoid == "to_a",
      `:=`(wt_var = 1, afact = 0.25)] |>
    _[from_geoid == "from_a" & is.na(to_geoid),
      `:=`(wt_var = 3, afact = 0.75)] |>
    _[from_geoid == "from_b" & to_geoid == "to_a",
      `:=`(wt_var = 1, afact = 0.25)] |>
    _[from_geoid == "from_b" & to_geoid == "to_b",
      `:=`(wt_var = 1, afact = 0.25)] |>
    _[from_geoid == "from_b" & is.na(to_geoid),
      `:=`(wt_var = 2, afact = 0.5)] |>
    _[order(from_geoid, to_geoid)]

  expect_equal(result, expected_result)

})



test_that("create_cw_worker() works when dt_to covers dt_from with a partial intersection with dt_wts", {

  from_sf <- st_sf(
    from_geoid = paste0("from_", c("a", "b", "c", "d")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 4, side_length = 2, squares_per_row = 4
    ),
    crs = 5070
  )
  to_sf <- st_sf(
    to_geoid = paste0("to_", c("a", "b")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 2, side_length = 4,  # Larger side length
      squares_per_row = 2
    ),
    crs = 5070
  )
  wts_sf <- st_sf(
    wts_geoid = paste0("w", 1:50), 
    wt_var = 1:50, 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 50, side_length = 1,  
      squares_per_row = 10, start_at_xy = c(-0.5, -0.5)
    ),
    crs = 5070
  )

  ## ggplot() + 
  ##   geom_sf(data = from_sf, aes(fill = from_geoid)) +
  ##   geom_sf(data = to_sf, aes(fill = to_geoid), alpha = 0.1) + 
  ##   geom_sf(data = wts_sf, color = "green", fill = NA) +
  ##   geom_sf_text(data = wts_sf, aes(label = wts_geoid), color = "black")

  dt_from <- data.table(from_sf)
  dt_to <- data.table(to_sf)
  dt_wts <- data.table(wts_sf)

  result <- create_cw_worker(dt_from, dt_to, dt_wts)

  expect_equal(sort(result$from_geoid), sort(dt_from$from_geoid))
  expect_true(all(result$afact == 1))
  expect_equal(result[from_geoid == "from_a", to_geoid], "to_a")
  expect_equal(result[from_geoid == "from_b", to_geoid], "to_a")
  expect_equal(result[from_geoid == "from_c", to_geoid], "to_b")
  expect_equal(result[from_geoid == "from_d", to_geoid], "to_b")
  # The wt var should be NA, since it is not used and it would be best
  # for the user to determine the wt from `dt_from` rather than slicing
  # `dt_wts` to get the wt_var
  expect_true(all(is.na(result$wt_var)))

  expected_result <- data.table(
    from_geoid = c("from_a", "from_b", "from_c", "from_d"),
    to_geoid = c("to_a", "to_a", "to_b", "to_b"),
    wt_var = as.numeric(NA), 
    afact = c(1, 1, 1, 1)
  )

  expect_equal(result, expected_result)
    
})


test_that("create_cw_worker() works when with partial intersection where from_sf and to_sf also have a partial intersection with the wts using equal wts", {
  
  from_sf <- st_sf(
    from_geoid = paste0("from_", c("a", "b")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 2, side_length = 4,  
      squares_per_row = 2
    ),
    crs = 5070
  )
  
  to_sf <- st_sf(
    to_geoid = paste0("to_", c("a", "b")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 2, side_length = 4, squares_per_row = 2, start_at_xy = c(1, 1)
    ),
    crs = 5070
  )
  
  wts_sf <- st_sf(
    wts_geoid = sprintf("w%02.f", 1:60), 
    wt_var = 1, 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 60, side_length = 1,  
      squares_per_row = 10, start_at_xy = c(-0.5, -0.5)
    ),
    crs = 5070
  )

  ## ggplot() + 
  ##   geom_sf(data = from_sf, aes(fill = from_geoid), alpha = 0.25) +
  ##   geom_sf(data = to_sf, aes(fill = to_geoid), alpha = 0.25) + 
  ##   geom_sf(data = wts_sf, color = "green", fill = NA) +
  ##   geom_sf_text(data = wts_sf, aes(label = wts_geoid), color = "black") 

  dt_from <- data.table(from_sf)
  dt_to <- data.table(to_sf)
  dt_wts <- data.table(wts_sf)

  result <- create_cw_worker(dt_from, dt_to, dt_wts)

  expected_result <- data.table(
    from_geoid = c("from_a", "from_a", "from_b", "from_b", "from_b"),
    to_geoid = c("to_a", NA_character_, "to_a", "to_b", NA_character_),
    wt_var = as.numeric(NA)
  ) |>
    # The is a 3 x 3 square
    _[from_geoid == "from_a" & to_geoid == "to_a",
      `:=`(wt_var = 3 * 3)] |>
    # The unallocated portion of from_a is an "L" shape
    _[from_geoid == "from_a" & is.na(to_geoid),
      `:=`(wt_var = 0.25 * 5 + 0.5 * 10 + 0.75)] |>
    # A 1 x 3 rectangle
    _[from_geoid == "from_b" & to_geoid == "to_a",
      `:=`(wt_var = 1 * 3)] |>
    # A 3 x 3 square
    _[from_geoid == "from_b" & to_geoid == "to_b",
      `:=`(wt_var = 3 * 3)] |>
    # The unallocated portion of from_b is a 1 x 4 rectangle
    _[from_geoid == "from_b" & is.na(to_geoid),
      `:=`(wt_var = 1 * 4)] |>
    _[, .(wt_var = sum(wt_var)), keyby = c("from_geoid", "to_geoid")] |>
    _[, afact := wt_var / sum(wt_var), by = from_geoid] |>
    setkeyv(c("from_geoid", "to_geoid")) |>
    _[order(from_geoid, to_geoid)]

  expect_equal(result, expected_result)

  
})


test_that("create_cw_worker() returns correct weights with partial intersection between from_sf and to_sf where there is also a partial intersection with the wts", {

  from_sf <- st_sf(
    from_geoid = paste0("from_", c("a", "b")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 2, side_length = 6,  
      squares_per_row = 2
    ),
    crs = 5070
  )
  
  to_sf <- st_sf(
    to_geoid = paste0("to_", c("a", "b")),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 2, side_length = 6, squares_per_row = 2, start_at_xy = c(2, 2)
    ),
    crs = 5070
  )

  num_wts <- 175
  wts_sf <- st_sf(
    wts_geoid = sprintf("w%03.f", seq_len(num_wts)), 
    wt_var = 1, 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = num_wts, side_length = 1,  
      squares_per_row = 17, start_at_xy = c(-1.5, -1.5)
    ),
    crs = 5070
  )

  ## ggplot() + 
  ##   geom_sf(data = from_sf, aes(fill = from_geoid), alpha = 0.25) +
  ##   geom_sf(data = to_sf, aes(fill = to_geoid), alpha = 0.25) + 
  ##   geom_sf(data = wts_sf, color = "green", fill = NA) +
  ##   geom_sf_text(data = wts_sf, aes(label = wts_geoid), color = "black",
  ##                size = 2) ## +
  ##   ## scale_y_continuous(breaks = 1:10) + scale_x_continuous(breaks = 1:20)

  dt_from <- data.table(from_sf)
  dt_to <- data.table(to_sf)
  dt_wts <- data.table(wts_sf)

  result <- create_cw_worker(dt_from, dt_to, dt_wts)
  
  # Note: each square in from and to is 6 x 6, so the total area is 36
  expected_result <- rbind(
    data.table(
      from_geoid = "from_a",
      to_geoid = "to_a",
      wt_var = 4 * 4, 
      afact = (4 * 4)/ 36
    ),
    data.table(
      from_geoid = "from_a",
      to_geoid = NA_character_,
      wt_var = 20, 
      afact = 20 / 36
    ),
    data.table(
      from_geoid = "from_b",
      to_geoid = "to_a",
      wt_var = 2 * 4, 
      afact = (2 * 4) / 36
    ),
    data.table(
      from_geoid = "from_b",
      to_geoid = "to_b",
      wt_var = 4 * 4, 
      afact = (4 * 4) / 36
    ),
    data.table(
      from_geoid = "from_b",
      to_geoid = NA_character_,
      wt_var = 2 * 6, 
      afact = (2 * 6) / 36
    )
  )

  expect_equal(result, expected_result)
  
})

test_that("create_cw_worker() works when dt_from, dt_to, and dt_wts polygons are all the same", {

  from_sf <- st_sf(
    from_geoid = paste0("from_", 1:4),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 4, side_length = 1, squares_per_row = 2
    ),
    crs = 5070
  )
  to_sf <- st_sf(
    to_geoid = paste0("to_", 1:4),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 4, side_length = 1, squares_per_row = 2
    ),
    crs = 5070
  )
  wts_sf <- st_sf(
    wts_geoid = paste0("wts_", 1:4),
    wt_var = 1, 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 4, side_length = 1, squares_per_row = 2
    ),
    crs = 5070
  )
  
  expect_equal(st_area(from_sf), st_area(to_sf))
  expect_equal(st_area(from_sf), st_area(wts_sf))

  result <- create_cw_worker(data.table(from_sf), data.table(to_sf), data.table(wts_sf))

  expected_result <- data.table(
    from_geoid = from_sf$from_geoid, 
    to_geoid = to_sf$to_geoid,
    wt_var = NA_real_,
    afact = 1
  )

  expect_equal(result, expected_result)

})

test_that("create_cw_worker() works when dt_from polygons are the same size as dt_wts polygons, but dt_to has smaller polygons", {
  
  from_sf <- st_sf(
    from_geoid = paste0("from_", 1:8),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 8, side_length = 1, squares_per_row = 4
    ),
    crs = 5070
  )

  to_sf <- st_sf(
    to_geoid = paste0("to_", sprintf("%02.f", 1:32)),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 32, side_length = 0.5, squares_per_row = 8
    ),
    crs = 5070
  )

  wts_sf <- st_sf(
    wts_geoid = paste0("wts_", 1:8),
    wt_var = 1, 
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 8, side_length = 1, squares_per_row = 4
    ),
    crs = 5070
  )

  expect_equal(st_area(from_sf), st_area(wts_sf))

  ## ggplot() + 
  ##   geom_sf(data = from_sf, color = "blue", fill = NA, linewidth = 3) +
  ##   geom_sf(data = wts_sf, color = "green", fill = NA, linewidth = 1.1) + 
  ##   geom_sf(data = to_sf, color = "red", fill = NA) +
  ##   geom_sf_text(data = from_sf, aes(label = from_geoid), color = "black") +
  ##   geom_sf_text(data = to_sf, aes(label = to_geoid), color = "red")
    

  dt_from <- data.table(from_sf)
  dt_to <- data.table(to_sf)
  dt_wts <- data.table(wts_sf)

  result <- create_cw_worker(dt_from, dt_to, dt_wts) |>
    _[order(from_geoid, to_geoid)]

  expected_result_sparse_idx <- sf::st_covered_by(to_sf, from_sf)

  expected_result <- lapply(seq_along(expected_result_sparse_idx), function(i) {
    data.table(from_geoid = from_sf$from_geoid[expected_result_sparse_idx[[i]]], 
               to_geoid = to_sf$to_geoid[i])
  }) |>
    rbindlist() |>
    _[, wt_var := 0.25] |>
    _[, afact := 0.25] |>
    setkey(from_geoid, to_geoid) |>
    _[order(from_geoid, to_geoid)]

  expect_equal(result, expected_result)

})


test_that("create_cw_worker() works for 2020 to 2022 CT counties, weighting by census tracts", {
  
  ct_tracts20 <- f_get_ct20_shp("tracts") |> 
    as.data.table() |>
    setnames("GEOID", "wts_geoid") |>
    setnames("hh2020", "wt_var")

  ct_cnty20 <- tigris::counties(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |> 
    _[, .(GEOID, geometry)] |> 
    setnames("GEOID", "from_geoid")
  
  ct_cnty22 <- tigris::counties(state = "CT", year = 2022) |>
    st_transform(crs = 5070) |>
    as.data.table() |> 
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "to_geoid")

  result <- create_cw_worker(dt_from = ct_cnty20,
                             dt_to = ct_cnty22,
                             dt_wts = ct_tracts20)

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


test_that("create_cw_worker() works for 2022 to 2020 CT counties, weighting by census tracts", {

  ct_tracts20 <- f_get_ct20_shp("tracts") %>%
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
                             dt_wts = ct_tracts20)

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

    ## For cnty20, check that result has an afact smaller than 0.01% for the counties
    ## not in the visual cw
    expect_true(all(result[from_geoid == c(cnty22)
                           & to_geoid %notin% visual_cw_to_cntyfps,
                           afact] < 0.0001),
                info = paste("cnty22:", cnty22))

  }
  
})

test_that("create_cw_worker() works for 2020 to 2022 CT counties, weighting by census block groups", {

  ct_blkgrp20 <- f_get_ct20_shp("blkgrp") %>%
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
                             dt_wts = ct_blkgrp20)

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

  }
  
})


test_that("create_cw_worker() works for 2022 to 2020 CT counties, weighting by census block groups", {

  ct_blkgrp20 <- f_get_ct20_shp("blkgrp") |>
    as.data.table() |>
    setnames("GEOID", "wts_geoid") |> 
    setnames("hh2020", "wt_var")

  ct_cnty20 <- tigris::counties(state = "CT", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |> 
    _[, .(GEOID, geometry)] |>
    setnames("GEOID", "to_geoid")

  ct_cnty22 <- tigris::counties(state = "CT", year = 2022) |>
    st_transform(crs = 5070) |> 
    as.data.table() |>
    _[, .(GEOID, geometry)] |> 
    setnames("GEOID", "from_geoid")

  result <- create_cw_worker(dt_from = ct_cnty22,
                             dt_to =, ct_cnty20, 
                             dt_wts = ct_blkgrp20)

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

test_that("create_cw_worker() works in real data when there is a 1:1 where from is fully covered", {

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

  ## ggplot() +
  ##   geom_sf(data = ct_zip20[from_geoid == "06905", geometry],
  ##           fill = NA, color = "blue") +
  ##   geom_sf(data = ct_cntysub20[to_geoid == "0900173070", geometry],
  ##           fill = NA, color = "red")

  result <- create_cw_worker(dt_from = ct_zip20[from_geoid == "06905"],
                             dt_to = ct_cntysub20[to_geoid == "0900173070"],
                             dt_wts = ct_blocks20)

  expect_equal(nrow(result), 1)
  expected_result <- data.table(
    from_geoid = "06905",
    to_geoid = "0900173070",
    wt_var = NA_real_,
    afact = 1
  )

  expect_equal(result, expected_result)

})


test_that("create_cw_worker() works in real data when there is a 1:1 where `from` is fully covered but there is another `from` polygon that is not", { 

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
    st_transform(crs = 5070) |>
    as.data.table() |> 
    setnames("GEOID", "wts_geoid") |> 
    setnames("hh2020", "wt_var")

  ## ggplot() +
  ##   geom_sf(data = ct_zip20[from_geoid %chin% c("06905", "06906"), geometry],
  ##           fill = NA, color = "blue") +
  ##   geom_sf(data = ct_cntysub20[to_geoid == "0900173070", geometry],
  ##           fill = NA, color = "red")
  
  result <- create_cw_worker(dt_from = ct_zip20[from_geoid %chin% c("06905", "06906")],
                             dt_to = ct_cntysub20[to_geoid == "0900173070"],
                             dt_wts = ct_blocks20)

  expected_result_for_06905 <- data.table(
    from_geoid = "06905",
    to_geoid = "0900173070",
    wt_var = NA_real_,
    afact = 1
  ) |>
    setkey(from_geoid, to_geoid)

  expect_equal(nrow(result[from_geoid == "06905"]), 1)
  ## For zip 06905, result2 should match expected_result_for_06905
  expect_equal(result[from_geoid == "06905"], expected_result_for_06905)

})


test_that("create_cw_worker() works in real data does not show afact when afact <= 1e-06", {

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

  ## ggplot() +
  ##   geom_sf(data = ct_zip20[from_geoid %chin% c("06905", "06906"), geometry],
  ##           fill = NA, color = "blue") +
  ##   geom_sf(data = ct_cntysub20[to_geoid == "0900173070", geometry],
  ##           fill = NA, color = "red")

  
  result <- create_cw_worker(dt_from = ct_zip20[from_geoid %chin% c("06905", "06906")],
                             dt_to = ct_cntysub20[to_geoid == "0900173070"],
                             dt_wts = ct_blocks20)

  ## Check that afact is not shown when afact <= 1e-06
  expect_true(all(result$afact > 1e-06))
  
})

test_that("create_cw_worker() has similar weights to Missouri geocorr for 2020 CT blocks to ztcas, weighting by blocks", {

  ct_blocks20 <- f_get_ct20_shp("blocks") |>
    as.data.table() |>
    setnames("GEOID", "from_geoid") |>
    setnames("hh2020", "wt_var") |>
    _[substr(from_geoid, 1, 5) == "09003"]

  ct_zip20 <- tigris::zctas(starts_with = "06", year = 2020) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(ZCTA5CE20, geometry)] |>
    setnames("ZCTA5CE20", "to_geoid") |>
    _[to_geoid %chin% c("06001", "06002")]

  ggplot() + geom_sf(data = ct_blocks20$geometry, color = "blue", fill = NA) +
    geom_sf(data = ct_zip20$geometry, color = "red", fill = NA)

  result <- create_cw_worker(
    dt_from = ct_blocks20,
    dt_to = ct_zip20,
    dt_wts = ct_blocks20[, .(wts_geoid = from_geoid, wt_var = wt_var, geometry)]
  )

  geocorr_cw_ct20_blocks_to_zcta <-
    f_get_geocorr_test_data("ct20_cw_block_to_zcta.csv") |>
    _[-1] |>
    _[, .(block_geocorr = paste0(county, tract, block), zcta_geocorr = zcta,
          wt_var_geocorr = as.integer(hus20), afact_geocorr = as.numeric(afact))] |>
    _[, block_geocorr := sub("\\.", "", block_geocorr)] |>
    _[block_geocorr %chin% ct_blocks20$from_geoid
      & zcta_geocorr %chin% ct_zip20$to_geoid]

  test_data <- merge(result[afact > 0.000001], geocorr_cw_ct20_blocks_to_zcta,
                     by.x = c("from_geoid", "to_geoid"),
                     by.y = c("block_geocorr", "zcta_geocorr"),
                     all = TRUE)

  ## Ensure that no result has no missing records in the test data
  expect_equal(nrow(test_data[is.na(afact)]), 0)

  ## Ensure that the afact equals afact_geocorr
  expect_true(test_data[!is.na(afact_geocorr), all(afact == afact_geocorr)])
                
})

test_that("create_cw_worker() works with a tract to zip example", {

  dt_from <- readRDS(
    ex_test_data_path("tract_to_zip_example_trctG23000909666_shp.rds")
  )
  dt_to <- readRDS(ex_test_data_path("tract_to_zip_example_zip04683_shp.rds"))
  dt_wts <- readRDS(ex_test_data_path("tract_to_zip_example_dt_wts.rds"))

  result <- create_cw_worker(dt_from, dt_to, dt_wts)

  expect_true("data.table" %chin% class(result))

  
})

test_that("create_cw_worker() works with a tract to zip example for zip 19108", {

  dt_from <- readRDS(ex_test_data_path("trct_to_zip_ex_dt_from_for_zip_19108.rds"))
  dt_to <- readRDS(ex_test_data_path("trct_to_zip_ex_dt_to_for_zip_19108.rds"))
  dt_wts <- readRDS(ex_test_data_path("trct_to_zip_ex_dt_wts_for_zip_19108.rds"))

  result <- create_cw_worker(dt_from, dt_to, dt_wts)

  expect_true("data.table" %chin% class(result))
  
  
})
