library(sf); library(data.table); library(ggplot2)

test_that("get_wts_in_from_not_overlapping_with_to() handles missing 'from_geoid' column in dt_from", {
  
  from_sf <- st_sf(
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 1, side_length = 2, squares_per_row = 1, crs = 5070
    )
  )  # No 'from_geoid' column

  to_sf <- st_sf(
    to_geoid = "b",
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 1, side_length = 2, squares_per_row = 1, crs = 5070
    )
  )
  wts_sf <- st_sf(
    wts_geoid = "c",
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 1, side_length = 1, squares_per_row = 1, crs = 5070
    ),
    wt_var = 10
  )

  from_dt <- data.table(from_sf)
  to_dt <- data.table(to_sf)
  dt_wts <- data.table(wts_sf)

  expect_error(get_wts_in_from_not_overlapping_with_to(from_dt, to_dt, dt_wts))
})

test_that("get_wts_in_from_not_overlapping_with_to() handles incorrect geometry type in dt_to", {

  from_sf <- st_sf(
    from_geoid = "a",
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 1, side_length = 2, squares_per_row = 1, crs = 5070
    )
  )
  to_sf <- st_sf(
    to_geoid = "b",
    geometry = sf::st_as_sfc("POINT (1 1)")  # Incorrect geometry type
  )
  wts_sf <- st_sf(
    wts_geoid = "c",
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 1, side_length = 1, squares_per_row = 1, crs = 5070
    ),
    wt_var = 10
  )

  from_dt <- data.table(from_sf)
  to_dt <- data.table(to_sf)
  dt_wts <- data.table(wts_sf)

  expect_error(get_wts_in_from_not_overlapping_with_to(from_dt, to_dt, dt_wts))
})


test_that("get_wts_in_from_not_overlapping_with_to() returns correct weights when weights are fully contained in both dt_from and dt_to", {

  from_sf <- st_sf(
    from_geoid = c("a", "b"),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 2, side_length = 4, squares_per_row = 2, crs = 5070
    )
  )
  to_sf <- st_sf(
    to_geoid = c("c", "d"),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 2, side_length = 4, squares_per_row = 2, crs = 5070
    )
  )
  wts_sf <- st_sf(
    wts_geoid = c("e", "f", "g"),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 3, side_length = 2, squares_per_row = 3, crs = 5070
    ),
    wt_var = c(10, 20, 30)
  )

  ## ggplot() + 
  ##   geom_sf(data = from_sf, aes(fill = from_geoid), alpha = 0.4) + 
  ##   geom_sf(data = to_sf, aes(fill = to_geoid), alpha = 0.1) + 
  ##   geom_sf(data = wts_sf, color = "green", fill = NA) +
  ##   geom_sf_text(data = to_sf, aes(label = to_geoid), color = "black") + 
  ##   geom_sf_text(data = wts_sf, aes(label = wts_geoid), color = "black")

  dt_from <- data.table(from_sf)
  dt_to <- data.table(to_sf)
  dt_wts <- data.table(wts_sf)

  result <- get_wts_in_from_not_overlapping_with_to(dt_from, dt_to, dt_wts)

  expected_result <- data.table(
    from_geoid = c("a", "a", "b"),
    to_geoid = c("c", "c", "d"),
    wts_geoid = c("e", "f", "g"),
    wt_var = c(10, 20, 30)
  ) |>
    setkey("wts_geoid")

  expect_equal(result, expected_result)
})

test_that("get_wts_in_from_not_overlapping_with_to() returns all wts covered by dt_from even if they are not covered by dt_to", {

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

  result <- get_wts_in_from_not_overlapping_with_to(dt_from, dt_to, dt_wts) |>
    _[order(from_geoid, to_geoid, wts_geoid)]

  expected_result <- rbind(
    data.table(
      from_geoid = "from_a",
      to_geoid = "to_a",
      wts_geoid = c("w1", "w2", "w9", "w10")
    ),
    data.table(
      from_geoid = "from_a",
      to_geoid = "to_b",
      wts_geoid = c("w3", "w4", "w11", "w12")
    ),
    data.table(
      from_geoid = "from_b",
      to_geoid = "to_c",
      wts_geoid = c("w5", "w6", "w13", "w14")
    ),
    data.table(
      from_geoid = "from_b",
      to_geoid = "to_d",
      wts_geoid = c("w7", "w8", "w15", "w16")
    ),
    data.table(
      from_geoid = "from_a",
      to_geoid = NA_character_,
      wts_geoid = paste0("w", c(17:20, 25:28))
    ),
    data.table(
      from_geoid = "from_b",
      to_geoid = NA_character_,
      wts_geoid = paste0("w", c(21:24, 29:32))
    )
  ) |>
    _[, wt_var := 1] |>
    _[order(from_geoid, to_geoid, wts_geoid)]

  expect_equal(result, expected_result)
  
})

test_that("get_wts_in_from_not_overlapping_to() works with no overlap between from_sf and to_sf but with overlap between from_sf and wts_sf", {
  
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

  result <- get_wts_in_from_not_overlapping_with_to(dt_from, dt_to, dt_wts) |>
    _[order(from_geoid, to_geoid, wts_geoid)]

  expected_result <- rbind(
    data.table(
      from_geoid = "from_a",
      to_geoid = NA_character_,
      wts_geoid = c("w1", "w2", "w7", "w8")
    ),
    data.table(
      from_geoid = "from_b",
      to_geoid = NA_character_,
      wts_geoid = c("w13", "w14", "w19", "w20")
    ),
    data.table(
      from_geoid = "from_c",
      to_geoid = NA_character_,
      wts_geoid = c("w25", "w26", "w31", "w32")
    )
  ) |>
    _[, wt_var := 1] |>
    _[order(from_geoid, to_geoid, wts_geoid)]

  expect_equal(result, expected_result)

})


test_that("get_wts_in_from_not_overlapping_with_to() returns correct weights with partial intersection between from_sf and to_sf where there is also a partial intersection with the wts", {

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
  ##                size = 2)

  dt_from <- data.table(from_sf)
  dt_to <- data.table(to_sf)
  dt_wts <- data.table(wts_sf)
  
  result <- get_wts_in_from_not_overlapping_with_to(dt_from, dt_to, dt_wts) |>
    _[order(from_geoid, to_geoid, wts_geoid)]

  dt_wts_covered_by_from <- st_covered_by(dt_wts$geometry, dt_from$geometry) |> 
    lengths() |> as.logical() %>% 
    dt_wts[.,.(wts_geoid, is_covered_by_from = 1L)]

  dt_wts_covered_by_to <- st_covered_by(dt_wts$geometry, dt_to$geometry) |> 
    lengths() |> as.logical() %>% 
    dt_wts[., .(wts_geoid, is_covered_by_to = 1L)]

  dt_wts_not_intersecting_with_to <- st_intersects(
    dt_wts$geometry, dt_to$geometry
  ) |> 
    lengths() |> as.logical() %>% sapply(., isFALSE) %>% 
    dt_wts[., .(wts_geoid, is_not_intersecting_with_to = 1L)]

  dt_wts_not_overlapping_with_to <- st_overlaps(
    dt_wts$geometry, dt_to$geometry
  ) |> 
    lengths() |> as.logical() %>% sapply(., isFALSE) %>% 
    dt_wts[., .(wts_geoid, is_not_overlapping_with_to = 1L)]

  # overlapping should mean not covering and not intersecting.
  # So not `wts` overlapping with `to` means that `to` covers `wts` or
  #   that `wts` are outside of the `to` space. 
  expect_equal(
    c(
      dt_wts_not_overlapping_with_to$wts_geoid,
      dt_wts_not_intersecting_with_to$wts_geoid
    ) |> unique() |> sort(),
    dt_wts_not_overlapping_with_to$wts_geoid |> sort()
  )

  expected_wts <- dt_wts_covered_by_from %>%
    .[wts_geoid %chin% dt_wts_not_overlapping_with_to$wts_geoid, wts_geoid] %>%
    sort()

  # The result has all of right wts_geoid
  expect_equal(sort(result$wts_geoid), sort(expected_wts))

  expected_result <- rbind(
    data.table(from_geoid = "from_a", to_geoid = "to_a",
               wts_geoid = c("w073", "w074", "w075", "w090", "w091", "w092",
                             "w107", "w108", "w109")),
    data.table(from_geoid = "from_a", to_geoid = NA_character_,
               wts_geoid = c("w037", "w038", "w039", "w040", "w041",
                             "w054", "w071", "w088", "w105")),
    data.table(from_geoid = "from_b", to_geoid = "to_a",
               wts_geoid = c("w077", "w094", "w111")),
    data.table(from_geoid = "from_b", to_geoid = "to_b",
               wts_geoid = c("w079", "w080", "w081", "w096", "w097", "w098",
                             "w113", "w114", "w115")),
    data.table(from_geoid = "from_b", to_geoid = NA_character_,
               wts_geoid = c("w043", "w044", "w045", "w046", "w047"))
  ) |>
    _[, wt_var := 1] |> 
    _[order(from_geoid, to_geoid, wts_geoid)]

  expect_equal(result, expected_result)
  
})

test_that("get_wts_in_from_not_overlapping_with_to() returns weights when dt_from polygons are the same size as dt_wts polygons, but dt_to has smaller polygons", {
  from_sf <- st_sf(
    from_geoid = paste0("from_", 1:8),
    geometry = gen_nonoverlapping_square_polygons(
      num_squares = 8, side_length = 1, squares_per_row = 4
    ),
    crs = 5070
  )

  to_sf <- st_sf(
    to_geoid = paste0("to_", 1:32),
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

  expect_equal(st_area(from_sf$geometry), st_area(wts_sf$geometry))

  ## ggplot() + 
  ##   geom_sf(data = from_sf, color = "blue", fill = NA, linewidth = 1.25) +
  ##   geom_sf(data = to_sf, color = "red", fill = NA)


  dt_from <- data.table(from_sf)
  dt_to <- data.table(to_sf)
  dt_wts <- data.table(wts_sf)

  result <- get_wts_in_from_not_overlapping_with_to(dt_from, dt_to, dt_wts) |>
    _[order(from_geoid, to_geoid, wts_geoid)]

  expected_result <- data.table(
    from_geoid = dt_from$from_geoid,
    to_geoid = NA_character_,
    wts_geoid = dt_wts$wts_geoid,
    wt_var = 1
  ) |>
    setkey(wts_geoid)

  expect_equal(result, expected_result)

})

