library(sf)

test_that(
  "polygon_intersects_with_us_state() correctly identifies all states", {
    
    # Check if the polygon intersects with all states
    result <- polygon_intersects_with_us_state(geolinkr::us_states_sf$geometry)
    
    # Expect all states to be intersected
    expect_equal(result, rep(1L, 51))
  }
)

test_that(
  "polygon_intersects_with_us_state() returns 0L for puerto rico", {
      
    # Check if the polygon intersects with Puerto Rico
    result <- polygon_intersects_with_us_state(geolinkr::puerto_rico_sf$geometry)
      
    # Expect Puerto Rico to not be intersected
    expect_equal(result, 0L)
  }
)

test_that(
  "polygon_is_ak() correctly identifies Alaska out of all US states", {

    dt_us_states <- geolinkr::us_states_sf |>
      as.data.table() |>
      _[order(statefp)] |> 
      _[, is_ak := fifelse(stusps == "AK", 1L, 0L)]
    
    # Check if the polygon intersects with Alaska
    result <- polygon_is_ak(dt_us_states$geometry)
    
    # Expect Alaska to be intersected
    expect_equal(result, dt_us_states$is_ak)

  }
)

test_that(
  "polygon_is_hi() correctly identifies Hawaii out of all US states", {

    dt_us_states <- geolinkr::us_states_sf |>
      as.data.table() |>
      _[order(statefp)] |> 
      _[, is_hi := fifelse(stusps == "HI", 1L, 0L)]
    
    # Check if the polygon intersects with Hawaii
    result <- polygon_is_hi(dt_us_states$geometry)
    
    # Expect Hawaii to be intersected
    expect_equal(result, dt_us_states$is_hi)
  }
)



test_that("convert_goemcollection_to_multipolygon() handles polygons and points", {
  
  geom_collection <- sf::st_as_sfc(
    c("GEOMETRYCOLLECTION(POLYGON((0 0, 0 1, 1 1, 1 0, 0 0)), POINT(2 2))"),
    crs = 5070
  )
  result <- convert_goemcollection_to_multipolygon(geom_collection)
  expect_equal(as.character(sf::st_geometry_type(result)), "MULTIPOLYGON")
  expect_true(sf::st_is_valid(result))
  expect_true(!sf::st_is_empty(result))
  expect_equal(st_crs(result), st_crs(geom_collection))
  
})

# Test with a geometry collection containing only points
test_that("convert_goemcollection_to_multipolygon() handles only points", {
  
  geom_collection_points <- sf::st_as_sfc(
    c("GEOMETRYCOLLECTION(POINT(2 2), POINT(3 3))")
  )
  multipolygon_points <- convert_goemcollection_to_multipolygon(geom_collection_points)
  expect_equal(as.character(sf::st_geometry_type(multipolygon_points)), "MULTIPOLYGON")
  expect_true(sf::st_is_empty(multipolygon_points))
  
})

# Test with an empty geometry collection
test_that("convert_goemcollection_to_multipolygon() handles empty geometry collection", {
  
  geom_collection_empty <- sf::st_as_sfc("GEOMETRYCOLLECTION EMPTY", crs = 5070)
  result <- convert_goemcollection_to_multipolygon(geom_collection_empty)
  expect_equal(as.character(sf::st_geometry_type(result)), "MULTIPOLYGON")
  expect_true(sf::st_is_empty(result))
  expect_equal(st_crs(result), st_crs(geom_collection_empty))
  
})

# Test with invalid input (not a geometry collection)
test_that("convert_goemcollection_to_multipolygon() throws error for invalid input", {
  
  invalid_input <- sf::st_as_sfc("POINT(1 1)")
  expect_error(convert_goemcollection_to_multipolygon(invalid_input))
  
})

test_that("convert_geometrycollection_to_multipolygon() handles a malformed GEOMETRYCOLLECTION", {
  
  bad_geom_collection <- readRDS(
    test_path("internal-testdata", "malformed_geom_collection.rds")
  )
  result <- convert_goemcollection_to_multipolygon(bad_geom_collection)

  expect_true(sf::st_is_empty(result))
  expect_equal(as.character(sf::st_geometry_type(result)), "MULTIPOLYGON")
  expect_equal(st_crs(result), st_crs(bad_geom_collection))
  
})

test_that("gen_nonoverlapping_square_polygons() returns sfc (multipolygon) object", {
  
  squares <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 1, squares_per_row = 2
  )
  
  expect_true(all(class(squares) == c("sfc_MULTIPOLYGON", "sfc")))
})

test_that("gen_nonoverlapping_square_polygons() generates MULTIPOLYGONs", {
  
  squares <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 1, squares_per_row = 2
  )

  geom_type <- st_geometry_type(squares) |>
    as.character() |>
    unique()
  
  expect_equal(geom_type, "MULTIPOLYGON")
})



test_that("gen_nonoverlapping_square_polygons() generates correct number of squares", {
  
  # Test with different num_squares values
  squares_1 <- gen_nonoverlapping_square_polygons(
    num_squares = 5, side_length = 1, squares_per_row = 3
  )
  squares_2 <- gen_nonoverlapping_square_polygons(
    num_squares = 10, side_length = 2, squares_per_row = 2
  )
  squares_3 <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 2, squares_per_row = 2
  )
  squares_4 <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 2, squares_per_row = 1
  )

  expect_equal(length(squares_1), 5)
  expect_equal(length(squares_2), 10)
  expect_equal(length(squares_3), 2)
  expect_equal(length(squares_4), 2)
})

test_that("gen_nonoverlapping_square_polygons() generates squares with correct side length", {
  # Test with different side_length values
  squares_1 <- gen_nonoverlapping_square_polygons(
    num_squares = 4, side_length = 1, squares_per_row = 2
  )
  squares_2 <- gen_nonoverlapping_square_polygons(
    num_squares = 4, side_length = 2.5, squares_per_row = 2
  )

  # Check the distance between the first two x coordinate of the first square
  expect_equal(
    as.numeric(
      st_coordinates(squares_1)[2, c("X")] - st_coordinates(squares_1)[1, c("X")]
    ),
    1
  )
  expect_equal(
    as.numeric(st_coordinates(squares_2)[2, "X"] - st_coordinates(squares_2)[1, "X"]),
    2.5
  )

  # Check the distance between the first two y coordinate of the first square
  expect_equal(
    as.numeric(
      st_coordinates(squares_1)[3, "Y"] - st_coordinates(squares_1)[1, "Y"]
    ),
    1
  )
  expect_equal(
    as.numeric(st_coordinates(squares_2)[3, "Y"] - st_coordinates(squares_2)[1, "Y"]),
    2.5
  )
  
})

test_that("gen_nonoverlapping_square_polygons() arranges squares in correct rows", {
  
  # Test with different squares_per_row values
  squares_1 <- gen_nonoverlapping_square_polygons(
    num_squares = 6, side_length = 1, squares_per_row = 3
  )
  squares_2 <- gen_nonoverlapping_square_polygons(
    num_squares = 6, side_length = 1, squares_per_row = 2
  )

  # Check the y-coordinates of the squares
  expect_equal(as.numeric(st_coordinates(squares_1)[1, "Y"]), 0)
  # 4th square should be in the second row
  expect_equal(as.numeric(st_coordinates(squares_1)[4, "Y"]), 1)
  # 3rd square should be in the second row
  expect_equal(as.numeric(st_coordinates(squares_2)[3, "Y"]), 1)  
})

test_that("gen_nonoverlapping_square_polygons starts() at the correct coordinates", {
  
  # Test with different start_at_xy values
  squares_1 <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 1, squares_per_row = 2, start_at_xy = c(2, 3)
  )
  squares_2 <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 1, squares_per_row = 2, start_at_xy = c(-1, 5)
  )

  # Check the coordinates of the first square
  expect_equal(as.numeric(st_coordinates(squares_1)[1, "X"]), 2)
  expect_equal(as.numeric(st_coordinates(squares_1)[1, "Y"]), 3)
  expect_equal(as.numeric(st_coordinates(squares_2)[1, "X"]), -1)
  expect_equal(as.numeric(st_coordinates(squares_2)[1, "Y"]), 5)
})

test_that("gen_nonoverlapping_square_polygons() sets the correct CRS", {
  # Test with and without CRS
  squares_1 <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 1, squares_per_row = 2, crs = 4326
  )
  squares_2 <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 1, squares_per_row = 2
  )

  expect_equal(st_crs(squares_1)$epsg, 4326)
  expect_true(is.na(st_crs(squares_2)$epsg)) 
})
