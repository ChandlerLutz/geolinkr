library(data.table); library(sf)

test_that("st_intersection_to_multipolygon() returns correct object type and attributes", {
  
  squares_x <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 2, squares_per_row = 2
  )
  squares_y <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 2, squares_per_row = 2, start_at_xy = c(1, 1)
  )

  dt_x <- data.table(id_x = 1:2, geometry = squares_x)
  dt_y <- data.table(id_y = 1:2, geometry = squares_y)

  result <- st_intersection_to_multipolygon(dt_x, dt_y)

  expect_s3_class(result, "data.table")
  expect_true("geometry" %in% names(result))
  expect_true(all(as.character(sf::st_geometry_type(result$geometry)) == "MULTIPOLYGON"))
  expect_true("id_x" %chin% names(result))
  expect_true("id_x" %chin% names(result))

})

test_that("st_intersection_to_multipolygon() handles no intersection", {

  squares_x <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 2, squares_per_row = 2
  )
  squares_y <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 2, squares_per_row = 2, start_at_xy = c(5, 5)
  )

  dt_x <- data.table(id_x = 1:2, geometry = squares_x)
  dt_y <- data.table(id_y = 1:2, geometry = squares_y)

  result_intersection <- st_intersection(dt_x$geometry, dt_y$geometry) %>%
    as.data.table()
  expect_equal(nrow(result_intersection), 0)

  result <- st_intersection_to_multipolygon(dt_x, dt_y)
  
  expect_equal(nrow(result), 0)
})


test_that("st_intersection_to_multipolygon() returns nothing for intersections that result in LINESTRINGs", {

  squares_x <- gen_nonoverlapping_square_polygons(
    num_squares = 1, side_length = 2, squares_per_row = 1
  )
  squares_y <- gen_nonoverlapping_square_polygons(
    num_squares = 1, side_length = 2, squares_per_row = 1, start_at_xy = c(0, 2)
  )

  dt_x <- data.table(id_x = 1, geometry = squares_x)
  dt_y <- data.table(id_y = 1, geometry = squares_y)

  result_multipolygon <- st_intersection_to_multipolygon(dt_x, dt_y)
  result_intersection <- sf::st_intersection(
    squares_x, squares_y
  )
  
  expect_equal(nrow(result_multipolygon), 0)
  expect_equal(length(result_intersection), 1)
  expect_true(
    "LINESTRING" %chin% as.character(st_geometry_type(result_intersection))
  )
})  


test_that("st_intersection_to_multipolygon() returns nothing for intersections that result in POINTs", {

  squares_x <- gen_nonoverlapping_square_polygons(
    num_squares = 1, side_length = 2, squares_per_row = 1
  )
  squares_y <- gen_nonoverlapping_square_polygons(
    num_squares = 1, side_length = 2, squares_per_row = 1, start_at_xy = c(2, 2)
  )

  dt_x <- data.table(id_x = 1, geometry = squares_x)
  dt_y <- data.table(id_y = 1, geometry = squares_y)

  result_multipolygon <- st_intersection_to_multipolygon(dt_x, dt_y)
  result_intersection <- sf::st_intersection(
    squares_x, squares_y
  )
  
  expect_equal(nrow(result_multipolygon), 0)
  expect_equal(length(result_intersection), 1)
  expect_true(
    "POINT" %chin% as.character(st_geometry_type(result_intersection))
  )
})
  

test_that("st_intersection_to_multipolygon() matches sf::st_intersection()", {

  squares_x <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 2, squares_per_row = 2
  )
  squares_y <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 2, squares_per_row = 2, start_at_xy = c(1, 1)
  )

  dt_x <- data.table(id_x = 1:2, geometry = squares_x)
  dt_y <- data.table(id_y = 1:2, geometry = squares_y)

  result <- st_intersection_to_multipolygon(dt_x, dt_y)

  result_intersection <- sf::st_intersection(
    sf::st_as_sf(dt_x$geometry), sf::st_as_sf(dt_y$geometry)
  ) |>
    st_cast(to = "MULTIPOLYGON")

  expect_equal(nrow(result), 3)  
  expect_equal(nrow(result_intersection), 3)
  expect_true("sfc_MULTIPOLYGON" %chin% class(result$geometry))

  # Extract the geometries from both results
  geom_multipolygon <- st_sfc(result$geometry)
  geom_intersection <- st_geometry(result_intersection) |>
    st_cast(to = "MULTIPOLYGON")

  expect_equal(geom_multipolygon, geom_intersection)
})


test_that("st_intersection_to_multipolygon() handles a single polygon intersection", {

  squares_x <- gen_nonoverlapping_square_polygons(
    num_squares = 1, side_length = 2, squares_per_row = 1
  )
  squares_y <- gen_nonoverlapping_square_polygons(
    num_squares = 1, side_length = 2, squares_per_row = 1, start_at_xy = c(1, 1)
  )

  dt_x <- data.table(id_x = 1, geometry = squares_x)
  dt_y <- data.table(id_y = 1, geometry = squares_y)

  result_intersection <- st_intersection(dt_x$geometry, dt_y$geometry) |> 
    st_cast(to = "MULTIPOLYGON")
  expect_equal(length(result_intersection), 1)

  result <- st_intersection_to_multipolygon(dt_x, dt_y)
  
  # Expect a single row in the data.table
  expect_equal(nrow(result), 1)

  # Extract the geometries from both results
  geom_multipolygon <- st_sfc(result$geometry)
  geom_intersection <- st_geometry(result_intersection) |>
    st_cast(to = "MULTIPOLYGON")
    

  # Check if the multipolygons from your function match the expected polygons  
  # after converting the intersection result (excluding the point)
  expect_equal(
    st_cast(geom_multipolygon, "MULTIPOLYGON"),
    geom_intersection
  )
})

test_that("st_intersection_to_multipolygon() handles invalid input types", {
  squares_x <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 2, squares_per_row = 2
  )
  dt_x <- data.table(id_x = 1:2, geometry = squares_x)

  expect_error(st_intersection_to_multipolygon(dt_x, "invalid"))
  expect_error(st_intersection_to_multipolygon(123, dt_x))
})

test_that("st_intersection_to_multipolygon() handles missing geometry column", {

  squares_x <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 2, squares_per_row = 2
  )
  dt_x <- data.table(id_x = 1:2, geometry = squares_x)
  dt_y <- data.table(id_y = 1:2, non_geometry = squares_x) # Rename geometry column

  # Expect an error due to the missing "geometry" column
  expect_error(st_intersection_to_multipolygon(dt_x, dt_y))
})

test_that("st_intersection_to_multipolygon() handles split geometries", {

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

  result_intersection <- st_intersection(dt_from$geometry, dt_wts$geometry) |>
    st_cast(to = "MULTIPOLYGON")
  result2_intersection <- st_intersection(result_intersection, dt_to$geometry) |>
    st_cast(to = "MULTIPOLYGON")


  result <- st_intersection_to_multipolygon(dt_from, dt_wts)
  result2 <- st_intersection_to_multipolygon(result, dt_to)

  expect_equal(result$geometry, result_intersection)
  expect_equal(result2$geometry, result2_intersection)

  
})

test_that("st_intersection_to_multipolygon() correctly handles geometry collections", {
    
  squares_x <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 2, squares_per_row = 2
  )
  squares_y <- gen_nonoverlapping_square_polygons(
    num_squares = 2, side_length = 2, squares_per_row = 2, start_at_xy = c(1, 1)
  )
  
  squares_x <- st_sfc(squares_x) |> st_cast(to = "GEOMETRYCOLLECTION")
  squares_y <- st_sfc(squares_y) |> st_cast(to = "GEOMETRYCOLLECTION")
  
  dt_x <- data.table(id_x = 1:2, geometry = squares_x)
  dt_y <- data.table(id_y = 1:2, geometry = squares_y)
  
  result <- st_intersection_to_multipolygon(dt_x, dt_y)
  
  result_intersection <- sf::st_intersection(
    squares_x, squares_y
  ) |> st_cast(to = "MULTIPOLYGON")
  
  expect_equal(nrow(result), 3)  
  expect_equal(length(result_intersection), 3)
  expect_true("sfc_MULTIPOLYGON" %chin% class(result$geometry))
  
  expect_equal(result$geometry, result_intersection)
  
})

test_that("st_intersection_to_multipolygon() handles an intersection that leads to a geometrycollection that only has linestrings and points (no polygons)", {
  
  load(
    test_path(
      "internal-testdata",
      "sf_datatables_that_intersect_to_a_geomcollection_without_polygons.RData"
    )
  )

  st_x <- sf::st_as_sf(dt_x)
  st_y <- sf::st_as_sf(dt_y)

  ## Assume that non-geometry columns are constant across space
  st_x <- sf::st_set_agr(st_x, "constant")
  st_y <- sf::st_set_agr(st_y, "constant")

  dt_intersection <- sf::st_intersection(st_x, st_y) |> 
    as.data.table()

  dt_intersection <- dt_intersection |>  
    _[, geom_type := sf::st_geometry_type(geometry)] |>
    _[, geom_type := as.character(geom_type)] |> 
    _[geom_type %notin% c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING")]

  expect_true(sf::st_geometry_type(dt_intersection$geometry) == "GEOMETRYCOLLECTION")
  expect_true("POLYGON" %notin% attr(dt_intersection$geometry, "classes"))

  result <- st_intersection_to_multipolygon(dt_x, dt_y)
  expect_equal(nrow(result), 0)
  expect_equal(
    names(result),
    c("from_geoid", "wts_geoid", "to_geoid", "geometry", "geom_type")
  )
  
})

test_that("st_intersection_to_multipolygon() handles an intersection that leads to a list of sfc objects where one does not have a CRS", {

  load(
    test_path(
      "internal-testdata",
      "for_not_same_crs_error_in_st_intersection_to_multipolygon.RData"
    )
  )

  result <- st_intersection_to_multipolygon(dt_x, dt_y)
  expect_true(is.data.table(result))
  expect_true("sfc_MULTIPOLYGON" %chin% class(result$geometry))
  expect_true(sf::st_crs(result$geometry) == sf::st_crs(dt_x$geometry))
  
})

test_that("st_intersection_to_multipolygon() handles an intersection that leads to a single polygon object", {

  polygon_x <- sf::st_polygon(list(matrix(c(0, 0, 0, 5, 5, 5, 5, 0, 0, 0),
                                      ncol = 2, byrow = TRUE)))
  st_x <- sf::st_sf(id_x = 1, geometry = st_sfc(polygon_x)) |>
    sf::st_set_agr("constant")
  
  polygon_y <- sf::st_polygon(list(matrix(c(2, 2, 2, 7, 7, 7, 7, 2, 2, 2),
                                      ncol = 2, byrow = TRUE)))
  st_y <- sf::st_sf(id_y = 2, geometry = st_sfc(polygon_y)) |>
    sf::st_set_agr("constant")

  result_intersection <- st_intersection(st_x, st_y)

  expect_true(
    sf::st_geometry_type(result_intersection) |> as.character() == "POLYGON"
  )

  result_intersection <- result_intersection |>
    st_cast(to = "MULTIPOLYGON")

  expect_true(
    sf::st_geometry_type(result_intersection) |> as.character() == "MULTIPOLYGON"
  )

  result <- st_intersection_to_multipolygon(as.data.table(st_x), as.data.table(st_y))

  expect_equal(nrow(result), 1)
  expect_true(
    sf::st_geometry_type(result$geometry) |> as.character() == "MULTIPOLYGON"
  )
  expect_equal(result$geometry, result_intersection$geometry)

})


test_that("st_intersection_to_multipolygon() handles intersection leading to multiple POLYGON rows", {

  polygon_x1 <- sf::st_polygon(list(matrix(c(0,0, 0,2, 2,2, 2,0, 0,0),
                                           ncol = 2, byrow = TRUE)))
  polygon_x2 <- sf::st_polygon(list(matrix(c(3,3, 3,5, 5,5, 5,3, 3,3),
                                           ncol = 2, byrow = TRUE)))
  
  # Create an sf object with two rows for x
  st_x <- sf::st_sf(id_x = 1:2, 
                   geometry = st_sfc(polygon_x1, polygon_x2)) |>
    sf::st_set_agr("constant")
  
  # Define one polygon for y that overlaps both polygons in x
  polygon_y <- sf::st_polygon(list(matrix(c(-1,-1, -1,6, 6,6, 6,-1, -1,-1),
                                          ncol = 2, byrow = TRUE)))
  
  # Create an sf object with one row for y
  st_y <- sf::st_sf(id_y = 10, 
                   geometry = st_sfc(polygon_y)) |>
    sf::st_set_agr("constant")

  result_intersection <- sf::st_intersection(st_x, st_y)

  expect_equal(nrow(result_intersection), 2, 
               info = "Intersection should produce two rows.")
  
  expect_true(all(sf::st_geometry_type(result_intersection) == "POLYGON"))

  result_intersection <- result_intersection |>
    sf::st_cast(to = "MULTIPOLYGON")
  
  result <- st_intersection_to_multipolygon(as.data.table(st_x), as.data.table(st_y))
    
  expect_true(all(sf::st_geometry_type(result$geometry) == "MULTIPOLYGON"))
  expect_equal(nrow(result), 2)
  expect_true(
    sf::st_geometry_type(result$geometry) |> as.character() |> unique() == "MULTIPOLYGON"
  )
  expect_equal(result$geometry, result_intersection$geometry)

})

test_that("st_intersection_to_multipolygon() handles intersection yielding mixed POLYGON/MULTIPOLYGON rows", {

  # --- Define Input Geometries ---
  # Define parts for the multipolygon (will be id_x = 1)
  x1_part1 <- list(matrix(c(0,0, 0,1, 1,1, 1,0, 0,0), ncol = 2, byrow = TRUE))
  x1_part2 <- list(matrix(c(1.5,0, 1.5,1, 2.5,1, 2.5,0, 1.5,0), ncol = 2, byrow = TRUE))
  # Create the multipolygon
  mp1 <- sf::st_multipolygon(list(x1_part1, x1_part2))

  # Define a simple polygon (will be id_x = 2)
  p2 <- sf::st_polygon(list(matrix(c(3,3, 3,5, 5,5, 5,3, 3,3),
                                   ncol = 2, byrow = TRUE)))
  
  # Create an sf object for x with one MULTIPOLYGON row and one POLYGON row
  st_x <- sf::st_sf(id_x = 1:2, 
                   geometry = st_sfc(mp1, p2)) |> # Order matters: mp1 first, p2 second
    sf::st_set_agr("constant")
  
  # Define one polygon for y that overlaps both geometries in x entirely
  polygon_y <- sf::st_polygon(list(matrix(c(-1,-1, -1,6, 6,6, 6,-1, -1,-1),
                                          ncol = 2, byrow = TRUE)))
  
  st_y <- sf::st_sf(id_y = 10, 
                   geometry = st_sfc(polygon_y)) |>
    sf::st_set_agr("constant")

  # --- Perform Intersection ---
  # Intersection should yield two rows:
  # Row 1 (from mp1): MULTIPOLYGON (identical to mp1)
  # Row 2 (from p2): POLYGON (identical to p2)
  result_intersection <- sf::st_intersection(st_x, st_y)

  expect_equal(nrow(result_intersection), 2)

  # *** Key Check: Verify the direct result has mixed types ***
  result_types <- sf::st_geometry_type(result_intersection)
  expect_equal(as.character(result_types[1]), "MULTIPOLYGON")
  expect_equal(as.character(result_types[2]), "POLYGON")

  result_intersection <- result_intersection |>
    sf::st_cast(to = "MULTIPOLYGON")

  # Call the function under test
  result <- st_intersection_to_multipolygon(as.data.table(st_x), as.data.table(st_y))

  expect_true(all(sf::st_geometry_type(result$geometry) == "MULTIPOLYGON"))
  expect_equal(nrow(result), 2)
  expect_true(
    sf::st_geometry_type(result$geometry) |> as.character() |> unique() == "MULTIPOLYGON"
  )
  expect_equal(result$geometry, result_intersection$geometry)

   
})


test_that("st_intersection_to_multipolygon() handles intersection yielding MULTIPOLYGON rows directly", {

  # --- Define Input Geometries ---
  # Define parts for the first multipolygon
  x1_part1 <- list(matrix(c(0,0, 0,1, 1,1, 1,0, 0,0), ncol = 2, byrow = TRUE))
  x1_part2 <- list(matrix(c(1.5,0, 1.5,1, 2.5,1, 2.5,0, 1.5,0), ncol = 2, byrow = TRUE))
  # Create the first multipolygon
  mp1 <- sf::st_multipolygon(list(x1_part1, x1_part2))

  # Define parts for the second multipolygon
  x2_part1 <- list(matrix(c(3,3, 3,4, 4,4, 4,3, 3,3), ncol = 2, byrow = TRUE))
  x2_part2 <- list(matrix(c(4.5,3, 4.5,4, 5.5,4, 5.5,3, 4.5,3), ncol = 2, byrow = TRUE))
  # Create the second multipolygon
  mp2 <- sf::st_multipolygon(list(x2_part1, x2_part2))
  
  st_x <- sf::st_sf(id_x = 1:2, 
                    geometry = st_sfc(mp1, mp2)) |>
    sf::st_set_agr("constant")
  
  # Define one polygon for y that overlaps both multipolygons in x entirely
  polygon_y <- sf::st_polygon(list(matrix(c(-1,-1, -1,6, 6,6, 6,-1, -1,-1),
                                          ncol = 2, byrow = TRUE)))
  
  # Create an sf object with one row for y
  st_y <- sf::st_sf(id_y = 10, 
                   geometry = st_sfc(polygon_y)) |>
    sf::st_set_agr("constant")

  result_intersection <- sf::st_intersection(st_x, st_y)

  expect_equal(nrow(result_intersection), 2, 
               info = "Intersection should produce two rows.")
  
  expect_true(all(sf::st_geometry_type(result_intersection) == "MULTIPOLYGON"))
  

  dt_x <- as.data.table(st_x)
  dt_y <- as.data.table(st_y)

  # Call the function under test
  result <- st_intersection_to_multipolygon(as.data.table(st_x), as.data.table(st_y))

  expect_true(all(sf::st_geometry_type(result$geometry) == "MULTIPOLYGON"))
  expect_equal(nrow(result), 2)
  expect_true(
    sf::st_geometry_type(result$geometry) |> as.character() |> unique() == "MULTIPOLYGON"
  )
  expect_equal(result$geometry, result_intersection$geometry)



})


test_that("st_intersection_to_multipolygon() works for a real example that results in a single polygon intersection", {
  
  dt_x <- readRDS(
    ex_test_data_path("st_intersection_to_multipolygon_ex_trct_to_zip_dt_x.rds")
  )
  
  dt_y <- readRDS(
    ex_test_data_path("st_intersection_to_multipolygon_ex_trct_to_zip_dt_y.rds")
  )
  
  result_intersection <- st_intersection(dt_x$geometry, dt_y$geometry) |>
    st_cast(to = "MULTIPOLYGON")
  
  result <- st_intersection_to_multipolygon(dt_x, dt_y)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$geometry, result_intersection)
  
})

test_that("st_intersection_to_multipolygon() works for a real example that results in a single polygon intersection", {
  
  dt_x <- readRDS(
    ex_test_data_path("trct_to_zip_ex_dt_x_for_zip_19108.rds")
  )
  
  dt_y <- readRDS(
    ex_test_data_path("trct_to_zip_ex_dt_y_for_zip_19108.rds")
  )
  
  result <- st_intersection_to_multipolygon(dt_x, dt_y)
  
  expect_equal(nrow(result), 1)
  expect_true(sf::st_geometry_type(result$geometry) == "MULTIPOLYGON")
  expect_no_error(sf::st_area(result$geometry))
  
})




