library(data.table); library(sf); library(ggplot2)

test_that("st_buffer_frac() can increase the area by 1% on each side of square, leading to a larger area", {
  orig_square <- gen_nonoverlapping_square_polygons(
    num_squares = 1, side_length = 1, squares_per_row = 1
  )
  orig_square_area <- sf::st_area(orig_square)

  ## Buffer the square by 1% on all sides. This will result in a square with
  ## an area 1.04 times the original area.
  buffered_square <- st_buffer_frac(orig_square, buffer_frac = 0.01)

  buffered_square_area <- sf::st_area(buffered_square)

  ggplot() + geom_sf(data = buffered_square, color = "blue", fill = NA) +
    geom_sf(data = orig_square, color = "red", fill = NA) 

  expect_equal(round(buffered_square_area, 1), round(orig_square_area * 1.04, 1))
  
})

test_that("st_buffer_frac() can buffer multipolygons with a small buffer", {
  squares <- gen_nonoverlapping_square_polygons(4, 10, 2, crs = 3857)
  buffered <- st_buffer_frac(squares, 0.05)
  
  expect_s3_class(buffered, "sfc")
  expect_equal(sf::st_crs(buffered), sf::st_crs(squares))
  expect_true(all(sf::st_area(buffered) > sf::st_area(squares)))
  expect_equal(sf::st_geometry_type(squares), sf::st_geometry_type(buffered))
})

test_that("st_buffer_frac() can return the input geometry when buffer is zero", {
  squares <- gen_nonoverlapping_square_polygons(9, 5, 3, crs = 4326)
  buffered <- st_buffer_frac(squares, 0)
  
  expect_equal(buffered, squares)
})

test_that("st_buffer_frac() can buffer multipolygons with a larger buffer", {
  squares <- gen_nonoverlapping_square_polygons(2, 20, 1, crs = 3857)
  buffered <- st_buffer_frac(squares, 0.2)
  
  expect_s3_class(buffered, "sfc")
  expect_true(all(sf::st_area(buffered) > sf::st_area(squares)))
  expect_equal(sf::st_geometry_type(squares), sf::st_geometry_type(buffered))
})

test_that("st_buffer_frac() preserves the CRS", {
  squares <- gen_nonoverlapping_square_polygons(5, 8, 5, crs = 5070) 
  buffered <- st_buffer_frac(squares, 0.1)
  
  expect_equal(sf::st_crs(buffered), sf::st_crs(squares))
})

test_that("st_buffer_frac() buffers a single square multipolygon", {
  square <- gen_nonoverlapping_square_polygons(1, 15, 1, crs = 5070)
  buffered <- st_buffer_frac(square, 0.15)
  
  expect_s3_class(buffered, "sfc")
  expect_equal(sf::st_crs(buffered), sf::st_crs(square))
  expect_true(all(sf::st_area(buffered) > sf::st_area(square)))
  expect_equal(sf::st_geometry_type(square), sf::st_geometry_type(buffered))
})

test_that("st_buffer_frac() throws an error for negative buffer fraction", {
  squares <- gen_nonoverlapping_square_polygons(4, 10, 2, crs = 5070)
  expect_error(st_buffer_frac(squares, -0.1))
})
