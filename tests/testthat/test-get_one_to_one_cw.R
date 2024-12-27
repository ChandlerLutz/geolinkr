library(tigris)

options(tigris_use_cache = TRUE)

# Mock data for testing
create_mock_data_for_spatial_relationships <- function() {
  # Define four geometries with different spatial relationships:
  library(sf)

  # Define four geometries with different spatial relationships:
  # - Geometry 1: A square polygon.
  # - Geometry 2: A square polygon that will be equal to a geometry in geom_to.
  # - Geometry 3: A polygon that will intersect with a geometry in geom_to but not
  #               be equal or covered.
  # - Geometry 4: A non-intersecting polygon.

  geom_from <- st_sfc(
    # Geometry 1: A standalone square polygon
    st_multipolygon(list(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))))),
    
    # Geometry 2: A square polygon that will be equal to a geometry in geom_to
    st_multipolygon(list(list(rbind(c(5, 5), c(6, 5), c(6, 6), c(5, 6), c(5, 5))))),
    
    # Geometry 3: A polygon that will intersect with a geometry in geom_to but is
    # not equal or covered
    st_multipolygon(list(list(rbind(c(10, 10), c(12, 10), c(12, 12), c(10, 12),
                                    c(10, 10))))),
    
    # Geometry 4: A non-intersecting polygon
    st_multipolygon(list(list(rbind(c(15, 15), c(16, 15), c(16, 16), c(15, 16),
                                    c(15, 15))))),
    crs = 5070
  )

  geom_to <- st_sfc(
    # Geometry 2: Equal to Geometry 2 in geom_from
    st_multipolygon(list(list(rbind(c(5, 5), c(6, 5), c(6, 6), c(5, 6), c(5, 5))))),
    
    # Geometry 1: Covers Geometry 1 in geom_from but is located separately from other
    # geometries
    st_multipolygon(list(list(rbind(c(-1, -1), c(3, -1), c(3, 3), c(-1, 3),
                                    c(-1, -1))))),
    
    # Geometry 3: Intersects with Geometry 3 in geom_from, but not equal or covered
    st_multipolygon(list(list(rbind(c(11, 11), c(13, 11), c(13, 13), c(11, 13),
                                    c(11, 11))))),
    
    # Geometry 4: Non-intersecting geometry
    st_multipolygon(list(list(rbind(c(18, 18), c(19, 18), c(19, 19), c(18, 19),
                                    c(18, 18))))),
    crs = 5070
  )
  
  # Example of creating data.tables for testing
  dt_from <- data.table(
    from_geoid = c("001", "002", "003", "004"),
    geometry = geom_from
  )

  dt_to <- data.table(
    to_geoid = c("101", "102", "103", "104"),
    geometry = geom_to
  )
  
  list(dt_from = dt_from, dt_to = dt_to)
}


test_that("get_one_to_one_cw() throws an error when `dt_to` geometries overlap, creating more many:1 match between `dt_from` and `dt_to`", {
  data <- create_mock_data_for_spatial_relationships()
  dt_from <- data$dt_from
  dt_to <- data$dt_to

  # Add a duplicated geometry to dt_to
  dt_to <- rbind(dt_to, dt_from[1, .(to_geoid = "105", geometry)])
  tmp <- sf::st_covered_by(dt_from$geometry, dt_to$geometry) %>%
    as.data.frame %>% as.data.table
  # Expect one of the polygon in `dt_from` to match with two polygons in `dt_to`
  expect_true(tmp[, any(duplicated(row.id))],
              info = "Expected at least one geometry in `dt_from` to match multiple geometries in `dt_to`")

  expect_error(get_one_to_one_cw(dt_from, dt_to, funs = list(st_covered_by)),
               info = "Expected get_one_to_one_cw to throw an error for many-to-one matches with st_covered_by")
})
  

test_that("get_one_to_one_cw() removes duplicated geometries in `dt_from`", {
  data <- create_mock_data_for_spatial_relationships()
  dt_from <- data$dt_from
  dt_to <- data$dt_to

  # Add a duplicated geometry to dt_from
  dt_from <- rbind(dt_from[1], dt_from[1])

  result <- get_one_to_one_cw(dt_from, dt_to, funs = list(st_covered_by))
  expect_equal(nrow(result), 1)
})

test_that("get_one_to_one_cw() removes duplicated geometries in `dt_to`", {
  data <- create_mock_data_for_spatial_relationships()
  dt_from <- data$dt_from
  dt_to <- data$dt_to

  # Add a duplicated geometry to dt_from
  dt_to <- rbind(dt_to[1], dt_to[1])

  result <- get_one_to_one_cw(dt_from, dt_to, funs = list(st_covered_by))
  expect_equal(nrow(result), 1)
})

test_that("get_one_to_one_cw() identifies when `dt_to` overlaps with `dt_from` using `st_covered_by`", {

  data <- create_mock_data_for_spatial_relationships()
  dt_from <- data$dt_from
  dt_to <- data$dt_to

  # Check the test data with st_covered_by()
  tmp <- st_covered_by(dt_from$geometry[[1]], dt_to$geometry[[2]]) %>%
    as.data.frame %>% as.data.table
  expect_equal(nrow(tmp), 1)

  result <- get_one_to_one_cw(dt_from[1], dt_to[2])
  expect_equal(nrow(result), 1)

})

test_that("get_one_to_one_cw() identifies when `dt_to` equals `dt_from`", {

  data <- create_mock_data_for_spatial_relationships()
  dt_from <- data$dt_from
  dt_to <- data$dt_to

  # Check the test data with st_equals()
  tmp <- st_equals(dt_from$geometry[[2]], dt_to$geometry[[1]]) %>%
    as.data.frame %>% as.data.table
  expect_equal(nrow(tmp), 1)

  result <- get_one_to_one_cw(dt_from[1], dt_to[2])
  expect_equal(nrow(result), 1)


})


test_that("get_one_to_one_cw() returns no rows for non-intersecting polygons", {
  data <- create_mock_data_for_spatial_relationships()
  dt_from <- data$dt_from
  dt_to <- data$dt_to

  # Check the test data with st_covered_by()
  tmp <- st_intersects(dt_from$geometry[[4]], dt_to$geometry[[4]]) %>%
    as.data.frame %>% as.data.table
  expect_equal(nrow(tmp), 0)

  result <- get_one_to_one_cw(dt_from[4], dt_to[4], funs = list(st_covered_by))
  expect_equal(nrow(result), 0)
})


test_that("get_one_to_one_cw() works with all mock cases", {
  data <- create_mock_data_for_spatial_relationships()
  dt_from <- data$dt_from
  dt_to <- data$dt_to

  tmp <- st_covered_by(dt_from$geometry, dt_to$geometry) |> 
    as.data.frame() |> as.data.table()
  expect_equal(nrow(tmp), 2)

  result <- get_one_to_one_cw(dt_from, dt_to, funs = list(st_covered_by))
  expect_equal(nrow(result), 2)
  
})

test_that("get_one_to_one_cw() identifies CT counties to CT state", {
  
  ct_cnty22_sf <- tigris::counties(state = "CT", year = 2022) |>
    st_transform(crs = 5070) |>
    as.data.table() |>
    _[, .(geoid = GEOID, geometry)] |>
    st_as_sf()
  ct_st <- geolinkr::us_states_sf
  ct_st <- ct_st[ct_st$statefp == "09", ]

  covered_by_check <- sf::st_covered_by(ct_cnty22_sf, ct_st) |>
    as.data.frame() |> as.data.table()

  expect_equal(nrow(covered_by_check), nrow(ct_cnty22_sf))

  dt_ct_cnty22 <- ct_cnty22_sf |> 
    as.data.table() |> 
    _[, .(from_geoid = geoid, geometry)]

  dt_ct_st <- ct_st |> 
    as.data.table() |> 
    _[, .(to_geoid = statefp, geometry)]

  cw <- get_one_to_one_cw(dt_from = dt_ct_cnty22,
                          dt_to = dt_ct_st)

  
  expect_equal(nrow(cw), nrow(ct_cnty22_sf))
  expect_equal(sort(cw$from_geoid), sort(ct_cnty22_sf$geoid))
  expect_equal(cw[, unique(to_geoid)], "09")
  expect_equal(cw[, sum(afact)], nrow(ct_cnty22_sf))
  
})

