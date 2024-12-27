# Description: Create a simple feature object of Puerto Rico

suppressPackageStartupMessages({
  library(CLmisc); library(tigris); library(sf)
})

puerto_rico_sf <- tigris::states(year = 2022) %>%
  as.data.table %>%
  setnames(names(.), tolower(names(.))) %>%
  .[statefp == "72"] %>%
  .[, geometry := st_transform(geometry, 5070)] %>%
  select_by_ref(c("statefp", "stusps", "geometry")) %>%
  .[order(statefp)] %>%
  st_as_sf %>%
  st_transform(crs = 5070)

usethis::use_data(puerto_rico_sf, overwrite = TRUE)
