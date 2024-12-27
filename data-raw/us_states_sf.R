# Description: Create a simple feature object of US states

suppressPackageStartupMessages({
  library(CLmisc); library(tigris); library(sf)
})

us_states_sf <- tigris::states(year = 2022) %>%
  as.data.table %>%
  setnames(names(.), tolower(names(.))) %>%
  .[statefp <= "56"] %>%
  .[, geometry := st_transform(geometry, 5070)] %>%
  select_by_ref(c("statefp", "stusps", "geometry")) %>%
  .[order(statefp)] %>%
  st_as_sf %>%
  st_transform(crs = 5070)

usethis::use_data(us_states_sf, overwrite = TRUE)
