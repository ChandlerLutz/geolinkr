# Description: Create a simple feature object of Puerto Rico

suppressPackageStartupMessages({
  library(CLmisc); library(tigris); library(sf)
})



ct_cnty22_sf <- tigris::counties(state = "CT", year = 2022) %>%
  as.data.table %>%
  setnames(names(.), tolower(names(.))) %>%
  .[, geometry := st_transform(geometry, 5070)] %>%
  select_by_ref(c("geoid", "statefp", "countyfp", "geometry")) %>%
  .[order(geoid)] %>%
  st_as_sf %>%
  st_transform(crs = 5070)

usethis::use_data(ct_cnty22_sf, overwrite = TRUE)
