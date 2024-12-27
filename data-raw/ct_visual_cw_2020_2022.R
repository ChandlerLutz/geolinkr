## ./ct_visual_cw_2020_2022.R

suppressPackageStartupMessages({library(CLmisc); library(readxl)})

## Excel file created using:

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

ct_cnty20 <- ct_cnty20 |>
  as.data.table()

ct_cnty22 <- ct_cnty22 |>
  as.data.table()

ggplot() +
  geom_sf(data = ct_cnty20[from_geoid == "09011", geometry],
          fill = "red", color = "red") +
  geom_sf(data = ct_cnty22[to_geoid %chin% c("09110", "09130", "09150", "09180"),
                           geometry], fill = NA, color = "blue") +
  geom_sf_text(data = st_as_sf(ct_cnty22[to_geoid %chin% c("09110", "09130", "09150",
                                                           "09180")]),
               aes(label = to_geoid), color = "black")

ct_visual_cw_2020_2022 <- read_excel(
  here::here("data-raw/ct_cnty_2020_2022_cw_visual_intersection.xlsx"),
) |>
  as.data.table()

usethis::use_data(ct_visual_cw_2020_2022, overwrite = TRUE)
