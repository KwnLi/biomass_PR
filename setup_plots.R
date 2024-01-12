library(sf)
library(tidyverse)

plots10_22 <- st_read("./data/GIS/plots10_2022.geojson") %>% mutate(plot_num =1)
plots10_23 <- st_read("./data/GIS/plots10_2023.geojson")

plots10 <- bind_rows(plots10_22, plots10_23) %>% 
  mutate(Plot = paste0("Plot ", plot_num),
         Size = "10x10",
         FARM = ifelse(PESTS_site == "JAYU2", "JAYU2/3", PESTS_site)) %>%
  select(-PESTS_site)

plots30 <- st_read("./data/GIS/plots30_2023.geojson") %>%
  mutate(Plot = paste0("Plot ", plot_num),
         Size = "30x30",
         FARM = ifelse(PESTS_site == "JAYU2", "JAYU2/3", PESTS_site)) %>%
  select(-PESTS_site)

plots_all <- bind_rows(plots10, plots30)

# write out combined data
st_write(plots10, "./data/GIS/plots10.geojson")
st_write(plots30, "./data/GIS/plots30.geojson")
st_write(plots_all, "./data/GIS/plots_all.geojson")
