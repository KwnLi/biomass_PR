library(tidyverse)

bm <- read.csv("biomass_2022.csv")

# make long
bm_l <- bm %>%
  mutate(plantID = paste(FARM, row_number(), sep = "_")) %>%
  pivot_longer(cols = DBH01:DBH20) %>%
  filter(!is.na(value)) %>%
  mutate(DBH_cm = ifelse(UNITS == "mm", value/10, value)) %>%
  select(FARM, TYPE, DBH_cm) %>%
  mutate(basalA = pi*(DBH_cm/2)^2)

# write out
# write.csv(bm_l, "biomass_2022_long.csv", row.names = FALSE)

# calc basal area
coffee <- bm_l %>% filter(TYPE == "Co")
