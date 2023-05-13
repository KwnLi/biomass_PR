library(tidyverse)

bm <- read.csv("biomass_2022.csv")

# make long
bm_l <- bm %>%
  mutate(plantID = paste(FARM, row_number(), sep = "_")) %>%
  pivot_longer(cols = DBH01:DBH20) %>%
  filter(!is.na(value)) %>%
  mutate(DBH_cm = ifelse(UNITS == "mm", value/10, value)) %>%
  select(FARM, plantID, TYPE, DBH_cm) %>%
  mutate(basalA = pi*(DBH_cm/2)^2)

# write out
# write.csv(bm_l, "biomass_2022_long.csv", row.names = FALSE)

# calc basal area
coffee <- bm_l %>% filter(TYPE == "Co") %>%
  group_by(FARM, TYPE, plantID) %>%
  summarize(totalBasal = sum(basalA), .groups = "drop") %>%
  mutate(AGM_kg = 0.3189*(totalBasal^0.7406))

