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

musa <- bm_l %>% filter(TYPE == "Ba") %>%
  select(FARM, TYPE, plantID, DBH_cm) %>%
  mutate(AGM_kg = 0.0303*(DBH_cm^2.1345))
  
citrus <- bm_l %>% filter(TYPE == "Citr") %>%
  group_by(FARM, TYPE, plantID) %>%
  summarize(totalBasal = sum(basalA), .groups = "drop") %>%
  mutate(AGM_kg = -6.64 + (0.279*(totalBasal)) + (0.000514*(totalBasal^2)))

inga <- bm_l %>% filter(TYPE == "tree(inga?)") %>%
  select(FARM, TYPE, plantID, DBH_cm) %>%
  mutate(AGM_kg = 10^(-0.8890 + (2.317*(log10(DBH_cm)))))

shrubs_trees <- bm_l %>% filter(TYPE %in% c("shrub", "Shrub", "Tree-Center", "tree", "unk.")) %>%
  group_by(FARM, TYPE, plantID) %>%
  summarize(totalBasal = sum(basalA), .groups = "drop") %>%
  mutate(AGM_kg = exp(-0.535 + log10(totalBasal)))

biomass_all <- bind_rows(
  coffee, musa, citrus, inga, shrubs_trees
)
