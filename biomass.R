library(tidyverse)
library(egg)

bm22 <- read.csv("data/biomass_2022.csv") %>%
  mutate(FARM = recode(FARM,
    JAYU2 = "JAYU2/3",
    YAUC3 = "YAUC4"
  ))
bm23 <- read.csv("data/biomass_2023.csv") %>%
  mutate(across(DBH01:DBH23, .fns=as.numeric)) # NAs are okay (they are from the * marking dead stems and we ignore dead stems)

# make long
bm22_l <- bm22 %>%
  mutate(plantID = paste(FARM, "22", row_number(), sep = "_")) %>%
  pivot_longer(cols = DBH01:DBH20) %>%
  filter(!is.na(value)) %>%
  mutate(DBH_cm = ifelse(UNITS == "mm", value/10, value), 
         year=2022, Plot="Plot 1", Size="10x10") %>%
  select(FARM, year, Plot, Size, plantID, TYPE, DBH_cm) %>%
  mutate(basalA = pi*(DBH_cm/2)^2)

bm23_l <- bm23 %>%
  mutate(plantID = paste(Farm, "23",
                         ifelse(!is.na(ID), ID, 1000+row_number()), sep = "_")) %>%
  pivot_longer(cols = DBH01:DBH23, values_to = "DBH_cm") %>%
  filter(!is.na(DBH_cm)) %>%
  mutate(year=2023, basalA = pi*(DBH_cm/2)^2) %>%
  rename(FARM=Farm, TYPE=Plant.Type) %>%
  select(FARM, year, Plot, Size, ID, plantID, TYPE, Notes, DBH_cm, basalA) %>%
  rename(gpsID = ID)

bm_l <- bind_rows(bm22_l, bm23_l) %>%
  mutate(TYPE_clean =
           case_when(
             TYPE %in% c("Co", "Co ") ~ "Co",
             TYPE == "Ba" ~ "Ba",
             TYPE %in% c("Citr", "Ci") ~ "Ci",
             TYPE %in% c("tree(inga?)", "In") ~ "In",
             TYPE %in% c("Tr", "shrub", "Shrub", "Shrub?", "Tree-Center", "tree", "unk.") ~ "Tr"
           )) %>% 
  # correction of plot numbers
  mutate(Plot = ifelse(FARM=="JAYU2/3" & Plot=="Plot 1" & year == 2023, "Plot 4", Plot))

# calc basal area
coffee <- bm_l %>% filter(TYPE_clean == "Co") %>%
  group_by(FARM, year, Plot, Size, TYPE_clean, plantID) %>%
  summarize(totalBasal = sum(basalA), .groups = "drop") %>%
  mutate(AGM_kg = 0.3189*(totalBasal^0.7406))

musa <- bm_l %>% filter(TYPE_clean == "Ba") %>%
  mutate(AGM_kg = 0.0303*(DBH_cm^2.1345)) %>%
  group_by(FARM, year, Plot, Size, TYPE_clean, plantID) %>%
  summarize(AGM_kg = sum(AGM_kg), .groups = "drop")
  
citrus <- bm_l %>% filter(TYPE_clean == "Ci") %>%
  group_by(FARM, year, Plot, Size, TYPE_clean, plantID) %>%
  summarize(totalBasal = sum(basalA), .groups = "drop") %>%
  mutate(AGM_kg = -6.64 + (0.279*(totalBasal)) + (0.000514*(totalBasal^2)))

inga <- bm_l %>% filter(TYPE_clean == "In") %>%
  mutate(AGM_kg = 10^(-0.8890 + (2.317*(log10(DBH_cm))))) %>%
  group_by(FARM, year, Plot, Size, TYPE_clean, plantID) %>%
  summarize(AGM_kg = sum(AGM_kg), .groups = "drop")

shrubs_trees <- bm_l %>% filter(TYPE_clean == "Tr") %>%
  group_by(FARM, year, Plot, Size, TYPE_clean, plantID) %>%
  summarize(totalBasal = sum(basalA), .groups = "drop") %>%
  mutate(AGM_kg = exp(-0.535 + log10(totalBasal)))

biomass_all <- bind_rows(
  coffee, musa, citrus, inga, shrubs_trees
)


co_farm <- coffee %>% group_by(FARM) %>%
  summarize(agm_mn = mean(AGM_kg),
            agm_sd = sd(AGM_kg),
            agm_n = n(),
            agm_se = agm_sd/sqrt(agm_n))

ggplot(co_farm, aes(FARM, agm_mn)) + 
  geom_pointrange(aes(ymin = agm_mn-agm_se, ymax = agm_mn+agm_se)) +
  theme_article()


biomass_tot_10 <- biomass_all %>% filter(Size != "30x30") %>% select(-Size) %>%
  group_by(FARM,year,Plot) %>%
  summarize(AGMtot_kg = sum(AGM_kg), .groups = "drop")

biomass_tot_30 <- biomass_all %>% filter(Size != "10x10") %>% select(-Size) %>%
  group_by(FARM,year,Plot) %>%
  summarize(AGMtot_kg = sum(AGM_kg), .groups = "drop")

# check against plot polygons
library(sf)
plots.sf <- st_read("./data/GIS/plots_all.geojson")

plots.sf.bm10 <- plots.sf %>% filter(Size == "10x10") %>%
  left_join(biomass_tot_10) %>%
  mutate(AGMtot_kg = ifelse(is.na(AGMtot_kg),0,AGMtot_kg))

plots.sf.bm30 <- plots.sf %>% filter(Size == "30x30") %>%
  left_join(biomass_tot_30)

write.csv(bm_l, "clean_data/dbh_long_allyears.csv", row.names = FALSE)
write.csv(biomass_all, "clean_data/biomass_allyears.csv", row.names = FALSE)

st_write(plots.sf.bm10, "clean_data/biomass_plots_10.geojson")
st_write(plots.sf.bm30, "clean_data/biomass_plots_30.geojson")

write.csv(plots.sf.bm10 %>% st_drop_geometry(), "clean_data/biomass_tot_10.csv", row.names = FALSE)
write.csv(plots.sf.bm30 %>% st_drop_geometry(), "clean_data/biomass_tot_30.csv", row.names = FALSE)
