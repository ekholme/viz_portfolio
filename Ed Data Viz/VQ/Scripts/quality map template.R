library(tidyverse)
library(osmdata)
library(ggmap)
library(janitor)
library(sf)
library(USAboundaries)
#library(ggthemes)
library(scales)
library(patchwork)

ecce_pal <- c("#187E8A", "#084C61", "#DC3A35", "#FFC857", "#323031")

set.seed(0408)

#set api_key before running again

readpath <- c("S:/Office of Standards, Curriculum and Instruction/School Readiness/E Ekholm/Data and Analysis/Virginia Quality/Data/")
vizpath <- c("S:/Office of Standards, Curriculum and Instruction/School Readiness/E Ekholm/Data and Analysis/Virginia Quality/Viz/")

vq <- read_csv(paste0(readpath, "vq_combined.csv"))

license <- readxl::read_xlsx(paste0(readpath, "DOE_Licensing_Data.xlsx")) %>%
  clean_names(case = "snake") %>%
  glimpse() %>%
  filter(min_age_year <= 4)
#ok, so, won't be able to map this

#test <- vq %>%
#  mutate_geocode(address, output = "more")

#write_csv(test, path = paste0(readpath, "vq_geocoded.csv"))

vq_geo <- read_csv(paste0(readpath, "vq_geocoded.csv")) %>%
  mutate(serve_inf_todd = if_else(
    str_detect(age_groups, "Inf|Todd"), TRUE, FALSE
  ))

va_map <- us_counties(states = "Virginia", resolution = "high")

#all sites participating in VQ
all_vq <- ggplot() +
  geom_sf(data = va_map, fill = "grey90", color = "grey90") +
  geom_point(data = vq_geo %>% filter(!is.na(serve_inf_todd)), aes(x = lon, y = lat, color = serve_inf_todd), size = 1.5, alpha = .6, inherit.aes = FALSE) +
  theme_void() +
  scale_color_manual(
    values = c(ecce_pal[[3]], ecce_pal[[1]]),
    name = "Serves Infants and Toddlers?",
    labels = c("No", "Yes")
  ) +
  labs(
    title = "All Programs Participating in VQ"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = .5, size = 18)
  )

#high quality sites
hq_vq <- ggplot() +
  geom_sf(data = va_map, fill = "grey90", color = "grey90") +
  geom_point(data = vq_geo %>%
               filter(!is.na(serve_inf_todd) & quality_level >= 4),
             aes(x = lon, y = lat, color = serve_inf_todd), size = 1.5, alpha = .6, inherit.aes = FALSE) +
  theme_void() +
  scale_color_manual(
    values = c(ecce_pal[[3]], ecce_pal[[1]]),
    name = "Serves Infants and Toddlers?",
    labels = c("No", "Yes")
  ) +
  labs(
    title = "VQ Levels 4 and 5 Only"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = .5, size = 18)
  )

#patching together
all_vq / hq_vq

ggsave(paste0(vizpath, "vq_maps.jpeg"), device = "jpeg")


#making a community-level map with programs
#example in RVA

rva_streets <- getbb("Richmond Virginia United States") %>%
  opq() %>%
  add_osm_feature(key = "highway",
                   value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

rva_small_streets <- getbb("Richmond Virginia United States") %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living street", "unclassified", "service", "footway")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = rva_streets$osmlines,
          color = "black",
          size = .4,
          alpha = .6,
          inherit.aes = FALSE) +
  geom_sf(data = rva_small_streets$osm_lines,
          color = "black",
          size = .4, 
          alpha = .4,
          inherit.aes = FALSE) +
  coord_sf(
    xlim = c(-77.6, -77.25),
    ylim = c(37.35, 37.7),
    expand = FALSE
  ) +
  geom_point(data = vq_geo %>%
               filter(!is.na(serve_inf_todd)),
             aes(x = lon, y = lat, color = if_else(quality_level >= 4, TRUE, FALSE)),
             size = 3, alpha = .9, inherit.aes = FALSE) +
  #geom_text(data = vq_geo %>%
  #            filter(!is.na(serve_inf_todd) & quality_level >= 4),
  #          aes(x = lon, y = lat, label = name), color = ecce_pal[[2]], size = 10, fontface = "bold", inherit.aes = FALSE) +
  theme_void() +
  scale_color_manual(
    values = c(ecce_pal[[3]], ecce_pal[[1]]),
    name = "VQ Program Level",
    labels = c("3 or Below", "4 or Above")
  ) +
  labs(
    title = "VQ Programs in Richmond"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = .5, size = 18)
  )

ggsave(paste0(vizpath, "rva_map.jpeg"), device = "jpeg")
