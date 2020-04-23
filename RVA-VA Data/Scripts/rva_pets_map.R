
library(tidyverse)
library(osmdata)
library(sf)
library(extrafont)
library(janitor)
library(wesanderson)
library(ggtext)

pal <- wes_palette("Zissou1")

colors <- c("Dog" = pal[1], "Cat" = pal[3])

#grabbing streetmaps for RVA
get_rva_maps <- function(key, value) {
  getbb("Richmond Virginia United States") %>%
    opq() %>%
    add_osm_feature(key = key,
                    value = value) %>%
    osmdata_sf()
}

rva_streets <- get_rva_maps(key = "highway", value = c("motorway", "primary", "secondary", "tertiary"))
small_streets <- get_rva_maps(key = "highway", value = c("residential", "living_street",
                                                         "unclassified",
                                                         "service", "footway", "cycleway"))
river <- get_rva_maps(key = "waterway", value = "river")

df <- tibble(
  type = c("big_streets", "small_streets", "river"),
  lines = map(
    .x = lst(rva_streets, small_streets, river),
    .f = ~pluck(., "osm_lines")
  )
)

coords <- pluck(rva_streets, "bbox")

##reading in pet data
pets_raw <- read_csv(here::here("RVA-VA Data/Data/rva_pets_2019.csv"))

pets_clean <- pets_raw %>%
  clean_names() %>%
  extract(col = location_1,
          into = c("address", "zip", "lat", "long"),
          regex = "(.*)\n.*(\\d{5})\n\\((.*), (.*)\\)") %>%
  mutate(animal_type = str_replace_all(animal_type, c("Puppy" = "Dog", "Kitten" = "Cat")))

pets_map <- st_as_sf(pets_clean %>%
                       filter(!is.na(long)), coords = c("long", "lat"),
                     crs = 4326)

annotations <- tibble(
  label = c("<span style='color:#FFFFFF'><span style='color:#EBCC2A'>**Cats**</span> and <span style='color:#3B9AB2'>**Dogs**</span> in RVA</span>"),
  x = c(-77.555),
  y = c(37.605),
  hjust = c(0)
)

rva_pets <- ggplot() +
  geom_sf(data = df$lines[[1]],
          inherit.aes = FALSE,
          size = .3,
          alpha = .8, 
          color = "white") +
  geom_sf(data = df$lines[[2]],
          inherit.aes = FALSE,
          size = .1,
          alpha = .6,
          color = "white") +
  geom_sf(data = pets_map, aes(color = animal_type), alpha = .6, size = .75) +
  geom_richtext(data = annotations, aes(x = x, y = y, label = label, hjust = hjust), fill = NA, label.color = NA, 
                label.padding = grid::unit(rep(0, 4), "pt"), size = 11, family = "Bahnschrift") + 
  coord_sf(
    xlim = c(-77.55, -77.4),
    ylim = c(37.5, 37.61),
    expand = TRUE
  ) +
  theme_void() +
  scale_color_manual(
    values = colors
  ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey10"),
    panel.background = element_rect(fill = "grey10"),
    text = element_markdown(family = "Bahnschrift")
  )

ggsave(here::here("RVA-VA Data/rva_pets_map.png"), rva_pets, device = "png")
