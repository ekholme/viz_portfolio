##creating a streetmap of RVA using the osmdata package

library(tidyverse)
library(osmdata)

rva_streets <- getbb("Richmond Virginia United States") %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "secondary", "tertiary")
                  ) %>%
  osmdata_sf()

small_streets <- getbb("Richmond Virginia United States")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway", "cycleway")) %>%
  osmdata_sf()

river <- getbb("Richmond Virginia United States") %>%
  opq() %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

df <- tibble(
  type = c("big_streets", "small_streets", "river"),
  lines = map(
    .x = lst(rva_streets, small_streets, river),
    .f = ~pluck(., "osm_lines")
  )
)

coords <- pluck(rva_streets, "bbox")
coords2 <- str_split(coords, ",", simplify = TRUE) %>% as.vector() %>% as.numeric()

vcu_gold <- c("#fdbd10")

ggplot() +
  geom_sf(data = df$lines[[1]],
          inherit.aes = FALSE,
          color = vcu_gold,
          size = .6,
          alpha = .8) +
  geom_sf(data = df$lines[[2]],
          inherit.aes = FALSE,
          color = vcu_gold,
          size = .1,
          alpha = .6) +
  geom_sf(data = df$lines[[3]],
          color = "steelblue",
          size = 2,
          alpha = .8) +
  coord_sf(
    xlim = c(-77.525, -77.375),
    ylim = c(37.5, 37.6),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
#pick back up with colors later