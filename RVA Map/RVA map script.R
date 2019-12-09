
library(tidyverse)
library(osmdata)
library(extrafont)

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

rva_plot <- ggplot() +
  geom_sf(data = df$lines[[1]],
          inherit.aes = FALSE,
          color = vcu_gold,
          size = .6,
          alpha = .8) +
  geom_sf(data = df$lines[[2]],
          inherit.aes = FALSE,
          color = vcu_gold,
          size = .05,
          alpha = .4) +
  coord_sf(
    xlim = c(-77.525, -77.375),
    ylim = c(37.5, 37.6),
    expand = FALSE
  ) +
  labs(
    caption = "RVA"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.border = element_rect(color = "white", fill = NA),
    plot.caption = element_text(hjust = .5, color = vcu_gold, size = 32, family = "Rockwell"),
    plot.margin = unit(c(.2, .4, .2, .4), "cm")
  )

ggsave(here::here("RVA Map/rva_map.jpeg"), plot = rva_plot, device = "jpeg")
