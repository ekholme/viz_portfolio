##creating a streetmap of RVA using the osmdata package

library(tidyverse)
library(osmdata)

rva_streets <- getbb("Richmond Virginia United States") %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "secondary", "tertiary")
                  ) %>%
  osmdata_sf()

