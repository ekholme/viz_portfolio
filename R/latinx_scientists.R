
library(tidyverse)
library(ggtext)
library(ragg)
library(janitor)
library(extrafont)

scientists <- readxl::read_excel(here::here("Data/LatinxScientists.xlsx"))

text_col <- "#FFFFFF"
bckgrnd_col <- "#000000"
family <- "Segoe UI Black"

# Cleaning Data -----------------------------------------------------------


scientists_clean <- scientists %>%
  clean_names() %>%
  mutate(first_name = if_else(str_detect(first_name, "Mexican|Buenos"), NA_character_, first_name),
         last_name = str_remove_all(last_name, ",.*"),
         full_name = if_else(!is.na(first_name), glue::glue("{ first_name } { last_name }"), last_name) %>%
           str_to_title()) %>%
  arrange(full_name) %>%
  mutate(id = row_number())

pad_vals <- c((nrow(scientists_clean) + 1):(2*nrow(scientists_clean)))

scientists_padded <- scientists_clean %>%
  add_row(id = pad_vals) %>%
  mutate(tot_row = nrow(.),
         angle = 360*(id-.5)/tot_row,
         hjust = if_else(angle <= 90, 1, 0),
         use_angle = if_else(angle <= 90, 0 - angle, 180 - angle))


# Creating Plot -----------------------------------------------------------

#file <- here::here("Output/latinx_scientists.png")

#agg_png(file)

ggplot(scientists_padded) +
  geom_segment(aes(x = id, xend = id, y = if_else(is.na(full_name), NA_real_, 0), yend = if_else(is.na(full_name), NA_real_, 4)), color = text_col) +
  geom_text(aes(x = id, y = 4.1, label = full_name, hjust = hjust, angle = use_angle), color = text_col, size = 2, family = family) +
  coord_polar(start = (3*pi)/2) +
  theme_void() +
  labs(
    title = "Notable Latinx Scientists",
    subtitle = ""
  ) +
  theme(
    panel.background = element_rect(fill = bckgrnd_col),
    plot.background = element_rect(fill = bckgrnd_col),
    plot.margin = margin(t = 128, b = -300, r = 115, l = 115),
    plot.title = element_markdown(color = text_col, hjust = .5, family = family, size = 20)
  )

ggsave(here::here("Output/latinx_scientists.png"), device = "png", units = "in", width = 14, height = 9)

#invisible(dev.off())