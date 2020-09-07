
##note that much of the script here is heavily "borrowed" from https://themockup.blog/posts/2020-08-28-heatmaps-in-ggplot2/

library(tidyverse)
library(ggtext)
library(patchwork)
library(paletteer)
library(scales)
library(ragg)

fam <- "Roboto Condensed"

# Get Data ----------------------------------------------------------------

souce_url <- "https://raw.githubusercontent.com/ArrowheadAnalytics/next-gen-scrapy-2.0/master/pass_and_game_data.csv"

pass_map_df <- read_csv(souce_url) %>%
  na.omit() %>%
  select(-X1)

wash_df <- pass_map_df %>%
  filter(team == "washington-football-team" & season == 2019)


# Make Field --------------------------------------------------------------

not_div_5 <- function(x) {
  # select only elements of the vector not divisible by 5
  x[x %% 5 != 0]
}

center_df <- tibble(
  x_coord = c(rep(-3.1, 60), rep(3.1, 60)),
  y_coord = seq(-14, 59, 1) %>% rep(2) %>% not_div_5(),
  text = "--"
)

# line labels
annotate_df <- tibble(
  x_coord = c(12.88, -12.88) %>% rep(each = 5),
  y_coord = seq(10, 50, 10) %>% rep(2),
  text = seq(10, 50, 10) %>% rep(2) %>% str_replace("(.)(.)", "\\1 \\2"),
  rotation = c(90, 270) %>% rep(each = 5)
)

# yardlines
yardline_df <- tibble(
  y = seq(-15, 60, 5),
  yend = seq(-15, 60, 5),
  x = rep(-56 / 2, 16),
  xend = rep(56 / 2, 16)
)

# sidelines
sideline_df <- tibble(
  y = c(-15.15, -15.15),
  yend = c(60.15, 60.15),
  x = c(-56 / 2, 56 / 2),
  xend = c(-56 / 2, 56 / 2)
)

add_field <- function() {
  list(
    coord_cartesian(
      xlim = c(-53.333 / 2, 53.333 / 2),
      ylim = c(-15, 60)
    ),
    geom_text(
      data = annotate_df, aes(label = text, angle = rotation),
      color = front_col, size = 8
    ),
    geom_segment(
      data = yardline_df, color = front_col, size = 1,
      aes(x = x, y = y, xend = xend, yend = yend)
    ),
    geom_segment(
      x = -56 / 2, y = 0, xend = 56 / 2, yend = 0,
      color = "blue", size = 1, alpha = 0.5
    ),
    geom_segment(
      data = sideline_df, color = front_col, size = 2,
      aes(x = x, y = y, xend = xend, yend = yend)
    ),
    geom_text(
      data = center_df,
      aes(label = text), color = front_col, vjust = 0.32
    ),
    theme_void(),
    theme(
      strip.text = element_text(size = 20, color = front_col),
      plot.background = element_rect(fill = back_col, color = NA),
      legend.position = "none",
      plot.margin = unit(c(2, 1, 0.5, 1), unit = "cm"),
      plot.caption = element_text(color = front_col),
      plot.title = element_text(color = front_col),
      plot.subtitle = element_text(color = front_col),
      panel.background = element_rect(fill = back_col, color = NA),
      panel.border = element_blank()
    )
  )
}

add_field_notext <- function() {
  list(
    coord_cartesian(
      xlim = c(-53.333 / 2, 53.333 / 2),
      ylim = c(-15, 60)
    ),
    geom_segment(
      data = yardline_df, color = front_col, size = 1,
      aes(x = x, y = y, xend = xend, yend = yend)
    ),
    geom_segment(
      x = -56 / 2, y = 0, xend = 56 / 2, yend = 0,
      color = "blue", size = 1, alpha = 0.5
    ),
    geom_segment(
      data = sideline_df, color = front_col, size = 2,
      aes(x = x, y = y, xend = xend, yend = yend)
    ),
    geom_text(
      data = center_df,
      aes(label = text), color = front_col, vjust = 0.32
    ),
    theme_void(),
    theme(
      strip.text = element_text(size = 20, color = front_col),
      plot.background = element_rect(fill = back_col, color = NA),
      legend.position = "none",
      plot.margin = unit(c(2, 1, 0.5, 1), unit = "cm"),
      plot.caption = element_text(color = front_col),
      plot.title = element_text(color = front_col),
      plot.subtitle = element_text(color = front_col),
      panel.background = element_rect(fill = back_col, color = NA),
      panel.border = element_blank()
    )
  )
}

# Set Colors --------------------------------------------------------------

gold <- '#ffb612'
burg <- '#5a1414'
front_col <- "black"
back_col <- "grey70"

wash_cols <- colorRampPalette(c(burg, gold))(10)


# Season Heatmap ----------------------------------------------------------

season <- wash_df %>%
  ggplot(aes(x = x_coord, y = y_coord)) +
  geom_density_2d_filled(aes(fill = ..level..),
                         contour_var = "ndensity",
                         breaks = seq(.1, 1.0, length.out = 10)) +
  scale_y_continuous(breaks = seq(-10, 50, 5)) +
  add_field() +
  scale_fill_manual(values = wash_cols, aesthetics = c("fill", "color")) +
  labs(
    title = "Pass Distribution, 2019 Season"
  ) +
  theme(
    plot.title = element_markdown(hjust = .5, size = 18, face = "bold"),
    plot.title.position = "plot"
  )


# Facets ------------------------------------------------------------------

facets <- wash_df %>%
  mutate(opponent = if_else(home_team == "WAS", glue::glue("vs { away_team }"), glue::glue("at { home_team }")),
         type = if_else(pass_type == "COMPLETE" | pass_type == "TOUCHDOWN", "Complete or TD", "Incomplete or Int")) %>%
  ggplot(aes(x = x_coord, y = y_coord)) +
  facet_wrap(~fct_reorder(opponent, week.x), ncol = 4) +
  scale_y_continuous(breaks = seq(-10, 50, 5)) +
  add_field_notext() +
  geom_point(aes(color = type), size = 2, alpha = .9) +
  scale_color_manual(values = c(gold, burg), name = NULL) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    legend.text = element_markdown(size = 12)
  )


# Annotation --------------------------------------------------------------

inc_text <- "
The plot below shows the distribution of passes across the<br>entire 2019 season, with more passes thrown to <span style=color:'#ffb612'>**gold**</span> areas<br>than to <span style=color:'#5a1414'>**burgundy**</span> areas. The plot to the right shows each pass<br>thrown by  the game. Across all plots, the blue line represents the<br>line of scrimmage. Pass data doesn't include any yards after catch."


text_plot <- ggplot() +
  annotate("richtext", x = 2, y = 25, label = inc_text, label.color = NA, fill = NA, fontface = "italic", size = 5.5) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = back_col)
  )

# Patching ----------------------------------------------------------------


layout <- c(
  area(t = 1, l = 1, b = 1, r = 1),
  area(t = 2, l = 1, b = 3, r = 1),
  area(t = 1, l = 2, b = 3, r = 2)
)


p <- text_plot + season + facets + 
  plot_layout(design = layout) +
  plot_annotation(title = "<span style=color:'#5a1414'>**Washington**</span> Football Team Passing, 2019",
                  caption = "Viz: Eric Ekholm (@ekholm_e) | Data: @ChiefsAnalytics",
                  theme = theme(
                    plot.title = element_markdown(size = 26, hjust = .5)
                  )) &
  theme(
    panel.background = element_rect(fill = back_col, color = NA),
    plot.background = element_rect(fill = back_col, color = NA),
    plot.title.position = "plot",
    plot.title = element_markdown(hjust = .5),
    text = element_text(family = fam),
    plot.margin = margin(t = 20, b = 20, l = 10, r = 10),
    plot.caption = element_markdown(size = 11, face = "italic")
  )


# Gen Plot ----------------------------------------------------------------

file <- here::here("NFL Viz/wash.png")

agg_png(file, width = 1200, height = 900, units = "px")
p
invisible(dev.off())