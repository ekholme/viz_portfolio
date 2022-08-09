library(tidyverse)
library(cfg)
library(eemisc)
library(glue)
library(ggtext)

# Wrangle Data ------------------------------------

women_22 <- fetch_leaderboard(division = "women")

scores <- running_scores(women_22)

lh_scores <- scores[scores$athlete == "Laura Horvath", ] |>
    select(event, cum_points, athlete)

# function to get 3rd place for a given event
third_place <- function(x, event) {
    tmp <- event_leaderboard(x, event)

    res <- tmp[tmp$place == 3, c("event", "cum_points")]

    res$athlete <- "third"

    res
}

events <- unique(scores$event)

thirds <- map_dfr(events, ~ third_place(women_22, .x))

x <- bind_rows(lh_scores, thirds)

# Make Plot -----------------------------------
hungary_green <- "#436F4D"
bronze <- "#CD7F32"

p <- ggplot(x, aes(x = event, y = cum_points, color = athlete)) +
    geom_line() +
    geom_point(shape = 21, fill = "white", size = 10) +
    geom_text(aes(label = cum_points)) +
    scale_color_manual(
        values = c(hungary_green, bronze)
    ) +
    labs(
        title = glue("<span style='color:{hungary_green}'>Laura Horvath's</span> Charge to the <span style='color:{bronze}'>Podium</span>"),
        y = "Total Points",
        x = "Event #",
        caption = "Viz: @ekholm_e | Data: Morning Chalk Up"
    ) +
    theme_ee(size = 12) +
    theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_markdown(size = 36, hjust = .5)
    )

ggsave(here::here("Crossfit/lh_comeback.png"), p, device = "png")
