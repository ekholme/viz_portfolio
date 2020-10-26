
library(tidyverse)
library(gt)
library(lubridate)


# Cleaning and Setup ------------------------------------------------------

#loading in broadway data from wk 18 of TidyTuesday 2020
grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)

#removing 2020 bc it's not yet complete & is an awkward year
grosses <- grosses %>%
  filter(year(week_ending) != 2020)

#getting top 20 selling shows
top_shows <- grosses %>%
  count(show, wt = weekly_gross, sort = TRUE) %>%
  slice_max(order_by = n, n = 10) %>%
  pull(show)

#getting data for line plot
annual_sales <- grosses %>%
  filter(show %in% top_shows) %>%
  mutate(year = year(week_ending)) %>%
  group_by(show, year) %>%
  summarize(tix = sum(seats_sold, na.rm = TRUE)) %>%
  ungroup()

#setting values for plots/tables
labs_col <- '#d9b51c'
labs_back <- '#373737'
bckgrnd <- '#474747'
bck_light <- '#515151'
text_col <- '#fdfdfd'
font <- google_font("Rubik")
lob <- google_font("Lobster")

bckgrnd_vec <- rep(c(bckgrnd, bck_light), times = 5)

#getting playbill img urls
playbills <- c("https://bsp-static.playbill.com/dims4/default/25f6db1/2147483647/resize/x250%3C/quality/90/?url=http%3A%2F%2Fpb-asset-replication.s3.amazonaws.com%2F06%2Ffb%2Ff6ba0a7c40ca918068a0fdc04bf7%2Fthe-lion-king-playbill-2017-11-web.jpg",
               "https://bsp-static.playbill.com/dims4/default/823dbab/2147483647/resize/x250%3C/quality/90/?url=http%3A%2F%2Fpb-asset-replication.s3.amazonaws.com%2Feb%2Fc2%2F90b25fdb41f7a75d2f9532ff7da4%2Fwicked-playbill-2017-05-web.jpg",
               "https://bsp-static.playbill.com/dims4/default/a4bd340/2147483647/resize/x250%3C/quality/90/?url=http%3A%2F%2Fpb-asset-replication.s3.amazonaws.com%2F50%2Fa4%2Fa71805ca472a8f12c97cac39f1e7%2Fthe-phantom-of-the-opera-playbill-2019-01-01-web.jpg",
               "https://bsp-static.playbill.com/dims4/default/641d7e2/2147483647/resize/x250%3C/quality/90/?url=http%3A%2F%2Fpb-asset-replication.s3.amazonaws.com%2F1d%2Fa9%2F224d538645058e0877d400321174%2Fchicago-playbill-2020-01-01-web.jpg",
               "https://bsp-static.playbill.com/dims4/default/38f17a2/2147483647/resize/x250%3C/quality/90/?url=http%3A%2F%2Fpb-asset-replication.s3.amazonaws.com%2F8e%2Fc9%2F60189ac7483b86d4f26f0d26d85e%2Fthe-book-of-mormon-playbill-2011-2-24-web.jpg",
               "https://bsp-static.playbill.com/dims4/default/5101191/2147483647/resize/x250%3C/quality/90/?url=http%3A%2F%2Fpb-asset-replication.s3.amazonaws.com%2F23%2Fde%2F41ceb4094b0d9b7c9e53a51f2731%2Fmamma-mia-playbill-2001-10-05-web.jpg",
               "https://bsp-static.playbill.com/dims4/default/9ce9b12/2147483647/resize/x250%3C/quality/90/?url=http%3A%2F%2Fpb-asset-replication.s3.amazonaws.com%2Fb2%2Fd5%2Fd007cd9d468f8b0ed107c00d119e%2Fhamilton-playbill-2018-01-web.jpg",
               "https://bsp-static.playbill.com/dims4/default/a66da3d/2147483647/resize/x250%3C/quality/90/?url=http%3A%2F%2Fpb-asset-replication.s3.amazonaws.com%2F5f%2F0d%2F9372f5f347ee824597d030323716%2Fjersey-boys-playbill-2005-10-17-web.jpg",
               "https://bsp-static.playbill.com/dims4/default/b0dcc9c/2147483647/resize/x250%3C/quality/90/?url=http%3A%2F%2Fpb-asset-replication.s3.amazonaws.com%2F8f%2Fd7%2F3d838c6441c192023d35429c4de5%2Fles-miserables-playbill-2014-03-01-web.jpg",
               "https://bsp-static.playbill.com/dims4/default/4b36540/2147483647/resize/x250%3C/quality/90/?url=http%3A%2F%2Fpb-asset-replication.s3.amazonaws.com%2F41%2F3d%2Fc505cabe4e0fb51d74e83f26b453%2Faladdin-playbill-2018-04-01-web.jpg")

#creating plot function
plot_line <- function(show, col = bckgrnd) {
  annual_sales %>%
    filter(show == {{ show }}) %>%
    ggplot(aes(x = year, y = tix)) +
    geom_line(size = 2, color = text_col) +
    scale_x_continuous(limits = c(min(annual_sales$year) - 1, max(annual_sales$year) + 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = col, color = col),
      panel.background = element_rect(fill = col, color = col)
    )
}

#set up tbl
tbl <- grosses %>%
  filter(show %in% top_shows) %>%
  group_by(show) %>%
  summarize(yr_of_premier = min(year(week_ending)),
            total_gross = sum(weekly_gross),
            avg_capacity = mean(pct_capacity),
            tix_year = sum(seats_sold)/(max(year(week_ending)) - min(year(week_ending)) + 1)) %>%
  arrange(desc(total_gross)) %>%
  mutate(plots = map2(show, bckgrnd_vec, plot_line),
         ranking = row_number(),
         playbills = playbills)


# Creating Table ----------------------------------------------------------

styled_tbl <- tbl %>%
  select(ranking, show, playbills, everything()) %>%
  gt() %>%
  opt_table_font(
    font = font
  ) %>%
  opt_row_striping() %>%
  tab_header(
    title = md("**Top Earning Broadway Shows**"),
    subtitle = md("*through 2019*")
  ) %>%
  cols_label(
    ranking = "Rank",
    show = "Show",
    playbills = "",
    yr_of_premier = "Premiere Year",
    total_gross = "Total Earnings",
    avg_capacity = md("Avg Theater<br>Capacity Filled"),
    tix_year = "Average",
    plots = "1987 - 2019"
  ) %>%
  tab_spanner(
    label = "Tickets Sold Per Year",
    columns = vars(tix_year, plots)
  ) %>%
  tab_style(
    style = cell_text(font = lob, align = "center", size = px(50), color = labs_col),
    locations = cells_title("title")
  ) %>%
  tab_style(
    style = cell_fill(color = labs_back),
    location = cells_title("title")
  ) %>%
  tab_style(
    style = cell_text(font = lob, align = "center", size = px(12), color = labs_col),
    locations = cells_title("subtitle")
  ) %>% 
  tab_style(
    style = cell_fill(color = labs_back),
    locations = cells_title("subtitle")
  ) %>%
  tab_style(
    style = cell_text(size = px(15), style = "italic"),
    locations = list(cells_column_labels(vars(ranking, show, yr_of_premier, total_gross, avg_capacity)), cells_column_spanners(everything()))
  ) %>%
  tab_style(
    style = cell_text(size = px(12), style = "italic", align = "center"),
    locations = cells_column_labels(vars(tix_year, plots))
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(everything())
  ) %>%
  tab_style(
    style = cell_borders(sides = c("top", "bottom"), color = bckgrnd),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  ) %>%
  text_transform(locations = cells_body(vars(plots)),
                 fn = function(x) {
                   map(tbl$plots, ggplot_image)
                 }) %>%
  text_transform(
    locations = cells_body(vars(playbills)),
    fn = function(x) {
      map(tbl$playbills, ~web_image(.x, height = 50))
    }
  ) %>%
  fmt_currency(
    columns = vars(total_gross),
    currency = "USD",
    decimals = 2,
    suffixing = TRUE
  ) %>%
  fmt_number(
    columns = vars(tix_year),
    decimals = 0,
    sep_mark = ",",
    use_seps = TRUE
  ) %>%
  fmt_percent(
    columns = vars(avg_capacity),
    decimals = 1
  ) %>%
  tab_options(
    column_labels.background.color = labs_back,
    table.background.color = bckgrnd,
    table.font.color = text_col,
    data_row.padding = -30,
    row.striping.background_color = bck_light
  ) %>%
  tab_source_note(
    source_note = md("**Data:** Playbill | **Table:** Eric Ekholm (@ekholm_e)")
  )

styled_tbl

#gtsave(styled_tbl, here::here("Output/broadway_tbl.png"), expand = -.5)
