# libraries ---------------------------------------------------------------
library(tidyverse)
library(extrafont)
library(sysfonts)
library(glue)
library(lubridate)
library(httr)
library(ggtext)  ## devtools::install_github("clauswilke/ggtext")
library(grid)
library(ggrepel)
library(gganimate)

# just once
# font_add_google("Montserrat")
# font_import()

# get and read data -------------------------------------------------------
url_list <- list(
  canizal = "https://raw.githubusercontent.com/RUsersAsturias/contest/gh-pages/_data/Canizal.RData",
  carrizal = "https://raw.githubusercontent.com/RUsersAsturias/contest/gh-pages/_data/Carrizal.RData",
  granja = "https://raw.githubusercontent.com/RUsersAsturias/contest/gh-pages/_data/Granja.RData"
)

if(length(list.files("./data")) == 0) {
  iwalk(url_list, ~ GET(.x, write_disk(paste0("./data/", .y, ".RData"))))
}

load(list.files("./data", pattern = "RData$", full.names = TRUE)[1])
load(list.files("./data", pattern = "RData$", full.names = TRUE)[2])
load(list.files("./data", pattern = "RData$", full.names = TRUE)[3])

# transform ---------------------------------------------------------------
canizal <- canizal %>%
  set_names(~ tolower(.))

carrizal <- carrizal %>%
  set_names(~ tolower(.))

granja <- granja %>%
  set_names(~ tolower(.)) %>% 
  mutate(estacion = "granja")

avg_temp_day <-
  canizal %>%
  group_by(year      = year(timestamp),
           month_day = strftime(timestamp, "%m/%d")) %>%
  summarise(temp_day_avg = mean(temp_aire_avg)) %>%
  ungroup()

vpd_avg_day <-
  canizal %>%
  group_by(year      = year(timestamp),
           month_day = strftime(timestamp, "%m/%d")) %>%
  summarise(vpd_avg = mean(vpd_kpa_avg)) %>%
  ungroup()

# plot avg temp -------------------------------------------------------
avg_temp_plot <- 
  avg_temp_day %>% 
  filter(!year %in% c(2008, 2019)) %>% 
  ggplot(aes(x = month_day, y = year, fill = temp_day_avg)) +
  geom_tile(alpha = .8) +
  scale_x_discrete(breaks = sprintf("%02d/%02d", 1:12, 1),
                   labels = month.abb) +
  scale_y_reverse(breaks = 2009:2018, expand = c(0, 0)) +
  scale_fill_gradientn(colors = hcl.colors(8, "Berlin"),
                       breaks = seq(min(avg_temp_day$temp_day_avg), max(avg_temp_day$temp_day_avg), 4),
                       labels = function(x) round(x, 1),
                       guide = guide_colorbar(title = expression("Temperature " (degree ~ C)),
                                              title.position = "top",
                                              title.hjust = .5,
                                              direction = "horizontal",
                                              label.position = "bottom",
                                              barheight = .5,
                                              barwidth = 20,
                                              ticks = FALSE
                       )) +
  labs(title = "Average daily temperature since 2009 in Cañizal, Zamora",
       subtitle = "Recorded by the automatic weather station belonging to REMEDHUS",
       caption = "Graphic: Pablo Rodriguez @pabrodez | Data: https://rusersasturias.github.io/contest/") +
  theme_minimal(base_family = "Montserrat") +
  theme(axis.title = element_blank(),
        text = element_text(color = "grey80"),
        axis.text = element_text(family = "Ubuntu Mono", color = "grey80"),
        plot.background = element_rect(fill = "grey20", color = "grey20"),
        panel.background = element_rect(fill = "grey20", color = "grey20"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 20, color = "white", margin = margin(t = .5, b = .5, unit = "cm")),
        plot.subtitle = element_text(margin = margin(b = 3, unit = "cm")),
        legend.margin = margin(t = .5, unit = "cm"),
        legend.position = "bottom",
        legend.text = element_text(family = "Ubuntu Mono", size = 8),
        plot.caption = element_text(margin = margin(t = 4, unit = "cm")),
        plot.margin = margin(.5, 1, .5, 1, unit = "cm"))

ggsave(plot = avg_temp_plot, filename = "./plots/avg_temp_day.png", height = 8, width = 12, dpi = "retina")

# plot avg hum -------------------------------------------------------
avg_vpd_plot <- 
  vpd_avg_day %>% 
  filter(!year %in% c(2008, 2019)) %>% 
  ggplot(aes(x = month_day, y = year, fill = vpd_avg)) +
  geom_tile(alpha = .8) +
  scale_x_discrete(breaks = sprintf("%02d/%02d", 1:12, 1),
                   labels = month.abb) +
  scale_y_reverse(breaks = 2009:2018, expand = c(0, 0)) +
  scale_fill_gradientn(colors = hcl.colors(8, "Berlin"),
                       breaks = seq(min(vpd_avg_day$vpd_avg), max(vpd_avg_day$vpd_avg)),
                       labels = function(x) round(x, 2),
                       guide = guide_colorbar(title = "Kilopascals (kPa)",
                                              title.position = "top",
                                              title.hjust = .5,
                                              direction = "horizontal",
                                              label.position = "bottom",
                                              barheight = .5,
                                              barwidth = 20,
                                              ticks = FALSE
                       )) +
  labs(title = "Average daily vapour-pressure deficit since 2009 in Cañizal, Zamora",
       subtitle = "Recorded by the automatic weather station belonging to REMEDHUS",
       caption = "Graphic: Pablo Rodriguez @pabrodez | Data: https://rusersasturias.github.io/contest/") +
  theme_minimal(base_family = "Montserrat") +
  theme(axis.title = element_blank(),
        text = element_text(color = "grey80"),
        axis.text = element_text(family = "Ubuntu Mono", color = "grey80"),
        plot.background = element_rect(fill = "grey20", color = "grey20"),
        panel.background = element_rect(fill = "grey20", color = "grey20"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 20, color = "white", margin = margin(t = .5, b = .5, unit = "cm")),
        plot.subtitle = element_text(margin = margin(b = 3, unit = "cm")),
        legend.margin = margin(t = .5, unit = "cm"),
        legend.position = "bottom",
        legend.text = element_text(family = "Ubuntu Mono", size = 8),
        plot.caption = element_text(margin = margin(t = 4, unit = "cm")),
        plot.margin = margin(.5, 1, .5, 1, unit = "cm"))

ggsave(plot = avg_vpd_plot, filename = "./plots/avg_vpd_day.png", height = 8, width = 12, dpi = "retina")

# plot solar radiation -------------------------------------------------
avg_radiation_plot <- 
canizal %>% 
  filter(!year(timestamp) %in% c(2008, 2019)) %>% 
  group_by(year = year(timestamp), day = yday(timestamp)) %>% 
  summarise(radiation_avg = mean(radiacion_avg)) %>% 
  ggplot(aes(x = day, y = radiation_avg)) +
  geom_point(data = . %>% filter(year == 2009), color = "lightblue", alpha = .7) +
  geom_point(data = . %>%  filter(year == 2018), color = "orchid4", alpha = .7) +
  theme_minimal(base_family = "Montserrat") +
  theme(axis.title = element_blank(),
        text = element_text(color = "grey80"),
        axis.text = element_text(family = "Ubuntu Mono", color = "grey80"),
        plot.background = element_rect(fill = "grey20", color = "grey20"),
        panel.background = element_rect(fill = "grey20", color = "grey20"),
        panel.border = element_rect(fill = "transparent", color = "transparent"),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 20, color = "white", margin = margin(t = .5, b = .5, unit = "cm")),
        plot.subtitle = element_text(margin = margin(b = 3, unit = "cm")),
        legend.margin = margin(t = .5, unit = "cm"),
        legend.position = "bottom",
        legend.text = element_text(family = "Ubuntu Mono", size = 8),
        plot.caption = element_text(margin = margin(t = 4, unit = "cm")),
        plot.margin = margin(.5, 1, .5, 1, unit = "cm"))


# plot avg temp 2 ---------------------------------------------------------

annotations <- tibble(x = c(rep(as.Date("2009-01-13"), 2), as.Date("2009-02-25"), as.Date("2009-02-15")),
                      y = c(14, 15, 14, 15),
                      label = c("<span style='color: #009E73'>2009</span>", "<span style='color: #D55E0080'>2018</span>",
                                "<span style='color: #009E73'>Average decrease</span>", "<span style='color: #D55E0080'> Average increase</span>")
                      )

curves <- tibble(x = c(as.Date("2009-01-03"), as.Date("2009-01-23"), as.Date("2009-03-13"), as.Date("2009-02-15")),
                 y = c(11, 10.5, 12, 8),
                 xend = c(as.Date("2009-01-08"), as.Date("2009-01-18"), as.Date("2009-02-225"), as.Date("2009-02-04")),
                 yend = c(15, 14, 13.5, 14.5))

curve_2009 <- curves[2, ]

curves <- slice(curves, -2)

avg_temp_plot2 <-
  canizal %>%
  filter(year(timestamp) %in% c(2009, 2018)) %>%
  mutate(year = year(timestamp), day = yday(timestamp)) %>%
  group_by(year, day) %>%
  summarise(avg_temp = mean(temp_aire_avg)) %>%
  pivot_wider(
    names_from = "year",
    names_prefix = "year_",
    values_from = "avg_temp"
  ) %>%
  mutate(day = as.Date(day - 1, origin = "2009-01-01")) %>%
  ggplot() +
  geom_linerange(
    data = . %>% mutate(
      diff_temp = case_when(
        sign(year_2018 - year_2009) == 1 ~ "Increase",
        sign(year_2018 - year_2009) == -1 ~ "Decrease",
        sign(year_2018 - year_2009) == 0 ~ "Same"
      )
    ),
    aes(
      x = day,
      ymin = year_2009,
      ymax = year_2018,
      color = diff_temp
    )
  ) +
  geom_point(
    data = . %>% pivot_longer(-day, names_to = "year", values_to = "avg_temp"),
    aes(x = day, y = avg_temp, fill = year),
    color = "grey30",
    alpha = .8,
    shape = 21
  ) +
  scale_x_date(
    breaks = seq.Date(as.Date("2009-01-01"), as.Date("2009-12-31"), "1 month"),
    date_labels = "%b", limits = c(as.Date("2009-01-01"), as.Date("2009-12-31"))
  ) +
  scale_y_continuous(labels = function(x) glue("{x}°C")) +
  scale_color_manual(values = c("#009E7380", "#D55E0080"), guide = "none") +
  scale_fill_manual(values = c("#009E7380", "#D55E0080"), guide = "none") +
  annotate("text",
    label = "Average daily temperature for 2009 and 2018 in Cañizal, Zamora (Spain)",
    x = as.Date("2009-04-25"), y = 3, family = "Montserrat", fontface = "bold", color = "white", hjust = 0, size = 4.5
  ) +
  annotate("text",
    label = "Recorded by the automatic weather station belonging to REMEDHUS",
    x = as.Date("2009-04-25"), y = 1, family = "Montserrat", color = "grey80", hjust = 0
  ) +
  labs(caption = "Graphic: Pablo Rodriguez @pabrodez | Data: https://rusersasturias.github.io/contest/") +
  geom_richtext(
    data = annotations, 
    aes(x = x, y = y, label = label),
    fill = NA, label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt"),
    family = "Montserrat",
    fontface = "bold"
  ) +
  annotate("curve", x = curves$x, y = curves$y, xend = curves$xend, yend = curves$yend,
           linetype = "dotted", color = "white", curvature = -.5) +
  annotate("curve", x = curve_2009$x, y = curve_2009$y, xend = curve_2009$xend, yend = curve_2009$yend, 
           curvature = .5, linetype = "dotted", color = "white") +
  theme_minimal(base_family = "Ubuntu Mono") +
  theme(
    axis.title = element_blank(),
    text = element_text(color = "grey80"),
    axis.text = element_text(color = "grey80"),
    plot.background = element_rect(fill = "grey20", color = "grey20"),
    panel.background = element_rect(fill = "grey20", color = "grey20"),
    panel.border = element_rect(fill = "transparent", color = "transparent"),
    panel.grid.major.y = element_line(color = alpha("grey30", .5)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 20,
      color = "white",
      margin = margin(t = .5, b = .5, unit = "cm")
    ),
    plot.subtitle = element_text(margin = margin(b = 3, unit = "cm")),
    legend.margin = margin(t = .5, unit = "cm"),
    legend.text = element_text(size = 8),
    plot.caption = element_text(margin = margin(t = 4, unit = "cm")),
    plot.margin = margin(.5, 1, .5, 1, unit = "cm"),
    legend.direction = "horizontal",
    legend.justification = "left"
  )

ggsave(plot = avg_temp_plot2, filename = "./plots/avg_temp_plot2.png", height = 10, width = 18, dpi = "retina")


# plot avg temp 3 stations -------------------------------------------------------------------------
avg_temp_plot3 <- 
bind_rows(carrizal, canizal, granja) %>% 
  filter(year(timestamp) %in% c(2010, 2018)) %>% 
  select(timestamp, estacion, temp_aire_avg) %>%
  group_by(estacion, year = year(timestamp), md = format(timestamp, "%m-%d")) %>% 
  summarise(temp_avg = mean(temp_aire_avg)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "year", values_from = "temp_avg") %>% 
  mutate(avg_diff = `2018` - `2010`,
         change = case_when(sign(avg_diff) == 1 ~ "increase",
                            sign(avg_diff) == -1 ~ "decrease",
                            sign(avg_diff) == 0 ~ "same")) %>% 
  ggplot() +
  geom_linerange(aes(x = md, ymin = `2010`, ymax = `2018`, color = change)) +
  geom_point(data = . %>% pivot_longer(cols = `2010`:`2018`, names_to = "year", values_to = "avg_temp"),
             aes(x = md, y = avg_temp, fill = year), shape = 21) +
  scale_x_discrete(breaks = 20) + 
  scale_y_continuous(labels = function(x) glue("{x}°C")) +
  facet_wrap(~ estacion, ncol = 1) +
  theme_minimal(base_family = "Ubuntu Mono") +
  theme(
    axis.title = element_blank(),
    text = element_text(color = "grey80"),
    axis.text = element_text(color = "grey80"),
    plot.background = element_rect(fill = "grey20", color = "grey20"),
    panel.background = element_rect(fill = "grey20", color = "grey20"),
    panel.border = element_rect(fill = "transparent", color = "transparent"),
    panel.grid.major.y = element_line(color = alpha("grey30", .5)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 20,
      color = "white",
      margin = margin(t = .5, b = .5, unit = "cm")
    ),
    plot.subtitle = element_text(margin = margin(b = 3, unit = "cm")),
    legend.margin = margin(t = .5, unit = "cm"),
    legend.text = element_text(size = 8),
    plot.caption = element_text(margin = margin(t = 4, unit = "cm")),
    plot.margin = margin(.5, 1, .5, 1, unit = "cm"),
    legend.direction = "horizontal",
    legend.justification = "left"
  )

# plot precipitations 3 stations ------------------------------------------
prec_stations_df <- 
bind_rows(canizal, granja, carrizal) %>% 
  select(estacion, timestamp, lluvia_tot) %>% 
  group_by(estacion, year = as.character(year(timestamp)), md = format(timestamp, "%m/%d")) %>% 
  summarise(sum_prec = sum(lluvia_tot)) %>% 
  mutate(csum_pre = cumsum(sum_prec)) %>% 
  ungroup() %>% 
  filter(!year %in% c("2008", "2009", "2019")) %>% 
  mutate(alpha = if_else(year %in% c("2010", "2018"), 1, .3),
         estacion = str_to_title(estacion))

labels_years <- 
  prec_stations_df %>% 
  group_by(estacion, year) %>% 
  summarise(max_pre = sum(sum_prec)) %>% 
  ungroup()

cummu_prec_plot <- 
ggplot(prec_stations_df, aes(x = md, y = csum_pre, group = year, color = year)) +
  geom_step(aes(alpha = alpha)) +
  scale_x_discrete(breaks = sprintf("%02d/%02d", 1:12, 1),
                   labels = month.abb,
                   expand = expand_scale(add = c(0, 25))) +
  scale_color_manual(values = c("#E69F00", "#999999", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "white")) + 
  geom_text_repel(data = labels_years, aes(x = "12/31", y = max_pre, label = year, group = estacion), 
                  hjust = 0, xlim = c(365, NA)) +
  scale_y_continuous(labels = function(x) glue("{x}mm")) +
  scale_alpha_identity() +
  facet_wrap(~ estacion) +
  theme_minimal(base_family = "Ubuntu Mono", base_size = 14) +
  labs(title = "Cummulative precipitations from 2010 to 2018 in Zamora (Spain)",
       subtitle = "As recorded by the automatic weather stations (named below) belonging to REMEDHUS",
       caption = "Graphic: Pablo Rodriguez @pabrodez | Data: https://rusersasturias.github.io/contest/") +
  theme(
    axis.title = element_blank(),
    text = element_text(color = "grey80"),
    axis.text = element_text(color = "grey80"),
    plot.background = element_rect(fill = "grey20", color = "grey20"),
    panel.background = element_rect(fill = "grey20", color = "grey20"),
    panel.border = element_rect(fill = "transparent", color = "transparent"),
    panel.grid.major.y = element_line(color = alpha("grey30", .5)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      family = "Montserrat",
      face = "bold",
      size = 20,
      color = "white",
      margin = margin(t = .5, b = .5, unit = "cm")
    ),
    plot.subtitle = element_text(margin = margin(b = 3, unit = "cm")),
    legend.margin = margin(t = .5, unit = "cm"),
    legend.text = element_text(size = 8),
    plot.caption = element_text(size = 11, margin = margin(t = 4, unit = "cm")),
    plot.margin = margin(.5, 1, .5, 1, unit = "cm"),
    legend.position = "none",
    strip.text = element_text(color = "white", size = 13)
  )

ggsave(filename = "./plots/cummu_prec_plot.png", width = 20, height = 10, dpi = "retina")
