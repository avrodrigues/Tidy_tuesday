
# load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ragg)
library(here)
library(ggforce)
library(cowplot)


tuesday <- tidytuesdayR::tt_load("2021", week = 18)

departures <- tuesday$departures

glimpse(departures)

d.exec <- duplicated(departures$exec_fullname)
exec <-  unique(departures$exec_fullname[d.exec])
 
n_departures <- 
departures %>% 
  group_by(max_tenure_ceodb) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) 

pos <- seq(0, sqrt(10000), by = 0.5)
set.seed(2020)
data.df <- tibble(
  x0 = sample(c(pos, -pos), 4), 
  y0 = sample(c(pos, -pos), 4), 
  n_departures
)


(planets <- 
 ggplot(
    data.df, 
    aes(x0 = x0, 
        y0 = y0, 
        fill = max_tenure_ceodb,
        color = max_tenure_ceodb)
    ) +
  geom_circle(aes(r = sqrt(n)), show.legend = F) +
  scale_fill_gradient(low = "#7d6128", high = "#debe7b") +
  scale_color_gradient(low = "#7d6128", high = "#debe7b") +
 # scale_color_manual(values = "grey70") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#034e5e", color = NA)
  )
)

title <- "CEO departures in the\nS&P 1500 firms"
subtitle <- 
"How many times has an outgoing 
CEO served as CEO before?"
description <- 
"It is very unusual for an outgoing
CEO be hired as CEO by other
companies. Of a total of 9,423 
layoffs between 2000-2018, only
451 CEOs had been CEO of other 
companies before.
The graph shows how many CEOs
had been CEO in 1, 2, 3 or 4
companies."



# info --------------------------------------------------------------------

info.text <- tibble(
  x = c(0.565, 0.757, 0.73, 0.546),
  y = c(0.1, 0.375, 0.677, 0.685), 
  label = paste0(
    data.df$n, " CEOs\nworked in\n", 
    data.df$max_tenure_ceodb, 
    ifelse(data.df$max_tenure_ceodb == 1, " company", " companies")
  )
)

chart <- 
ggdraw() +
  draw_plot(
    planets,
    y = -0.12,
    x = 0.1,
    scale = 0.75
  ) +
  draw_text(
    title,
    y = 0.915,
    family = "Linux Libertine Capitals",
    size = 28,
    fontface = "bold",
    color = "grey98",
    lineheight = 0.75
  ) +
  draw_text(
    subtitle,
    x = 0.05, 
    y = 0.78,
    family = "Inter",
    size = 12,
    fontface = "bold",
    color = "grey98",
    
    hjust = 0,
    lineheight = 0.85
  ) +
  draw_text(
    description,
    x = 0.05, 
    y = 0.65,
    family = "Inter",
    size = 11,
    color = "grey98",
    hjust = 0,
    lineheight = 0.9
  ) +
  draw_text(
    info.text$label,
    x = info.text$x, 
    y = info.text$y,
    family = "FiraCode",
    size = 8,
    color = "grey98",
    hjust = 0,
    lineheight = 0.85
  ) +
  draw_text(
    "@avrodrigues_ | Source: Gentry et al. (2021). Strategic Management Society",
    x = 0.5, 
    y = 0.02,
    family = "Inter",
    size = 5,
    color = "grey98",
    hjust = 0.5,
    lineheight = 0.85
  ) +
  theme(
    plot.background = element_rect(fill = "#034e5e", color = NA)
  ) 


# save progress -----------------------------------------------------------

agg_png(
  here("2021",
       "week18", 
       "progress", 
       paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
  width = 6,
  height = 8,
  units = "in",
  res = 300
)
chart
dev.off()



# save plot ----------------------------------------------------------------
agg_png(
  here("2021",
       "week18", 
       "departures.png"),
  width = 6,
  height = 8,
  units = "in",
  res = 300
)
chart
dev.off()  
  





