
# load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ragg)
library(here)
library(cowplot)


# load data ---------------------------------------------------------------

tuesdata  <- tidytuesdayR::tt_load(2021, week = 17)
netflix <- tuesdata$netflix

month.levels <- c("January", "February", "March", "April", "May", "June", "July",
                  "August", "September", "October", "November", "December")

netflix_month <- netflix %>% 
  mutate(
    date_added = str_remove(date_added, "^ "),
    month_added = word(date_added, 1),
    year_added = as.double(word(date_added, sep = ",", start = 2))) %>% 
  drop_na() %>% 
  mutate(month_added = factor(month_added, levels = month.levels)) %>% 
  group_by(month_added, year_added) %>% 
  summarise(n.title = n()) %>% 
  group_by(month_added) %>% 
  summarise(mean.title.added = mean(n.title))


# plot --------------------------------------------------------------------

bg.col = "grey10"
red.net.dark <- "#661111"
red.net = "#CD2323"
grey.net = "#f4f4f4"


net_plot <- 
ggplot(netflix_month, aes(x = month_added, y = mean.title.added)) +
  geom_col(aes(fill = mean.title.added), show.legend = F) +
  scale_fill_gradient(low = red.net.dark, high = red.net) +
  geom_hline(yintercept = 70, linetype = 3, color = "grey30") +
  geom_text(
    aes(y = 70, label = month_added),
    family = "Ubuntu",
    size = 3,
    color = grey.net) +
  geom_text(
    aes(y = mean.title.added - 6, label = round(mean.title.added)),
    family = "Cabin") +
  coord_polar() +
  theme_void() +
  labs(
    subtitle = "NETFLIX",
    title = "Average number of titles released each month in",
    caption = "@avrodrigues_ | Source: Kaggle") +
  theme(
    plot.subtitle = element_text(
      family = "Source Sans Pro",
      face = "bold", 
      hjust = 0.5,
      size = 60,
      color = red.net, 
      margin = margin(t = 5, b = 10)
    ), 
    plot.title = element_text(
      family = "Source Sans Pro",
      face = , 
      hjust = 0.5,
      size = 13,
      color = grey.net,
      margin = margin(t = 20, b = 5)
    ),
    plot.caption = element_text(
      family = "Source Sans Pro",
      face = , 
      hjust = 0.5,
      size = 6,
      color = grey.net
    ),
    plot.background = element_rect(fill = bg.col, color = NA)
)

net_title <- get_title(net_plot)
net_sub <- get_subtitle(net_plot)
net_plot2 <- net_plot +
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank())


chart <- 
ggdraw() +
  draw_plot(net_plot2, y = -0.1) +
  draw_plot(net_title, y = 0.42) +
  draw_plot(net_sub, y = 0.32) +
  theme(plot.background = element_rect(fill = "grey10"))



# save progress -----------------------------------------------------------

agg_png(
  here("2021",
       "week17", 
       "progress", 
       paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
  width = 5,
  height = 7,
  units = "in",
  res = 300
)
chart
dev.off()



# save plot ----------------------------------------------------------------
agg_png(
  here("2021",
       "week17", 
       "netflix.png"),
  width = 5,
  height = 7,
  units = "in",
  res = 300
)
chart
dev.off()
