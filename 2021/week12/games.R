
# load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(ragg)
library(lubridate)
library(ggforce)
library(magick)
library(colorfindr)
library(ggfx)

addUnits <- function(n) {
  n <- abs(n)
  labels <- ifelse(
    n < 1000, n,  # less than thousands
    ifelse(
      n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
      ifelse(
        n < 1e9, paste0(round(n/1e6, 1), 'M'),  # in millions
        ifelse(
          n < 1e12, paste0(round(n/1e9), 'B'), # in billions
          ifelse(
            n < 1e15, paste0(round(n/1e12), 'T') # in trillions
          )))))
  return(labels)
}


# load data ---------------------------------------------------------------

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

top_10_peak <- 
games %>% 
  group_by(gamename) %>% 
  summarise(max_peak = max(peak)) %>% 
  arrange(desc(max_peak)) %>% 
  slice_max(max_peak, n = 10)

top_10 <- 
games %>% 
  filter(peak %in% top_10_peak$max_peak) %>% 
  mutate(peak_date = paste(substr(month, 1,3), year))
top_10$gamename[6] <- "HITMAN 2"

top_10$gamename <- factor(top_10$gamename )
top_10$gamename <- fct_reorder(top_10$gamename, 
                               top_10$peak)


# color pallete ----------------------------------------------------------

# image from: https://unsplash.com/photos/W53FwtFtoM0
ref.imgs <- list.files(here("2021", "week12", "pallete_ref_opt"),
                       full.names = T, pattern = ".jpg")

# rescale images to get_colors be faster

img1 <- image_read(ref.imgs[1])
img1 <- image_scale(img1, 200)
image_write(img1, here("2021", "week12", "pallete_ref_opt", "mgk_1.png"),
            format = "png")

ref.imgs2 <- list.files(here("2021", "week12", "pallete_ref_opt"),
                        full.names = T, pattern = ".png")

set.seed(1985)
gamer <- 
  get_colors(ref.imgs2[2]) %>% 
  make_palette(n = 8)


# set color ---------------------------------------------------------------

back <- gamer[1] 
back2 <- gamer[3]

primary <- gamer[8]
secondary <- gamer[5]
tertiary <- gamer[2]

barplot(rep(1, 8), rep(1, 8), col = gamer)

# plot --------------------------------------------------------------------



(g.plot <- 
ggplot(top_10, 
       aes(y = gamename, x = peak)) +
  with_outer_glow(
    geom_link(aes(xend = 0, yend = gamename),
              lineend = "round", color = primary,
              size = 4), 
    colour = primary,
    sigma = 7
    )+
    scale_x_continuous(labels = addUnits, 
                       n.breaks = 5, 
                       expand = expansion(mult = .0),
                       limits = c(0, 4e6)) +
    geom_text(aes(label = peak_date, x = peak + 2.5e5), 
              size = 2,
              family = "Cabin",
              fontface = "bold",
              color = tertiary) +
  theme_bw() +
    theme(
      axis.text = element_text(
        family = "Bebas",
        colour = primary,
        ), 
      axis.title = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = secondary),
      panel.grid.minor.x = element_line(color = secondary),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(
        fill = back2
      )
      
    )
)

g.plot.title <- 
g.plot +
  labs(
    title = "Top 10 simultaneous players peak events",
    subtitle = 
      "The higher peak in simultaneous players for each game and when the peak occured on Steam.",
    caption = "@avrodrigues | source: Steam "
  ) +
  theme(plot.title = element_text(color = "white", family = "Cabin"),
        plot.subtitle = element_text(color = "white", family = "Cabin", size = 9),
        plot.title.position = 'plot',
        plot.caption = element_text(
          color = "white", 
          family = "Cabin",
          size = 6),
        plot.margin = margin(20,20,20,20)
        )

agg_png(
 here("2021",
      "week12", 
      "progress", 
      paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
 width = 6,
 height = 4,
 units = "in",
 res = 300
)
g.plot.title
dev.off()


