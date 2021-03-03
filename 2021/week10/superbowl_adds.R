
# load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(ggimage)
library(ggforce)
library(cowplot)
library(patchwork)
library(magick)
library(ggtext)
library(ragg)

# create directory --------------------------------------------------------

# create directory
if(!dir.exists(here("2021", "week10"))) dir.create(here("2021", "week10"))
# create directory to save progress
prog.dir <- here("2021", "week10", "progress")
if(!dir.exists(prog.dir)) dir.create(prog.dir)


# load data ---------------------------------------------------------------


youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')


yt.content <- 
youtube %>% 
  select(funny, show_product_quickly, patriotic, celebrity, danger,
         animals, use_sex) 


freq.cont <- colSums(yt.content) / nrow(yt.content) *100
type.cont <- names(freq.cont)
labels <- c(
  "Funny",
  "Show  Product  Quickly",
  "Patriotic", 
  "Celebrity", 
  "Danger", 
  "Animals",
  "Sexuality")



icon.dir <- here("2021", "week10", "icons")
img.file <- list.files(icon.dir, full.names = T)


nfl_ball <- image_read_svg(img.file)
nfl_ball <- image_fill(nfl_ball, color = "#7D4834", "+400+50", fuzz = 50)
nfl_ball <- image_fill(nfl_ball, color = "#7D4834", "+200+150", fuzz = 50)
nfl_ball <- image_fill(nfl_ball, color = "#7D4834", "+150+500", fuzz = 50)

image_write(nfl_ball, here(icon.dir, "nfl_ball.png"))


adds.content <- tibble(
  type.cont =  factor(type.cont, 
                      c("patriotic", "celebrity", "animals",
                        "funny", "show_product_quickly",
                        "danger", "use_sex")),
  freq.cont,
  labels ,
  img =  here(icon.dir, "nfl_ball.png")
)

# plot  ---------------------------------------------------------------

# bar plot style
(goal.try <- 
ggplot(adds.content) +
  geom_image(aes(x = type.cont, y = freq.cont, image = img),
             size = 0.1, asp = 0.5) +
  scale_y_continuous(
    labels = paste0(seq(0, 100, 10), "%"),
    breaks = seq(0, 100, 10),
    limits = c(0,100)
  ) +
  geom_segment(
    aes(x = type.cont,
        y = freq.cont - 6 ,
        xend = type.cont, 
        yend = c(10, 33, 9.59919, 15, 13, 13, 14)), 
    linetype = 2) +
  geom_richtext(
    aes(x = type.cont, y = 0, label = labels),
    size = 4, fill = NA, label.color = NA,
    lineheight = .3,
    family = "Cabin",
    hjust = 0,
    vjust = 0.5,
    angle = 90
    ) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      margin = margin(r = 20),
      color = "grey95", 
      face = "bold",
      size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey95",
      linetype = 3)
))

# nfl goal
goal.df <- tibble(
  x = c(0, 0, 8, 8),
  y = c(100, 0, 0, 100)
)


goal <- 
ggplot(goal.df) +
  geom_link2(aes(x = x, y = y),
             colour = "gold", size = 4,
             lineend = 'round') +
  geom_link(aes(x = 4, y = 0, xend = 4, yend = -20),
               colour = "gold", size = 4,
               lineend = 'round') +
  theme_void() +
  theme(
    plot.margin = margin(70, 100, 10, 100)
  )

# title and caption
(t <- 
ggplot() +
  labs(title = "Frequency of content type\nin SuperBowl Ads since 2000",
       caption = "@avrodrigues_ | source: FiveThirtyEight  ") +
    
    theme(
      plot.title = element_text(
        family = "Bebas",
        size = 26,
        hjust = 0.5,
        color = "white"),
      plot.caption = element_text(
        family = "Inter", 
        color = "white"
      )
      
    ) 
  )

tt <- get_title(t)
cap <- get_plot_component(t, "caption")


# superbowl plot ----------------------------------------------------------

superbowl <- 
  ggdraw() +
  draw_plot(
    tt,
    hjust = 0.5,
    vjust = 0.5,
    x = 0.5, 
    y = 0.92
    ) +
  draw_plot(
    cap,
    vjust = 0.5,
    y = 0.02) +
  draw_plot(goal) +
  draw_plot(
    goal.try, 
    x = 0.14,
    y = 0.16,
    width = 0.6,
    height = 0.70
    ) +
  theme(
    plot.background = element_rect(fill = "#5ac18e", color = NA)
  ) 



# save progress -----------------------------------------------------------

#agg_png(
#  here(prog.dir,  paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
#  width = 6,
#  height = 8,
#  units = "in",
#  res = 300
#)
#superbowl
#dev.off()


# save final plot ---------------------------------------------------------

agg_png(
  here("2021", "week10", "superbowl.png"),
  width = 6,
  height = 8,
  units = "in",
  res = 300
)
superbowl
dev.off()
