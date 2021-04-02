# load packages -----------------------------------------------------------

library(tidyverse)
library(ragg)
library(here)
library(ggforce)
library(cowplot)
library(patchwork)
library(ggridges)


# load and prepare data ---------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 14)

mk_shades <- 
tuesdata$allShades %>% distinct(hex, hue, sat, lightness) %>% 
  mutate(gr = "A")

man_cols <- mk_shades$hex
names(man_cols) <- man_cols



# individual plots --------------------------------------------------------

axis.font <- "Source Sans Pro" 
bg.col <- "#455146"

jitter <- 
ggplot(mk_shades) +
  geom_jitter(aes(x = lightness, y = gr, color = hex),
              show.legend = F,
              size = 0.5) +
  scale_color_manual(values = man_cols) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_discrete(expand = expansion(mult = 0.45, add = 0)) +
  theme_minimal() +
  xlab("Lightness") +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(
      family = axis.font,
      size = 14,
      color = "grey95"
    ),
    axis.text.y = element_blank(), 
    axis.text.x = element_text(
      family = axis.font,
      color = "grey95"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid = element_line(linetype = 3),
    axis.line.x = element_line(
      color = "grey95"
    ),
    plot.margin = margin(0, b = 5)
  )
  
  
ridge <- 
ggplot(mk_shades) +
  geom_density_ridges(aes(x = lightness, y = gr), 
                      alpha = 0.7, 
                      fill = "#EBB67B", 
                      color = NA) +
  scale_x_continuous(limits = c(0,1)) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid = element_line(linetype = 3),
    axis.line.x = element_blank(),
    plot.margin = margin(b = 0)
  )

chart <- 
plot_grid(
  ridge, jitter, 
  ncol = 1,
  rel_heights = c(0.5, 1)
) +
  theme(
  plot.background = element_rect(
    fill = "transparent", color = "grey30",
  ),
  plot.margin = margin(5,5,5,5)
)



# final plot --------------------------------------------------------------

final.plot <-
ggdraw() +
  draw_plot(
    chart,
    x = 0.5,
    y = 0.1,
    width = 0.9,
    height = 0.6,
    hjust = 0.5,
    vjust = 0,
  ) +
  draw_label(
    "The bias in the makeup shades",
    x = 0.06,
    y = 0.9,
    fontfamily = "Cabin", 
    fontface = "bold",
    size = 22,
    color = "grey95",
    hjust = 0
  ) +
  draw_label(
    "There is a bias towards lighter skin tones\nin the range of shades of the makeup industry",
    x = 0.065,
    y = 0.85,
    fontfamily = "Cabin", 
    fontface = "plain",
    size = 12,
    color = "grey95",
    hjust = 0,
    vjust = 1
  ) +
  annotate("curve", x = 0.3, xend = 0.38, y = 0.1, yend = 0.25, 
           color = "grey85", curvature = 0.5) +
  draw_label(
    "Each dot is\na makeup\nshade color",
    x = 0.25,
    y = 0.1,
    fontfamily = "Young", 
    size = 7,
    color = "grey95",
    hjust = 0.5,
    vjust = 0.5
  ) +
  annotate("curve", x = 0.8, xend = 0.65, y = 0.79, yend = 0.68, 
           color = "grey85", curvature = 0.3) +
  draw_label(
    "The peak of highest\ndensity of shades is at\n0.65 of lightness",
    x = 0.88,
    y = 0.762,
    fontfamily = "Young", 
    size = 7,
    color = "grey95",
    hjust = 0.5,
    vjust = 0.5
  ) +
  theme(
    plot.background = element_rect(
      fill = bg.col
    )
  ) +
  draw_label(
    "@avrodrigues | Source: The Pudding data",
    x = 0.82,
    y = 0.05,
    fontfamily = "Cabin", 
    size = 7,
    color = "grey95",
    hjust = 0.5,
    vjust = 0.5
  ) 
  

# save progress -----------------------------------------------------------

# agg_png(
# here("2021",
#      "week14", 
#      "progress", 
#      paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
# width = 6,
# height = 4,
# units = "in",
# res = 300
# )
# final.plot
# dev.off()


# save final plot ---------------------------------------------------------

agg_png(
  here("2021", "week14", "makeup.png"),
  width = 6,
  height = 4,
  units = "in",
  res = 300
)
final.plot
dev.off()
