library(dplyr)
library(ggplot2)
library(colorfindr)
library(showtext)

font_add_google("Roboto Condensed", "roboto")
font_add_google("Nunito Sans", "nunito")

showtext_auto()

# read data
ninja_warrior <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv')


diversity <- 
  ninja_warrior %>% 
  group_by(season) %>% 
  summarise(rich = length(unique(obstacle_name)),
            n_rounds = length(unique(round_stage)),
            n_location = length(unique(location)),
            std_rich = rich/n_location) 

## colors
ninja.blue <- "#1ea5ef"
ninja.red <- "#e90021"
ninja.bg <- "grey15"
ninja.txt <- "grey90"
line.size <- 1.2
pt.size <- line.size*2.5

# coefficient to second y axis
coeff <- 4

ninja.plot <- 
  ggplot(diversity, aes(x = season)) +
  geom_line(aes(y = rich), 
            colour = ninja.red, 
            size = line.size) +
  geom_line(aes(y = std_rich*coeff), 
            colour = ninja.blue, 
            size = line.size) +
  geom_point(aes(y = rich), 
             colour = ninja.red, 
             size = pt.size) +
  geom_point(aes(y = std_rich*coeff), 
             colour = ninja.blue,  
             size = pt.size) +
  scale_y_continuous(
    # Features of the first axis
    name = "Obstacle diversity",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Standard Obstacle diversity")
  ) +
  scale_x_continuous(breaks = 1:10, ) +
  theme_classic() +
  labs(title = "Diversity of obstacules in the American Ninja Warrior Challenge",
       subtitle = "Diversity is not simple to measure, and depending on the metric used the conclusion could be ambigous. 

Here I show two metrics for diveristy: 
Obstacles Diversity is the number of different obstacle used in a season. 
Standard Obstacles Diversity is is the number of different obstacle used in a season divided by the number of locations in that season.

Along the seasons, while the number of obtacles increased they were more repeated among locations",
       caption = "@avrodrigues_ | Source: Data.World | #TidyTuesday Week 51") +
  labs(x = "Season") +
  theme(plot.title = element_text(family = "roboto", 
                                  face =  "bold.italic", 
                                  size = 26,
                                  hjust=0,
                                  colour = ninja.txt),
        plot.subtitle = element_text(family = "nunito", 
                                     face =  "plain", 
                                     size = 12, 
                                     hjust=0,
                                     colour = ninja.txt),
        plot.caption = element_text(size = 10, 
                                    colour = ninja.txt),
        axis.title.y.right = element_text(colour = ninja.blue, 
                                          size = 20,
                                          family = "roboto",
                                          face = "bold",
                                          vjust = 3),
        axis.text.y.right  = element_text(colour = ninja.blue, 
                                          size = 14,
                                          family = "roboto",
                                          face = "bold"),
        axis.line.y.right  = element_line(colour = ninja.blue),
        axis.ticks.y.right = element_line(colour = ninja.blue),
        axis.title.y.left = element_text(colour = ninja.red,
                                         size = 20,
                                         family = "roboto",
                                         face = "bold",
                                         vjust = 3),
        axis.text.y.left  = element_text(colour = ninja.red, 
                                         size = 14,
                                         family = "roboto",
                                         face = "bold"),
        axis.line.y.left  = element_line(colour = ninja.red),
        axis.ticks.y.left = element_line(colour = ninja.red),
        axis.line.x.bottom = element_line(colour = ninja.txt),
        axis.ticks.x = element_line(colour = ninja.txt),
        axis.text.x = element_text(colour = ninja.txt,
                                   size = 16,
                                   family = "roboto",
                                   face = "bold"),
        axis.title.x = element_text(colour = ninja.txt,
                                    size = 20,
                                    family = "roboto",
                                    face = "bold"),
        legend.position = 'none',
        plot.background = element_rect(fill = ninja.bg),
        panel.background = element_rect(fill = ninja.bg),
        plot.margin = margin(25,25,25,25)) 

png(file = here::here("2020", "week51", "ninja.png"),
    1100, 640)
ninja.plot
dev.off()
