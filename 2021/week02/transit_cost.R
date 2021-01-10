library(here)
library(dplyr)
library(ggplot2)
library(countrycode)
library(stringr)
library(ggtext)
library(showtext)

font_add_google("Rubik", "rubik")
font_add_google("Inter", "inter")

showtext_auto()


transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')


transit <- transit_cost %>% 
  mutate(proj_length_year = as.numeric(end_year) - as.numeric(start_year),
         continent = countrycode(country, 'iso2c', 'continent'),
         region =  countrycode(country, 'iso2c', 'region'), 
         ) %>% 
  filter(continent == "Americas") 

transit$region  <- factor(transit$region , 
                          levels = levels(as.factor(transit$region))[c(2,1,3)])

col <- c("#0e93ab", "#81ad71",  "#ffa785")
bg.col <- "grey95"
txt.col <- "grey30"

sub.txt <- 
"In the Americas, the cost per kilometer of transit-infrastructure projects 
decreases as the<br>length of the project increases. 
In <span style= 'color: #0e93ab'>**North America**</span>, 
there are more investiment in short<br>projects, which could be very expensive, 
while in the <span style= 'color: #81ad71'>**Central**</span>
and <span style= 'color: #ffa785'>**South America**</span> the<br>investiments
are in projects for long transit lines, with lower cost per Km."

#windows(9,7)

plot <-
ggplot(transit) +
  geom_smooth(aes(x = length,
                  y = cost_km_millions),
              method = glm, 
              formula = y ~ x,
              method.args = list(family = poisson(link = "log")),
              se = T, 
              colour = "grey30",
              fill = "grey30") +
  geom_point(aes(x = length,
                 y = cost_km_millions,
                 color = region),
                 size = 2) +
  scale_color_manual(values = col) +
  labs(x = "Length of the line (Km)", 
       y = "Cost / km (Millions USD)",
       title = "Cost of transit projects in Americas",
       subtitle = sub.txt,
       caption = "@avrodrigues_ | Transit Costs Project | #TidyTuesday week 2 2021") +
  scale_x_log10(n.breaks = 8) +
  theme_bw() +
  theme(legend.position = 'none', 
        plot.background = element_rect(fill = bg.col),
        panel.background = element_rect(fill = bg.col),
        axis.title = element_text(
          color = txt.col, 
          face = "bold", 
          family = "inter",
          size = 16),
        axis.text = element_text(
          color = txt.col, 
          face = "bold", 
          family = "inter",
          size = 12),
        panel.grid = element_line(color = "grey85"),
        plot.title = element_markdown(size = 32, color = txt.col, face = "bold",
                                      family = "rubik"),
        plot.subtitle = element_markdown(size = 16, color = txt.col,
                                         family = "inter",
                                         lineheight = 0.4)
        )
#plot


ggsave(here("2021", "week02", "transit.png"),
       plot, 
       width = 9, height = 7, units = "cm")
