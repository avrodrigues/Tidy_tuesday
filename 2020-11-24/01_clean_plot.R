dir.create("2020-11-24")

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2020-11-24')
#tuesdata <- tidytuesdayR::tt_load(2020, week = 48)

hike_data <- tuesdata$hike_data

library(tidyverse)
library(ggplot2)
library(magrittr)
library(forcats)
library(paletteer)

# cleaning data
hike_data$gain %<>% as.numeric
hike_data$rating %<>% as.numeric

hike_data$trial.length <- str_remove_all(hike_data$length, 
                                         "[[a-z]]|[[:punct:]]|\\ ")
hike_data$trial.length %<>% as.numeric

# organize data
hike_regions <- hike_data %>% 
  mutate(region = sapply(str_split(location, "\\ -- "), "[", 1)) %>% 
  group_by(region) %>%
  summarize(mean.length = mean(trial.length),
            max.length = max(trial.length),
            mean.gain =  mean(gain),
            max.gain =  max(gain),
            mean.rating = mean(rating, na.rm = T),
            n.trials = n()) %>% 
  arrange(desc(mean.gain))

# Color palette for backgroud
col.10 <- paletteer_c("ggthemes::Classic Gray", 10)
col.10
# backgroud color
col.back <- col.10[8]

#chart title
c.title <- "Washington Hike"
c.subtitle <- "Avarage elevation gain of trials in Whashington regions. 
Color gradient repesents the avarage rating of trails."

# Chart
windows(12,7)
hike_regions %>%
  mutate(name = fct_reorder(region, mean.gain)) %>%
  ggplot( aes(x=name, y=mean.gain,  fill = mean.rating)) +
  geom_bar(stat="identity") +
  ylim(0,2500) +
  scale_fill_paletteer_c("ggthemes::Classic Orange", ) +
  labs(fill = "Average Trial Rating") +
  xlab("") +
  coord_flip() +
  ggtitle(c.title,
          subtitle = c.subtitle) +
  theme_bw() +
  theme(
    plot.title = element_text(color = "white", 
                         family = "serif", 
                         size = 26, 
                         hjust = 0),
    plot.subtitle = element_text(color = "white", 
                                 family = "serif", 
                                 size = 15, 
                                 hjust = 0),
    axis.ticks = element_line(color = "white"),
    axis.text = element_text(color = "white", family = "sans", size = 11),
    plot.background = element_rect(fill = col.back),
    panel.background = element_rect(fill = col.back),
    legend.background = element_rect(fill = col.back),
    legend.title = element_text(color = "white", 
                                family = "sans", 
                                size = 10,  
                                face = "bold"),
    legend.text = element_text(color = "white",
                               family = "sans", 
                               size = 9, 
                               face = "bold"),
    
    panel.border = element_rect(color = col.back),
    legend.key = element_rect(color = col.back),
    axis.title = element_blank(),
    panel.grid = element_line(color = col.10[6]),
    plot.margin = unit(rep(1,4), "cm"),# This remove unnecessary margin around plot
  )

