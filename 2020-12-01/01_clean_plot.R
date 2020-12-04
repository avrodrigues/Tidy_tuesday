library(tidyverse)
library(ggplot2)
library(lubridate)


# load data ---------------------------------------------------------------


shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

str(shelters)


# Summarize data ----------------------------------------------------------


shelter_capacity <- shelters %>%  
  mutate(year = year(occupancy_date),
         month = month(occupancy_date),
         percent_occupancy = (occupancy/capacity)*100) %>% 
  group_by(sector, year, month) %>% 
  summarise(mean_capatity = mean(capacity, na.rm = T),
            max_capacity = max(capacity, na.rm = T)) %>% 
  mutate(month2 = as.Date(paste0("2020-", month,"-01"),"%Y-%m-%d")) 


# Plot --------------------------------------------------------------------



back.col <- "#2A3C46"
text.col <- "#CBCBCB"

file.place <- here::here("2020-12-01", "sheltes.png")
png(file.place, width = 12, height = 5, units = "in", res = 300)
ggplot(shelter_capacity, aes(y = mean_capatity, 
                             x = month2, 
                             color = sector)) +
  geom_line(size = 2) +
  scale_x_date(date_labels = "%b") +
  facet_wrap( ~ year ) + 
  theme_bw() +
  labs(title = "Monthly average capacity of the shelter system in Toronto by sectors",
       subtitle = "Shelters for families had a great increase in its capacity",
       x = "Month",
       y = "Average capacity") +
  theme(plot.background = element_rect(back.col),
        legend.background = element_rect(back.col),
        legend.key = element_rect(back.col),
        text = element_text(color = text.col),
        axis.text = element_text(color = text.col))

dev.off()
