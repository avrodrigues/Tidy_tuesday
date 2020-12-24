
# Load packages -----------------------------------------------------------
library(here)
library(dplyr)
library(lubridate)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(raster)
library(ggimage)
library(gganimate)

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2020, week = 52)
big_mac <- tuesdata$`big-mac`

# Countries of South America
SA <- ne_countries(continent  = 'south america',returnclass = "sf")

# Filter South American coutries
SA.bigmac <- big_mac %>% 
  filter(iso_a3 %in% SA$adm0_a3)
  
# flags downloaded from: https://www.countryflags.com/
flags <- list.files(here::here("2020", "week52", "img"), full.names = T)
country.flag <- tibble(name = c("Argentina", 
                   "Brazil", 
                   "Chile", 
                   "Colombia", 
                   "Peru", 
                   "Uruguay"),
       flags = flags)

# data with flags
SA.bigmac <- right_join(SA.bigmac, country.flag,  by = "name")

# Plot
gg <- ggplot(SA.bigmac, 
             aes(x = date, 
                 y = usd_raw, 
                 group = name)) +
  geom_line(aes(color = name), size = 1.2)+
  scale_color_manual(values = c("lightblue", 
                                "darkgreen",
                                "darkred", 
                                "yellow", 
                                "red", 
                                "blue"), name = " ") +
  geom_image(aes(x = date, y=usd_raw, image = flags),
             size = 0.04, asp = 2.2) +
  theme_bw() +
  scale_y_continuous(n.breaks = 8, limits = c(-0.8,0.6))+
  scale_x_date(date_breaks = "1 years", 
               date_labels = as.character(1999:2021), 
               date_minor_breaks = "1 years")+
  labs(title = "Big Mac Index for South America Countries",
    y = "Big Mac Index", 
    x = "Year", 
    caption = "@avrodrigues_ | The Economist | TidyTuesday 2020 week 52") +
  theme(text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1))

# static Version
ggsave(here::here("2020", "week52", "bigmac_index.png"),
       plot = gg,
       width = 6,
       height = 3,
       units = "in",
       dpi = 300)

# animation
anim <- gg + transition_reveal(date)

animate(anim, height = 3, width = 6, units = "in", res = 150)
anim_save(here::here("2020", "week52", "bigmac_index.gif"))
