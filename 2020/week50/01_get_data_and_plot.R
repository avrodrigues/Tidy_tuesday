
# Load packages and data --------------------------------------------------

library(here)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(raster)
library(stringr)
library(dplyr)
library(ggimage)
library(gridExtra)
library(extrafont)
library(cowplot)

loadfonts(device="win")

## BBC Women of the Year (2020)
woy <- tidytuesdayR::tt_load(2020, week = 50)

women <- woy$women

# Countries of Latin America
la.countries <- c("Mexico",
                  "Guatemala", 
                  "Honduras", 
                  "El Salvador", 
                  "Nicaragua", 
                  "Costa Rica",
                  "Panama",
                  "Colombia",
                  "Venezuela",
                  "Ecuador",
                  "Peru", 
                  "Bolivia", 
                  "Chile", 
                  "France", #"French Guyana"
                  "French Guiana",
                  "Paraguay", 
                  "Brazil", 
                  "Argentina",
                  "Uruguay",
                  "Cuba", 
                  "Dominican Republic",
                  "Puerto Rico") 

latin.america <- ne_countries(country = la.countries, returnclass = "sf")
latin.america <- st_crop(latin.america, extent(c(-125, -30, -60, 40)))


# Define color palletes ---------------------------------------------------

pastel <- c("#85aae4",
            "#87be7e",
            "#d795c9",
            "#60c4bd",
            "#e49182",
            "#ffffbc",
            "#c8ab68")

intense <- c("#60b14a",
             "#7c51c3",
             "#be9b31",
             "#cd4997",
             "#ce4f37")

back <- c("#bdd4e3",
          "#e1e5cf",
          "#e0c7ce",
          "#cde8e2",
          "#bec2ad",
          "#a5c3c3")

text.col <- "grey20"


# Organize data to circular plot ------------------------------------------

# Exclude France 
LA.women <- women %>% 
  filter(country %in% la.countries,
         country != "France") 

# Data frame for circular plot
df.plot <- LA.women %>% 
  group_by(category) %>% 
  summarise(count = n()) %>% 
  mutate(fraction = count/sum(count),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(category, "\n", count))

# Circular plot
stat <- ggplot(df.plot, 
               aes(ymax=ymax, ymin=ymin, xmax=3, xmin=4, fill=category)) +
  geom_rect() +
  labs(fill = "Category") +
  scale_fill_manual(values = pastel) +
  coord_polar(theta="y") + 
  xlim(c(0, 4)) +
  geom_text(aes(x = 0, 
                y = 0 ,
                label = sum(count)),
            family = "Franklin Gothic Book",
            size = 25,
            colour = text.col) +
  theme_void() +
  theme(plot.background = element_rect(fill = back[2] , color = NA),
        legend.title = element_text(color = text.col, 
                                    family = "Franklin Gothic Heavy", 
                                    size = 10),
        legend.text = element_text(color = text.col,
                                   family = "Franklin Gothic Book", 
                                   size = 11),
        legend.direction = "horizontal",
        legend.position = "bottom",
        plot.margin = unit(x = c(1,1,1,1), units = "lines"))



# Organize data to map plot ----------------------------------------------

#data
names.country <- tibble(country = latin.america$admin)

n.per.country <- LA.women %>% 
  group_by(country) %>% 
  count(country) %>% 
  mutate(cited = "yes")

latin.america$n <- left_join(names.country, n.per.country)$n
latin.america$cited <- left_join(names.country, n.per.country)$cited

cent <- st_centroid(latin.america) %>% 
  filter(cited == "yes") %>% 
  mutate(country = admin) %>% 
  select(country, geometry) %>% 
  as_tibble() %>% 
  mutate(x = coordinates(as_Spatial(geometry))[,1],
         y = coordinates(as_Spatial(geometry))[,2])

LA.women.map <- right_join(LA.women, cent, by = "country") %>% 
  mutate(label = paste0(name, "\n", role))

xend <- c(-65, # 1 "Evelina Cabrera"            
          -65, # 2 "Wendy Beatriz Caishpal Jaco"
          5, # 3 "Carolina Castro"            
          -140, # 4 "Claudia López"              
          -140, # 5 "Nemonte Nenquimo"           
          5, # 6 "Cibele Racy"                
          5, # 7 "Susana Raffalli "           
          -140, # 8 "Ruth Shady"                 
          5, # 9 "Lea T"                      
          -140) # 10 "Arussi Unda"  

yend <- c(-110, #  1 "Evelina Cabrera"            
          58, #  2 "Wendy Beatriz Caishpal Jaco"
          -110, #  3 "Carolina Castro"            
          0, #  4 "Claudia López"              
          -56, #  5 "Nemonte Nenquimo"           
          0, #  6 "Cibele Racy"                
          58, #  7 "Susana Raffalli "           
          -110, #  8 "Ruth Shady"                 
          -56, #  9 "Lea T"                      
          58) # 10 "Arussi Unda"  

LA.women.map$xend <- xend
LA.women.map$yend <- yend

# map plot
m <- ggplot(data = latin.america) +
  geom_sf(aes(fill = cited)) +
  scale_fill_manual(values = pastel[5], na.value = "grey80") +
  xlim(c(-170, 30)) +
  ylim(c(-110, 90)) +
  theme_void() +
  theme(legend.position = "none") +
  geom_segment(data = LA.women.map,
               aes(x = x, y = y, 
                   xend = xend, yend = yend + 15, 
                   colour = category),
               size = 1.5)  +
  scale_colour_manual(values = pastel, na.value = "grey80") +
  geom_text(aes(label = label), 
            data = LA.women.map,
            x = xend,
            y = yend,
            size = 3.9,
            colour = text.col,
            family = "Franklin Gothic Book",
            lineheight = 0.8) +
  theme(plot.background = element_rect(fill = back[2], color = NA)) +
  geom_image(data= LA.women.map, 
             aes(x = xend, y=yend + 25, image = img),
             size = 0.15)

# prepare elements to chart -----------------------------------------------

# Title
title <- ggdraw() +
  draw_label(label = "BBC's women of 2020 - Latin America",
             fontfamily = "Franklin Gothic Heavy",
             color = text.col,
             x = 0,
             hjust = 0,
             size = 20) +
  theme(plot.margin = margin(0,0,0,0))

# Subtitle
subtitle <- ggdraw() +
  draw_label(label = "In the BBC's list there are 10 latin american women from 8 countries.",
             fontfamily = "Franklin Gothic Book", 
             color = text.col,
             x = 0,
             hjust = 0,
             size = 15) +
  theme(plot.margin = margin(1,1,1,1))

# Chart
chart <- plot_grid(stat, m, ncol = 2, rel_widths = c(0.8, 1)) +
  theme(plot.background = element_rect(fill=back[2], color = NA))


# Save chart --------------------------------------------------------------

png(here::here("2020-12-08", "latin_women.png"), 9, 6, units = "in", res = 300)
plot_grid(title, subtitle, chart,
  ncol = 1,  rel_heights = c(0.05, 0.05, 1))+
  theme(plot.background = element_rect(fill=back[2], color = NA),
        plot.margin = unit(c(1,1,1,1), "lines"))
dev.off()
