
# load packages -----------------------------------------------------------
library(tidyverse)
library(countrycode)
library(lubridate)
library(here)
library(cowplot)
library(ggtext)
library(extrafont)
library(rgdal)
library(sf)

loadfonts()


# load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 5)
plastics <- tuesdata$plastics

# create directory
if(!dir.exists(here("2021", "week05"))) dir.create(here("2021", "week05"))
# create directory to save progress
prog.dir <- here("2021", "week05", "progress")
if(!dir.exists(prog.dir)) dir.create(prog.dir)

# preparing data ----------------------------------------------------------


# transfom to long format
plastic.types <- 
plastics %>% 
  pivot_longer(hdpe:pvc, names_to = "type", values_to = "value")

# amount of plastic in each continent
df.plastic <- 
plastic.types %>% 
  mutate(country = str_to_title(country)) %>% 
  filter(country !="Empty", 
         parent_company != "Grand Total") %>% 
  mutate(continent = countrycode(sourcevar = country,
                                 origin = "country.name",
                                 destination = "continent")) %>% 
  group_by(continent, type) %>% 
  summarise(amount = sum(value, na.rm = T)) 

# events in each continent
events <- 
plastics %>% 
  mutate(country = str_to_title(country)) %>% 
  filter(country !="Empty") %>% 
  #       parent_company == "Grand Total") %>% 
  mutate(continent = countrycode(sourcevar = country,
                                 origin = "country.name",
                                 destination = "continent")) %>% 
  select(continent, country, year, parent_company, num_events, volunteers) %>% 
  group_by(continent, country, year) %>%  
  summarise(num_events = mean(num_events),
            num_volunteers = max(volunteers)) %>% 
  ungroup() %>% 
  group_by(continent, country) %>% 
  summarise(num_events = sum(num_events),
            num_volunteers = sum(num_volunteers)) %>% 
  ungroup() %>% 
  group_by(continent) %>% 
  summarise(num_events = sum(num_events),
            num_volunteers = sum(num_volunteers, na.rm = T),
            num_country = n()) 

# plastic amount and events in continents
plastic_type_amount <- 
left_join(df.plastic, events) %>% 
  mutate(volunt_event = num_volunteers/num_events,
         amount_event = amount/num_events,
         amount_volunt = amount/num_volunteers,
         continent = factor(continent, 
                            levels=c("Africa", "Oceania", "Asia",   
                                     "Europe", "Americas")),
         type =  factor(type,
                        levels = c("hdpe", "ldpe", 
                                   "pet", "pp", "ps", 
                                   "pvc", "o")))

# Plot --------------------------------------------------------------------
theme_set(theme_minimal())

# colors
bg.col <- "#708ba6"
amount.col <- c("#e5fafa", "#00cdcd")
grid.col <- "#8098b0"
text.col <- "grey90"

manual.col <- c(
  "#d7bfbf",
  "#c4a0a0",
  "#b08080",
  "#9d6161",
  "#8d5757",
  "#6d4343",
  "#4e3030"
)


# font family
serif <- "Lucida Bright"
sans <- "Franklin Gothic Book"   

plot.set <- list(
  theme(panel.grid.major = element_line(color = grid.col))
)


# plot plastic types ------------------------------------------------------


type_cont <- 
ggplot(plastic_type_amount,aes(x = type, y = amount_volunt, color = type)) +
  geom_point(size = 4) + 
  geom_segment(aes(x=type, xend=type, y=0, yend=amount_volunt), 
               size = 2, 
               show.legend = F) +
  labs(y = "",
       x = "Average plastic per volunteer") +
  facet_grid(facet = ~factor(continent, 
                             levels = c("Africa", "Americas", "Asia", 
                                        "Europe", "Oceania"), 
                              ), 
             scales = "free") +
  scale_color_manual(labels = c("High density polyethylene", 
                                  "Low density polyethylene", 
                                  "Polyester", "Polypropylene", 
                                  "Polystyrene", "PVC", "Other"),
                     values = manual.col) +
  scale_x_discrete(labels = rep("", 7)) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(colour = "black"),
    panel.grid.major.x = element_blank(), 
    legend.title = element_blank(),
    legend.text = element_text(family = sans, size = 12),
    legend.position="top",
    legend.spacing.x = unit(0.1,"cm")) +
  guides(color=guide_legend(nrow=3, byrow=F)) +
  plot.set


leg <- get_legend(type_cont)
type_cont2 <- type_cont + theme(legend.position = 'none')

# extract single plots
asia <- type_cont2
afri <- type_cont2
amer <- type_cont2
euro <- type_cont2
ocea <- type_cont2

asia$data <- asia$data %>% filter(continent == "Asia")
afri$data <- afri$data %>% filter(continent == "Africa")
amer$data <- amer$data %>% filter(continent == "Americas")
euro$data <- euro$data %>% filter(continent == "Europe")
ocea$data <- ocea$data %>% filter(continent == "Oceania")

# size in the chart
l.size <- list(width = 0.22, height = 0.22)


# Points representing the amount of plantic in each Continent -------------

# size of points based on plastic amount
p.size <- 
  plastic_type_amount %>% 
  group_by(continent) %>% 
  summarise(amount_volunt = sum(amount_volunt))

# See the amazing zine of Ijeamaka Anyene on radial patterns:
# https://ijeamaka-anyene.netlify.app/posts/2021-01-04-radial-patterns-in-ggplot2/
# I used the circle object as presented in that zine

# define radius
r <- 0.38
# circle
circle.pt <- tibble(
  len = seq(0, 2*pi, length.out = 6),
  x = sin(len) * r + 0.5,
  y = (cos(len)*-1) * r + 0.65)

circle.pt <- circle.pt[-6,]
circle.pt$label <- levels(plastic_type_amount$continent)
circle.pt$amount_volunt <- p.size$amount_volunt

## plot of plastic in each continent
plastic.size <-
ggplot(circle.pt, aes(x, y)) +
  geom_point(aes(x, y, color = amount_volunt),
             size = circle.pt$amount_volunt, 
             show.legend = F) +
  scale_color_gradient(low = amount.col[1], 
                       high = amount.col[2]) +
  
  geom_text(aes(x, y-0.03, label = label), 
                family = serif, 
                #fontface = "bold", 
                size = 8) +
  xlim(0,1) + ylim(0,1) +
  theme_void() 

## legend of plastic in each continent
leg.size <- 
ggplot(data.frame(x = c(0.5, 1, 1.5), 
                  y = rep(0, 3),
                  col =  c(5, 10, 25))) +
  geom_point(aes(x, y, color = col),
             size =  c(5, 10, 25), 
             show.legend = F) +
  geom_text(aes(x, y-0.5, label = col, 
                family = sans)) +
  scale_color_gradient(low = amount.col[1], 
                       high = amount.col[2]) +
  xlim(0,2) +
  ylim(-2,2) +
  theme_void() +
  theme(plot.background = element_rect(fill = "transparent", color = NA))
  

# map ---------------------------------------------------------------------

# Continent shapefile downloaded from:
# https://hub.arcgis.com/datasets/57c1ade4fa7c4e2384e6a23f2b3bd254_0

# get the path to shapefile 
shp.file <- list.files(here("shapefiles", "World_Continents"), pattern = "\\.shp", full.names = T)
# load
cont <- readOGR(shp.file)
# convert to sf
cont_sf <- st_as_sf(cont)
# Set continents names and exclude Antarctica
cont_sf <- cont_sf %>% 
  mutate(CONTINENT = if_else(str_detect(CONTINENT, "America"), 
                             "Americas",
                             CONTINENT),
         CONTINENT = if_else(str_detect(CONTINENT, "Australia"), 
                             "Oceania",
                             CONTINENT)) %>% 
  filter(CONTINENT != "Antarctica") %>% 
  rename("continent" = CONTINENT)

# change map projection to World_Winkel_II
crs <- "ESRI:54019"
cont_sf <- st_transform(cont_sf, crs)

# join sf and data on plastic
cont_sf <- left_join(cont_sf, p.size)

# build the map
map <-
  ggplot(cont_sf)  +
  geom_sf(aes(fill = amount_volunt), color = NA, show.legend = F) +
  scale_fill_gradient(low = amount.col[1], 
                      high = amount.col[2]) +
  theme(plot.background = element_rect(fill = "transparent", color = NA)) +
  plot.set

# barplots plots position -------------------------------------------------------

# define radius of the barplots
r2 <- 0.34
# circle position of the barplots
circle <- tibble(
  len = seq(0, 2*pi, length.out = 6),
  x = sin(len) * r2 + 0.5,
  y = (cos(len)*-1) * r2 + 0.48)

circle <- circle[-6,]
circle$size <- p.size$amount_volunt


# Main graph --------------------------------------------------------------

graph <-
  ggdraw() +
    draw_plot(map,
            scale = 0.45, 
            x = 0.5, y  = 0.52, 
            hjust = 0.5,
            vjust = 0.5) +
  draw_plot(plastic.size, 
            x = 0.5, y  = 0.5, 
            hjust = 0.5,
            vjust = 0.5) +
  draw_plot(asia, 
            width = l.size$width, 
            height = l.size$height,
            x = circle$x[3], 
            y  =  circle$y[3], 
            hjust = 0.5,
            vjust = 0.5) +
  draw_plot(amer, 
            width = l.size$width, 
            height = l.size$height,
            x =  circle$x[5], 
            y  =  circle$y[5], 
            hjust = 0.5,
            vjust = 0.5) +
  draw_plot(afri, 
            width = l.size$width, 
            height = l.size$height,
            x =  circle$x[1], 
            y  =  circle$y[1], 
            hjust = 0.5,
            vjust = 0.5) +
  draw_plot(ocea, 
            width = l.size$width, 
            height = l.size$height,
            x =  circle$x[2], 
            y  =  circle$y[2],  
            hjust = 0.5,
            vjust = 0.5) +
  draw_plot(euro, 
            width = l.size$width, 
            height = l.size$height,
            x =  circle$x[4], 
            y  =  circle$y[4], 
            hjust = 0.5,
            vjust = 0.5) 


# Chart title -------------------------------------------------------------

title <- c("Plastic Pollution")

subtitle <- c("Between 2019-2020 **Break Free From Plastic** conducted *913
events* of colletcing<br>plastic waste around the world. These events were 
attended by more than<br>*85 thousand volunteers*  in *66 countries*. 
The graph shows the average amount of<br>plastic colected per volunteers 
in each continent and by plastic type.  
")

# plot to titles
p.title <- 
ggplot()+
  labs(title = title, 
       subtitle = subtitle) +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(family = serif,
                                  face = "bold",
                                  size = 52, 
                                  hjust = .5),
        plot.subtitle = element_markdown(family = sans , 
                                         size = 14, 
                                         hjust = .5))


# Chart -------------------------------------------------------------------

chart <- 
  ggdraw(graph) +
  theme(plot.margin = margin(t = 300, r = 10, b = 20, l = 10)) +
  draw_plot(p.title, 
            x = 0.5, y  = 1.05, 
            hjust = 0.5,
            vjust = 0.5) +
  draw_plot(leg,  
            x = 0.65, y  = 1.14, 
            hjust = 0.5,
            vjust = 0.5)  +
  draw_plot(leg.size,  
            width = 0.35,
            height = 0.65,
            x = 0.20, y  = 1.15, 
            hjust = 0.5,
            vjust = 0.5)  +
draw_text(c("Averange per Volunteer", "Plastic Type"),
           x = c(0.21, 0.65),
           y = rep(1.23, 2),
           family = sans) +
  draw_line(
    x = c(0.05, 0.95),
    y = rep(1.04, 2),
    linetype = 3, col = "grey10") +
  draw_line(
    x = c(0.05, 0.95),
    y = rep(1.27, 2),
    linetype = 3, col = "grey10"
  ) +
    draw_text("@avrodrigues_ | #TidyTuesday | Break Free From Plastic",
              x = 0.8, y = 0.04, size = 8, family = sans) +
  theme(plot.background = element_rect(fill = bg.col, color = NA)) 
  

# Save progress -----------------------------------------------------------

#ggsave(here(prog.dir, paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
#       chart,
#       width = 8, height = 11)


# Save final chart --------------------------------------------------------

ggsave(here("2021", "week05", "plastic.png"),
       chart,
       width = 8, height = 11)  

