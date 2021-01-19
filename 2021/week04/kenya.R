# Kenya
library(tidyverse)
library(reshape2)
library(rnaturalearth)
library(sp)
library(sf)
library(raster)
library(rgdal)
library(dplyr)
library(cowplot)
library(extrafont)
library(here)

## Loading fonts 
font_import()
loadfonts(device = "win")
fonts <- fonts()

theme_set(theme_bw())
#remotes::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus)

# Load maps ---------------------------------------------------------------

africa <- ne_countries(continent = "Africa")
# Kenya county: https://data.humdata.org/dataset/47-counties-of-kenya
kenya <- readOGR(here("2021", "week04", "Shapefile", "ke_county.shp"))
crs(kenya) <- crs(africa)
kenya_sf <- st_as_sf(kenya)
africa_sf <- st_as_sf(africa)

# dir.create(here("2021", "week04"))

# Load data ---------------------------------------------------------------

gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/gender.csv')
crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/crops.csv')
households <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv')


k_crops <-
  crops %>% 
  filter(SubCounty != "KENYA") %>% 
    dplyr::select(-Farming) %>% 
    mutate(across(where(is.numeric), 
                  ~(.x/1000)),
           SubCounty = str_replace_all(SubCounty, "[:punct:]", " ")
           ) %>% 
  rename(county = SubCounty,
         Cashew = `Cashew Nut`,
         Miraa = `Khat (Miraa)`)


kenya_sf$county <- toupper(str_replace_all(kenya_sf$county, "[:punct:]", " "))
kenya_sf_crop <- left_join(kenya_sf, k_crops)


# Set colors --------------------------------------------------------------

plot.bg <- "grey20"
text.col <- "grey99"
pol.border <- "grey70"


crop.col <- data.frame(
  low.base  = "grey50",
  Tea  = "#d64100",
  Coffee = "#e30000",
  Avocado = "#2ac81b",
  Citrus = "#ffa700",
  Mango = "#ffdb00",
  Coconut = "#b27337",
  Macadamia = "#fff6a0",
  Cashew = "#ffd88f",
  Miraa = "#b41467"
    
)


# Set theme ---------------------------------------------------------------


setings <- list( theme_void(),
    theme(plot.background = element_rect(fill = plot.bg, 
                                         color = NA),
          legend.title = element_text(color = text.col,
                                      family = "Young"),
          legend.text = element_text(color = text.col,
                                     family = "Young"),
          plot.title = element_text(color = text.col,
                                    hjust = 0.5, 
                                    size = 20,
                                    family = "Young",
                                    face  = "bold"),
          plot.margin = margin(20,20,20,20)) 
)


# Maps for each crop ------------------------------------------------------

# Tea
k_tea <- 
ggplot(kenya_sf_crop) +
  geom_sf(aes(fill = Tea), color = pol.border) +
  scale_fill_gradient(low = crop.col$low.base,
                      high = crop.col$Tea,
                      na.value = "transparent",
                      name = "Pop. (k)",
                      n.breaks = 8) +
  labs(title = "Tea") +
  setings

# Coffee
k_coffee <- 
  ggplot(kenya_sf_crop) +
  geom_sf(aes(fill = Coffee), color = pol.border) +
  scale_fill_gradient(low = crop.col$low.base,
                      high = crop.col$Coffee,
                      na.value = "transparent",
                      name = "Pop. (k)",
                      n.breaks = 8) +
  labs(title = "Coffee") +
  setings

# Avocado 
k_avocado <- 
  ggplot(kenya_sf_crop) +
  geom_sf(aes(fill = Avocado), color = pol.border) +
  scale_fill_gradient(low = crop.col$low.base,
                      high = crop.col$Avocado,
                      na.value = "transparent",
                      name = "Pop. (k)",
                      n.breaks = 5) +
  labs(title = "Avocado") +
  setings

# Citrus 
k_citrus <- 
  ggplot(kenya_sf_crop) +
  geom_sf(aes(fill = Citrus), color = pol.border) +
  scale_fill_gradient(low = crop.col$low.base,
                      high = crop.col$Citrus,
                      na.value = "transparent",
                      name = "Pop. (k)",
                      n.breaks = 5) +
  labs(title = "Citrus") +
  setings

# Mango 
k_mangoo <- 
  ggplot(kenya_sf_crop) +
  geom_sf(aes(fill = Mango), color = pol.border) +
  scale_fill_gradient(low = crop.col$low.base,
                      high = crop.col$Mango,
                      na.value = "transparent",
                      name = "Pop. (k)",
                      n.breaks = 10) +
  labs(title = "Mangoo") +
  setings

# Coconut 
k_coconut <- 
  ggplot(kenya_sf_crop) +
  geom_sf(aes(fill = Coconut), color = pol.border) +
  scale_fill_gradient(low = crop.col$low.base,
                      high = crop.col$Coconut,
                      na.value = "transparent",
                      name = "Pop. (k)",
                      n.breaks = 5) +
  labs(title = "Coconut") +
  setings

# Macadamia 
k_macadamia <- 
  ggplot(kenya_sf_crop) +
  geom_sf(aes(fill = Macadamia), color = pol.border) +
  scale_fill_gradient(low = crop.col$low.base,
                      high = crop.col$Macadamia,
                      na.value = "transparent",
                      name = "Pop. (k)",
                      n.breaks = 10) +
  labs(title = "Macadamia") +
  setings

# Cashew 
k_cashew <- 
  ggplot(kenya_sf_crop) +
  geom_sf(aes(fill = Cashew), color = pol.border) +
  scale_fill_gradient(low = crop.col$low.base,
                      high = crop.col$Cashew,
                      na.value = "transparent",
                      name = "Pop. (k)",
                      n.breaks = 6) +
  labs(title = "Cashew nut") +
  setings

# Miraa 
k_miraa <- 
  ggplot(kenya_sf_crop) +
  geom_sf(aes(fill = Miraa), color = pol.border) +
  scale_fill_gradient(low = crop.col$low.base,
                      high = crop.col$Miraa,
                      na.value = "transparent",
                      name = "Pop. (k)",
                      n.breaks = 6) +
  labs(title = "Khat (Miraa)") +
  setings
 


# Chart crops -------------------------------------------------------------

crop_chart <- plot_grid(k_avocado, k_cashew, k_citrus,
                        k_coconut, k_coffee, k_macadamia,
                        k_mangoo, k_miraa, k_tea,
                        nrow = 3, ncol = 3)

k.col <- ifelse(africa_sf$sovereignt == "Kenya", "#CC0101", "grey30")

# Africa
kenya_map <- 
ggplot(africa_sf) +
  geom_sf(fill = k.col, color = "grey70") +
  setings +
  theme(plot.background = element_rect(fill = "transparent")) 

description <- "Size of the population farming 
permanent crops by type and county." 


# Graph -------------------------------------------------------------------


crop_graph <- 
ggdraw(crop_chart) +
  labs(caption = "@avrodriges_ | #TidyTuesday | Source: rKenyaCensus") +
  setings +
  theme(plot.margin = margin(250,10,50,10),
        plot.caption = element_text(color = text.col,
                                    family = "Segoe UI")
        ) +
  draw_plot(kenya_map, 
            x = 0.185, y = 1.17,
            hjust = 0.5,
            vjust = 0.5,
            scale = 0.35) +
  draw_text("Permanent Crops in Kenya",
            x = 0.615, y = 1.25,
            size = 37, 
            fontface = "bold",
            family = "Young",
            color = text.col) +
  draw_text(description,
            x = 0.615, y = 1.13,
            size = 18, 
            family = "Segoe UI",
            color = text.col) +
  draw_line(
    x = c(0.1,0.9),
    y = c(1,1),
    linetype = 3, col = "grey90"
  ) 
  


# Save  -------------------------------------------------------------------


ggsave(here("2021", "week04", "kenya_crops.png"),
       crop_graph,
       width = 10,
       height = 14)
      
