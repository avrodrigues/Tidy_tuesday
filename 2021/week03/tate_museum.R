library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggridges)
library(grid)
library(gridtext)
library(here)

# dir.create(here("2021", "week03"))


# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 3)

artwork <- tuesdata$artwork
artists <- tuesdata$artists

dim(artwork)
dim(artists)


# preparing data ----------------------------------------------------------


top.artists <-
  artwork %>% 
  group_by(artist) %>% 
  summarise(n = n()) %>% 
  mutate(n_percent = (n/sum(n))*100) %>% 
  arrange(desc(n)) %>% 
  top_n(5)

top.countries <-
  artists %>% 
  mutate(country = sapply(str_split(placeOfBirth, ", "), "[", 2)) %>% 
  group_by(country) %>% 
  summarise(n = n()) %>% 
  mutate(n_percent = (n/sum(n))*100) %>% 
  drop_na() %>% 
  arrange(desc(n)) %>% 
  top_n(5)
  
artists.country <- 
  artists %>% 
  mutate(country = sapply(str_split(placeOfBirth, ", "), "[", 2)) %>% 
  select(id, name, country) %>% 
  rename(artistId = id, artist = name)

acquisition.artwork <-
  artwork %>% 
  select(artistId, artist, acquisitionYear) %>% 
  right_join(artists.country) 


# preparing plots ---------------------------------------------------------


# colors 
bg.col <- "#f5f5dc"
txt.col <- "gray20"
artists.col <- "#992040"
country.col <- "#6b3b6b"


geom_text_size <- 3

top.artists.plot <- 
top.artists %>% 
  mutate(artist = fct_reorder(artist, n)) %>% 
  ggplot() +
  geom_col(aes(y = artist, x = n), fill = artists.col, show.legend = F) +
  scale_x_continuous(labels = c("0", "10k", "20k", "30k", "40k")) +
  geom_text(aes(x = n - c(500, rep(-500, 4)), y = 5:1, 
                label = paste0(round(n_percent, 1), 
                               "%")), 
            color = c('white', rep("black", 4)),
            hjust = c(1, rep(0, 4)),
            size = geom_text_size) +
  theme_bw() +
  labs(title = "Collection's size",
       x = "",
       y = "") 

top.countries.plot <- 
top.countries %>% 
  mutate(country = ifelse(country == "Deutschland", "Germany", country),
         country = ifelse(country == "Italia", "Italy", country),
    country = fct_reorder(country, n)) %>% 
  ggplot() +
  geom_col(aes(y = country, x = n), fill = country.col, show.legend = F) +
  geom_text(aes(x = n - c(50, rep(-20, 4)), y = 5:1, 
                label = paste0(round(n_percent, 1), 
                               "%")), 
            color = c('white', rep("black", 4)),
            hjust = c(1, rep(0, 4)),
            size = geom_text_size) +
  labs(
       x = "Number of artwoks",
       y ="") +
  theme_bw()

acq.artists.plot <- 
acquisition.artwork %>% 
  filter(artist %in% top.artists$artist) %>% 
  left_join(top.artists, by = "artist") %>% 
  mutate(artist = fct_reorder(artist, n)) %>% 
  ggplot(aes(x = acquisitionYear)) +
  geom_density_ridges2(aes(y = artist), 
                       stat = "binline", 
                       binwidth=10,
                       draw_baseline = T,
                       alpha = 0.8,
                       scale = 0.8,
                       show.legend = F, 
                      fill = artists.col,
                      color = artists.col) +
  scale_x_continuous(breaks = seq(1820, 2020, 20),
                     limits = c(1823, 2019)) +
  theme_bw() +
  labs(title = "Acquisition history",
       x = "",
       y = "") +
  theme(axis.text.y = element_blank())

acq.countries.plot <- 
acquisition.artwork %>% 
  filter(country %in% top.countries$country) %>% 
  left_join(top.countries, by = "country") %>% 
  mutate(country = ifelse(country == "Deutschland", "Germany", country),
         country = ifelse(country == "Italia", "Italy", country))%>% 
  mutate(country = fct_reorder(country, n)) %>% 
  ggplot(aes(x = acquisitionYear, fill = country, color = country)) +
  geom_density_ridges2(aes(y = country), 
                       stat = "binline", 
                       binwidth=10,
                       alpha = 0.8,
                       scale = 0.8,
                       show.legend = F, 
                       fill = country.col,
                       color = country.col) +
  scale_x_continuous(breaks = seq(1820, 2020, 20),
                     limits = c(1823, 2019)) +
  theme_bw() +
labs(
     x = "Year",
     y = "") +
  theme(axis.text.y = element_blank())


# chart -------------------------------------------------------------------
 theme_settings <- list(
   theme(panel.background = element_rect(fill = bg.col, color = NA),
         plot.background = element_rect(fill = bg.col, color = NA),
         text = element_text(size = 11),
         title = element_text(size = 10),
         panel.grid = element_line(colour = "grey90"))
   
   )

title <- textbox_grob(
  "Tate's artwork collection",
  gp = gpar(fontsize = 24)
)

text <- textbox_grob(
  "Tate museum has **69201** artworks in its collection. 
  Here you see the collection's size and acquisition history for 
  the <span style= 'color: #992040'>**top 5 artists**</span> 
  and the <span style= 'color: #6b3b6b'>**top 5 coutries**</span> 
  where the artists were born.",
  gp = gpar(fontsize = 12, col = txt.col)
)

pt.x <- 0.49
pt.y <- 0.4

chart <-
ggdraw() + 
  theme_settings +
  draw_grob(as_grob(top.artists.plot + theme_settings), 
            x = pt.x, y = pt.y,
            width = 0.5,
            height = 0.4, 
            scale = 1,
            hjust = 1,
            vjust = 0) +
  draw_grob(as_grob(acq.artists.plot + theme_settings), 
            x = pt.x, y = pt.y,
            width = 0.5,
            height = 0.4, 
            scale = 1,
            hjust = 0,
            vjust = 0) +
  draw_grob(as_grob(top.countries.plot + theme_settings), 
            x = pt.x, y = pt.y,
            width = 0.4,##### largura #####
            height = 0.37, 
            scale = 1,
            hjust = 1,
            vjust = 1) +
  draw_grob(as_grob(acq.countries.plot + theme_settings), 
            x = pt.x, y = pt.y,
            width = 0.5,
            height = 0.37, 
            scale = 1,
            hjust = 0,
            vjust = 1) +
  draw_grob(title, 
            x = 0.5,  y = 0.92,
            width = 0.95,
            height = 1, 
            scale = 1,
            hjust = 0.5,
            vjust = 0.5) +
  draw_grob(text, 
            x = 0.5,  y = 0.85,
            width = 0.95,
            height = 0.3, 
            scale = 1,
            hjust = 0.5,
            vjust = 0.5)
  

ggsave(here("2021", "week03", "tate.png"),
       chart, 
       width = 8,
       height = 6,
       units = "in")


