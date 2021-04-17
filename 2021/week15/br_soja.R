
# load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(ggtext)
library(patchwork)
library(here)
library(ragg)

addUnits <- function(n) {
  n <- abs(n)
  labels <- ifelse(
    n < 1000, n,  # less than thousands
    ifelse(
      n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
      ifelse(
        n < 1e9, paste0(round(n/1e6, 1), 'M'),  # in millions
        ifelse(
          n < 1e12, paste0(round(n/1e9), 'B'), # in billions
          ifelse(
            n < 1e15, paste0(round(n/1e12), 'T') # in trillions
          )))))
  return(labels)
}

# load data ---------------------------------------------------------------

tuesday <- tidytuesdayR::tt_load("2021", week = 15)

soja  <- 
tuesday$soybean_use %>% 
  filter(entity  == "Brazil")  %>%
  pivot_longer(
    human_food:processed, 
    names_to = "type", 
    values_to = "tons"
    )


lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")


year.processed <- 
lm_mod %>% 
  fit(tons ~ year, data = soja %>%  filter(type == "processed")) %>% 
  tidy() %>% 
  filter(term == "year") %>% 
  mutate(ci = std.error*1.96) %>% 
  select(estimate, ci) %>% 
  round(0)

year.animal.feed <- 
lm_mod %>% 
  fit(tons ~ year, data = soja %>%  filter(type == "animal_feed")) %>% 
  tidy() %>% 
  filter(term == "year") %>% 
  mutate(ci = std.error*1.96) %>% 
  select(estimate, ci) %>% 
  round(0)

year.human.feed <- 
lm_mod %>% 
  fit(tons ~ year, data = soja %>%  filter(type == "human_food")) %>% 
  tidy() %>% 
  filter(term == "year") %>% 
  mutate(ci = std.error*1.96) %>% 
  select(estimate, ci) %>% 
  round(0)

increase.df <- 
bind_rows(year.processed, 
          year.animal.feed,
          year.human.feed) %>% 
  mutate(type = c("processed", "animal_feed", "human_food"))


titulo <- "A produção de soja no Brasil"

sub <- "Entre 1961 e 2013 a produção de soja tem aumentado continuamente.<br>
Em 2013, foram produzidas 35 milhões de toneladas de 
<b style = 'color:#0E5F87;'>soja processada </b>, 725 mil<br> 
toneladas para <b style = 'color:#A92874;'>consumo animal</b>
 e 637 mil toneladas para <b style = 'color:#249759;'>consumo humano</b>."

desc.proc <- "A soja processada (em forma de óleo, biocombustível ou como 
ração animal) corresponde ao maior destino da soja brasileira.
Essa produção cresce em média 714.415 ton/ano."

desc.hum <-
"Já o uso de soja para consumo animal 
e humano crescem em média 14.558 e 
11.459 ton/ano, repectivamente."


line.cols <- c("#0E5F87", 
               "#A92874",
               "#249759")
names(line.cols) <- c("processed", "animal_feed", "human_food")

soja_plot <- 
soja %>% 
ggplot(aes(x = year, y = tons, color = type)) +
  geom_line(show.legend = F,
            size = 1.5, alpha = 0.7) +
  scale_color_manual(values = line.cols) +
  scale_x_continuous(breaks = seq(1960, 2015, 5)) +
  scale_y_continuous(labels = addUnits, n.breaks = 8) +
  labs(title = titulo, subtitle = sub, x = "Ano", y = "Toneladas",
       caption = "@avrodrigues_ | Fonte: Our World in Data") +
  geom_text(
    aes(x = 1980, y = 29e6, label = desc.proc),
    color = "grey20",
    family = "Inter", 
    size = 3
  ) +
  annotate(
    "curve", 
    x = 2002, xend = 1997,
    y = 25.5e6, yend = 29e6, 
    color = "grey15", curvature = 0.3
    ) +
  geom_text(
    aes(x = 1995, y = 6.5e6, label = desc.hum),
    color = "grey20",
    family = "Inter", 
    size = 3
  ) +
  annotate(
    "curve", 
    x = 1985.5, xend = 1980.2,
    y = 5.25e6, yend = 1e6, 
    color = "grey15", curvature = 0.3
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(
      family = "Linux Libertine", 
      size = 25,
      face = "bold"
      ),
    plot.subtitle = element_markdown(
      family = "Linux Libertine", 
      size = 14, 
      lineheight= 1
    ),
    axis.text = element_text(
      family =  "Cabin",
      size = 10
    ),
    axis.title = element_text(
      family =  "Cabin",
      size = 12
    ),
    plot.title.position = "plot",
    legend.position = "none",
    plot.margin = margin(20,20,10,20),
    plot.background = element_rect(fill = "grey90", color = NA), 
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.caption = element_text(
      family =  "Cabin",
      size = 8
    )
    
  )



# save progress -----------------------------------------------------------

 agg_png(
 here("2021",
      "week15", 
      "progress", 
      paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
 width = 7,
 height = 6,
 units = "in",
 res = 300
 )
 soja_plot
 dev.off()


# save plot ----------------------------------------------------------------
agg_png(
  here("2021",
       "week15", 
       "br_soja.png"),
  width = 7,
  height = 6,
  units = "in",
  res = 300
)
 soja_plot
dev.off()
