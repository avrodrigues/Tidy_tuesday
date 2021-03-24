
# load pakages ------------------------------------------------------------

library(tidyverse)
library(here)
library(ragg)
library(ggfx)


# load and process data ---------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 13)

unvotes <- tuesdata$unvotes

glimpse(unvotes)
glimpse(tuesdata$roll_calls)
unique(tuesdata$issues$issue)

votes_br <- 
tuesdata$issues %>% 
  left_join(tuesdata$unvotes, by = 'rcid') %>% 
  left_join(tuesdata$roll_calls, by = 'rcid') %>% 
  filter(country_code == 'BR') %>% 
  mutate(year = year(date)) 

t_year_issue <-
votes_br %>%  
  group_by(year, issue) %>% 
  summarise(total = n()) 

votes_br_perc <-
votes_br %>% 
  group_by(year, issue, vote) %>% 
  summarise(count = n()) %>% 
  left_join(t_year_issue) %>% 
  mutate(perc = count/total*100,
         vote = factor(vote, c("no", "abstain", "yes")),
         issue = case_when(
           issue == "Colonialism" ~ "Colonialismo",
           issue == "Economic development" ~ "Desenvolvimento Econômico",
           issue == "Human rights" ~ "Direitos Humanos",
           issue == "Palestinian conflict" ~ "Conflitos Palestinos",
           issue == "Arms control and disarmament" ~ "Controle de Armas e Desarmamento",
           issue == "Nuclear weapons and nuclear material" ~ "Armas Nucleares e Material Nuclear"
         )) 


# plot --------------------------------------------------------------------

#colors
col.votes <- c("yes" = "#3F90DF",
               "no" = "#cf2020", 
               "abstain" = "#cbcba9")

back <- "grey35"


g.plot <- 
ggplot(
  votes_br_perc, 
  aes(y = perc, x = year, fill = vote)
  ) +
  geom_col()+
  scale_fill_manual(
    values = col.votes, name = "", labels = c("Não", "Abstenção", "Sim")) +
  scale_x_continuous(
    breaks = c(1946, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2019),
    expand = expansion(mult = 0, add = 0),
    limits = c(1945, 2020)
    ) +
  facet_wrap("issue", ncol = 1)  +
  theme_void() +
  labs(
    title = "Votos do Brasil nas Nações Unidas\npor tipo de matéria desde 1946",
    caption = "@avrodrigues_ | Fonte: Harvard's Dataverse",
    x = NULL, 
    y = NULL) +
  theme(
    plot.title = element_text(
      family = "Aldo", face = "bold", color = "white",
      size = 22, hjust = 0.5, margin = margin(5,20,35,20)
      ),
    plot.caption = element_text(
      family = "Cabin", color = "white", face = "bold",
       size = 7
    ),
    strip.text = element_text(
      family = "Aldo", color = "white",
      size = 15, hjust = 0, margin = margin(5,5,5,5)
    ),
    axis.text.x = element_text(
      family = "Cabin", color = "white", face = "bold",
      angle = 90, size = 9, margin = margin(5,5,5,5)
    ),
    axis.line.x = element_line(
      color = "grey95"
      ),
    plot.margin = margin(20,20,5,20),
    plot.background = element_rect(fill = back, color = NA),
    legend.position = "top",
    legend.text = element_text(
      family = "Aldo", color = "white",
      size = 11, hjust = 0.5, margin = margin(5,5,5,5)
    )
  )
  
  
# save progress -----------------------------------------------------------

agg_png(
here("2021",
     "week13", 
     "progress", 
     paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
width = 6,
height = 9,
units = "in",
res = 300
)
g.plot
dev.off()


# save plot ----------------------------------------------------------------
agg_png(
  here("2021",
       "week13", 
       "un_votes.png"),
  width = 6,
  height = 9,
  units = "in",
  res = 300
)
g.plot
dev.off()


