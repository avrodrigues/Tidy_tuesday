
# load packages ----------------------------------------------------------
library(tidyverse)
library(here)
library(ragg)
library(ggtext)


# load data ---------------------------------------------------------------

movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')


# prepare data to plot ----------------------------------------------------

test.df <- 
  movies %>% 
  group_by(binary) %>% 
  summarise(n = n()) %>% 
  mutate(percent = paste0(round(n/sum(n) * 100, 1), "%"),
         label = c("FAILed", "PASSed"),
         test = "test", 
         binary = factor(binary, levels = c("PASS", "FAIL"))) 

p.title <- "The Bechdel Rule"
p.description <- 
"In 1985, the cartonist Alison Bechdel writes in one of her strips that to see
a movie it has to pass a rule, with three criteria:

1 - there are at least two named women in the picture;
2 - they have a conversation with each other at some point;
3 - and that conversation isn’t about a male character

1,794 Hollywood movies released between 1970-2013 were tested against the rule:   

"
col.mov <- c("#0e2f44", "#daa520")


# plot --------------------------------------------------------------------

g.plot <-
  ggplot(test.df, aes(y = test, x = n)) +
  geom_col(aes(fill = binary), 
           show.legend = F) +
  scale_fill_manual(values = col.mov) +
  geom_richtext(
    aes(
      x = c(50, 1041),
      y = rep(1.3, 2),
      label = percent
      ),
    fill = NA, 
    label.color = NA,
    family = "Bebas",
    color = "white", 
    hjust = 0,
    size = 12
    ) +
  geom_richtext(
    aes(
      x = c(50, 1041),
      y = rep(1.15, 2),
      label = label
    ),
    fill = NA, 
    label.color = NA,
    family = "Bebas",
    color = "white", 
    hjust = 0,
    size = 8
  ) +
  theme_void() +
  labs(title = p.title,
       subtitle = p.description, 
       caption = "@avrodrigues_ | source: FiveThirtyEight") +
  theme(
    plot.title = element_text(
      family = "Inter", 
      size = 28, 
      hjust = 0.5,
      margin = margin(t = 10, b = 10)
      ),
    plot.subtitle = element_text(
      family = "Inter", 
      size = 10,
      margin = margin(b = 0)
    ),
    plot.caption = element_text(
      family = "Inter", 
      size = 7
    ),
    plot.margin = margin(10,20,5,20),
    plot.background = element_rect(fill = "grey95", color = NA)
  )


# save progress -----------------------------------------------------------

# agg_png(
#   here("2021",
#        "week11", 
#        "progress", 
#        paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
#   width = 6,
#   height = 5,
#   units = "in",
#   res = 300
# )
# g.plot
# dev.off()


# save final plot ---------------------------------------------------------

agg_png(
  here("2021",
       "week11", 
       "the_rule.png"),
  width = 6,
  height = 5,
  units = "in",
  res = 300
)
g.plot
dev.off()
