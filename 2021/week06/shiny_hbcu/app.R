# load packages -----------------------------------------------------------

library(shiny)
library(tidyverse)
library(here)
library(bslib)


# load data ---------------------------------------------------------------

hs_students <- read.csv(here("www", "hs_students.csv"))
bach_students <- read.csv(here("www", "bach_students.csv"))
hbcu_by_gender <- read.csv(here("www", "hbcu_by_gender.csv"))
hbcu_by_program <- read.csv(here("www", "hbcu_by_program.csv"))

df_hs <- 
hs_students %>% 
    drop_na() %>%
    mutate(across(total:standard_errors, ~.x/100)) 
df_bach <- 
bach_students %>% 
    drop_na() %>%
    mutate(across(total:standard_errors, ~.x/100))

df_degree <- list(hs = df_hs, bach = df_bach)


# set shiny theme and colors ----------------------------------------------

my_theme <- bs_theme(bg = "#333333", fg = "#ffb90f", 
                     primary = "#FFECBE", secondary = "#C6B383", 
                     success = "#47DB69", warning = "#FFDB07", danger = "#F73346", 
                     base_font = font_google("Inter"), code_font = font_google("Fira Code"), 
                     heading_font = "Open Serif", `enable-rounded` = FALSE)



bg.col <- "#333333" # equal bg in theme
fg.col <- "#ffb90f" # equal fg in theme

col_p_leng <- c("#20b2aa" ,"#b22028")
col.et <-
    c("grey90",
      "#ff00ff",
      "#00ff00", 
      "#f6546a", 
      "#00ced1", 
      "#b27fb2",
      "#ffff66", 
      "brown")
names(col.et) <- unique(df_hs$race)


# Set texts ---------------------------------------------------------------

title <- "Historically Black Colleges and Universities"

title_degree <- c(hs = "High School's Degree Attainment",
                  bach = "Bachelor's Degree Attainment")



# User interface ----------------------------------------------------------

ui <- navbarPage(
    "HBCU",
    theme = my_theme,
    tabPanel(
        "Introduction", 
        fluidPage(
            fluidRow(
                h1(title),
                br(),
                includeMarkdown("Intro.Rmd"),
                br()
                ))
            ),
    tabPanel(
        "Plot",
        tabsetPanel(
            tabPanel("Degree Attainment",
                     fluidRow(
                     column(3,
                            selectInput("dg.data", "Choose data",
                                        choices = c("High School" = "hs",
                                                    "Bachelor" = "bach")), 
                            selectizeInput("dg.race", "Select ethnic group:",
                                           choices = unique(df_hs$race),
                                           selected = "Black",
                                           multiple = T)
                            ),
                     column(1),
                     column(7,
                         h3(textOutput("dg.title")),
                         plotOutput("dg.plot")
                         ),
                     column(1)
                     )
                     ), 
            tabPanel(
                "College Enrollment", 
                fluidRow(
                    column(
                        3,
                        selectInput(
                            "prog.lg", "Program Length:",
                            choices = unique(hbcu_by_program$program_length), 
                            selected = "2 years"
                        )
                           ),
                    column(1),
                    column(7,
                           h3(textOutput("prog.title")),
                           "",
                           plotOutput("prog.plot")
                    ),
                    column(1)
                )
                     )
            )
        )
    )




# Server ------------------------------------------------------------------


server <- function(input, output) {
    
    output$dg.title <- renderText({
        dg.reac <- input$dg.data
        title_degree[dg.reac]
    })
    
    output$dg.plot <- renderPlot({
        df_plot <- df_degree[[input$dg.data]] %>%  
            filter(race %in% input$dg.race) 
        
        df_plot %>% 
            ggplot(aes(
                year,
                y = total,
                ymax = total + standard_errors,
                ymin = total - standard_errors,
                group = race
            )) +
            geom_ribbon(aes(fill = race), alpha = .40) +
            scale_fill_manual(values = col.et, name = "") +
            geom_line(aes(color = race)) +
            scale_color_manual(values = col.et, name = "") +
            scale_y_continuous(labels = scales::percent,
                               limits = c(0,1)) +
            labs(x = "Year",
                 y =  "Percentege of Population") +
            theme_bw() +
            theme(plot.title.position = "plot", 
                  plot.background = element_rect(fill = bg.col, color = bg.col),
                  panel.background = element_rect(fill = bg.col, color = NA),
                  legend.background = element_rect(fill = bg.col, color = NA),
                  legend.key = element_rect(fill = bg.col, color = NA),
                  text = element_text(color = fg.col),
                  axis.text = element_text(color = fg.col),
                  panel.grid = element_line(color = paste0(fg.col, "35"))
                  )
        
    })
    
    output$prog.title <- renderText({
        paste("College Enrolment in", 
              str_to_title(input$prog.lg),
              "Program")
    })
    
    
    output$prog.plot <- renderPlot({
        
        hbcu_by_program %>% 
            filter(program_length == input$prog.lg) %>% 
            ggplot(aes(x = year, y = students/1000, color = public_private )) +
            geom_line(size = 1) +
            scale_color_manual(values = col_p_leng, name = "") +
            labs(x = "Year", y = "Number of Students\n(thousands)") +
            scale_x_continuous(breaks = c(1976, seq(1980, 2015, 5))) +
            scale_y_continuous(n.breaks = 5, limits = c(0,250)) +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  plot.title.position = "plot", 
                  plot.background = element_rect(fill = bg.col, color = bg.col),
                  panel.background = element_rect(fill = bg.col, color = NA),
                  legend.background = element_rect(fill = bg.col, color = NA),
                  legend.key = element_rect(fill = bg.col, color = NA),
                  text = element_text(color = fg.col),
                  axis.text = element_text(color = fg.col),
                  panel.grid = element_line(color = paste0(fg.col, "35"))
            )
        
    })
}


#  Run the application ----------------------------------------------------
shinyApp(ui = ui, server = server)

