# Load Packages -----------------------------------------------------------
library(tidyverse)
library(here)

# create directory
if(!dir.exists(here("2021", "week06"))) dir.create(here("2021", "week06"))
# create directory to save progress
prog.dir <- here("2021", "week06", "progress")
if(!dir.exists(prog.dir)) dir.create(prog.dir)


# load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 6)

# Preparing data ----------------------------------------------------------
# high scholl students

hs_students <- 
  tuesdata$hs_students %>%
  mutate(Total = if_else(Total > 10000, str_sub(Total, 1, 4) %>% as.double(), Total)) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  pivot_longer(-Total) %>% 
  filter(!str_detect(name, "Total - Asian/Pacific Islander")) %>% 
  mutate(name = str_remove(name, "Asian/Pacific Islander - |1")) %>% 
  separate(name, into = c("stat","race"), sep = " - ", fill = "left")  %>%
  mutate(
    race = str_remove_all(
      race, 
      ", percent of all persons age 25 and over|\r\n")
  ) %>% 
  mutate(stat = replace_na(stat, "Total"),
         race = str_replace(
           race, 
           "American Indian/Alaska Native", 
           "Indigenous")
         ) %>% 
  rename(year = Total) %>% 
  drop_na(year) %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  janitor::clean_names() 


# bachalor students
bach_students <- 
  tuesdata$bach_students %>%
  mutate(across(everything(), as.numeric)) %>%
  pivot_longer(-Total) %>% 
  filter(!str_detect(name, "Total - Asian/Pacific Islander")) %>%
  mutate(name = str_remove(name, "Asian/Pacific Islander - |1")) %>% 
  separate(name, into = c("stat","race"), sep = " - ", fill = "left") %>% 
  mutate(stat = replace_na(stat, "Total")) %>% 
  rename(year = Total) %>%
  pivot_wider(names_from = stat, values_from = value)  %>%
  janitor::clean_names() %>%
  mutate(
    race = str_remove_all(
      race, 
      ", percent of all persons age 25 and over|\r\n"),
    race = str_replace(
      race, 
      "American Indian/Alaska Native", 
      "Indigenous")
  )

# HCBU
hbcu_by_gender <- 
  tuesdata$hbcu_all %>%
    # We only need year and gender columns
  select(Year, Males, Females) %>% 
  # Convert to tidy format, collapsing male/female into one descriptor field
  pivot_longer(Males:Females,
               names_to = "gender",
               values_to = "students") %>% 
  # Convert from plural to singular for cleaner data
  # "s$" specifies an "s" character at the end of a string 
  # ("$" is end of string in regular expressions)
  mutate(gender = str_remove(gender, "s$"))

  
  
hbcu_by_program <- 
  tuesdata$hbcu_all %>%
    janitor::clean_names() %>% 
    # We need fields with "public" or "private" in the name
    # (They also have 2- vs 4-year)
    # We DON'T need fields with "total" in the name, since this is redundant
    select(year,
           contains(c("public", "private")),
           -contains("total")) %>%
    # names_pattern argument does the heavy lifting
    # It separates names into groups, as specified by parentheses "(group)"
    # Field names are structured so that program length is followed by public/private
    # We also specift "x_" as an optional argument using regular expressions
    pivot_longer(cols = x4_year_public:x2_year_private,
                 names_pattern = "[x_]?(.*)_(.*)",
                 names_to = c("program_length", "public_private"),
                 values_to = "students") %>% 
    mutate(program_length = paste(parse_number(program_length), "years"),
           public_private = str_to_title(public_private))
  

# Save tidy data for Shiny app --------------------------------------------

www_dir <- here("2021", "week06", "shiny_hcbu", "www")


write.csv(hs_students, here(www_dir, "hs_students.csv"), row.names = F)
write.csv(bach_students, here(www_dir, "bach_students.csv"), row.names = F)
write.csv(hbcu_by_gender, here(www_dir, "hbcu_by_gender.csv"), row.names = F)
write.csv(hbcu_by_program, here(www_dir, "hbcu_by_program.csv"), row.names = F)
