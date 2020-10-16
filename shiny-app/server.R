#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(readxl)
library(janitor)

juvenile_stats_2014 <- read_excel("Juvenile_stats_2014.xlsx") %>%
  clean_names() %>% 
  select(!total) %>%
  mutate(year = 2014)

juvenile_stats_2015 <- read_excel("Juvenile_stats_2015.xlsx") %>%
  clean_names() %>% 
  select(!grand_total) %>%
  rename(barnstable = barnstable_town_of_plymouth) %>%
  mutate(year = 2015)

juvenile_stats_2016 <- read_excel("Juvenile_stats_2016.xlsx") %>%
  clean_names() %>%
  select(!total) %>%
  rename(barnstable = barnstable_town_of_plymouth) %>%
  mutate(year = 2016) %>%
  mutate(suffolk = round(suffolk),
         bristol = round(bristol),
         essex = round(essex),
         hampden = round(hampden),
         middlesex = round(middlesex),
         worcester = round(worcester))

juvenile_stats_2017 <- read_excel("Juvenile_stats_2017.xlsx") %>%
  clean_names() %>%
  select(!total) %>%
  rename(barnstable = barnstable_town_of_plymouth) %>%
  mutate(suffolk = round(suffolk),
         bristol = round(bristol),
         essex = round(essex),
         hampden = round(hampden),
         middlesex = round(middlesex),
         worcester = round(worcester))

juvenile_stats_2018 <- read_excel("Juvenile_stats_2018.xlsx") %>%
  clean_names() %>%
  select(!total) %>%
  rename(barnstable = barnstable_town_of_plymouth) %>%
  mutate(year = 2018) %>%
  mutate(suffolk = round(suffolk),
         bristol = round(bristol),
         essex = round(essex),
         hampden = round(hampden),
         middlesex = round(middlesex),
         worcester = round(worcester))

juvenile_stats_2019 <- read_excel("Juvenile_stats_2019.xlsx") %>%
  clean_names() %>%
  select(!total) %>%
  rename(barnstable = barnstable_town_of_plymouth) %>%
  mutate(year = 2019) %>%
  mutate(suffolk = round(suffolk),
         bristol = round(bristol),
         essex = round(essex),
         hampden = round(hampden),
         middlesex = round(middlesex),
         worcester = round(worcester))

juvenile_6_years <- 
  bind_rows(list(juvenile_stats_2014, juvenile_stats_2015,
                 juvenile_stats_2016, juvenile_stats_2017,
                 juvenile_stats_2018, juvenile_stats_2019),
            .id = "year") %>% 
  mutate(year = as.numeric(year) + 2013) %>%
  group_by(year)
# Define server logic required to draw a histogram

shinyServer(function(input, output) {
    output$juvenile_case_type <- renderPlot({
       ggplot(juvenile_stats_2014, 
              aes(x = case_type, y = middlesex)) +
        coord_flip() +
        geom_col(fill = "navy") + 
        labs(
        title = "Cases brought to Juvenile Courts in Middlesex County, MA",
              x = " ",
              y = "Number of cases per kind") +
        theme_bw() +
        theme(plot.title = element_text(size = 18),
              axis.text = element_text(size = 12))
        
    })

})
