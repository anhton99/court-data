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

juvenile_6_years <- readRDS(file = "data/juvenile_6_years.rds")
juvenile_stats_2014<- readRDS("data/juvenile_stats_2014.rds")

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
