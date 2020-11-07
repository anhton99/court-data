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
library(tidycensus)
library(viridis)
library(leaflet)
library(stringr)
library(sf)

slc_value <- readRDS("data/slc_value.rds")
mid_race_percent <- readRDS("data/mid_race_percent.rds")

shinyServer(function(input, output) {
    
    output$map <- renderLeaflet({

      
      pal <- colorNumeric(palette = "viridis", 
                          domain = slc_value$estimate)
      
      st_crs(slc_value) <- 4326
      
      slc_value %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet(width = "100%") %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.7,
                    color = ~ pal(estimate)) %>%
        addLegend("bottomright", 
                  pal = pal, 
                  values = ~ estimate,
                  title = "Median Income Value",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 1)
      
    })
    
    output$mid_race_graph <- renderPlot({
      
      mid_race_percent %>%
        filter(district_court == input$district_court) %>%
        ggplot(aes(x = percent_race, y = percent)) +
        geom_col(fill = "blue") +
        theme_bw() 
      
    })
    
})     
        


