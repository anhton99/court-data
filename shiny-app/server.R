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
library(gt)
library(shinycssloaders)

slc_value <- readRDS("data/slc_value.rds")
middle_table <- readRDS(middle_table, file = "data/middle_table.rds")
mid_race_percent <- readRDS("data/mid_race_percent.rds")
sum_count_pt <- readRDS(sum_count_pt, file = "data/sum_count_pt.rds")
mid_dis <- readRDS(mid_dis, file = "data/mid_dis.rds")
mid_sup <- readRDS(mid_sup, file = "data/mid_sup.rds")
total_sum_count <- readRDS(total_sum_count, file = "data/total_sum_count.rds")
m3 <- readRDS(total_sum_count, file = "data/m3.rds")
case_pop_race <- readRDS(case_pop_race, file = "data/case_pop_race.rds")





# plot mid_dis 
shinyServer(function(input, output){
  
  output$middle_table <- gt::render_gt({
    middle_table <- middle_table %>% 
      gt()
  })
  
  output$mid_dis <- renderPlot({
    mid_dis %>% 
      ggplot(aes(x = fct_reorder(district_court, count), fill = district_court)) +
      geom_col(aes(y = count)) +
      geom_text(aes(label = comma(count), y = count), 
                nudge_y = 1100, size = 3) + 
      coord_flip() + 
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous(labels = comma_format()) + 
      labs(title = "Total Cases by District Courts",
           subtitle = "Year: 2014-2019",
           x = "",
           y = "")
  })

# Plot mid_sup
  
  output$mid_sup <- renderPlot({
    
    mid_sup %>% 
      ggplot(aes(x = fct_reorder(court_location, count), fill = court_location)) +
      geom_col(aes(y = count)) +
      geom_text(aes(label = comma(count), y = count), nudge_y = 50, size = 3) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = comma_format()) + 
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Total Cases by Superior Courts",
           subtitle = "Year: 2014-2019",
           x = "",
           y = "") +
      scale_x_discrete(labels = c("Lowell \n Superior Court", 
                                  "Middlesex County \n Superior Court"))
  })
  

# Table 
  
  output$sum_count <- render_gt({
    sum_count_pt %>% 
      summarize(min = percent(min(count)), 
                median = percent(median(count)),
                max = percent(max(count)),) %>% 
      pivot_longer(cols = everything(), 
                   names_to = "summary", values_to = "value") %>%
      gt() %>%
      tab_header(title = "Total cases as % of Population served",
                 subtitle = "12 district courts in Middlesex County")
  })

# Plot sum_count
  output$sum_count_pt <- renderPlot({
    
    sum_count_pt %>%
      pivot_longer(cols = -district_court, 
                   names_to = "ratio", 
                   values_to = "value") %>%
      mutate(order = rep(seq(1, 12, by = 1), each = 2)) %>% 
      mutate(order = rev(order)) %>% 
      ggplot(aes(x = fct_reorder(district_court, order), 
                 y = value, fill = forcats::fct_rev(ratio))) +
      geom_col(position = "stack") +
      coord_flip() +
      labs(title = "Cases as % Total Population served",
           subtitle = "District courts, Middlesex County",
           x = "",
           y = "Population") +
      scale_fill_manual(name = "",
                        values = c("red", "blue"),
                        breaks = c("count", "stack"),
                        labels = c("Total cases", "Population served")) +
      scale_y_continuous(labels = percent_format()) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
  })
  
# Plot total_sum_count
  output$total_sum_count <- renderPlot({
    
    total_sum_count %>%
      ggplot(aes(x = district_court, y = value, fill = type)) +
      geom_col(position = "dodge") + 
      labs(title = "Cases by Total Population served",
           subtitle = "District courts, Middlesex County",
           x = "",
           y = "Population") +
      scale_fill_manual(name = "",
                        values = c("red", "blue"),
                        breaks = c("count", "sum"),
                        labels = c("Total cases", "Population served")) +
      scale_y_continuous(labels = percent_format()) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      coord_flip()
  })
  
# Plot m3
  output$dispo_offense <- renderPlot({
    
    m3 %>% 
      filter(district_court == input$district_court) %>%
      filter(stage == input$stage) %>%
      ggplot(aes(x = length)) +
      geom_histogram(bins = 1000, fill = "blue3") +
      labs(title = "Total cases by the number of days", 
           x = "# of days", y = "Total cases with # days") + 
      theme_minimal() +
      geom_vline(aes(xintercept = median(length), color = "median")) + 
      scale_color_manual(name = "", values = c(median = "red")) 
    
  })

# Plot m3 filing dispo

  output$filing_dispo <- renderPlot({
    
    m3 %>%
      filter(district_court == input$district_court_2) %>%
      filter(stage == "dispo_filing_diff") %>%
      ggplot(aes(x = length)) +
      geom_histogram(bins = 1000, fill = "blue3") + 
      labs(title = "Total cases by the number of days \n between filing date and disposition date",
           x = "# of days", y = "Total cases with # days") +
      theme_minimal() +
      geom_vline(aes(xintercept = median(length), color = "median")) + 
      scale_color_manual(name = "", values = c(median = "red")) 
    
  })
  
# Plot
  output$case_pop_race <- renderPlot({
    
    case_pop_race %>% 
      ggplot(aes(x = factor(race, levels = c("White", "Black", "Hispanic")), 
                 y = value, fill = forcats::fct_rev(yes))) +
      geom_col(position = "dodge") +
      theme_minimal() +
      scale_fill_manual(name = "Race",
                        values = c("salmon", "dodgerblue"),
                        breaks = c("race_pop_percent", "cases"),
                        labels = c("% Total population", "% Total cases")) +
      labs(title = "Cases by Population (by Race)",
           subtitle = "All District Courts, Middlesex County",
           x = "",
           y = "") +
      geom_text(aes(label = percent(value), y = value), 
                position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
      scale_y_continuous(limits = c(0,1), labels = percent_format()) +
      theme(legend.position = "top")
    
  })

# Map
  
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

        


