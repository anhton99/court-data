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
library(scales)
library(rstanarm)
library(gtsummary)
library(broom.mixed)

slc_value <- readRDS("data/slc_value.rds")
middle_table <- readRDS("data/middle_table.rds")
mid_race_percent <- readRDS("data/mid_race_percent.rds")
sum_count_pt <- readRDS("data/sum_count_pt.rds")
mid_dis <- readRDS("data/mid_dis.rds")
mid_sup <- readRDS("data/mid_sup.rds")
total_sum_count <- readRDS("data/total_sum_count.rds")
m3 <- readRDS("data/m3.rds")
case_pop_race <- readRDS("data/case_pop_race.rds")
join_count_dispo <- readRDS("data/join_count_dispo.rds")
join_adverse <- readRDS("data/join_adverse.rds")
summary_count_case <- readRDS("data/summary_count_case.rds")
middlesex_3 <- readRDS("data/middlesex_3.rds")
ratio_table <- readRDS("data/ratio_table.rds")
decline_a <- readRDS("data/decline_a.rds")
offense_total <- readRDS("data/offense_total.rds")
model_1 <- readRDS("data/model_1.rds")
model_1_tibble <- readRDS("data/model_1_tibble.rds")

# plot mid_dis 
shinyServer(function(input, output){
  
  # Output image prosecutor 
  
  output$prosecutor <- renderImage({
    list(
      src = './images/prosecutor.jpg',
      contentType='image/jpg',
      width = 595,
      height = 335,
      style="display: block; margin-left: auto; margin-right: auto;"
    )}, deleteFile = F)
  
# image middlesex race 
  
  output$middlesex_race <- renderImage({
    list(
      src = './images/middlesex_race.png',
      contentType='image/png'
    )}, deleteFile = F)
  
# image middle income 
  
  output$middlesex_income_2 <- renderImage({
    list(
      src = './images/middlesex_income_2.png',
      contentType='image/png'
    )}, deleteFile = F)
  
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
                max = percent(max(count))) %>% 
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
      ggplot(aes(x = fct_reorder(district_court, value), y = value, fill = type)) +
      geom_col(position = "dodge") + 
      labs(title = "Cases by Total Population served",
           subtitle = "District courts, Middlesex County",
           x = "",
           y = "Population") +
      scale_fill_manual(name = "",
                        values = c("red", "blue"),
                        breaks = c("count", "sum"),
                        labels = c("Total cases", "Population served")) +
      theme_minimal() +
      scale_y_continuous(labels = number_format()) +
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
  
# figure 7
  output$join_count_dispo <- renderPlot({
    
    join_count_dispo %>% 
      ungroup() %>%
      select(type, n) %>% 
      mutate(type = as.factor(type)) %>% 
      group_by(type) %>%
      summarize(count = sum(n)) %>%
      mutate(total = sum(count),
             ratio = count/total) %>%
      ggplot(aes(x = type, y = ratio, fill = type)) + 
      geom_col(position = "dodge") +
      scale_fill_brewer(palette = "Set1") + 
      guides(fill = guide_legend(nrow = 2)) +
      geom_text(aes(label = percent(ratio), y = ratio), 
                position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
      theme(legend.position = "bottom",
            axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.title = element_blank()) +
      ylim(0, 0.6) 
    
  })
  
# figure 8
  
  output$join_adverse <- renderPlot({
    
    join_adverse %>% 
      mutate(type = as.factor(type)) %>% 
      group_by(type) %>% 
      summarize(n = sum(n)) %>% 
      mutate(ratio = n/sum(n)) %>% 
      ggplot(aes(x = type, y = ratio, fill = type)) + 
      geom_col(position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + 
      geom_text(aes(label = percent(ratio), y = ratio), 
                position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
      theme(legend.position = "bottom",
            axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.title = element_blank()) +
      labs(title = "Types of Disposition Outcomes by Race",
           subtitle = "All District Courts, Middlesex County, MA") +
      ylim(0, 1) 
    
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
                        breaks = c("percent", "cases"),
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


  output$summary_count_case <- gt::render_gt({
    
    summary_count_case %>% 
      gt() %>% 
      tab_header(title = "Summary of Criminal Counts by Race",
                 subtitle = "Examining total criminal counts by total caseload for differant racial groups") %>% 
      cols_label(defendant_race = "Race",
                 count_sum = "Total counts",
                 case_sum = "Total cases", 
                 ratio = "Ratio")
    
  })

  output$ratio_table <- render_gt({
    
      
      ratio_table <- ratio_table %>% 
        filter(type == input$type) %>%
        select(-type) %>% 
        gt() %>% 
        cols_label(race = "Race",
                   charges = "Charges", 
                   per_capita = "Per Capita",
                   ratio_to_white_persons = "Ratio to White Persons")

    
  })
  
  output$outcome_dpt <- renderPlot({
    
    
    decline_a %>% 
      ungroup() %>%
      group_by(defendant_race) %>% 
      summarize(sum = sum(n)) %>% 
      right_join(decline_a, by = "defendant_race") %>% 
      select(defendant_race, type, n, sum) %>% 
      mutate(ratio = n/sum) %>% 
      mutate(ratio = round(ratio, 3)) %>% 
      mutate(type = as.factor(type)) %>% 
      ggplot(aes(x = type, y = ratio, fill = type)) + 
      geom_col(position = "dodge") +
      facet_wrap(~defendant_race) + 
      scale_fill_brewer(palette = "Set1") + 
      geom_text(aes(label = percent(ratio), y = ratio), 
                position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
      theme(legend.position = "bottom",
            axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.title = element_blank()) +
      labs(title = "Disposition Outcomes for DPT Charges",
           subtitle = "All District Courts, Middlesex County, MA") +
      ylim(0, 1) 
    
    
  })
  
  output$dispo <- renderPlot({
    
    join_count_dispo %>% 
      ggplot(aes(x = type, y = ratio, fill = type)) + 
      geom_col(position = "dodge") +
      facet_wrap(~defendant_race) + 
      scale_fill_brewer(palette = "Set1") + 
      guides(fill = guide_legend(nrow = 2)) +
      geom_text(aes(label = percent(ratio), y = ratio), 
                position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
      theme(legend.position = "bottom",
            axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.title = element_blank()) +
      labs(title = "Types of Disposition Outcomes by Race",
           subtitle = "All District Courts, Middlesex County, MA") +
      ylim(0, 0.6) 
    
  })
  
  
  output$offense <- renderPlot({
    
    offense_total %>% 
      ungroup() %>% 
      select(defendant_race, n) %>% 
      group_by(defendant_race) %>% 
      summarize(count = sum(n), .groups = "drop") %>% 
      left_join(offense_total, by = "defendant_race") %>% 
      mutate(ratio = n/count) %>% 
      mutate(ratio = round(ratio, 3)) %>%
      mutate(defendant_race = factor(defendant_race, 
                                     levels = c("Black", "White"))) %>% 
      ggplot(aes(x = fct_reorder(offense, ratio), y = ratio, fill = offense)) + 
      geom_col(position = "dodge") +
      facet_wrap(~defendant_race) + 
      guides(fill = guide_legend(nrow = 2)) +
      geom_text(aes(label = percent(ratio), y = ratio), 
                position=position_dodge(width = 0.9), 
                vjust = 0.5, hjust = -0.1, size = 2.5) +
      scale_fill_hue(c = 40) +
      theme(legend.position = "none",
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.title = element_blank()) + 
      labs(title = "Offense types by Race",
           subtitle = "All District Courts, Middlesex, MA") +
      ylim(0, 1) + 
      coord_flip()
    
  })
  
# image 
  
  output$court_abbreviation <- renderImage({
    list(
      src = './images/court_abbreviation.png',
      contentType='image/png',
      style="display: block; margin-left: auto; margin-right: auto;"
    )}, deleteFile = F)
  
# model 1
  output$model_1 <- render_gt({
    
    
    tbl_regression(model_1, intercept = TRUE) %>%
      
      # use as_gt() to create a pretty table
      
      as_gt() %>%
      tab_header(title = "Regression of Charges Filed",
                 subtitle = "The Effect of Race and Individual Courts on the number of Charges filed") %>%
      tab_source_note("Source: Prosecution Data and Statistics, MCDAO")
    
    
  })
  
  output$posterior_1 <- renderPlot({
    
    new_obs <- tibble(court_location = c("CAM", "LOW"),
                      defendant_race = "Black")
    
    set.seed(10)
    posterior_predict(model_1, newdata = new_obs) %>% 
      as_tibble() %>% 
      pivot_longer(cols = 1:2, 
                   names_to = "Parameter",
                   values_to = "values") %>% 
      ggplot(aes(values, fill = Parameter)) +
      geom_histogram(aes(y = after_stat(count/sum(count))),
                     alpha = 0.5, 
                     bins = 100, 
                     position = "identity") +
      labs(title = "Posterior Probability Distribution",
           subtitle = "For Black defendants in 2 different district courts",
           x = "Expected criminal counts",
           y = "Probability") + 
      scale_x_continuous(labels = scales::number_format()) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(name = "Posterior value",
                        labels = c("CAM", "LOW"),
                        values = c("dodgerblue", "salmon"),
                        breaks = c("1", "2")) +
      theme_classic()
    
    
  })
  
  output$posterior_2 <- renderPlot({
    
    # add input$variable within the data frame new_obs
    
    new_obs <- tibble(court_location = c(input$variable, input$variable_2),
                      defendant_race = "Black")
    
    set.seed(10)
    posterior_predict(model_1, newdata = new_obs) %>% 
      as_tibble() %>% 
      pivot_longer(cols = 1:2, 
                   names_to = "Parameter",
                   values_to = "values") %>% 
      ggplot(aes(values, fill = Parameter)) +
      geom_histogram(aes(y = after_stat(count/sum(count))),
                     alpha = 0.5, 
                     bins = 100, 
                     position = "identity") +
      labs(title = "Posterior Probability Distribution",
           subtitle = "For Black defendants in 2 different district courts",
           x = "Expected criminal counts",
           y = "Probability") + 
      scale_x_continuous(labels = scales::number_format()) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(name = "Posterior value",
                        labels = c("1st choice court", "2nd choice court"),
                        values = c("dodgerblue", "salmon"),
                        breaks = c("1", "2")) +
      theme_classic()
    
    
  })
  
})

        


