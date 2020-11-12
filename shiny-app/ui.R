#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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



# Define UI for application that draws a histogram
shinyUI(navbarPage(theme = shinytheme("flatly"),
                   "Racial Disparities in 
                   Middlesex County, MA", 
   
############################
                   
                   
############## FIRST PAGE ##############   
 
tabPanel("About",
         br(),
          p(em("Note: This tab is temporary"
            ), align = "center"),
          br(),
        h3("About my project"),
        p("My project focuses on the Prosecution data set by Middlesex, which can be found",
          a("here", 
            href = "https://www.middlesexda.com/public-information/pages/prosecution-data-and-statistics"),
          ". The big questions I'm interested in pursuing are:"),
        
        p("1. How long -on average - do cases move through the system, from the day an 
        offense allegedly occurred, to the day criminal charges were officially documented, 
        to the day the final outcome of the case was determined? Controlling for the types 
        of crime committed, are there time differentials 
        for Black and White people?"),
        
        p("3. What is the total number of criminal charges for district courts? 
        Do people from a certain race receive more charges per case than others?"),
        
        p("4. What are the statistics for the types of charges - arranged from the most 
        to least severe - for each race? Are there patterns of offense across race?"),
        
        p("5. What about the manner of disposition? Do people from a certain race tend to 
        receive more favorable/less favorable outcomes?"),
        br(),
        p(em("For the majority of the time I was trying to do data wrangling and thinking
        about how to interpret the data. Due to time constraint, 
        I only spent a little time trying to get some graphs from my R markdown 
        to display on Shiny. A more accurate representation
          of what I have done would be in my gather.Rmd & my", 
             a("google document", href = "https://docs.google.com/document/d/10W-ZO8rtGOK86xTlYWeWKyMPj9C6-uAtRFrZNBo0RSQ/edit?usp=sharing"),
               ". Any comment/feedback/question is welcome!")),
        br(),
        p(em("Thank you for your time and support! :)"), align = "center"),
        h3("Github Repo"),
        p("Please find my Github Repo here: 
          https://github.com/anhton99/court-data"))
        , 
   tabPanel("Efficiency",
            h3("Efficiency in the Current Court System"),
            h4("The Burden on the Court"),
            p("Efficiency in courts is dependent on a variety of factors, one of which 
            is the amount of caseload each court received. The original dataset contains 
            343,099 observations, with some observations. After filter for cases that 
            started and ended within 2014-2019, I found that there are 88,651 cases recorded 
            within 6 years. Here are more details of cases by types of courts:"),
            p(em("Table 1. Total cases by Court types"), align = "center"),
            withSpinner(gt_output(outputId = "middle_table")),
            br(),
            splitLayout(cellWidths = c("70%", "30%"),
            p(em("Figure 1: Total cases in District Courts")),
            p(em("Figure 2: Total cases in Superior Courts"))),
            splitLayout(cellWidths = c("70%", "30%"),
                    withSpinner(plotOutput("mid_dis")),
                    withSpinner(plotOutput("mid_sup"))),
            br(),
            p("As over 90% of all cases happen in District Courts, this project will focus on 
            analyzing prosecution data at the district level. Middlesex has 12 District courts, 
            each serving a number of cities within the county (details can be found",
              a("here", href = "https://www.middlesexda.com/about-us/pages/district-court-locations"),
              ". Overall, for the past 6 years, the number of people involved with district courts 
              were relatively low to the total of the population that each court served. 
              50% of the district courts each received cases that comprise only 3-5% 
              population of its assigned cities."),
            br(),
            p("Figure 3 shows the total number of cases and the total population 
            that each district court served for the past 6 years (2014-2019). 
            Figure 4 transforms those numbers into percentage formats"),
            
            p("While Newton District Court served the smallest 
            population (~12,500 people), it had the largest ratio of total cases to total 
            population served. By contrast, Cambridge District Court served the second 
            largest population (over 186,000 people) but received the second lowest number 
            of cases relative to its population."),
            p(em("Figure 3: Total cases versus total population served"), align = "center"),
            plotOutput("total_sum_count"),

            splitLayout(cellWidths = c("70%", "30%"),
            p(em("Figure 4: Cases as % Total Population served")),
            p(em("Table 2: Summary of cases as % total population"))),
            splitLayout(cellWidths = c("70%", "30%"),
            plotOutput("sum_count"),
            gt_output(outputId = "sum_count_mid")),
            h4("Moving through the system"),
            p("How fast a case moves through the system is critical to assessing court 
            efficiency. There are 3 time points representing 2 stages by which a case 
            moves through the system: the day an offense allegedly occurred, the day 
            criminal charges were officially documented, and the day the final outcome 
            of the case was determined. In the interactive plot below, we can see the 
            number of days it takes for each court to handle a case. While most cases 
            go through the first stage from offense date to filing date quickly 
            (~10-20 days), it takes much longer for the court to dispose of the case. 
            Figure 6 is an interactive plot showing how the duration of each stage 
            vary among courts."),
            br(),
            p(em("Figure 6: Duration of 2 stages a case go through in the district 
                 court system")),
            sidebarLayout(sidebarPanel(
                    h3("From Offense to Filing"),
                    selectInput("court_location",
                                label = "Choose a District Court:",
                                choices = c("Ayer District Court",
                                            "Cambridge District Court",
                                            "Concord District Court",
                                            "Framingham District Court",
                                            "Lowell District Court",
                                            "Malden District Court",
                                            "Marlborough District Court",
                                            "Natick District Court",
                                            "Newton District Court",
                                            "Somerville District Court",
                                            "Waltham District Court",
                                            "Woburn District Court"),
                                selected = "Lowell District Court"),
                    selectInput("stage",
                                label = "Choose a stage:",
                                choices = c("offense to filing" = "filing_offense_diff",
                                            "filing to disposition" = "dispo_filing_diff"),
                                selected = "filing to disposition")
            ), 
            mainPanel(withSpinner(plotOutput("dispo_offense")))),
            ),

   tabPanel("Racial Disparity",
            p("If we look at the whole district court system and consider the number of 
            cases by population by race, there are signs of racial disparity, As shown 
              in Figure 7, although White people account for 5.9% of Middlesex population, 
              they account for 13.8% total cases. Meanwhile, White people account for 
              78.2% of total population and 57.4% of total cases."),
            plotOutput("case_pop_race"),
            p(em("To be continued"), align = "center")
               ),
   
   tabPanel("Middlesex",
         h3("Middlesex County - Racial and Income Demographics",
            align = "center"),
         br(),
         fluidRow(column(2), column(8,
                                    p("Here I want to show the median income demographics 
                                      in Middlesex County, MA"),
                                    leafletOutput("map")
         )))

))
        


  