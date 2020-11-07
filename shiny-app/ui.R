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




# Define UI for application that draws a histogram
shinyUI(navbarPage(theme = shinytheme("flatly"),
                   "Racial Disparities in 
                   Middlesex County, MA", 
   
############################
                   
                   
############## FIRST PAGE ##############   
 
tabPanel("Weekly Progress",
         br(),
          p(em("Note: This tab is temporary"
            ), align = "center"),
          br(),
        h3("What you will find on my Shiny"),
        p("This page has 3 tabs: Weekly progress, Middlesex, and Overview"),
        p("On the
          Middlesex tab, you will find a leaflet map. On the Overview
          tab, you will find a basic interactive plot"),
        h3("Update for Week 10: 11/2 - 8"),
        p("I first focused on trying to understand the research 
          literature by reading the most recent report from HLS. I met with
          Wyatt outside section to discuss my final project outline (written in
          google docs) and received so much support and guidance from him."),
        p("For the majority of the time I was trying to do data wrangling and thinking
        about how to interpret the data. Due to time constraint, 
        I only spent a little time trying to get some graphs from my R markdown 
        to display on Shiny. A more accurate representation
          of what I have done would be in my gather.Rmd"),
        h3("Plan for next week"),
        p("- Reach out to Harvard law clinics and others for advice on how to 
          interpret and analyze the data e.g. how to analyze race"),
        p("- Put at least one more graph and one more table on Shiny along with
          some interpretation"),
        p("- Continue to read on RED analysis + connect with the textbook"),
        br(),
        h3("Github Repo"),
        p("Please find my Github Repo here: 
          https://github.com/anhton99/court-data"))
        , 
   tabPanel("Middlesex",
         h3("Middlesex County - Racial and Income Demographics",
            align = "center"),
         br(),
            fluidRow(column(2), column(8,
         p("Here I want to show the median income demographics in Middlesex County, MA"),
            leafletOutput("map")
         ))),

   tabPanel("Overview",
            p("Hello"),
            sidebarLayout(
                sidebarPanel(
                   selectInput(
                        inputId = "district_court",
                        label = "Select Court District:",
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
                        selected = "Ayer District Court")
                    ),
                mainPanel(
                        plotOutput("mid_race_graph")
                            )
                    )
               )
            )
        
         
)         


  