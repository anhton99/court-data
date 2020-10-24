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
library(janitor)
library(tidycensus)
library(viridis)
library(leaflet)
library(stringr)
library(sf)



# Define UI for application that draws a histogram
shinyUI(navbarPage(theme = shinytheme("flatly"),
                   "Racial Disparities in 
                   Juvenile Courts, MA", 
   
# Application title
   
 tabPanel("About",
          br(),
          br(),
          p(em("Note: This tab will later be made the last to appear on shinyapp. For now 
          I will make it as the first tab so you can see my updates as my project 
            progresses. Thank you so much for your feedback and support! :)"
            )),
          br(),
        h3("About This Project"),
        p("The ongoing Black Live Matters movement and the recent events 
        concerning racial injustice in legal institutions have motivated me 
        to understand racial justice via data analysis and visualization"),
        p("My current intenrnship with a juvenile justice organization
        further my interests in racial disparities in the courtroom with a 
        focus on justice for the youth (age 11-17)."),
        p("As for my approach, I plan to use data from mass.gov
        to look at some basic statistics in the types and
        volume of cases brought to juvenile courts by county during 2014-2018. 
        I will also use the prosecution data for Middlesex and Berkshire -
        the two datasets that I currently have access to."),
        p("For each county, I will first provide some demographic backgrounds
        regarding income and racial geography. I will then 
        analyze indicators that shed light on 
        court efficiency (how long they pursue a case
        and whether the case is dropped or resolved) and racial equity 
        (whether the length of time from case filed to case resolution, 
        case charge, and sentence types are systematically different for
        people belonging to different races."),
        p("I will also draw some comparisons between the two counties to see
          which county has a more effective and just juvenile system given
          the indicators chosen."),
        p(em("One thing I hope to achieve besides showing correlation is to 
          actually investigating the impact race has on court decisions after
          controlling for other demographic variables (I currently have income
          but I'm thinking more to see if there are other potential confounds).
          I hope to learn more about regression analysis and apply it to my
             project.")),
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
         p(em("I initially created maps showing racial and income demographics in 
         Middlesex and Berkshire using ggmap
         like the one shown in The Primer, 
          but when I published my webpage via Shiny I received an error: Check your logs or contact the app author 
          for clarification. It's a bit confusing for me because the maps worked just fine when
          I tried running them in R. In the interest of time, I decided
              to learn about leaflet - a package built in Shiny - and create a map showing
              median income - an indicator of socioeconomic status. I will need more time
              to think about key information that might be confounded with racial injustice
              and present them in my website. I hope to populate this page in the next 
              milestone.")
           ))),
         fluidRow(column(2), column(8, 
            align = "center",
            leafletOutput("middlesex_income_map"))
            ))
))

  