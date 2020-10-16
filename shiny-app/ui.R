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


# Define UI for application that draws a histogram
shinyUI(navbarPage(theme = shinytheme("flatly"),
                   "Racial Disparities in 
                   Juvenile Courts, MA", 
   
# Application title
   
 tabPanel("Overview",
        h2("About This Project"),
        p("The ongoing Black Live Matters movement and my internship 
        with an organization doing advocacy work for racial equity in
        the juvenile legal system has got me interested in learning 
        more about the issue. I plan to use the data from the official
        Commonwealth of Massachusetts website for the year of 2014 - 2019
        to look at some basic statistical background in the types and
        volume of cases brought to the juvenile courts. I also want to 
        investigate the prosecution data for Middlesex and Berkshire -
        the two datasets that I currently have access to - to find some
        insights regarding court efficiency (how long they pursue a case
        and whether the case is dropped or resolved) and racial equity 
        (whether the length of time from case filed to case resolution, 
        case charge, and sentence types are systematically different for
        people belonging to different races."),
        br(),
        br(),
        h3("Note"),
        p("I did some basic cleaning for the court data from mass.gov, but I 
        have not processed the Middlesex and Berkshire data yet because I am 
        still waiting for my supervisor's clarifications of many 
        abbreviations used in the Middlesex dasaset and especially the 
        Berkshire datasets, some technical terms, and what other information 
        would be helpful to be included."),
        br(),
        br(),
        h3("Github Repo"),
        p("Please find my Github Repo here: 
          https://github.com/anhton99/court-data"),
        ),
    
    #Output 
  tabPanel("Middlesex County",
        h2("Test"),
        p("This graph is just a shiny test and to fulfill the requirements
        of Milestone #4 as I still need more time to do some research and 
        think about what datasets are helpful to be put together to
        shed light on my questions of whether the courts are efficient and
        just, whether children of color are disproportionately involved 
        in the justice system, and why such is the case (Is that racial
        bias from the those enforcing and interpreting the laws? Economic
        inequality? What are other possible factors am I missing?"),
      
        fluidPage(plotOutput("juvenile_case_type")
    )
))
)
#more information HERE: https://www.w3schools.com/TAGS/default.ASP 
    



