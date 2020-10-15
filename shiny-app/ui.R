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


# Define UI for application that draws a histogram
shinyUI(navbarPage(theme = shinytheme("slate"),
                   "Efficiency and Equity in the Courtroom", 
    # Application title
    tabPanel("Middlesex",
             p("Hello World!", 
             a("Berkshire DA Data", 
    href = "https://www.dropbox.com/sh/fw4xfk02iiewvy2/AAASf7RBIKbbK8m2eT0WpKOea?dl=0" )),
             p(a("Middlesex County", 
                 href = "https://www.middlesexda.com/public-information/pages/prosecution-data-and-statistics")
               )),
    tabPanel("Berkshire", plotOutput("carPlot"))
    ))
    #more information HERE: https://www.w3schools.com/TAGS/default.ASP 
    



