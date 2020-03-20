#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("helpers.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Canadian Climate History"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            # Input: Select a dataset ----
            selectInput("month", "Choose a month:",
                choices = c("January", "February", "March","April","May","June","July","August","September","October","November", "December"),
            ),
            
            selectInput("temp_val", "Choose a Measuremnent:",
                choices = c("Minimum Temperature", "Maximum Temperature")
                # choices = c("Minimum Temperature", "Mean Temperature", "Maximum Temperature")
            
            ),
            
            selectInput("year_to_start", "Start Year", 
                         choices = seq(1840, 1986), selected = 1980),
            

            verbatimTextOutput("test_1"),
            verbatimTextOutput("test_2"),
            verbatimTextOutput("test_3"),
            verbatimTextOutput("test_4"),
            

            selectInput("prov", "Choose a province:",
                        choices = c()
            ),
            selectInput("city", "Choose a city:",
                        choices = c()
            ),
        ),
        
        
        mainPanel(
            tabsetPanel(
                tabPanel("Story", 
                    h1("Analyzing Temperature"),
                    h5("Aim - Answer some of the following questions..."),
                    h3("Local, Provincial and National Trends"),
                    h5("Taking a glance at temperature trends for a city, province and the nation"),
                    h5("Suggestions - Set Month to Feb, or July to withhold a strong analysis of the worst case/best case scenarios "),
                    plotOutput("slope"),
                    plotOutput("ggslope"),
                    h5("The different lines indicate different trends for selected regions"),
                    h5("Take a moment to play around with the trends of different cities "),
                    h5("Thus, remain cautious about trends forecasting trends for one region to the other"),
                    h5("Notice, the variation of temperature increases as the size of region does. Thus, stronger assumptions on a local regions can be deduced"),
                    h2("Local Analysis"),
                    h4("Histogram (s)"),
                    h5("Graphically summarize the distribution of a data set"),
                    plotOutput("gghist")
                ),
                
                tabPanel("About", 
                    HTML("Hello world")
                    # plotOutput("hist")
                )
            )
        )
    )
))
