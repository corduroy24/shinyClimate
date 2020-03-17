#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("test.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Canadian Climate Data - Temperature  "),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

            # Input: Select a dataset ----
            selectInput("month", "Choose a month:",
                choices = c("January", "February", "March","April","May","June","July","August","September","October","November", "December")
            ),
            
            selectInput("temp_meas", "Choose a Measuremnent:",
                choices = c("Minimum Temperature", "Mean Temperature", "Maximum Temperature")
            ),
            
            textInput("start_year", "Start Year", NULL),
            
            actionButton("param_submit", "Submit"),
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("distPlot")
            )
    ),
    
    DT::dataTableOutput("table")
)
    
))
