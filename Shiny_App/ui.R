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
    titlePanel("Canadian Climate Data - Temperature  "),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            # Input: Select a dataset ----
            selectInput("month", "Choose a month:",
                choices = c("January", "February", "March","April","May","June","July","August","September","October","November", "December"),
                selected = NULL
            ),
            
            selectInput("temp_val", "Choose a Measuremnent:",
                choices = c("Minimum Temperature", "Maximum Temperature")
                # choices = c("Minimum Temperature", "Mean Temperature", "Maximum Temperature")
            
            ),
            
            numericInput("year_to_start", "Start Year", 1980),
            
            actionButton("confirm", "Confirm Selection"),
            
            # textInput("testing_1", "Testing Output"),
            # textInput("value", "Testing Output"),
            

            verbatimTextOutput("test_1"),
            verbatimTextOutput("test_2"),
            verbatimTextOutput("test_3"),
            verbatimTextOutput("test_4"),
            
            # Input: Select a dataset ----
            selectInput("prov", "Choose a province:",
                        choices = c()
            ),
            
            # placeholder  = 
            textInput("city", "Enter City", value = 'Toronto')
            
        ),
        
        
        mainPanel(
            tabsetPanel(
                tabPanel("Story", 
                    DT::dataTableOutput("table"),
                    plotOutput("hist")
                ),
                

            #   tabPanel("Table", tableOutput("table")),

                tabPanel("About", 
                    HTML("Hello world")
                    # plotOutput("hist")
                )
            )
        )
    )

    
))
