#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Canadian Climate Data - Temperature  "),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
        
        # Input: Select a dataset ----
        selectInput("dataset", "Choose a dataset:",
                    choices = c("BC", "ON", "AB","SK","MB","QC","NB","NS","PE","NL","NU", "NT")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    ),
    DT::dataTableOutput("table")
)
    
))
