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
                    plotOutput("reg_temp"),
                    h5("The different lines indicate different trends for selected regions"),
                    h5("Take a moment to play around with the trends of different cities "),
                    h5("Thus, remain cautious about trends forecasting trends for one region to the other"),
                    h5("Notice, the variation of temperature increases as the size of region does. Thus, the strength of assumptions from the cities to provincices will be analyzed"),
                    h2("Provincial Analysis"),
                    h4("Histogram"),
                    h5("Graphically summarize the distribution of a data set"),
                    plotOutput("gghist_slope_prov"),
                    h5("[Insert Images - Names - implications of different distributions]"),
                    h4("reg line with CI?"),
                    h2("National Analysis"),
                    h4("Boxplot"),
                    h5("This suggests that a small percentage of the variance in Y is predictable by X. And thus, Year and temperature are slightly correlated"),
                    plotOutput("boxplot_r2"),
                    plotOutput("hist_slope"),
                    plotOutput("boxplot_slope"),
                    plotOutput("map_ON")
                    
                ),
                tabPanel("Quality Control", 
                         HTML("[Show data cleaning]")),
                tabPanel("About", 
                    HTML("Hello world")
                    
                )
            )
        )
    )
))
