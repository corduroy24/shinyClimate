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
  tags$head(
    tags$style(HTML("
                  .btn {
                    height: 100px;
                    width: 100px;
                    border-radius: 60%;
                    border: 1px solid red;
                    }
                    "))
  ),
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
                tabPanel("Historical Temperature", 
                    h1("What would you like to know?"),
                    selectInput("qbox", "Choose one of the following",
                                choices = c("How has climate change affected my city?",
                                            "How has climate chnage affected my province",
                                            "How has climate changed affected Canada"),
                                width = "50%"
                                ),
                    h2("Great! What is  your current viewpoint?"),
                    actionButton("option1", "No effect!"),
                    actionButton("option2", "Minor effect"), 
                    actionButton("option3", "Major effect"), 
                    # plotOutput("reg_temp"),
                    # plotOutput("gghist_slope_prov"),
                    # plotOutput("boxplot_r2"),
                    # plotOutput("hist_slope"),
                    # plotOutput("boxplot_slope"),
                    # plotOutput("map_ON")
                ),
                tabPanel("Temperature Predictions", 
                         HTML("")),
                tabPanel("Quality Control", 
                         HTML("[Show data cleaning]")),
                tabPanel("About", 
                    HTML("Hello world")
                )
            )
        )
    )
))
