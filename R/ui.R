#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
source("homeModule.R")
source("helpers.R")
source("prefModule.R")


shinyUI<- dashboardPage(
  
  #   tags$head(
  #     tags$style(HTML("
  #                   .btn {
  #                     height: 100px;
  #                     width: 100px;
  #                     border-radius: 60%;
  #                     border: 1px solid red;
  #                     }
  #                     "))
  #   ),
  dashboardHeader(title = "Climate For You"),
  
  #SideBar content 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Trivia", tabName = "trivia", icon = icon("th")),
      menuItem("Preferences", tabName = "pref", icon = icon("th")),
      menuItem("More Info", tabName = "more", icon = icon("th"))
    )
  ),
  
  # Body content 
  dashboardBody(
    # First tab content
    tabItems(
      tabItem(tabName = "home",
              homeLayoutUI("home")
      ),
      
      # Second tab content
      tabItem(tabName = "trivia",
              fluidRow(
                # box(plotOutput("plot1", height = 250)),
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Third tab content
      tabItem(tabName = "pref",
              prefLayoutUI("pref")
      ), 
      # Second tab content
      tabItem(tabName = "more",
              fluidRow(
                box(
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      )
      
    )

  )
)
# 
# shinyUI(fluidPage(

#     # Application title
#     titlePanel("Canadian Climate History"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         ),
# 
#         mainPanel(
#             tabsetPanel(
#                 tabPanel("Historical Temperature", 
#                     h1("What would you like to know?"),
#                     selectInput("qbox", "Choose one of the following",
#                                 choices = c("How has climate change affected my city?",
#                                             "How has climate chnage affected my province",
#                                             "How has climate changed affected Canada"),
#                                 width = "50%"
#                                 ),
#                     h2("Great! What is  your current viewpoint?"),
#                     actionButton("option1", "No effect!"),
#                     actionButton("option2", "Minor effect"), 
#                     actionButton("option3", "Major effect"), 
#                     # plotOutput("reg_temp"),
#                     # plotOutput("gghist_slope_prov"),
#                     # plotOutput("boxplot_r2"),
#                     # plotOutput("hist_slope"),
#                     # plotOutput("boxplot_slope"),
#                     # plotOutput("map_ON")
#                 ),
#                 tabPanel("Temperature Predictions", 
#                          HTML("")),
#                 tabPanel("Quality Control", 
#                          HTML("[Show data cleaning]")),
#                 tabPanel("About", 
#                     HTML("Hello world")
#                 )
#             )
#         )
#     )
# ))
