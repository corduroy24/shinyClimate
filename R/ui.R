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
library(shinyjs)
source("homeModule.R")
source("sidebarModule.R")
source("helpers.R")
source("prefModule.R")
source("triviaModule.R")
source("moreInfoModule.R")


shinyUI<- dashboardPage(

  skin = "yellow",
  
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
      # menuItem("Trivia", tabName = "trivia", icon = icon("th")),
      menuItem("Preferences", tabName = "pref", icon = icon("th")),
      # menuItem("More Info", tabName = "more", icon = icon("th")),
      useShinyjs(debug = TRUE),
      sidebarLayoutUI("sidebar")
    )
  ),

  # Body content 
  dashboardBody(
    # First tab content
    tabItems(
      tabItem(tabName = "home",
              useShinyjs(debug = TRUE),
              homeLayoutUI("home")
      ),
      
      # Second tab content
      # tabItem(tabName = "trivia",
      #         triviaLayoutUI('trivia')
      # ),
      
      # Third tab content
      tabItem(tabName = "pref",
              prefLayoutUI("pref")
      )
      # # Fourth tab content
      # tabItem(tabName = "more",
      #         moreInfoUI('more')
      # )
    )
  ),
)