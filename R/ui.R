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
library(shinydashboardPlus)
library(shinyjs)
library(rmarkdown)
source("homeModule.R")
source("sidebarModule.R")
source("helpers.R")
source('downloadModule.R')
source('plotModule.R')
source('analysisModule.R')
# source("triviaModule.R")
# source("moreInfoModule.R")

shinyUI<- dashboardPage(

  skin = "yellow",
  
  dashboardHeader(title = "Climate For You",
                  tags$li(class = "dropdown", actionLink("show", "About"))
  
                  ),
  
  #SideBar content 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      sidebarLayoutUI("sidebar")

      # menuItem("Trivia", tabName = "trivia", icon = icon("th")),
      # menuItem("More Info", tabName = "more", icon = icon("th")),
    )
  ),
  

  # Body content 
  dashboardBody(
    # First tab content
    tabItems(
      tabItem(tabName = "home",
              useShinyjs(debug = TRUE),
              homeLayoutUI("home")
      )
      
      # Second tab content
      # tabItem(tabName = "trivia",
      #         triviaLayoutUI('trivia')
      # ),
      
      # # Fourth tab content
      # tabItem(tabName = "more",
      #         moreInfoUI('more')
      # )
    )
  ),
)