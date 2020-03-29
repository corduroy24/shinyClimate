#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)


shinyServer(function(input, output, session) {
  vars <- callModule(prefLayout, 'pref')
  
  sidebar_vars <- callModule(sidebarLayout, 'sidebar') 
  
  callModule(homeLayout, 'home', vars = vars, sidebar_vars = sidebar_vars)
  
  callModule(triviaLayout, 'trivia')

  callModule(moreInfo, 'more')
  
  
})
