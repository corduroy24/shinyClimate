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

  sb_vars <- callModule(sidebarLayout, 'sidebar') 
  
  callModule(homeLayout, 'home', sb_vars = sb_vars)
  
  # callModule(triviaLayout, 'trivia')

  # callModule(moreInfo, 'more')
  
  
})
