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
# shinyOptions(cache = diskCache("./myapp-cache"))



shinyServer(function(input, output, session) {

  # sb_vars 
  sb_vars <- callModule(sidebarLayout, 'sidebar', home_plot_vars) 
  
  home_plot_vars <- callModule(homeLayout, 'home', sb_vars = sb_vars)
  

  # callModule(triviaLayout, 'trivia')

  # callModule(moreInfo, 'more')
  
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "About",
      h3("More"),
      h4("Refer to Preferences -> Other to change months and start year"),
      h4("Refer to Preferences -> Other to enable - disable city label for histograms"),
      h4("Close sidebar menu to have a better look at the plots"),
      h4("Download a report on the plots currently displayed"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
})
