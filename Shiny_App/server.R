#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("C:/Environment_Canada_Shiny_App/Shiny_App")

library(shiny)
library(tidyverse)
source("helpers.R", local = TRUE)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  rv <- reactiveValues()
  
  prov_vector <- c("ON","AB","BC","YT","NT","NU","SK", "MB", "QC", "NB", "NS", "PE", "NL")
  # observe({output_df_all})
  
    # value <- eventReactive(input$confirm, {
    # observeEvent(input$confirm, {
    #   validate(
    #     need(input$temp_val != '', 'Please choose a temperature value.'),
    #     need(input$month != '', 'Please choose a month.'),
    #     need(input$year_to_start != '', 'Please choose a valid start year.')
    #   )

      # params <- extractParams(input)
      # debug(logger, paste('|check value------ ' , '|',params[1],"|"))
      # output$test_1 <- renderText({params})
      
      # need atleast 2 values for regression...
      # debug(logger, paste('|TEMP_VAL 1|', temp_val,"|"))
      
    # })

  output$prov <- renderUI({
    selectInput("prov", "Choose a province:",choices = prov_vector)
  })
  
  observeEvent(input$prov,{
    output$city <- renderUI({
      city_vector<- get_city_vector(input$prov)
      selectInput("city", "Enter city:", choices = city_vector)
    })
    
  })

  
  
  update <- reactive({
    month <- (strtrim(input$month, 3))
    temp_val <- unlist(strsplit(input$temp_val, " "))
    isolate({
      temp_val_1 <- strtrim(tolower(temp_val[1]),3)
      temp_val_2 <- strtrim(tolower(temp_val[2]),4)
      temp_val<-(paste(temp_val_1,temp_val_2, sep='_'))
    })
    year_to_start<-(input$year_to_start)
    
    # test again...
    isolate({
      start_year_cutoff <- check_start_year_cutoff(temp_val)
      validate(
        need(input$year_to_start < (start_year_cutoff-2), 
             paste('Please choose a year prior to', start_year_cutoff)
        )
      )
      main(temp_val, month, year_to_start)
    })
    
    # beginning <- Sys.time()
    # end <- Sys.time()
    # output$test_2 <- renderText({end - beginning})
    
  })
    output$hist <- renderPlot({
      update()
      validate(
        need(input$city != '', 'Please enter a valid city')
      )
      city <- toupper(input$city)
      prov <- input$prov
      
      plot <- overlay_slopes(city, prov)
      validate(
        need(is.null(plot), 'Please enter a valid city')
      )
      plot
    })
      
})
