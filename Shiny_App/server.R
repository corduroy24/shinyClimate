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

  observe({
    prov_vector_sorted <- sort(prov_vector)
    updateSelectInput(session, "prov", "Choose a province", 
                      choices = prov_vector_sorted, 
                      selected = 'ON')
    # })
  }, priority = 200)
  
  observeEvent(input$prov,{
    city_vector<- get_city_vector(input$prov)
    updateSelectInput(session, "city", "Choose a city", 
                      choices = city_vector, 
                      selected = 'TORONTO')
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
    
    isolate({
      # controlled...no longer needed
      # start_year_cutoff <- check_start_year_cutoff(temp_val)
      # validate(
      #   need(input$year_to_start < (start_year_cutoff-1), 
      #        paste('Please choose a year prior to', start_year_cutoff)
      #   )
      # )
      main(temp_val, month, year_to_start)
    })
    # beginning <- Sys.time()
    # end <- Sys.time()
    # output$test_2 <- renderText({end - beginning})
  })
  
    
    output$reg_temp <- renderPlot({
      update()
      city <- toupper(input$city)
      prov <- input$prov
      reg_temp(city, prov)
    })
    
    # Single province
    output$gghist_slope_prov <- renderPlot({
      update()
      prov <- input$prov
      hist_slope_prov(prov)
    })
    
    # All provinces - CANADA 
    output$boxplot_r2 <- renderPlot({
      update()
      boxplot_val('r.squared')
    })
    output$hist_slope <- renderPlot({
      update()
      hist_slope()
    })
    output$boxplot_slope <- renderPlot({
      update()
      boxplot_val('slope')
    })
    output$map_ON <- renderPlot({
      update()
      map()
    })
})
