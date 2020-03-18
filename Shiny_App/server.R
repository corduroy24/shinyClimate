#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
minTempDir = "Homog_monthly_min_temp_cleaned"
maxTempDir = "Homog_monthly_max_temp_cleaned"
meanTempDir = "Homog_monthly_mean_temp_cleaned" 

# tempMax = list.files(path=maxTempDir, pattern="*.txt", full.names=TRUE)
# tempMin  = list.files(path=minTempDir, pattern="*.txt", full.names=TRUE)
# tempMean = list.files(path=meanTempDir, pattern="*.txt", full.names=TRUE)
library(shiny)
library(tidyverse)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    # x1 <- strtrim(input$month, 3)
    # 
    # 
    extractParams <- function(sent_variable){
      # the function that responds to this 
      # function call returns a list "list(outL, numericsL)
      month <- strtrim(input$month, 3)
      temp_val <- unlist(strsplit(input$temp_val, " "))
      
      temp_val_1 <- strtrim(tolower(temp_val[1]),3)
      temp_val_2 <- strtrim(tolower(temp_val[2]),4)
      
      temp_val <- paste(temp_val_1,temp_val_2, sep='_')
      
      start_year <- input$start_year
      
      params <- c(temp_val, month, start_year)
      
    }
    value <- eventReactive(input$submit, {
      validate(
        need(input$temp_val != '', 'Please choose a temperature value.'),
        need(input$month != '', 'Please choose a month.'),
        need(input$start_year !='', 'Please choose a valid start year.')
      )
      
      three_value_vector <- extractParams(input)
    })
    
    output$test_1 <- renderText({value()})
    # output$test_2 <- render

    # check <- output$value
    # p(check)
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
      params <- value()
      output$test_2 <- renderText({length(params[[1]])})
      output$test_3 <- renderText({length(params[[2]])})
      output$test_4 <- renderText({length(params[[3]])})
      
      param_1 <- params[[1]]
      param_2 <- params[[2]]
      param_3 <- params[[3]]
      

      output_df <- main(param_1, param_2, param_3)
    }))
     
    
     
     # observe({
     #   
     #   x1 <- strtrim(input$month, 3)
     #   
     #   
     #   x2 <- unlist(strsplit(input$temp_meas, " "))
     #   
     #   x3 <- strtrim(tolower(x2[1]),3)
     #   x4 <- strtrim(tolower(x2[2]),4) 
     # 
     #   
     #   # updateTextInput(session, "testing_1", value = paste(x1))
     #   # updateTextInput(session, "testing_2", value = paste(x3,x4, sep="_"))
     #   
     # })
     

     
     

})
