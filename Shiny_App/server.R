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
    value <- eventReactive(input$confirm, {
      validate(
        need(input$temp_val != '', 'Please choose a temperature value.'),
        need(input$month != '', 'Please choose a month.'),
        need(input$start_year !='', 'Please choose a valid start year.')
      )
      
      three_value_vector <- extractParams(input)
    })
    
    output$test_1 <- renderText({value()})
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
      params <- value()
      output_df_all <- main(params[[1]], params[[2]], params[[3]])
    }))
    
    output$hist <- renderPlot({
      
      hist_slopes(output_df_all)
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # hist(x, breaks = bins, col = "#75AADB", border = "white",
      #   xlab = "Waiting time to next eruption (in mins)",
      #   main = "Histogram of Slopes")
      
    })
     

})
