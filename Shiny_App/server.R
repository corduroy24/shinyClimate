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
source("helpers.R")


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
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
    
    # debug(logger, paste("temp_val", temp_val))

    value <- eventReactive(input$confirm, {

      validate(
        need(input$temp_val != '', 'Please choose a temperature value.'),
        need(input$month != '', 'Please choose a month.'),
        need(input$start_year != '', 'Please choose a valid start year.')
      )

      
      params <- extractParams(input)
      debug(logger, paste('|check value------ ' , '|',params[1],"|"))
      
      temp_val <- params[1]
      month<-  params[2]
      year_to_start <-params[3] 
      
      output$test_1 <- renderText({params})
      
      # need atleast 2 values for regression... 
      start_year_cutoff <- check_start_year_cuttoff(temp_val)
      validate(
        need(input$start_year < (start_year_cutoff-2), 
             paste('Please choose a year prior to', start_year_cutoff)
        )
      )
      
      beginning <- Sys.time()
      
      output_df_all <- main(temp_val, month, year_to_start)
      

      # check <- load(paste(temp_val,month, year_to_start,'.RData'))
      
      # debug(logger, paste('|check value------ ' , '|', check,"|"))
      
      # city_vector<- input_df_all 
      
      city_vector<- get_city_vector(temp_val, month, year_to_start)
      prov_vector<- get_prov_vector(temp_val, month, year_to_start)

      debug(logger, paste('|check prov ------ ' , '|', prov_vector,"|"))
      
      end <- Sys.time()
      output$test_2 <- renderText({end - beginning})
      # vars <- output_df_all
      vars <- list(output_df_all, prov_vector, city_vector)
      
    })
    

    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
      vars <- value()
      output_df_all <- vars[[1]]
    }))
    
    observe({
      vars <- value()
      debug(logger, paste('|check prov ------ ' , '|', vars[[2]],"|"))
      
      updateSelectInput(session,
                        "prov",
                        "Choose a province:",
                        choices = vars[[2]]
                        )


    })
    # output$hist <- renderPlot({
    #   # slopes <- 
    #   
    #   
    # })
    # output$hist <- renderPlot({
    #   
    #   hist_slopes(output_df_all)
    #   # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #   
    #   # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #   #   xlab = "Waiting time to next eruption (in mins)",
    #   #   main = "Histogram of Slopes")
    #   
    # })
     

})
