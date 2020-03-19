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
source("helpers.R", local = TRUE)

# input_df_all <- 1
# output_df_all <- 1
# city_vector <- 1
# prov_vector <- 1

# city_vector <- NULL
# prov_vector <- NULL


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #reactiveVal vs <<- 
  # month <- character()
  # temp_val <- character()
  # year_to_start <- numeric()
  month <- reactiveVal()
  temp_val <- reactiveVal()
  year_to_start <- reactiveVal()
  

    extractParams <- function(sent_variable){
      # the function that responds to this 
      # function call returns a list "list(outL, numericsL)
      month(strtrim(input$month, 3))
      temp_val <- unlist(strsplit(input$temp_val, " "))
      
      temp_val_1 <- strtrim(tolower(temp_val[1]),3)
      temp_val_2 <- strtrim(tolower(temp_val[2]),4)
      
      temp_val(paste(temp_val_1,temp_val_2, sep='_'))
      
      year_to_start(input$year_to_start)
      
      params <- c(temp_val(), month(), year_to_start() )
      
    }
    
    # debug(logger, paste("temp_val", temp_val))

    value <- eventReactive(input$confirm, {

      validate(
        need(input$temp_val != '', 'Please choose a temperature value.'),
        need(input$month != '', 'Please choose a month.'),
        need(input$year_to_start != '', 'Please choose a valid start year.')
      )

      
      params <- extractParams(input)
      debug(logger, paste('|check value------ ' , '|',params[1],"|"))
      
      output$test_1 <- renderText({params})
      
      # need atleast 2 values for regression...
      # test again...
      debug(logger, paste('|TEMP_VAL 1|', temp_val(),"|"))
      
      start_year_cutoff <- check_start_year_cutoff(temp_val())
      validate(
        need(input$year_to_start < (start_year_cutoff-2), 
             paste('Please choose a year prior to', start_year_cutoff)
        )
      )
      debug(logger, paste('|MONTH 1|', month(),"|"))
      debug(logger, paste('|YEAR_TO_START 1|', year_to_start(),"|"))
      
      beginning <- Sys.time()
      
      output_df_all <- main(temp_val(), month(), year_to_start())
      

      # check <- load(paste(temp_val,month, year_to_start,'.RData'))
      
      # debug(logger, paste('|check value------ ' , '|', check,"|"))
      
      # city_vector<- input_df_all 
      
      # city_vector<- get_city_vector(temp_val(), month(), year_to_start())
      # prov_vector<- get_prov_vector(temp_val(), month(), year_to_start())

      # debug(logger, paste('|check prov ------ ' , '|', prov_vector,"|"))
      
      end <- Sys.time()
      output$test_2 <- renderText({end - beginning})
      # vars <- list(output_df_all)
      
    })
    
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
      value()
      output_df_all
    }))
    
    observe({
      # value()

      debug(logger, paste('|TEMP_VAL |', temp_val(),"|"))
      debug(logger, paste('|MONTH|', month(),"|"))
      debug(logger, paste('|YEAR_TO_START|', year_to_start(),"|"))
      
      prov_vector<<- get_prov_vector(temp_val(), month(), year_to_start())
      
      updateSelectInput(session,
                        "prov",
                        "Choose a province:",
                        choices = prov_vector
                        )
    })
    
    output$hist <- renderPlot({
      # vars <- value()
      # output_df_all <- vars[[1]]
      # params <- vars[[4]]
      
      validate(
        need(input$city != '', 'Please enter a valid city')
      )
      city <- toupper(input$city)
      prov <- input$prov
      
      city_df <- output_df_all[ which(output_df_all$prov==prov
                    & output_df_all$city == city), ]
      debug(logger, paste('|check city df ------ ' , '|', nrow(city_df), "|"))
      validate(
        need(nrow(city_df) !=0, 'Please enter a valid city')
      )
      # prov_df <- output_df_all[which(output_df_all$prov==prov), ]
      # debug(logger, paste('|check prov df ------ ' , '|', prov_df,"|"))
      

      city_df <- select(city_df, intercept, slope)
      # prov_df <- select(prov_df, intercept, slope)
      
      
      output_df_prov <- reg_prov(input_df_all)
      test <- output_df_prov$prov
      # debug(logger, paste('|TEST |', test,"|"))
      
      # debug(logger, paste('|OUTPUT_PROV_DF |', output_df_prov,"|"))
      
      prov_df <- output_df_prov[which(output_df_prov$prov==prov), ]
      prov_df <- select(prov_df, intercept, slope)
      
      debug(logger, paste('|PROV_DF |', prov_df,"|"))
      
      # allow xlim and ylim to be chosen ?
      plot(1, type="l", xlab="", ylab="", xlim=c(1980, 2020), ylim=c(-20, 5))
      abline(h=0, lty = 4)
      abline(a = city_df$intercept, b = city_df$slope, col = 'red', lwd = 3)
      horiz_1 <- city_df$intercept + as.numeric(city_df$slope)*1980
      abline(h=horiz_1, lty = 3)
      abline(a = prov_df$intercept, b = prov_df$slope, col = 'blue', lwd  = 3)
      horiz_2 <- prov_df$intercept + as.numeric(prov_df$slope)*1980
      abline(h=horiz_2, lty = 3)
      
      legend("topright", 
             legend = c(city, prov, 'CANADA'), 
             col = c('red', 'blue', 'green'),
             cex = 1.2, 
             lty=3:3)
      # abline(a = $intercept, b = city_df$slope, col = 'green')

    })
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
