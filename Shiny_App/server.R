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
  #reactiveVal vs <<- 
  month <- character()
  temp_val <- character()
  year_to_start <- numeric()

  # rv <- reactiveValues(output_df_all, input_df_all )
  # rv <- reactiveValues()
  
  # month <- reactiveVal()
  # temp_val <- reactiveVal()
  # year_to_start <- reactiveVal()
  prov_vector <- c("ON","AB","BC","YT","NT","NU","SK", "MB", "QC", "NB", "NS", "PE", "NL")
  
    extractParams <- function(sent_variable){
      month<<-(strtrim(input$month, 3))
      temp_val <- unlist(strsplit(input$temp_val, " "))
      
      temp_val_1 <- strtrim(tolower(temp_val[1]),3)
      temp_val_2 <- strtrim(tolower(temp_val[2]),4)
      
      temp_val<<-(paste(temp_val_1,temp_val_2, sep='_'))
      
      year_to_start<<-(input$year_to_start)
      
      params <- c(temp_val, month, year_to_start )
      
    }
    
    # value <- eventReactive(input$confirm, {
    observeEvent(input$confirm, {
        
      validate(
        need(input$temp_val != '', 'Please choose a temperature value.'),
        need(input$month != '', 'Please choose a month.'),
        need(input$year_to_start != '', 'Please choose a valid start year.')
      )

      params <- extractParams(input)
      # debug(logger, paste('|check value------ ' , '|',params[1],"|"))
      output$test_1 <- renderText({params})
      
      # need atleast 2 values for regression...
      # test again...
      debug(logger, paste('|TEMP_VAL 1|', temp_val,"|"))
      
      start_year_cutoff <- check_start_year_cutoff(temp_val)
      validate(
        need(input$year_to_start < (start_year_cutoff-2), 
             paste('Please choose a year prior to', start_year_cutoff)
        )
      )
      debug(logger, paste('|MONTH 1|', month,"|"))
      debug(logger, paste('|YEAR_TO_START 1|', year_to_start,"|"))
      
      beginning <- Sys.time()
      
      main(temp_val, month, year_to_start)
      
      # rv$output_df_all <- output_df_all
      # rv$input_df_all <- input_df_all
      
      end <- Sys.time()
      output$test_2 <- renderText({end - beginning})
      
      
      output$hist <- renderPlot({
        # observe({rv$output_df_all})
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
        city_df <- select(city_df, intercept, slope)
        
        output_df_prov <- reg_prov(input_df_all)
        # test <- output_df_prov$prov
        # debug(logger, paste('|TEST |', test,"|"))
        # debug(logger, paste('|OUTPUT_PROV_DF |', output_df_prov,"|"))
        prov_df <- output_df_prov[which(output_df_prov$prov==prov), ]
        prov_df <- select(prov_df, intercept, slope)
        # debug(logger, paste('|PROV_DF |', prov_df,"|"))
        
        country_df <- reg_country(input_df_all)
        debug(logger, paste('|CANADA_DF |', country_df,"|"))
        
        horiz_city_1 <- city_df$intercept + city_df$slope*1980
        horiz_city_2 <- city_df$intercept + city_df$slope*2020
        horiz_prov_1 <- prov_df$intercept + prov_df$slope*1980
        horiz_prov_2 <- prov_df$intercept + prov_df$slope*2020
        horiz_can_1 <- country_df$intercept + country_df$slope*1980
        horiz_can_2 <- country_df$intercept + country_df$slope*2020
        
        ylim <- c(round(min(horiz_city_1, horiz_prov_1, horiz_can_1)),
                  round(max(horiz_city_2, horiz_prov_2, horiz_can_2)))
        
        plot(1, type="l", xlab="", ylab="", xlim=c(1980, 2020), ylim=ylim)
        abline(h=0, lty = 4)
        abline(a = city_df$intercept, b = city_df$slope, col = 'red', lwd = 3)
        abline(h=horiz_city_1, lty = 3, col = 'red')
        abline(a = prov_df$intercept, b = prov_df$slope, col = 'blue', lwd  = 3)
        abline(h=horiz_prov_1, lty = 3, col = 'blue')
        abline(a = country_df$intercept, b = country_df$slope, col = 'green', lwd  = 3)
        abline(h=horiz_can_1, lty = 3, col = 'green')
        
        legend("topright",
               legend = c(city, prov, 'CANADA'),
               col = c('red', 'blue', 'green'),
               cex = 1.2,
               lty=3:3)
      })
      
      
    })
      
      # output$table <- DT::renderDataTable(DT::datatable({
      #   # value()
      #   output_df_all
      # }))
      
      output$prov <- renderUI({
        selectInput("prov", "Choose a province:",choices = prov_vector)
      })
      
      output$city <- renderUI({
        textInput("city", "Enter city:",value = "TORONTO" )
      })
    
      
  
})
