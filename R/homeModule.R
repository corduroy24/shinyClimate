# source("helpers.R", local = TRUE)

# Module UI function
homeLayoutUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        selectInput(ns("prov"), "Choose a province:",
                    choices = c()
        ),
        selectInput(ns("city"), "Choose a city:",
                    choices = c()
        ),
        selectInput(ns("temp_val"), "Choose a Measuremnent:",
                    choices = c("Average Temperature","Minimum Temperature", "Maximum Temperature"),
                    selected = 'Average Temperature'
        ),
        verbatimTextOutput(NS(id, "txt1"))
      ),
      box(
        h3("Minimum - [purpose]"),
        background = 'red',
        width = 3,
        height = 100
      ),
      box(
        h3("Maximum - [purpose]"),
        background = 'red',
        width = 3,
        height = 100
      )
    ),
  
    plotOutput(ns("reg_temp")),
    verbatimTextOutput(NS(id, "txt2"))

    
  )
}



# Module server function
homeLayout <- function(input, output, session, vars) {

  prov_vector <- c("ON","AB","BC","YT","NT","NU","SK", "MB", "QC", "NB", "NS", "PE", "NL")
  output$txt1 <- renderPrint({
    paste(vars$month(), "&", vars$year_to_start())
  })
  observe({


    prov_vector_sorted <- sort(prov_vector)
    updateSelectInput(session, "prov", "Choose a province",
                      choices = prov_vector_sorted,
                      selected = 'ON')
  }, priority = 200)

  observeEvent(input$prov,{
    city_vector<- get_city_vector(input$prov)
    updateSelectInput(session, "city", "Choose a city",
                      choices = city_vector,
                      selected = 'TORONTO')
  })

  # observeEvent(input$,{
    update <- reactive({
      year_to_start <- vars$year_to_start()
      month <- (strtrim(vars$month(), 3))
      
      validate(need(month!='', 'no month'),
               need(year_to_start!='', 'no year')
               )
      temp_val <- unlist(strsplit(input$temp_val, " "))
      isolate({
        temp_val_1 <- strtrim(tolower(temp_val[1]),3)
        temp_val_2 <- strtrim(tolower(temp_val[2]),4)
        temp_val<-(paste(temp_val_1,temp_val_2, sep='_'))
      })

      isolate({
        main(temp_val, month, year_to_start)
      })
      # beginning <- Sys.time()
      # end <- Sys.time()
      # output$test_2 <- renderText({end - beginning})
    })
  # })
  #
  #
  output$reg_temp <- renderPlot({
    update()

    city <- toupper(input$city)
    prov <- input$prov
    reg_temp(city, prov)
  })
  
  # # Single province
  # output$gghist_slope_prov <- renderPlot({
  #   update()
  #   prov <- input$prov
  #   hist_slope_prov(prov)
  # })
  # 
  # # All provinces - CANADA 
  # output$boxplot_r2 <- renderPlot({
  #   update()
  #   boxplot_val('r.squared')
  # })
  # output$hist_slope <- renderPlot({
  #   update()
  #   hist_slope()
  # })
  # output$boxplot_slope <- renderPlot({
  #   update()
  #   boxplot_val('slope')
  # })
  # output$map_ON <- renderPlot({
  #   update()
  #   map()
  # })
  
  
  # # The selected file, if any
  # userFile <- reactive({
  #   # If no file is selected, don't do anything
  #   validate(need(input$file, message = FALSE))
  #   input$file
  # })
  # 
  # # The user's data, parsed into a data frame
  # dataframe <- reactive({
  #   read.csv(userFile()$datapath,
  #            header = input$heading,
  #            quote = input$quote,
  #            stringsAsFactors = stringsAsFactors)
  # })
  # 
  # # We can run observers in here if we want to
  # observe({
  #   msg <- sprintf("File %s was uploaded", userFile()$name)
  #   cat(msg, "\n")
  # })
  # 
  # # Return the reactive that yields the data frame
  return(update)
}