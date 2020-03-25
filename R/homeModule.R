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
        )
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
    )
  )
}



# Module server function
homeLayout <- function(input, output, session) {
  prov_vector <- c("ON","AB","BC","YT","NT","NU","SK", "MB", "QC", "NB", "NS", "PE", "NL")
  
  observe({
    prov_vector_sorted <- sort(prov_vector)
    updateSelectInput(session, "prov", "Choose a province", 
                      choices = prov_vector_sorted, 
                      selected = 'ON')
    # })
  }, priority = 200)
  
  
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
  # return(dataframe)
}