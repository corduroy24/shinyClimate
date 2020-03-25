# source("helpers.R", local = TRUE)

# Module UI function
prefLayoutUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Temperature",
        selectInput(ns("month"), "Choose a month:",
                    choices = c("January", "February", "March","April","May","June","July","August","September","October","November", "December"),
                    selected = 'February'
        ),
        
        selectInput(ns("year_to_start"), "Start Year",
                    choices = seq(1840, 1986), 
                    selected = 1980
        )
      )
    )
    
  )
}



# Module server function
prefLayout <- function(input, output, session) {

  return(
    list(
      month = reactive({input$month}),
      year_to_start = reactive({input$year_to_start})
      )
    )
}