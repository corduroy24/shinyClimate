# source("helpers.R", local = TRUE)

# Module UI function
prefLayoutUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Temperature",

      )
    )
  )
}


# Module server function
prefLayout <- function(input, output, session) {

  return(
    list(
      month_1 = reactive({strtrim(input$month_1, 3)}),
      month_2 = reactive({strtrim(input$month_2, 3)}),
      year_to_start = reactive({input$year_to_start})
      )
    )
}