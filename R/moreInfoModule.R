# Module UI function
moreInfoUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )
}


# Module server function
moreInfo <- function(input, output, session) {
  
}