# Module UI function
addNoteUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  
  fluidRow(
    box(
      width = 12, status = 'warning',
      h3("Add notes to your reoprt!", style = 'font-weight:bold;'),
      textAreaInput(ns('note'), '', value = "", width = NULL, height = NULL, placeholder = NULL)
      
    )
  )

}


# Module server function
addNote<- function(input, output, session) {
  
  
  observeEvent(input$file1,{
    print("im here")
    inFile <- input$file1
    
    print(inFile)
    if (is.null(inFile))
      return(NULL)
    
    df <- clean_user_data(inFile$datapath)
    
    
    # read.csv(inFile$datapath, header = input$header)
  })
}



