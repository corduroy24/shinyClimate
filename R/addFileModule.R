# Module UI function
addFileUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fileInput(ns("file1"), "Choose CSV File",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv")
  )
    # tableOutput("contents")
  
  # tagList(
  #   # Button
  #   div(style="width:100%;text-align: center;",
  #       actionButton(ns("file"), "Add File",width = '50%', 
  #                      style ='display:inline-block;background:black;border-color:orange;color:white')
  #   )
  # )
}


# Module server function
addFile <- function(input, output, session) {

  
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



