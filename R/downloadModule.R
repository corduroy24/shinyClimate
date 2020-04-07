
# Module UI function
downloadUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    column(4, align = 'center',
             actionButton(ns('add_to_pdf'), "Add Current plots to report",width = '100%',
                          style ='background:white;border-color:black')
             ),

    column(4, align = 'center',
           # Button
           downloadButton(ns("report"), "Generate Report",width = '100%', 
                          style ='background:white;border-color:black;')
    ),
    plotOutput(ns('test'))

  )
}



# Module server function
download <- function(input, output, session, plots) {
  
  output$report <- downloadHandler(
    filename = "report.pdf",

    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("../report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(p1 = plots$p1,
                     p2 = plots$p2,
                     p3 = plots$p3,
                     p4 = plots$p4
                     )
                     
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      out = rmarkdown::render(tempReport, pdf_document(),
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      file.rename(out, file) # move pdf to file for downloading
      
    }
  )
}



