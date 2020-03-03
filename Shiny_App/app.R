library(shiny)

temp = list.files(path = "C:\Shiny_App\Homog_monthly_max_temp", pattern="*.txt")
myfiles = lapply(temp, read.delim)

ui <- fluidPage(
  
)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)