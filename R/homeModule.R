# Status:
#   primary Blue (sometimes dark blue)
#   success Green
#   info Blue
#   warning Orange
#   danger Red
library(shinycssloaders)

# Module UI function
homeLayoutUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    # verbatimTextOutput(NS(id, "txt1")),
    fluidRow(
      box(
        width = "100%", status = 'warning',

        h3("Showcasing the strongest evidence for climate change", style = 'font-weight:bold;text-align:center;'),
        h5(em("Temperature Trends"), style = 'text-align:center;'),
        h4('Extreme values, as cautionary bounds'),
        
        tags$ul(tags$li('February and July'),
                tags$li('Minimum and Maximum')
        ),
        h3("Highlights"),
        tags$ul(
          tags$li('Temperature slopes vary within provinces and cities'),
          tags$li(HTML(paste('Temperatures increase or decrease at a',tags$strong('slow rate')))),
          tags$li('Provinces in the ... ')
        ),
      )
    ),
    fluidRow(
      box(
        title = NULL,
        status = "primary",
        # solidHeader = TRUE,
        height = 100, width = 6,
        # background = "green",
        selectInput(ns("plot_options"), "Choose a Statistic:",
                    choices = list(
                      'Slopes' = list("Histogram - Slopes - National",
                                      "Histogram - Slopes - Provincial",
                                      "Boxplot - Slopes - National/Provincial"),
                      'R-squared for Slopes' = list("Histogram - R-Squared for Slopes - National",
                                         "Boxplot - R-Squared for Slopes - National/Provincial"),
                      "Confidence Intervals for Slopes" = list("Histogram - CI_lower for Slopes - National",
                                                    "Histogram - CI_upper for Slopes - National")
                                
                                ),
                    selected = "Histogram - Slopes - National"
        )
      ),
      
      infoUI(ns("inner_info"))

    ),
    
    plotUI(ns("inner_plot")),
    plotOutput(ns('test')),
    fluidRow(
      infoBox(
        title = "Extremes - [purpose]",width = 4,
        icon = shiny::icon('info-circle', class = NULL, lib = "font-awesome"),
        color = 'red',
        fill = TRUE
      ),
      downloadUI(ns("inner_dl"))
      
    ),
    fluidRow(
      box(
        width = "100%", status = 'warning',
        h3("Discover the story!", style = 'font-weight:bold;'),
        tags$ul(tags$li('How strong is the evidence towards climate change?'),
                tags$li('Over the years, how much does temperature increase or decrease?'),
                tags$li('What is the variation in these slopes values?'),
                tags$li('Should my city - province - country be concerned about climate change?')
        ),
        h5("Refer to Preferences -> Other to change months and start year"),
        h5("Close sidebar menu to have a better look at the plots")
      )
    ),
  )
}


# Module server function
homeLayout <- function(input, output, session, sb_vars) {
  
  vars_plot <- callModule(plot, 'inner_plot', sb_vars = sb_vars, options = input$plot_options)
  
  callModule(info, 'inner_info', vars  = vars_plot)
  
  callModule(download, 'inner_dl', plots = vars_plot$pp)
   
  output$test <- renderPlot(
    grid.draw(vars_plot$pp$p1)
  )

  # beginning <- Sys.time()
  # end <- Sys.time()
  # output$txt1 <- renderText({end - beginning})

  

  return(update)
}