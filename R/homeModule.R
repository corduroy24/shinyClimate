
library(shinycssloaders)

# Module UI function
homeLayoutUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    # verbatimTextOutput(NS(id, "txt1")),
    fluidRow(
      box(
        width = 12, status = 'warning',
        h3("Showcasing evidence of climate change", style = 'font-weight:bold;text-align:center;'),
        h5(em("Temperature Trends"), style = 'text-align:center;'),
        h4('Extreme values, as cautionary bounds'),
        
        tags$ul(tags$li('January and July'),
                tags$li('Minimum and Maximum')
        ),
        h3("Highlights"),
        tags$ul(
          tags$li(HTML(paste('Temperature slopes', strong('vary'), 'within Canada, provinces and cities'))),
          tags$li(HTML(paste('Majority of Canada is in fact, experiencing ', strong('increasing'),' temperatures'))),
          tags$li(HTML(paste('Nunavut and the Northwest Territories - ', strong('Northern Region'),' temperatures increase at a faster rate than the', strong('southern regions')))),
          tags$li(HTML(paste(strong('December through February'), 'warm faster than the other months'))),
          tags$li(HTML(paste('Minimum temperatures can be', strong('decreasing') ,'(or increasing)', 'while maximum temperatures', strong('increasing'), '(or decreasing)'))),
          tags$li(HTML(paste('Cities with', strong('extreme warming'), '(within a province)'))),
          tags$ul(
            tags$li(strong("Yukon Territory"), " (Dawson, Pelly Ranch), ", strong("Northwest Territories")," (Fort Good Hope, Fort Smith), ", strong("Nunavut"), " (Pelly Bay, Ennadai Lake)"),
            tags$li(strong("British Columbia"), " (Creston, Glacier, Kelowna)"),
            tags$li(strong("Manitoba"), " (Flin Flon, Norway House), ", strong("Saskatchewan")," (Waskesiu Lake, Loon Lake, Yellow Grass), ", strong("Alberta"), " (Coronation, Entrance)"),
            tags$li(strong("Quebec"), " (Bagotville, Kuujjuarapik), ", strong("Ontario")," (Beatrice, Cornwall, Dryden, Ottawa)"),
            tags$li(strong("Newfoundland and Labrador"), " (Cartwright, Nain), ", strong("Prince Edward Island")," (Charlottetown, Monticello, Summerside), ",
                    strong("Nova Scotia"), " (Collegeville, Greenwood, Yarmouth), ", strong("New Brunswick"), " (Moncton, Woodstock)")
          )

        ),
        h3("Discover your regions story!"),
        tags$ul(
          tags$li(HTML(paste(strong('Begin'),' with the', strong('slopes'), ' to evaluate the steepness of the temperature trend'))),
          tags$li(HTML(paste('Proceed to, ', strong('Confidence Intervals'), 'considering the variation of the slopes found within a region'))),
          tags$li(HTML(paste(strong('R\U000B2'), 'to assess the goodness of the model. How much of the variation in slopes is explained by the temperatures over the years')))
        ),
      )
    ),
    
    fluidRow(
      box(
        title = NULL,
        status = "primary",
        height = 110, width = 6,
        selectInput(ns("plot_options"), "Choose a Statistic:",
                    choices = list()
        )
      ),
      
      box(
        id = ns('p_type'),
        title=NULL,
        height = 120, width = 3,
        background = "maroon",
        uiOutput(ns('plot_des_1'))
      ),
      box(
        id = ns('stat'),
        title=NULL, width = 3,
        height = 120,
        background = "aqua",
        uiOutput(ns('plot_des_2'))
      )    
    ),

    plotUI(ns("inner_plot")),
    analysisUI(ns("inner_analysis")),
    
    
    
    fluidRow(
      box(
        width = 12, status = 'warning',
        h3("Together we can respond to these inquiries!", style = 'font-weight:bold;'),
        tags$ul(tags$li('How strong is the evidence towards climate change?'),
                tags$li('Over the years, how much does temperature increase or decrease?'),
                tags$li('What is the variation in these slope values?'),
                tags$li('Which parts of Canada should be more concerned about climate change?')
        
        )
      )
    )
  )
}


# Module server function
homeLayout <- function(input, output, session, sb_vars) {
  
  # output$test<-renderPlot({
  #   grid.draw(vars_plot$pp$p1)
  # })
  
  h_vars <- reactiveValues()
  
  observe(h_vars$plot_ops <- input$plot_options)
  
  plot_vars <- callModule(plots, 'inner_plot', sb_vars = sb_vars, h_vars = h_vars)
  a_vars <- callModule(analysis, 'inner_analysis', sb_vars = sb_vars, p_vars = plot_vars)
  
  observeEvent(sb_vars$region(),{
    if(sb_vars$region()  == 'City'){
      updateSelectInput(session, 'plot_options',
                           choices = list(
                              Trend = list("Regression line - Temperatures vs Years")
                           ),
                           selected = "Regression line - Temperatures vs Years")
    }
    
    else if(sb_vars$region()  == 'Province'){
      updateSelectInput(session, 'plot_options',
                        choices = list(
                          'Slopes' = list("Histogram - Slopes"),
                          "Confidence Intervals for Slopes" = list("Histogram - Lower.Bound",
                                                                   "Histogram - Upper.Bound"),
                          'R\U000B2 for Slopes' = list('Histogram - R\U000B2')
                        ),
                        selected = "Histogram - Slopes")
    }
    else if(sb_vars$region()  == 'Canada'){
      updateSelectInput(session, 'plot_options',
                        choices = list(
                          'Slopes' = list("Histogram - Slopes",
                                          "Boxplot - Slopes"),
                          "Confidence Intervals for Slopes" = list("Histogram - Lower.Bound",
                                                                   "Histogram - Upper.Bound"),
                          'R\U000B2 for Slopes' = list("Histogram - R\U000B2",
                                                        "Boxplot - R\U000B2")
                        ),
                        selected = "Histogram - Slopes")
    }
  })

  

  output$plot_des_1<- renderUI({
    if(plot_vars$plot_type() == 'histogram')
      para_1<- "Summarize the distribution of data"
    else if(plot_vars$plot_type() == 'boxplot')
      para_1<- "Look for skewed data (whiskers and mean line), outliers (dots), and comparing spreads (variation)"
    else if(plot_vars$plot_type() == 'regression line')
      para_1<- "Best fitted line, y = b + mx. Where b is the intercept and m is the slope"
    
    para_1 <- p(para_1, style = 'margin:0;display:inline;')
    
    div(
      title_1 <- p(str_to_title(plot_vars$plot_type()), style = 'font-weight:bold; font-size:16px;margin:0;padding:0;display:inline'),
      para_1
    )
  })
  
  output$plot_des_2<- renderUI({
    title_2 <- p(strsplit(str_to_title(plot_vars$statistic()), '_')[[1]][1], style = 'font-weight:bold; font-size:16px;margin:0;display:inline')
    if(plot_vars$statistic() == 'Slopes')
      para_2<- "The rate of change in x - Years as y - temperature changes."
    else if(plot_vars$statistic() == 'Lower.Bound')
      para_2<- "The lower bound of a range of values we are fairly (95%) sure our true value lies in."
    else if(plot_vars$statistic() == 'Upper.Bound')
      para_2<- "The upper bound of a range of values we are fairly (95%) sure our true value lies in."
    else if(plot_vars$statistic() == 'R\U000B2')
      para_2<- "How well the data fit the trend - goodness of fit."
    else {
      title_2 <- div(
        title_2_1 <- p('Slope', style = 'font-weight:bold; font-size:16px;margin:0;display:inline'),
        para_2_1 <-"The rate of change in x"
      )
      para_2 <- div (
        title_2_1 <- p('R\U000B2', style = 'font-weight:bold; font-size:16px;margin:0;display:inline'),
        para_2_2<- HTML(paste0("goodness of fit. 0 < R", tags$sup(2), "< 1 ( <-> %)")),
      )
    }
    
    para_2 <- p(para_2, style = 'margin:0;display:inline;')
    
    div(
      title_2 ,
      para_2
    )
  })
  
  # beginning <- Sys.time()
  # end <- Sys.time()
  # output$txt1 <- renderText({end - beginning})

  return(plot_vars)
}