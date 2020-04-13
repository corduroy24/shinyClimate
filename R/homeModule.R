
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
          tags$li(HTML(paste('Temperature slopes', strong('vary'), 'within Canada, provinces and cities'))),
          tags$li(HTML(paste('Majority of Canada is in fact, experiencing ', strong('increasing'),' temperatures'))),
          tags$li(HTML(paste('Nunavut and the Northwest Territories - ', strong('North Region'),' temperatures increase at a faster rate than the southern regions'))),
          tags$li(HTML(paste(strong('December through February'), 'warm faster than the other months'))),
        ),
        h3("Discover your regions story!"),
        tags$ul(
          tags$li(HTML(paste(strong('Begin'),' with the', strong('slopes'), ' to evaluate the steepness of the temperature trend'))),
          tags$li(HTML(paste('Proceed to, ', strong('Confidence Intervals'), 'taking into account the variation of the slopes found within a region'))),
          tags$li(HTML(paste(strong('R-Squared'), 'to assess the goodness of the model. How much of the variation in slopes is explained by the temperatures over the years')))
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
    
    # fluidRow(
    #   valueBoxOutput(ns('info_slope')),
    #   valueBoxOutput(ns('info_ci')),
    #   valueBoxOutput(ns('info_r2'))
    # ),
    
    h5(em(textOutput(ns('city_stats'))), style = 'text-align: center'),
    # h5(em('The above is for annual mean temperatures for the specified cities'), style = 'text-align: center'),
    
    fluidRow(
      box(
        width = "100%", status = 'warning',
        h3("Together we can respond to these inquiries!", style = 'font-weight:bold;'),
        tags$ul(tags$li('How strong is the evidence towards climate change?'),
                tags$li('Over the years, how much does temperature increase or decrease?'),
                tags$li('What is the variation in these slopes values?'),
                tags$li('Which parts of Canada should be more concerned about climate change?')
        
        ),
        h3("More"),
        h5("Refer to Preferences -> Other to change months and start year"),
        h5("Close sidebar menu to have a better look at the plots"),
        h5("Download a report on the plots currently displayed")
      )
    ),
  )
}


# Module server function
homeLayout <- function(input, output, session, sb_vars) {
  
  # output$test<-renderPlot({
  #   grid.draw(vars_plot$pp$p1)
  # })
  h_vars <- reactiveValues()
  
  observe(h_vars$plot_ops <- input$plot_options)
  
  p_vars <- callModule(plot, 'inner_plot', sb_vars = sb_vars, h_vars = h_vars)
  
  observeEvent(sb_vars$region(),{
    if(sb_vars$region()  == 'City'){
      updateSelectInput(session, 'plot_options',
                           choices = list(
                              Trend = list("Regression line - Temperatures")
                           ),
                           selected = "Regression line - Temperatures")
    }
    
    else if(sb_vars$region()  == 'Province'){
      updateSelectInput(session, 'plot_options',
                        choices = list(
                          'Slopes' = list("Histogram - Slopes"),
                          "Confidence Intervals for Slopes" = list("Histogram - CI_lower for Slopes",
                                                                   "Histogram - CI_upper for Slopes"),
                          'R-squared for Slopes' = list("Histogram - R-Squared for Slopes")
                        ),
                        selected = "Histogram - Slopes")
    }
    else if(sb_vars$region()  == 'Canada'){
      updateSelectInput(session, 'plot_options',
                        choices = list(
                          'Slopes' = list("Histogram - Slopes",
                                          "Boxplot - Slopes"),
                          "Confidence Intervals for Slopes" = list("Histogram - CI_lower for Slopes",
                                                                   "Histogram - CI_upper for Slopes"),
                          'R-squared for Slopes' = list("Histogram - R-Squared for Slopes",
                                                        "Boxplot - R-Squared for Slopes")
                        ),
                        selected = "Histogram - Slopes")
    }
  })

  output$info_slope<- renderValueBox({
    df <- get_city_stats(sb_vars$city(), 'Annual',1980)
    val <-   stat_lab <-expression(signif(df$slope,4) *'( '*degree *'C)')
    valueBox(value = val, subtitle = paste(sb_vars$city(), '-Slope'), color = 'blue', icon = icon('info-circle'))
  })
  
  output$info_ci<- renderValueBox({
    df <- get_city_stats(sb_vars$city(), 'Annual',1980)
    v<- paste('(', signif(df$CI_lower,3), ',' ,signif(df$CI_upper,3),')')
    v<- tags$p(v, style = "font-size:27px;")
    valueBox(value = v, subtitle = paste(sb_vars$city(), '- CI for slopes'), color = 'blue', icon = icon('info-circle'))
  })
  output$info_r2<- renderValueBox({
    df <- get_city_stats(sb_vars$city(), 'Annual',1980)
    valueBox(value = signif(df$r.squared,4), subtitle = paste(sb_vars$city(), '- R2 for slopes'), color = 'blue', icon = icon('info-circle'))
  })
  
  output$city_stats <- renderText({
    'The above is for annual mean temperatures for the specified cities'
  })
  output$plot_des_1<- renderUI({
    if(p_vars$plot_type() == 'histogram')
      para_1<- "Summarize the distribution of data"
    else if(p_vars$plot_type() == 'boxplot')
      para_1<- "Look for skewed data (whiskers and mean line), outliers (dots), and comparing spreads (variation)"
    else if(p_vars$plot_type() == 'regression line')
      para_1<- "Best fitted line - y = b + mx, where b - intercept and m - slope"
    
    para_1 <- p(para_1, style = 'margin:0;display:inline;')
    
    div(
      title_1 <- p(str_to_title(p_vars$plot_type()), style = 'font-weight:bold; font-size:16px;margin:0;padding:0;display:inline'),
      para_1
    )
  })
  
  output$plot_des_2<- renderUI({
    title_2 <- p(strsplit(str_to_title(p_vars$statistic()), '_')[[1]][1], style = 'font-weight:bold; font-size:16px;margin:0;display:inline')
    if(p_vars$statistic() == 'Slopes')
      para_2<- "The rate of change in x - Years as y - temperature changes."
    else if(p_vars$statistic() == 'CI_lower for Slopes')
      para_2<- "The lower bound of a range of values we are fairly (95%) sure our true value lies in."
    else if(p_vars$statistic() == 'CI_upper for Slopes')
      para_2<- "The upper bound of a range of values we are fairly (95%) sure our true value lies in."
    else if(p_vars$statistic() == 'R-squared for Slopes')
      para_2<- "How well the data fit the trend - goodness of fit. "
    else{
      title_2 <- div(
        title_2_1 <- p('Slope', style = 'font-weight:bold; font-size:16px;margin:0;display:inline'),
        para_2_1 <-"The rate of change in x"
      )
      para_2 <- div (
        title_2_1 <- p('R-Squared for Slope', style = 'font-weight:bold; font-size:16px;margin:0;display:inline'),
        para_2_2<- "goodness of fit",
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

  return(p_vars)
}