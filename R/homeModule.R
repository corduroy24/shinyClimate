# source("helpers.R", local = TRUE)
# tags$style(HTML(".small_icon_test { font-size: 12px; }"))
# library(shinyjs)
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
    # center later 
    # h2("Temperature Trends"),
    
    # verbatimTextOutput(NS(id, "txt1")),
    fluidRow(
      box(
        width = "100%", status = 'warning',
        h3("Showcasing the strongest evidence for climate change"),
        tags$ul(tags$li('February and July'),
                tags$li('Minimum and Maximum')
        ),
        h4('Otherwise, known as Extreme values')
      )
    ),
    fluidRow(
      box(
        title = NULL,
        # h3("Refer to preferences to change"),
        status = "primary",
        # solidHeader = TRUE,
        height = 100, width = 6,
        # background = "green",
        selectInput(ns("plot_options"), "Choose a Plot:",
                    choices = c("Histogram - Slopes - National",
                                "Histogram - R-Squared for Slopes - National",
                                "Histogram - Slopes - Provincial",
                                "Boxplot - Slopes - National/Provincial",
                                "Boxplot - R-Squared for Slopes - National/Provincial"
                                # "Histogram of Confidence Intervals for Slopes - National"
                                ),
                    selected = "Histogram of slopes - National"
        )
      ),
      box(
        title=NULL,
        height = 100, width = 3,
        background = "maroon",
        uiOutput(ns('plot_des_1'))
      ),
      box(
        title=NULL, width = 3,
        height = 100,
        background = "aqua",
        uiOutput(ns('plot_des_2'))
      )
    ),
    fluidRow(
      tabBox(
        id = "tabset1", height = "100%",
        tabPanel(title = "Min-Max",withSpinner(plotOutput(ns("plot_1_min_max_temp"), height = 300))),
        tabPanel(title = 'Mean', withSpinner(plotOutput(ns("plot_1_mean_temp"), height = 300)))
      ),
      tabBox(
        id = "tabset2", height = "100%",
        tabPanel(title = "Min-Max",plotOutput(ns("plot_2_min_max_temp"), height = 300)),
        tabPanel(title = 'Mean', plotOutput(ns("plot_2_mean_temp"), height = 300))
      )
    ),

    fluidRow(
      infoBox(
        title = "Extremes - [purpose]",
        icon = shiny::icon('info-circle', class = NULL, lib = "font-awesome"),
        color = 'red',
        fill = TRUE
      )
      # infoBox(
      #   title = "  - [purpose]",
      #   icon = shiny::icon('info-circle', class = NULL, lib = "font-awesome"),
      #   color = 'red',
      #   fill = TRUE
      # ),
    )
  )
}


# Module server function
homeLayout <- function(input, output, session, sb_vars) {
  ns <- session$ns
  plot_type<-reactiveVal('histogram')
  statistic <- reactiveVal('Slopes')

  # beginning <- Sys.time()
  # end <- Sys.time()
  # output$txt1 <- renderText({end - beginning})
  output$plot_des_1<- renderUI({
    if(plot_type() == 'histogram')
      para_1<- "Histogram Description here"
    else if(plot_type() == 'boxplot')
      para_1<- "Boxplot Description here"
    
    para_1 <- p(para_1, style = 'margin:0;display:inline;')

    div(
      title_1 <- p(str_to_title(plot_type()), style = 'font-weight:bold; font-size:18px;margin:0;padding:0;display:inline'),
      para_1,
    )
  })
  output$plot_des_2<- renderUI({
    if(statistic() == 'Slopes')
      para_2<- "Slopes Description here"
    else if(statistic() == 'Confidence Intervals for Slopes')
      para_2<- "CI Description here"
    else if(statistic() == 'R-squared for Slopes')
      para_2<- "R-squared Description here"
    
    para_2 <- p(para_2, style = 'margin:0;display:inline;')
    
    div(
      title_2 <- p(str_to_title(statistic()), style = 'font-weight:bold; font-size:18px;margin:0;display:inline'),
      para_2
    )
  })
  
  observe({
    validate(need(sb_vars$year_to_start() != '', 'missing start year'),
             need(sb_vars$month_1() != '', 'missing month 1'),
             need(sb_vars$month_2() != '', 'missing month 2')
    )
    year_to_start<- sb_vars$year_to_start()
    if(input$plot_options == "Histogram - Slopes - National"){
      plot_type('histogram');location <- 'Canada';loc_type <- 'nation'; statistic('Slopes')
      df_consts <- data.frame(year_to_start, plot_type(), location, loc_type, statistic(), stringsAsFactors = FALSE)    
      output$plot_1_min_max_temp<-renderPlot({
        setup_plots('min_max_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_1_mean_temp<-renderPlot({
        setup_plots('mean_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_2_min_max_temp<-renderPlot({
        setup_plots('min_max_temp',sb_vars$month_2(),df_consts)
      })
      output$plot_2_mean_temp<-renderPlot({
        setup_plots('mean_temp',sb_vars$month_2(),df_consts)
      })
    }
    else if(input$plot_options == "Histogram - R-Squared for Slopes - National"){
      plot_type('histogram');location <- 'Canada';loc_type <- 'nation'; statistic('R-squared for Slopes')
      df_consts <- data.frame(year_to_start, plot_type(), location, loc_type, statistic(), stringsAsFactors = FALSE)    
      output$plot_1_min_max_temp<-renderPlot({
        setup_plots('min_max_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_1_mean_temp<-renderPlot({
        setup_plots('mean_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_2_min_max_temp<-renderPlot({
        setup_plots('min_max_temp',sb_vars$month_2(),df_consts)
      })
      output$plot_2_mean_temp<-renderPlot({
        setup_plots('mean_temp',sb_vars$month_2(),df_consts)
      })
    }
    else if(input$plot_options == "Boxplot - Slopes - National/Provincial"){
      plot_type <- 'boxplot';location <- 'Canada';loc_type <- 'nation'; statistic('Slopes')
      df_consts <- data.frame(year_to_start, plot_type(), location, loc_type, statistic(), stringsAsFactors = FALSE)    
      output$plot_1_min_max_temp<-renderPlot({
        setup_plots('min_max_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_1_mean_temp<-renderPlot({
        setup_plots('mean_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_2_min_max_temp<-renderPlot({
        setup_plots('min_max_temp',sb_vars$month_2(), df_consts)
      })
      output$plot_2_mean_temp<-renderPlot({
        setup_plots('mean_temp',sb_vars$month_2(),df_consts)
      })
    }    
    else if(input$plot_options == "Histogram - Slopes - Provincial"){
      plot_type <- 'histogram';location <- sb_vars$prov();loc_type <- 'prov'; statistic('Slopes')
      validate(need(location != '', 'missing prov'))
      df_consts <- data.frame(year_to_start, plot_type(), location, loc_type, statistic(), stringsAsFactors = FALSE)    
      output$plot_1_min_max_temp<-renderPlot({
        setup_plots('min_max_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_1_mean_temp<-renderPlot({
        setup_plots('mean_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_2_min_max_temp<-renderPlot({
        setup_plots('min_max_temp',sb_vars$month_2(), df_consts)
      })
      output$plot_2_mean_temp<-renderPlot({
        setup_plots('mean_temp',sb_vars$month_2(),df_consts)
      })
    }  
  })

  return(update)
}
