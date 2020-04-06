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
        title = NULL,
        # h2("These 2 months showcase the strongest evidence for climate change"),
        # h3("Refer to preferences to change"),
        status = "primary",
        # solidHeader = TRUE,
        height = 100,
        # background = "green",
        selectInput(ns("plot_options"), "Choose a Plot:",
                    choices = c("Histogram of slopes - Provincial",
                                "Histogram of Confidence Intervals for slopes - National",
                                "Histogram of slopes - National",
                                "Boxplot of slopes - National/Provincial",
                                "Histogram of Confidence Intervals for slopes - National"),
                    selected = "Histogram of slopes - National"
        )
      ),
      box(
        title=NULL,
        height = 100,
        background = "light-blue",
        uiOutput(ns('plot_des'))
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
  meas_type <- reactiveVal('Slopes')

  # beginning <- Sys.time()
  # end <- Sys.time()
  # output$txt1 <- renderText({end - beginning})
  output$plot_des<- renderUI({
    if(plot_type() == 'histogram')
      para_1<- "Histogram Description here"
    else if(plot_type() == 'boxplot')
      para_1<- "Boxplot Description here"
    
    if(meas_type() == 'Slopes')
      para_2<- "Slopes Description here"
    else if(meas_type() == 'Confidence Intervals')
      para_2<- "CI Description here"
    else if(meas_type() == 'R-Squared')
      para_2<- "R-squared Description here"
    
    para_1 <- p(para_1, style = 'margin:0;display:inline;')
    para_2 <- p(para_2, style = 'margin:0;display:inline;')
    
    div(
      title_1 <- p(str_to_title(plot_type()), style = 'font-weight:bold; font-size:18px;margin:0;padding:0;display:inline'),
      para_1,
      br(),
      title_2 <- p(str_to_title(meas_type()), style = 'font-weight:bold; font-size:18px;margin:0;display:inline'),
      para_2
    )
  
  })
  observe({
    validate(need(sb_vars$year_to_start() != '', 'missing start year'),
             need(sb_vars$month_1() != '', 'missing month 1'),
             need(sb_vars$month_2() != '', 'missing month 2')
    )
    year_to_start<- sb_vars$year_to_start()
    if(input$plot_options == "Histogram of slopes - National"){
      plot_type('histogram');location <- 'Canada';loc_type <- 'nation'
      df_consts <- data.frame(year_to_start, plot_type(), location, loc_type, stringsAsFactors = FALSE)    
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
    else if(input$plot_options == "Boxplot of slopes - National/Provincial"){
      plot_type <- 'boxplot';location <- 'Canada';loc_type <- 'nation'
      df_consts <- data.frame(year_to_start, plot_type, location, loc_type, stringsAsFactors = FALSE)
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
    else if(input$plot_options == "Histogram of slopes - Provincial"){
      plot_type <- 'histogram';location <- sb_vars$prov();loc_type <- 'prov'
      validate(need(location != '', 'missing prov'))
      df_consts <- data.frame(year_to_start, plot_type, location, loc_type, stringsAsFactors = FALSE)
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
      # else if(input$plot_options == "Boxplot of slopes - Provincial"){
      #   output$plot_1_min_max_temp<-renderPlot({
      #     setup_plots('min_max_temp',sb_vars$month_1, sb_vars$year_to_start, plot_type, location, loc_type)
      #   })
      #   output$plot_1_mean_temp<-renderPlot({
      #     setup_plots('mean_temp',sb_vars$month_1, sb_vars$year_to_start, plot_type, location, loc_type)
      #   })
      #   output$plot_2_min_max_temp<-renderPlot({
      #     setup_plots('min_max_temp',sb_vars$month_2, sb_vars$year_to_start, plot_type, location, loc_type)
      #   })
      #   output$plot_2_mean_temp<-renderPlot({
      #     setup_plots('mean_temp',sb_vars$month_2,sb_vars$year_to_start, plot_type, location, loc_type)
      #   })
      # } 
      # 
      # else if(input$plot_options == "Boxplot of slopes - National"){
      #   output$plot_1_min_max_temp<-renderPlot({
      #     setup_plots('min_max_temp',sb_vars$month_1, sb_vars$year_to_start, plot_type, location, loc_type)
      #   })
      #   output$plot_1_mean_temp<-renderPlot({
      #     setup_plots('mean_temp',sb_vars$month_1, sb_vars$year_to_start, plot_type, location, loc_type)
      #   })
      #   output$plot_2_min_max_temp<-renderPlot({
      #     setup_plots('min_max_temp',sb_vars$month_2, sb_vars$year_to_start, plot_type, location, loc_type)
      #   })
      #   output$plot_2_mean_temp<-renderPlot({
      #     setup_plots('mean_temp',sb_vars$month_2,sb_vars$year_to_start, plot_type, location, loc_type)
      #   })
      # } 
      # 
      # else if(input$plot_options == "Boxplot of slopes - National"){
      #   output$plot_1_min_max_temp<-renderPlot({
      #     setup_plots('min_max_temp',sb_vars$month_1, sb_vars$year_to_start, plot_type, location, loc_type)
      #   })
      #   output$plot_1_mean_temp<-renderPlot({
      #     setup_plots('mean_temp',sb_vars$month_1, sb_vars$year_to_start, plot_type, location, loc_type)
      #   })
      #   output$plot_2_min_max_temp<-renderPlot({
      #     setup_plots('min_max_temp',sb_vars$month_2, sb_vars$year_to_start, plot_type, location, loc_type)
      #   })
      #   output$plot_2_mean_temp<-renderPlot({
      #     setup_plots('mean_temp',sb_vars$month_2,sb_vars$year_to_start, plot_type, location, loc_type)
      #   })
      # } 

})

    
    return(update)
}
