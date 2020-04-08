
# Module UI function
plotUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidRow(
      tabBox(
        id = "tabset1", height = "100%",
        tabPanel(title = "Min-Max",withSpinner(plotOutput(ns("plot_1_min_max_temp"), height = 350))),
        tabPanel(title = 'Mean', withSpinner(plotOutput(ns("plot_1_mean_temp"), height = 350)))
      ),
      tabBox(
        id = "tabset2", height = "100%",
        tabPanel(title = "Min-Max",withSpinner(plotOutput(ns("plot_2_min_max_temp"), height = 350))),
        tabPanel(title = 'Mean', withSpinner(plotOutput(ns("plot_2_mean_temp"), height = 350)))
      )
    ),
    
  )
}



# Module server function
plot <- function(input, output, session, sb_vars, h_vars) {
  plot_type<-reactiveVal('histogram')
  statistic <- reactiveVal('Slopes')
  # p1 <- reactiveVal()
  # p2 <- reactiveVal()
  # p3 <- reactiveVal()
  # p4 <- reactiveVal()
  pp <- reactiveValues()
  plots <- reactiveValues()
  index <-reactiveVal(1)

  observe(print(h_vars$plot_ops))
  
  observe({
    validate(need(sb_vars$year_to_start() != '', 'missing start year'),
             need(sb_vars$month_1() != '', 'missing month 1'),
             need(sb_vars$month_2() != '', 'missing month 2')
    )
    year_to_start<- sb_vars$year_to_start()
    if(h_vars$plot_ops == "Histogram - Slopes - National"){
      plot_type('histogram');location <- 'Canada';loc_type <- 'nation'; statistic('Slopes')
      df_consts <- data.frame(year_to_start, plot_type(), location, loc_type, statistic(), stringsAsFactors = FALSE)    
      output$plot_1_min_max_temp<-renderPlot({
        pp$p1<-(setup_plots('min_max_temp',sb_vars$month_1(), df_consts))
      })
      output$plot_1_mean_temp<-renderPlot({
        pp$p2<-(setup_plots('mean_temp',sb_vars$month_1(), df_consts))
      })
      output$plot_2_min_max_temp<-renderPlot({
        pp$p3<-(setup_plots('min_max_temp',sb_vars$month_2(),df_consts))
      })
      output$plot_2_mean_temp<-renderPlot({
        pp$p4<-(setup_plots('mean_temp',sb_vars$month_2(),df_consts))
      })
    }
    else if(h_vars$plot_ops == "Histogram - R-Squared for Slopes - National"){
      plot_type('histogram');location <- 'Canada';loc_type <- 'nation'; statistic('R-squared for Slopes')
      df_consts <- data.frame(year_to_start, plot_type(), location, loc_type, statistic(), stringsAsFactors = FALSE)    
      output$plot_1_min_max_temp<-renderPlot({
        pp$p1<-setup_plots('min_max_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_1_mean_temp<-renderPlot({
        pp$p2<-setup_plots('mean_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_2_min_max_temp<-renderPlot({
        pp$p3<-setup_plots('min_max_temp',sb_vars$month_2(),df_consts)
      })
      output$plot_2_mean_temp<-renderPlot({
        pp$p4<-setup_plots('mean_temp',sb_vars$month_2(),df_consts)
      })
    }
    else if(h_vars$plot_ops == "Boxplot - Slopes - National/Provincial"){
      plot_type('boxplot');location <- 'Canada';loc_type <- 'nation'; statistic('Slopes')
      df_consts <- data.frame(year_to_start, plot_type(), location, loc_type, statistic(), stringsAsFactors = FALSE)    
      output$plot_1_min_max_temp<-renderPlot({
        pp$p1<-setup_plots('min_max_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_1_mean_temp<-renderPlot({
        pp$p2<-setup_plots('mean_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_2_min_max_temp<-renderPlot({
        pp$p3<-setup_plots('min_max_temp',sb_vars$month_2(), df_consts)
      })
      output$plot_2_mean_temp<-renderPlot({
        pp$p4<-setup_plots('mean_temp',sb_vars$month_2(),df_consts)
      })
    }    
    else if(h_vars$plot_ops == "Histogram - Slopes - Provincial"){
      plot_type('histogram');location <- sb_vars$prov();loc_type <- 'prov'; statistic('Slopes')
      print(sb_vars$prov())
      validate(need(location != '', 'missing prov'))
      df_consts <- data.frame(year_to_start, plot_type(), location, loc_type, statistic(), stringsAsFactors = FALSE)    
      output$plot_1_min_max_temp<-renderPlot({
        pp$p1<-(setup_plots('min_max_temp',sb_vars$month_1(), df_consts))
      })
      output$plot_1_mean_temp<-renderPlot({
        pp$p2<-setup_plots('mean_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_2_min_max_temp<-renderPlot({
        pp$p3<-setup_plots('min_max_temp',sb_vars$month_2(), df_consts)
      })
      output$plot_2_mean_temp<-renderPlot({
        pp$p4<-setup_plots('mean_temp',sb_vars$month_2(),df_consts)
      })
    }                                  
    else if(h_vars$plot_ops == "Boxplot - R-Squared for Slopes - National/Provincial"){
      plot_type('boxplot');location <- 'Canada';loc_type <- 'national'; statistic('R-squared for Slopes')
      df_consts <- data.frame(year_to_start, plot_type(), location, loc_type, statistic(), stringsAsFactors = FALSE)    
      output$plot_1_min_max_temp<-renderPlot({
        pp$p1<-setup_plots('min_max_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_1_mean_temp<-renderPlot({
        pp$p2<-setup_plots('mean_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_2_min_max_temp<-renderPlot({
        pp$p3<-setup_plots('min_max_temp',sb_vars$month_2(), df_consts)
      })
      output$plot_2_mean_temp<-renderPlot({
        pp$p4<-setup_plots('mean_temp',sb_vars$month_2(),df_consts)
      })
    }    
    else if(h_vars$plot_ops == "Histogram - CI_lower for Slopes - National"){
      plot_type('histogram');location <- 'Canada';loc_type <- 'national'; statistic('CI_lower for Slopes')
      df_consts <- data.frame(year_to_start, plot_type(), location, loc_type, statistic(), stringsAsFactors = FALSE)    
      output$plot_1_min_max_temp<-renderPlot({
        pp$p1<-setup_plots('min_max_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_1_mean_temp<-renderPlot({
        pp$p2<-setup_plots('mean_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_2_min_max_temp<-renderPlot({
        pp$p3<-setup_plots('min_max_temp',sb_vars$month_2(), df_consts)
      })
      output$plot_2_mean_temp<-renderPlot({
        pp$p4<-setup_plots('mean_temp',sb_vars$month_2(),df_consts)
      })
    }   
    else if(h_vars$plot_ops == "Histogram - CI_upper for Slopes - National"){
      plot_type('histogram');location <- 'Canada';loc_type <- 'national'; statistic('CI_upper for Slopes')
      df_consts <- data.frame(year_to_start, plot_type(), location, loc_type, statistic(), stringsAsFactors = FALSE)    
      output$plot_1_min_max_temp<-renderPlot({
        pp$p1<-setup_plots('min_max_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_1_mean_temp<-renderPlot({
        pp$p2<-setup_plots('mean_temp',sb_vars$month_1(), df_consts)
      })
      output$plot_2_min_max_temp<-renderPlot({
        pp$p3<-setup_plots('min_max_temp',sb_vars$month_2(), df_consts)
      })
      output$plot_2_mean_temp<-renderPlot({
        pp$p4<-setup_plots('mean_temp',sb_vars$month_2(),df_consts)
      })
    }   
  })
  # observe(print(reactiveValuesToList(pp)))
  
  return(
    list(
      plot_type = reactive({plot_type()}),
      statistic = reactive({statistic()}),
      pp = pp 
    )
  )
}



