
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
  plot_type<-reactiveVal('regression line')
  statistic <- reactiveVal('temperatures vs years')
  # p1 <- reactiveVal()
  # p2 <- reactiveVal()
  # p3 <- reactiveVal()
  # p4 <- reactiveVal()
  pp <- reactiveValues()
  plots <- reactiveValues()
  index <-reactiveVal(1)
  rv <- reactiveValues(dfnew=data.frame(matrix(ncol = 2, nrow = 0)) ,count=1)
  
  observe(print(h_vars$plot_ops))
  # observe(print(df()))
  # 
  # storedvalues <- observeEvent(pp$p1, {
  #     rv$dfnew <- rbind(rv$dfnew, df())
  #     rv$count = rv$count + 1
  # 
  # })
  # 
  # df <- reactive({
  #   data.frame(
  #     id = rv$count,
  #     value = pp$p1
  #   )
  # })
  
  
  observe({
    validate(need(sb_vars$year_to_start() != '', 'missing start year'),
             need(sb_vars$month_1() != '', 'missing month 1'),
             need(sb_vars$month_2() != '', 'missing month 2'),
             need(sb_vars$prov() != '', 'missing province'),
             need(sb_vars$city() != '', 'missing city')
    )
    year_to_start<- sb_vars$year_to_start()
    
    if(sb_vars$region() == 'City'){
      location <- paste0(sb_vars$city(),',' ,sb_vars$prov())
      city <- sb_vars$city()
    }
    else if(sb_vars$region() == 'Province'){
      location <- sb_vars$prov()
      city <- sb_vars$city()
      prov <- sb_vars$prov()
    }
    else if(sb_vars$region() == 'Canada'){
      location <- 'Canada'
      city <- sb_vars$city()
      prov <- sb_vars$prov()
    }
    
    plot_type(trimws((strsplit(tolower(h_vars$plot_ops),'-'))[[1]][1]))
    statistic(trimws((strsplit(h_vars$plot_ops,'-'))[[1]][2]))
    

    region <- sb_vars$region()
    city_lab <- sb_vars$city_lab()
    # print(plot_type())
    # print(statistic())
    # print(region)
    # print(location)
    
    df_consts <- data.frame(year_to_start, plot_type(), location, region,
                            statistic(),city,prov, city_lab, stringsAsFactors = FALSE)    
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



