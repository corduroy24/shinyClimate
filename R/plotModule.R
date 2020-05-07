
# Module UI function
plotUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidRow(
      tabBox(
        id = "tabset1", height = "100%",
        tabPanel(title = "Min-Max",withSpinner(plotOutput(ns("plot_1_min_max_temp"), height = 350)))
        # tabPanel(title = 'Mean', withSpinner(plotOutput(ns("plot_1_mean_temp"), height = 350)))
      ),
      tabBox(
        id = "tabset2", height = "100%",
        tabPanel(title = "Min-Max",withSpinner(plotOutput(ns("plot_2_min_max_temp"), height = 350)))
        # tabPanel(title = 'Mean', withSpinner(plotOutput(ns("plot_2_mean_temp"), height = 350)))
      )
    ),
    
  )
}



# Module server function
plots <- function(input, output, session, sb_vars, h_vars) {
  plot_type<-reactiveVal('regression line')
  statistic <- reactiveVal('temperatures vs years')

  pp <- reactiveValues()
  plots <- reactiveValues()
  index <-reactiveVal()
  rv <- reactiveValues(dfnew=list() ,count=1)
  plot_num <- reactiveVal()
  observe(print(h_vars$plot_ops))

  # storedvalues <- observeEvent(pp$p1, {
  #     rv$table <- rbind(rv$dfnew, df())
  #     rv$count = rv$count + 1
  #     print(rv$count)
  # })

  
  
  observe({
    validate(need(sb_vars$year_to_start() != '', 'missing start year'),
             need(sb_vars$month_1() != '', 'missing month 1'),
             need(sb_vars$month_2() != '', 'missing month 2'),
             need(sb_vars$prov() != '', 'missing province'),
             need(sb_vars$city() != '', 'missing city'),
             need(sb_vars$region() != '', 'missing region')
             
    )
    year_to_start<- sb_vars$year_to_start()
    city <- sb_vars$city()
    prov <- sb_vars$prov()
    region <- sb_vars$region()
    show_city_lab <- sb_vars$show_city_lab()

    
    if(sb_vars$region() == 'City'){
      location <- paste0(sb_vars$city(),',' ,sb_vars$prov())
    }
    else if(sb_vars$region() == 'Province'){
      location <- sb_vars$prov()
    }
    else if(sb_vars$region() == 'Canada'){
      location <- 'Canada'
    }
    
    plot_type(trimws((strsplit(tolower(h_vars$plot_ops),'-'))[[1]][1]))
    statistic(trimws((strsplit(h_vars$plot_ops,'-'))[[1]][2]))
    
    # print(plot_type())
    # print(statistic())
    # print(region)
    # print(location)
    
    df_consts <- data.frame(year_to_start, plot_type(), location, region,
                            statistic(),city,prov, show_city_lab, stringsAsFactors = FALSE)    
    output$plot_1_min_max_temp<-renderCachedPlot({

      pp$p1<-(setup_plots('min_max_temp',sb_vars$month_1(), df_consts))

      rv$dfnew[[rv$count]] <- pp$p1
      rv$count = rv$count + 1

    }, cacheKeyExpr = {list('min_max_temp', sb_vars$month_1(), df_consts)})
    output$plot_1_mean_temp<-renderCachedPlot({

      pp$p2<-(setup_plots('mean_temp',sb_vars$month_1(), df_consts))
      rv$dfnew[[rv$count]] <- pp$p2
      rv$count = rv$count + 1

    }, cacheKeyExpr = {list('mean_temp', sb_vars$month_1(), df_consts)})
    output$plot_2_min_max_temp<-renderCachedPlot({

      pp$p3<-(setup_plots('min_max_temp',sb_vars$month_2(),df_consts))
      rv$dfnew[[rv$count]] <- pp$p3
      rv$count = rv$count + 1

    }, cacheKeyExpr = {list('min_max_temp', sb_vars$month_2(), df_consts)})
    output$plot_2_mean_temp<-renderCachedPlot({
      pp$p4<-(setup_plots('mean_temp',sb_vars$month_2(),df_consts))
      rv$dfnew[[rv$count]] <- pp$p4
      rv$count = rv$count + 1

    }, cacheKeyExpr = {list('mean_temp', sb_vars$month_2(), df_consts)})
    # print(rv$count)
  })
  
  # observeEvent(pp$p1,{
  #   validate(need(pp$p1 != '', 'missing'))
  #         print(pp$p1)
  # }
  # )
  
  # observe(print(reactiveValuesToList(pp)))
  
  return(
    list(
      plot_type = reactive({plot_type()}),
      statistic = reactive({statistic()}),
      pp = pp,
      rv = rv
    )
  )
}



