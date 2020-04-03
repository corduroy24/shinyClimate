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
    # fluidRow(
    #   valueBoxOutput(ns('vbox_city')),
    #   valueBoxOutput(ns('vbox_prov')),
    #   valueBoxOutput(ns('vbox_can'))
    # ),

      # verbatimTextOutput(NS(id, "txt1")),
    fluidRow(
      tabBox(
        # height = 400,
        # textOutput(ns("month_1")),
        id = "tabset1", height = "100%",
        tabPanel(title = "Min",plotOutput(ns("plot_1_min_temp"), height = 300)),
        tabPanel(title = 'Max',plotOutput(ns("plot_1_max_temp"), height = 300)),
        tabPanel(title = 'Mean', plotOutput(ns("plot_1_mean_temp"), height = 300))
        # background = 
      ),
      tabBox(
        # height = 400,
        # textOutput(ns("month_1")),
        id = "tabset2", height = "100%",
        tabPanel(title = "Min",plotOutput(ns("plot_2_min_temp"), height = 300)),
        tabPanel(title = 'Max',plotOutput(ns("plot_2_max_temp"), height = 300)),
        tabPanel(title = 'Mean', plotOutput(ns("plot_2_mean_temp"), height = 300))
        # background = 
      )
    ),
    # fluidRow(
    #   box(
    #     title = "Main",
    #     # h2("These 2 months showcase the strongest evidence for climate change"),
    #     # h3("Refer to preferences to change"), 
    #     status = "primary",
    #     solidHeader = TRUE,
    #     # background = "green",
    #     selectInput(ns("plotOptions"), "Choose a Plot:",
    #                 choices = c("Histogram of slopes - Provincial",
    #                             "Boxplot of slopes - Provincial", 
    #                             "Histogram of Confidence Intervals for slopes - National",
    #                             "Histogram of slopes - National", 
    #                             "Histogram of Confidence Intervals for slopes - National"),
    #                 
    #     ) 
    #   ),
    # ),
    fluidRow(
      infoBox(
        title = "Minimum - [purpose]",
        icon = shiny::icon('info-circle', class = NULL, lib = "font-awesome"),
        color = 'red',
        fill = TRUE
      ),
      infoBox(
        title = "Maximum - [purpose]",
        icon = shiny::icon('info-circle', class = NULL, lib = "font-awesome"),
        color = 'red',
        fill = TRUE
      ),
    )
    
  )
}


# Module server function
homeLayout <- function(input, output, session, vars, sidebar_vars) {
  ns <- session$ns
  
    output$vbox_city <- renderValueBox({
      valueBox(
        tags$p("temp", style = "font-size:30px; font-weight: bold;"),
        icon = icon(NULL),
        subtitle =tags$p(sidebar_vars$city(), style = "font-size: 20px"),
        color = 'aqua'
      )
    })
    output$vbox_prov <- renderValueBox({
      valueBox(
        tags$p("temp", style = "font-size:30px; font-weight: bold;"),
        icon = icon(NULL),
        subtitle =tags$p(sidebar_vars$prov(), style = "font-size: 20px"),
        color = 'aqua'
      )
    })
    output$vbox_can <- renderValueBox({
      valueBox(
        tags$p("temp", style = "font-size:30px; font-weight: bold;"),
        icon = icon(NULL),
        subtitle =tags$p("CANADA", style = "font-size: 20px"),
        color = 'aqua'
      )
    })

    # think about using reactiveValues instead of returning dataframe
    update <- reactive({
      # debug(logger, paste("|TEMP_VAL|",sidebar_vars$temp_val(), '|' ))
      
      # beginning <- Sys.time()
      year_to_start <- vars$year_to_start()
      month_1 <- (strtrim(vars$month_1(), 3))
      month_2 <- (strtrim(vars$month_2(), 3))
      # print(month_1)
      # debug(logger, paste("|MONTH_1|", month_1, "|"))
      
      validate(need(month_1!='', 'missing month'),
               need(month_2!='', 'missing month'),
               need(year_to_start!='', 'missing year')
               )
      data.frame(year_to_start, month_1, month_2)
      # temp_val <- unlist(strsplit(sidebar_vars$temp_val(), " "))
      # isolate({
      #   temp_val_1 <- strtrim(tolower(temp_val[1]),3)
      #   temp_val_2 <- strtrim(tolower(temp_val[2]),4)
      #   temp_val<-(paste(temp_val_1,temp_val_2, sep='_'))
      # })

      # end <- Sys.time()
      # output$txt1 <- renderText({end - beginning})
      # showElement('txt1')
    })
    
      
    output$plot_1_min_temp<-renderPlot({
      isolate({
        main('min_temp',update()$month_1, update()$year_to_start)
      })
      hist_slope()
    })
      
    output$plot_1_max_temp<-renderPlot({
      isolate({
        main('max_temp',update()$month_1 , update()$year_to_start)
      })
      hist_slope()
    })
    output$plot_1_mean_temp<-renderPlot({
      update()
      isolate({
        main('ave_temp',update()$month_1 , update()$year_to_start)
      })
      hist_slope()
    })
    
      
    output$plot_2_min_temp<-renderPlot({
      isolate({
        main('min_temp',update()$month_2 , update()$year_to_start)
      })
      hist_slope()
    })
    
    output$plot_2_max_temp<-renderPlot({
      isolate({
        main('max_temp',update()$month_2 , update()$year_to_start)
      })
      hist_slope()
    })
    output$plot_2_mean_temp<-renderPlot({
      isolate({
        main('ave_temp',update()$month_2 , update()$year_to_start)
      })
      hist_slope()
    })
    


    
    # Sys.sleep(2)
    # hideElement('loading_page')
    # showElement("main_content")
    
    return(update)
  # output$qday <- renderText({
  #   qday <- randomQuestion()
  #   aday <- "yesyesyes"
  #   switch(qday,
  #          "Q1 ?" = {output$plot<-renderPlot({
  #            update()
  #            prov <- sidebar_vars$prov()
  #            hist_slope_prov(prov)})
  #            output$aday <-renderText({aday})
  #          },
  # 
  #          "Q2 ?"= output$plot<-renderPlot({
  #            update()
  #            boxplot_val('r.squared')
  #            }),
  #          "Q3 ?"= output$plot<-renderPlot({
  #            update()
  #            hist_slope()
  #            }),
  #          "Q4 ?"= output$plot<-renderPlot({
  #            update()
  #            boxplot_val('slope')
  #            }),
  #          print('default')
  #   )
  #   qday
  # })
}

# #filter questions by city, province and nation - for later 
# randomQuestion <- function(){
#   questions <- c("Q1 ?",
#     'Q2 ?',
#     # 'Q3 ?',
#     'Q4 ?'
#     # 'Q5 ?',
#     # 'Q6 ?',
#     # 'Q7 ?',
#     # 'Q8 ?',
#     # 'Q9 ?',
#     # 'Q10 ?',
#     # 'Q11 ?',
#     # 'Q12 ?',
#     # 'Q13 ?',
#     # 'Q14 ?',
#     # 'Q15 ?',
#     # 'Q16 ?',
#     # 'Q17 ?',
#     # 'Q18 ?'
#     )
# 
#   return(sample(questions,1))
# }