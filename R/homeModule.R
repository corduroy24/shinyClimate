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
    fluidRow(
      valueBoxOutput(ns('vbox_city')),
      valueBoxOutput(ns('vbox_prov')),
      valueBoxOutput(ns('vbox_can'))
    ),
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
    ),
      verbatimTextOutput(NS(id, "txt1")),
    fluidRow(
      box(
        title = "Question of the day",
        h3(textOutput(ns("qday")), align= 'center'),
        h3(textOutput(ns("aday")), align= 'center'),
        status = "primary",
        width  = '100%',
        solidHeader = TRUE,
        # background = "green",
        tabBox(
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1", height = "100%", width = '100%',
          tabPanel("Tab1", withSpinner(plotOutput(ns("plot")))),
          tabPanel("Tab2", textOutput(ns('text')))
        ),
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
    
    update <- reactive({
      debug(logger, paste("|TEMP_VAL|",sidebar_vars$temp_val(), '|' ))
      
      beginning <- Sys.time()
      year_to_start <- vars$year_to_start()
      month <- (strtrim(vars$month(), 3))
      
      validate(need(month!='', 'no month'),
               need(year_to_start!='', 'no year')
               )
      temp_val <- unlist(strsplit(sidebar_vars$temp_val(), " "))
      isolate({
        temp_val_1 <- strtrim(tolower(temp_val[1]),3)
        temp_val_2 <- strtrim(tolower(temp_val[2]),4)
        temp_val<-(paste(temp_val_1,temp_val_2, sep='_'))
      })

      isolate({
        main(temp_val, month, year_to_start)
      })
      end <- Sys.time()
      output$txt1 <- renderText({end - beginning})
      showElement('txt1')
    })
  
  output$qday <- renderText({
    qday <- randomQuestion()
    aday <- "yesyesyes"
    switch(qday,
           "Q1 ?" = {output$plot<-renderPlot({
             update()
             prov <- sidebar_vars$prov()
             hist_slope_prov(prov)})
             output$aday <-renderText({aday}) 
           },

           "Q2 ?"= output$plot<-renderPlot({
             update()
             boxplot_val('r.squared')
             }), 
           "Q3 ?"= output$plot<-renderPlot({
             update()
             hist_slope()
             }), 
           "Q4 ?"= output$plot<-renderPlot({
             update()
             boxplot_val('slope')
             }), 
           print('default')
    )
    qday
  })
  Sys.sleep(2)
  hideElement('loading_page')
  showElement("main_content")
  return(update)
}

#filter questions by city, province and nation - for later 
randomQuestion <- function(){
  questions <- c("Q1 ?",
    'Q2 ?',
    'Q3 ?',
    'Q4 ?'
    # 'Q5 ?',
    # 'Q6 ?',
    # 'Q7 ?',
    # 'Q8 ?',
    # 'Q9 ?',
    # 'Q10 ?',
    # 'Q11 ?',
    # 'Q12 ?',
    # 'Q13 ?',
    # 'Q14 ?',
    # 'Q15 ?',
    # 'Q16 ?',
    # 'Q17 ?',
    # 'Q18 ?'
    )

  return(sample(questions,1))
}