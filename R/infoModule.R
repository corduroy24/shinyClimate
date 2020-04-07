
# Module UI function
infoUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
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
    
  )
}



# Module server function
info <- function(input, output, session, vars) {
  
  output$plot_des_1<- renderUI({
    if(vars$plot_type() == 'histogram')
      para_1<- "Histogram Description here"
    else if(vars$plot_type() == 'boxplot')
      para_1<- "Boxplot Description here"
    
    para_1 <- p(para_1, style = 'margin:0;display:inline;')
    
    div(
      title_1 <- p(str_to_title(vars$plot_type()), style = 'font-weight:bold; font-size:18px;margin:0;padding:0;display:inline'),
      para_1
    )
  })
    
  output$plot_des_2<- renderUI({
    if(vars$statistic() == 'Slopes')
      para_2<- "Slopes Description here"
    else if(vars$statistic() == 'CI_lower for Slopes')
      para_2<- "CI Description here"
    else if(vars$statistic() == 'CI_upper for Slopes')
      para_2<- "CI Description here"
    else if(vars$statistic() == 'R-squared for Slopes')
      para_2<- "R-squared Description here"
    
    para_2 <- p(para_2, style = 'margin:0;display:inline;')
    
    div(
      title_2 <- p(str_to_title(vars$statistic()), style = 'font-weight:bold; font-size:18px;margin:0;display:inline'),
      para_2
    )
  })
    
}



