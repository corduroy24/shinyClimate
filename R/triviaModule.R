
# Module UI function
triviaLayoutUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # box(plotOutput("plot1", height = 250)),
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
    
  )
}



# Module server function
triviaLayout <- function(input, output, session) {
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