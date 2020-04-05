# sidebarInputsModule
# Module UI function
sidebarLayoutUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    selectInput(ns("prov"), "Choose a province:",
                choices = c()
    ),
    selectInput(ns("city"), "Choose a city:",
                choices = c()
    ),
    # selectInput(ns("temp_val"), "Choose a Measuremnent:",
    #             choices = c("Average Temperature","Minimum Temperature", "Maximum Temperature"),
    #             selected = 'Average Temperature'
    # ),
    # verbatimTextOutput(NS(id, "txt1"))
  )
}

# Module server function
sidebarLayout <- function(input, output, session) {
  prov_vector <- c("ON","AB","BC","YT","NT","NU","SK", "MB", "QC", "NB", "NS", "PE", "NL")
  
  observe({
    prov_vector_sorted <- sort(prov_vector)
    updateSelectInput(session, "prov", "Choose a province",
                      choices = prov_vector_sorted,
                      selected = 'ON')
  }, priority = 200)
  
  observeEvent(input$prov,{
    city_vector<- get_city_vector(input$prov)
    updateSelectInput(session, "city", "Choose a city",
                      choices = city_vector,
                      selected = 'TORONTO')
  })
  
  return(
    list(
      prov = reactive({input$prov}),
      city = reactive({input$city})
      # temp_val = reactive({input$temp_val})
    )
  )
}