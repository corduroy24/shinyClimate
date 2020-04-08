# sidebarInputsModule
# Module UI function
sidebarLayoutUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    menuItem("Preferences", tabName = "pref", icon = icon("cog"),
             startExpanded = TRUE,
             selectInput(ns("prov"), "Choose a province:",
                         choices = c()
             ),
             selectInput(ns("city"), "Choose a city:",
                         choices = c()
             ),
             
             menuItem("Other",
                      selectInput(ns("month_1"), "Choose a month (L) :",
                                  choices = c("January", "February", "March","April","May","June","July","August","September","October","November", "December"),
                                  selected = 'February'
                      ),
                      selectInput(ns("month_2"), "Choose a month (R):",
                                  choices = c("January", "February", "March","April","May","June","July","August","September","October","November", "December"),
                                  selected = 'July'
                      ),
                      selectInput(ns("year_to_start"), "Start Year",
                                  choices = seq(1840, 1986), 
                                  selected = 1980
                      )
             )
      ),
    
    br(),
    downloadUI(ns("inner_dl"))
    
    )
}

# Module server function
sidebarLayout <- function(input, output, session, vars_plot) {
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

  callModule(download, 'inner_dl', plots = vars_plot$pp)
  
  return(
    list(
      prov = reactive({input$prov}),
      city = reactive({input$city}),
      month_1 = reactive({strtrim(input$month_1, 3)}),
      month_2 = reactive({strtrim(input$month_2, 3)}),
      year_to_start = reactive({input$year_to_start})
    )
  )
}