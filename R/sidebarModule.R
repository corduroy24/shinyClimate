# sidebarInputsModule
# Module UI function
sidebarLayoutUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    menuItem("Preferences", tabName = "pref", icon = icon("cog"),
             startExpanded = TRUE,
             selectInput(ns("region"), "Choose a Region:",
                         choices = c('City', 'Province', 'Canada')
             ),
             selectInput(ns("prov"), "Choose a province:",
                         choices = c()
             ),
             selectInput(ns("city"), "Choose a city:",
                         choices = c()
             ),
             
             menuItem("Other",
                      selectInput(ns("month_1"), "Choose a month (L) :",
                                  choices = c("January", "February", "March","April","May","June","July","August","September","October","November", "December"),
                                  selected = 'January'
                      ),
                      selectInput(ns("month_2"), "Choose a month (R):",
                                  choices = c("January", "February", "March","April","May","June","July","August","September","October","November", "December"),
                                  selected = 'July'
                      ),
                      selectInput(ns("year_to_start"), "Start Year",
                                  choices = seq(1840, 1986), 
                                  selected = 1960
                      ),
                      div(style="text-align: center;",
                          radioButtons(ns('show_city_lab'), 'City Label',
                                   choices = c('Enable','Disable')))
             )
      ),
    
    br(),
    downloadUI(ns("inner_dl"))
    
    )
}

# Module server function
sidebarLayout <- function(input, output, session, plot_vars) {
  prov_vector <- c("ON","AB","BC","YT","NT","NU","SK", "MB", "QC", "NB", "NS", "PE", "NL")
  
  observeEvent(input$region,{
    if(input$region == 'City'){
      showElement('city')
      showElement('prov')
    }
    else if(input$region == 'Province'){
      showElement('prov')
      hideElement('city')
    }
      
    else if(input$region == 'Canada'){
      hideElement('city')
      hideElement('prov')
    }
  })
  
  # change to renderUI??
  observe({
    prov_vector_sorted <- sort(prov_vector)
    updateSelectInput(session, "prov", "Choose a province",
                      choices = prov_vector_sorted,
                      selected = 'MB')
  }, priority = 200)
  
  observeEvent(input$prov,{
    city_vector<- get_city_vector(input$prov)
    city<- NULL
    if(input$prov == 'MB') city <- 'FLIN FLON'
    
    updateSelectInput(session, "city", "Choose a city",
                      choices = city_vector,
                      selected = city)
  })

  callModule(download, 'inner_dl', plots = plot_vars$pp, rv = plot_vars$rv)
  
  return(
    list(
      region = reactive({input$region}),
      prov = reactive({input$prov}),
      city = reactive({input$city}),
      month_1 = reactive({strtrim(input$month_1, 3)}),
      month_2 = reactive({strtrim(input$month_2, 3)}),
      year_to_start = reactive({input$year_to_start}),
      show_city_lab = reactive({input$show_city_lab})
    )
  )
}