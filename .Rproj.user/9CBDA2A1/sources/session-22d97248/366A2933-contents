library(shiny)
library(shinyjs)
library(highcharter)
library(dplyr)

# Sample data
data <- readr::read_csv('data_long.csv', show_col_types = FALSE)
data <- data %>%
  group_by(country_code) %>%
  mutate(diff = Internet_usage - lag(Internet_usage)) %>%
  ungroup()



source("R/aux_functions.R")


ui <- fluidPage(
  useShinyjs(),

    # import font-style 
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Boogaloo&family=Cabin:ital,wght@0,400..700;1,400..700&family=Mina:wght@400;700&family=Oswald:wght@200..700&family=Roboto:ital,wght@0,100;0,300;0,400;0,500;0,700;0,900;1,100;1,300;1,400;1,500;1,700;1,900&display=swap",
      rel = "stylesheet"
    )),
  tags$link(rel = "stylesheet", type= "text/css", href = "style.css"),
  tags$script(src = "script.js"),

  # title and subtitles
  
  h1("Global Internet Usage Trends (2000 - 2023)"),
  h2("Exploring the Growth of Internet Adoption Across the World Over the Last Two Decades"),
  
  # map 
  div(id = "map-container", highchartOutput("g1_output", height = '700px'),
      sliderInput("year","year",2000,2023,value = 2000,step = 1)),
  
  # Floating container
  div(id = "float-container",
      h2("Detailed Country Analysis"),
      div(id = "map-wrapper",
          highchartOutput("g3_output"),
          highchartOutput("g4_output")
          ),
      actionButton('close_panel',"", icon = icon("window-close"))
  )
)

server <- function(input, output, session) {
  

  output$g1_output <- renderHighchart({
    
    hcmap(
      map = "custom/world",
      data = data %>% filter(Year == input$year),
      joinBy = c("iso-a3", "country_code"),
      name = "Country",
      value = "Internet_usage",
      tooltip = list(pointFormat = "{point.name}"),
      dataLabels = list(enabled = TRUE, format = "{point.country}")
    ) |>
      hc_chart(height = '700px') |>
      hc_plotOptions(
        series = list(
          point = list(
            events = list(
              click = JS("function() {
            console.log('Clicked country:', this.name);
            console.log('Clicked country code:', this.country_code);
            Shiny.setInputValue('selected_country', this.name, {priority: 'event'});
            Shiny.setInputValue('country_code', this.country_code, {priority: 'event'});
            
          }")
            )
          )
        )
      )
    
    
  })

  
  # Floating charts with reactive rendering
  output$g3_output <- renderHighchart({
    req(input$country_code)
    
    serie(data,input$country_code, input$selected_country)
    
  })
  
  output$g4_output <- renderHighchart({
    req(input$country_code)
    
    serie2(data,input$country_code, input$selected_country)
  })
  
  # Show container and trigger chart rerender
  observeEvent(input$selected_country, {
    if (input$selected_country != "") {
      shinyjs::show("float-container") # Force Highcharts to reflow after showing container
      delay(100, {
        runjs("
          if (window.Highcharts) {
            var charts = Highcharts.charts;
            charts.forEach(function(chart) {
              if (chart) {
                chart.reflow();
              }
            });
          }
        ")
      })
    }
  })
  
  
  observe({
    
    print(input$selected_country)
    print(input$country_code)
    
  })
  # Hide container
  observeEvent(input$close_panel, {
    shinyjs::hide("float-container")
    
    
  })
  



}

shinyApp(ui, server)