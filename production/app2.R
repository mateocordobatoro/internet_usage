library(shiny)
library(highcharter)
library(dplyr)
library(bslib)  # Para mejorar el diseño con Bootstrap moderno

# Cargar datos
data <- readr::read_csv('data_long.csv', show_col_types = FALSE)

# Crear mapa interactivo
m <- hcmap(
  map = "custom/world",
  data = data %>% filter(Year == min(unique(data$Year))),
  joinBy = c("iso-a3", "country_code"),
  name = "Country",
  value = "Internet_usage",
  tooltip = list(pointFormat = "{point.name}"),
  dataLabels = list(enabled = TRUE, format = "{point.country}")
) |>
  hc_plotOptions(
    series = list(
      point = list(
        events = list(
          click = JS("function() {
            console.log('Clicked country:', this.name);
            Shiny.setInputValue('map_click', this.name, {priority: 'event'});
          }")
        )
      )
    )
  )

# UI con diseño moderno
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),  # Tema moderno con Bootstrap
  tags$head(
    tags$style(HTML("
      
      #title-container {
        text-align: center;
        font-size: 28px;
        font-weight: bold;
        color: #2e7d32; /* Verde oscuro */
        margin-bottom: 20px;
      }
      
      #map-container {
        padding: 20px;
        border-radius: 15px;
        box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.2);
        text-align: center;
        width: 80%;
        margin: 0 auto;
      }
      
      .map-wrapper {
        border-radius: 15px;
        overflow: hidden;
      }
      
      .map-wrapper > div {
        height: 700px !important;
      }
      
      
            #float-container {
          display: none;
          position: relative;
          left: 15%;
          width: 74%;
          padding: 20px;
          background: rgba(0, 0, 0, 0.7);
          color: white;
          text-align: center;
          border-radius: 10px;
          z-index: 1000;
          margin-top: 16px;
      }
      .chart-container {
        margin-bottom: 20px;
      }
      
    ")),
    
    # Google Fonts
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;700&display=swap", rel = "stylesheet")
  ),
  
  # Contenedor del título
  div(id = "title-container", "Tracking Global Internet Adoption (2000-2023)"),
  
  # Contenedor del mapa con estilos modernos
  div(id = "map-container",
      div(class = "map-wrapper",
          highchartOutput('map', height = '700px')
      )
  ),
  # Floating container
  div(id = "float-container",
      h2("Detailed Country Analysis"),
      div(class = "chart-container", highchartOutput("g3_output")),
      div(class = "chart-container", highchartOutput("g4_output"))
  ),
  
  # Click outside handler
  tags$script(HTML("
    document.addEventListener('click', function(event) {
      var container = document.getElementById('float-container');
      var selectElement = document.getElementById('selected_country');
      if (!container.contains(event.target) && !selectElement.contains(event.target)) {
        Shiny.setInputValue('close_container', true, {priority: 'event'});
      }
    });
  ")),
  
  tags$script(HTML("
    
  "))
)

# Servidor
server <- function(input, output, session) {
  output$map <- renderHighchart({
    m |> hc_chart(height = '700px')
  })
  
  selected_country <- reactive({ input$map_click })
  
  observeEvent(selected_country(), {
    print(selected_country())
  })
  
  
  
  # Floating charts with reactive rendering
  output$g3_output <- renderHighchart({
    req(input$selected_country != "")
    g1 %>%
      hc_title(text = paste("Sales Over Time -", input$selected_country))
  })
  
  output$g4_output <- renderHighchart({
    req(input$selected_country != "")
    g2 %>%
      hc_title(text = paste("Monthly Revenue -", input$selected_country))
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
  
  # Hide container
  observeEvent(input$close_container, {
    shinyjs::hide("float-container")
  })
}

shinyApp(ui, server)
