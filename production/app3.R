library(shiny)
library(shinyjs)
library(highcharter)
library(dplyr)

# Sample data
data <- data.frame(x = 1:100, y = cumsum(rnorm(100)), y2 = rnorm(100))
data2 <- readr::read_csv('data_long.csv', show_col_types = FALSE)

m <- hcmap(
  map = "custom/world",
  data = data2,
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

# Create charts
g1 <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_title(text = "Sales Over Time", style = list(color = "#FFFFFF", useHTML = T)) %>%
  hc_xAxis(categories = data$x) %>%
  hc_series(list(name = "Series 1", data = data$y))%>%
  hc_legend(
    itemStyle = list(
      color = "#ffffff"
    ),
    itemHoverStyle = list(
      color = "#cccccc"
    )
  )

g2 <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Monthly Revenue", style = list(color = "#FFFFFF", useHTML = T)) %>%
  hc_xAxis(categories = data$x) %>%
  hc_series(list(name = "Series 2", data = data$y2))%>%
  hc_legend(
    itemStyle = list(
      color = "#ffffff"
    ),
    itemHoverStyle = list(
      color = "#cccccc"
    )
  )

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      #float-container {
          display: block;
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
      
    "))
  ),
  
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
  "))
)

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