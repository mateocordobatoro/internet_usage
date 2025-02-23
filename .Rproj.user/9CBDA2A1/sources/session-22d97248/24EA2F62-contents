serie <- function(data, country, country_name){
  
  
  df <- data %>% filter(country_code == country)
  
  g1 <- highchart() %>%
    hc_chart(type = "line", backgroundColor = "rgba(255, 255, 255, 0.5)") %>%  # Fondo semi-transparente
    hc_title(
      text = "Percentage of Total Population Using the Internet",
      align = "left",
      style = list(color = "#1F4E79", fontSize = "18px", fontWeight = "bold")
    ) %>%
    hc_subtitle(
      text = country_name,
      align = "left",
      style = list(color = "#4A90E2", fontSize = "14px")
    ) %>%
    hc_xAxis(
      categories = unique(df$Year),
      title = list(text = "Year", style = list(color = "#1F4E79", fontSize = "14px"))
    ) %>%
    hc_yAxis(
      title = list(text = "% of Population Using Internet", style = list(color = "#1F4E79", fontSize = "14px")),
      labels = list(style = list(color = "#1F4E79"))
    ) %>%
    hc_series(
      list(
        name = "Internet Usage",
        data = aggregate(Internet_usage ~ Year, df, mean)$Internet_usage,
        color = "#DD6E42" # Rojo para la serie
      )
    ) %>%
    hc_legend(
      itemStyle = list(color = "#DD6E42", fontSize = "12px"),
      itemHoverStyle = list(color = "#cccccc")
    ) %>%
    hc_exporting(enabled = TRUE)  # Permite exportar el gráfico
  
  
  
  return(g1)
  
  
}

serie2 <- function(data, country, country_name){
  
  
  df <- data %>% filter(country_code == country)
  
  g1 <- highchart() %>%
    hc_chart(type = "line", backgroundColor = "rgba(255, 255, 255, 0.5)") %>%  # Fondo semi-transparente
    hc_title(
      text = "Growth Rate of Internet Usage Over Time",
      style = list(color = "#1F4E79", fontSize = "18px", fontWeight = "bold")
    ) %>%
    hc_subtitle(
      text = country_name,
      style = list(color = "#4A90E2", fontSize = "14px")
    ) %>%
    hc_xAxis(
      categories = unique(df$Year),
      title = list(text = "Year", style = list(color = "#1F4E79", fontSize = "14px"))
    ) %>%
    hc_yAxis(
      title = list(text = "Annual Change in % of Population Using Internet", style = list(color = "#1F4E79", fontSize = "14px")),
      labels = list(style = list(color = "#1F4E79"))
    ) %>%
    hc_series(
      list(
        name = "Internet Usage",
        data = aggregate(diff ~ Year, df, mean)$diff,
        color = "#DD6E42" # Rojo para la serie
      )
    ) %>%
    hc_legend(
      itemStyle = list(color = "#DD6E42", fontSize = "12px"),
      itemHoverStyle = list(color = "#cccccc")
    ) %>%
    hc_exporting(enabled = TRUE)  # Permite exportar el gráfico
  
  
  
  return(g1)
  
  
}


