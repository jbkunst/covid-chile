grafico_pacientes_uci <- function() {
  
  d <- get_data_producto_8()
  
  d <- d %>%
    group_by(x = dia) %>%
    summarise(y = sum(nro_pascientes_uci))
  
  # from https://jkunst.com/blog/posts/2020-06-02-30diasdegraficos-parte-3/#d%C3%ADa-21-gr%C3%A1ficos-con-anotaciones
  eventos <- tibble(
    x = ymd(c("2020-05-15", "2021-05-9")),
    text = c("Primera cuarentena<br>en la RM", "Día de la madre")
    ) 
  
  eventos <- left_join(eventos, d, by = "x")
  eventos <- mutate(eventos, x = datetime_to_timestamp(x))
  
  titulo <- "Número diario de <b>pacientes</b> en <b>UCI</b> a nivel nacional."
  
  hchart(
    d,
    hcaes(x, y),
    type = "area",
    name = "Pacientes UCI",
    showInLegend = TRUE
    ) %>%
    hc_tooltip(table = TRUE, valueDecimals = 0) %>%
    # hc_xAxis(plotLines = list_parse(data_plotLine)) %>%
    hc_yAxis(title = list(text = "Cantidad")) %>%
    hc_xAxis(title = list(text = "Fecha")) %>%
    hc_subtitle(text = titulo) %>% 
    hc_annotations(
      list(
        labelOptions = list(
          shape = "connector",
          align = "right",
          justify = FALSE,
          crop = TRUE,
          style = list(fontSize = "0.8em", textOutline = "1px white")
        ),
        labels = df_to_annotations_labels(eventos)
        )
      )
  
}