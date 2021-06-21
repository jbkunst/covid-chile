grafico_pacientes_uci <- function() {
  
  d <- get_data_producto_8()
  
  d <- d %>%
    group_by(x = dia) %>%
    summarise(y = sum(nro_pascientes_uci))
  
  eventos <- left_join(EVENTOS, d, by = "x")
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

grafico_confirmados_diarios <- function(){
  
  d <- get_data_producto_3() 
  
  d <- d %>%
    group_by(x = dia) %>%
    summarise(y = sum(nro_casos))
  
  d <- d %>% 
    mutate(
      y_new = lag(y),
      y_new = ifelse(is.na(y_new), y, y - y_new)
    )
  
  d %>% filter(y_new == max(y_new))
  
  d_outlier <- d %>% 
    filter(x == ymd("20200617"))
  
  d <- d %>% filter(x != ymd("20200617"))
  
  d <- d %>% 
    mutate(
      y_movavg_7 = roll_mean(y_new, n = 7, fill = NA, align = "right"),
      y_movavg_7 = round(y_movavg_7, 0)
    )
  
  eventos <- left_join(EVENTOS, d %>% select(x, y = y_new), by = "x")
  eventos <- mutate(eventos, x = datetime_to_timestamp(x))
  
  titulo <- "Contagios por Coronavirus confirmados por exámenes. La curva de contagios
  considera sólo a las personas activamente afectadas por el virus."
  
  hchart(
    d %>% select(x, y = y_new),
    hcaes(x, y),
    type = "line",
    name = "Confirmados Diarios",
    showInLegend = TRUE,
    lineWidth = 1
  ) %>% 
    hc_add_series(
      d %>% select(x, y = y_movavg_7),
      hcaes(x, y),
      type = "line",
      name = "Media Móvil 7 días",
      showInLegend = TRUE
    ) %>%
    hc_add_series(
      d_outlier %>% select(x, y = y_new),
      hcaes(x, y),
      type = "scatter",
      showInLegend = TRUE,
      visible = FALSE,
      name = "Confirmados 17 Junio 2020"
    ) %>% 
    hc_tooltip(table = TRUE, valueDecimals = 0) %>%
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

grafico_porcentaje_vacunacion_edad <- function(){
  
  d1ra <- get_data_producto_78_1ra_dosis()
  d2da <- get_data_producto_78_2da_dosis()
  duni <- get_data_producto_78_unica_dosis()
  
  dpry <- get_data_ine_proyeccion_poblacion_2021()
  
  dpry <- dpry %>% 
    categorizar_edad() %>% 
    group_by(edad_c) %>% 
    summarise(poblacion = sum(poblacion, na.rm = TRUE), .groups = "drop")
  
  d <- list(d1ra, d2da, duni) %>% 
    map(categorizar_edad) %>% 
    map(group_by, edad_c) %>% 
    map(summarise, n = sum(n, na.rm = TRUE)) %>% 
    map2_df(c("1ª dosis", "2ª dosis", "Única dosis"), ~ mutate(.x, group = .y))
  
  d <- d %>% 
    mutate(group = ifelse(group == "1ª dosis", "1ª dosis", "2ª dosis")) %>% 
    group_by(edad_c, group) %>% 
    summarise(n = sum(n, na.rm = TRUE), .groups = "drop")
  
  d <- d %>%
    left_join(dpry, by = "edad_c")
  
  d <- d %>% mutate(prop = n/poblacion)
  
  d <- d %>% 
    group_by(edad_c) %>% 
    arrange(group) %>% 
    mutate(
      # prop_stack = lag(prop) - prop,
      # prop_stack = coalesce(prop_stack, prop),
      # prop_stack = 100 * prop_stack,
      prop       = 100 * prop
      ) 
    
  d
  
  titulo <- "Porcentaje de población vacunada por grupo de edad"
  
  hchart(
    d %>% select(x = edad_c, y = prop, group = group),
    hcaes(x, y, group = group),
    type = "column"
    ) %>% 
    hc_plotOptions(
      series = list(
        showInLegend = TRUE,
        grouping = FALSE,
        dataLabels = list(
          enabled = FALSE, 
          style = list(
            color = "opposite"
            # textOutline = "2px #FFFFFFAA",
            ),
          format = "{y:.1f}%"
          )
        )
      ) %>% 
    hc_tooltip(table = TRUE, valueDecimals = 2, valueSuffix =  "{value:.0f}%") %>%
    hc_xAxis(title = list(text = "Rango Etáreo")) %>%
    hc_yAxis(title = list(text = "Porcentaje"), labels = list(format =  "{value:.0f}%")) %>% 
    hc_subtitle(text = titulo) 
  
}

grafico_porcentaje_vacunacion_edad_fecha <- function(){
  
  d1ra <- get_data_producto_78_1ra_dosis()
  duni <- get_data_producto_78_unica_dosis()
  
  dpry <- get_data_ine_proyeccion_poblacion_2021()
  
  dpry <- dpry %>% 
    categorizar_edad() %>% 
    group_by(edad_c) %>% 
    summarise(poblacion = sum(poblacion, na.rm = TRUE), .groups = "drop")
  
  d <- list(d1ra, duni) %>% 
    map_df(categorizar_edad) %>% 
    group_by(dia, edad_c) %>% 
    summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>% 
    arrange(edad_c, dia) %>% 
    group_by(edad_c) %>% 
    left_join(dpry, by = "edad_c") %>% 
    mutate(
      n_cum = cumsum(n),
      p_cum = round(100 * n_cum/poblacion, 2)
      ) %>% 
    ungroup()
    
  d
  
  titulo <- "Porcentaje de población con almenos una dosis por grupo de edad"
  
  colores <- viridisLite::cividis(7, begin = 0.1, end = 0.9) %>% 
    hex_to_rgba()
  
  hchart(
    d %>% select(x = dia, y = p_cum, group = edad_c),
    hcaes(x, y, group = group),
    type = "line",
    color = colores
  ) %>% 
    hc_tooltip(table = TRUE, valueDecimals = 2, valueSuffix =  "{value:.0f}%", sort = TRUE) %>%
    hc_xAxis(title = list(text = "Fecha")) %>%
    hc_yAxis(title = list(text = "Porcentaje"), labels = list(format =  "{value:.0f}%")) %>% 
    hc_subtitle(text = titulo) 
  
}
