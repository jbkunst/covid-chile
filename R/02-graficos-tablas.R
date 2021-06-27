grafico_pacientes_uci <- function() {
  
  d <- get_data_producto_8()
  
  d <- d %>%
    group_by(x = dia) %>%
    summarise(y = sum(nro_pacientes_uci))
  
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
    
  d <- d %>% filter(dia >= ymd(20210201))
  
  titulo <- "Porcentaje de población con almenos una dosis por grupo de edad"
  
  colores <- paleta_colores(nrow(d %>% count(edad_c)))
  
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

grafico_letalidad <- function() {
  
  dc <- get_data_producto_3()
  df <- get_data_producto_14()
  
  d <- list(dc, df) %>% 
    map(group_by, dia) %>% 
    map(summarise_if, is.numeric, sum) %>% 
    reduce(inner_join, by = "dia") %>% 
    mutate(y = fallecimientos/nro_casos)
  
  d <- d %>% 
    mutate(
      ic = map2(y, nro_casos, function(eval = 1.4410024, n = 14365){ 
        as.vector(prop.test(n*eval, n)$conf.int)
        }),
      low = map_dbl(ic, first),
      low = ifelse(low < 0, 0, low),
      high = map_dbl(ic, last),
      high = ifelse(high > 1, 1, high)
    ) %>% 
    select(-ic) %>% 
    mutate(across(.cols = c(y, low, high), .fns = ~ round(.x * 100, 2)))
  
  d
  
  texto <- str_glue("<b>Tasa de Letalidad</b>, razón entre el número de fallecidos totales registrados
      hasta la fecha, sobre el número de casos totales reportados hasta la fecha")
  
  hchart(
    d %>% select(x = dia, y = y), 
    "line",
    hcaes(x, y),
    name = "Tasa de Letalidad",
    id = "fallecidos_contagiados",
    showInLegend = TRUE
  ) %>% 
    hc_add_series(
      d %>% filter(fallecimientos > 100) %>% select(x = dia, low, high),
      type = "arearange",
      hcaes(x, low = low, high = high),
      linkedTo = "fallecidos_contagiados",
      zIndex = -3,
      showInLegend = FALSE, 
      name = "Intervalo de Confianza (99%)",
      color = PARS$colors$gray
    ) %>% 
    hc_yAxis(
      allowDecimals = TRUE,
      labels = list(format = "{value}%"),
      title = list(text = "Tasa de Letalidad"),
      min = 0
    ) %>% 
    hc_xAxis(title = list(text = "Fecha")) %>% 
    hc_tooltip(valueSuffix = " %", valueDecimals = 2, shared = TRUE) %>% 
    hc_subtitle(text =  texto) 
  
  
}

grafico_defunciones_esperadas_arima <- function(){
  
  d <- get_data_producto_32()
  
  d <- d %>% 
    group_by(nro_semana, anio) %>% 
    summarise(
      nro_fallecidos = sum(nro_fallecidos), 
      n_dias = dplyr::n_distinct(dia),
      .groups = "drop") %>% 
    arrange(-anio) %>% 
    mutate(anio = as.character(anio))
  
  d
  
  dnormal <- d %>% 
    arrange(anio, nro_semana) %>% 
    filter(nro_semana != 53) %>% 
    filter(anio <= 2019)
  
  # serie de tiempo
  fname <- "data/modelo_arima.rds"
  
  if(file.exists(fname)) {
    
    mod <- readRDS(fname)
    
  } else {
    

    x <- ts(dnormal$nro_fallecidos, frequency = 52)
    
    plot(x)
    
    mod <- forecast::auto.arima(x, trace = TRUE)  
    
    plot(mod)
    
    tsdiag(mod, 52*3)
    
    saveRDS(mod, fname)
    
  }
  
  mod
  
  dcovid <- d %>% 
    arrange(anio, nro_semana) %>% 
    filter(anio >= 2020) %>% 
    # removemos si ultima semana/fila no tiene 7 dias de defuncionaes
    filter(!(row_number() == n() & n_dias != 7)) %>% 
    # removemos semanas 53 dado que arima usa frecuencia 52
    filter(nro_semana < 53)
  
  dfct <- forecast::forecast(mod, h = nrow(dcovid), level = 95) %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    janitor::clean_names()
  
  dcovid <- bind_cols(dcovid, dfct)
  
  dcovid <- dcovid %>% 
    mutate(fecha = ymd(as.numeric(anio)*10000 + 101) + weeks(nro_semana - 1))
  
  dnormal <- dnormal %>% 
    mutate(fecha = ymd(as.numeric(anio)*10000 + 101) + weeks(nro_semana - 1))
  
  titulo <- "Exceso de mortalidad. Comparación entre las defunciones semanales
  esperadas según patrón histórico de 2010-2019 y las observadas en pademia."
  
  hchart(
    dcovid %>% select(x = fecha, y = nro_fallecidos),
    "line",
    hcaes(x, y),
    name = "Fallecimientos semanales en pandemia",
    color = PARS$color$secondary,
    showInLegend = TRUE
  ) %>% 
    hc_add_series(
      dcovid %>% select(x = fecha, y = point_forecast),
      "line",
      hcaes(x, y),
      name = "Número de fallecimientos esperados",
      id = "numero_fallecidos_esperados",
      color = PARS$color$primary,
      showInLegend = TRUE
    ) %>% 
    hc_add_series(
      dcovid %>% select(x = fecha, low = lo_95, high = hi_95),
      type = "arearange",
      hcaes(x = x, low = low, high = high),
      color = PARS$color$gray,
      linkedTo = "numero_fallecidos_esperados",
      zIndex = -3,
      showInLegend = FALSE,
      name = "Intervalo 95% confianza"
    ) %>% 
    hc_add_series(
      dnormal %>% filter(anio %in% 2019) %>% select(x = fecha, y = nro_fallecidos),
      "line",
      hcaes(x, y),
      name = "Fallecimientos semanales 2019",
      showInLegend = TRUE,
      visible = FALSE,
      color = PARS$colors$gray
      ) %>% 
    hc_add_series(
      dnormal %>% filter(anio %in% 2010:2018) %>% select(x = fecha, y = nro_fallecidos),
      "line",
      hcaes(x, y),
      name = "Fallecimientos semanales 2010-2018",
      showInLegend = TRUE,
      visible = FALSE,
      color = PARS$colors$gray
    ) %>% 
    hc_tooltip(shared = TRUE, valueDecimals = 0) %>% 
    hc_yAxis(title = list(text = "Número de fallecidos"), min = 0, endOndTick = FALSE) %>%
    hc_xAxis(title = list(text = "Fecha")) %>% 
    hc_subtitle(text = titulo)
  
}

grafico_defunciones_semanales_pandemia <- function(){
  
  d <- get_data_producto_32()
  
  d %>% count(region)
  
  d <- d %>% 
    filter(dia >= ymd(20200401)) %>% 
    mutate(group = ifelse(region == "Metropolitana de Santiago", "RM", "Regiones")) %>% 
    group_by(group, anio, nro_semana) %>% 
    summarise(
      nro_fallecidos = sum(nro_fallecidos), 
      dias = n_distinct(dia),
      .groups = "drop"
      ) %>% 
    filter(dias == 7)
  
  d <- d %>% 
    mutate(fecha_auxiliar = ymd(as.numeric(2020)*10000 + 101) + weeks(nro_semana - 1)) 
    
  dtot <- d %>% 
    group_by(anio, nro_semana) %>% 
    summarise(nro_fallecidos = sum(nro_fallecidos), .groups = "drop") %>% 
    mutate(fecha = ymd(as.numeric(2020)*10000 + 101) + weeks(nro_semana - 1))
  
  hchart(
    d %>% mutate(g = paste(anio, group)) %>% select(x = fecha_auxiliar, y = nro_fallecidos, group = g),
    type = "line",
    hcaes(x, y, group = group)
  )
  
  titulo <- "Defunciones semanales 2020 vs 2021"
  
  hchart(
    dtot %>% select(x = fecha, y = nro_fallecidos, group = anio),
    type = "line",
    hcaes(x, y, group = group),
    # name = c("Totale 2020 2021", FALSE),
    linkedTo = c(NULL, ":previous")
  ) %>% 
    hc_tooltip(shared = TRUE, valueDecimals = 0) %>% 
    hc_yAxis(title = list(text = "Número de fallecidos"), min = 0, endOndTick = FALSE) %>%
    hc_xAxis(title = list(text = "Semana")) %>% 
    hc_subtitle(text = titulo)
  # %>% 
  #   hc_add_series(
  #     d %>%
  #       mutate(group = paste(anio, group)) %>%
  #       select(x = fecha_auxiliar, y = nro_fallecidos, group),
  #     type = "line",
  #     hcaes(x, y, group = group),
  #     id = c("a", NULL, NULL, NULL),
  #     linkedTo = c(NULL, "a", "a", "a", "a")
  #   )
  
  
}

grafico_defunciones_diarias <- function(){
  
  d <- get_data_producto_14()
  
  d <- d %>%
    group_by(x = dia) %>%
    summarise(y = sum(fallecimientos))
  
  d <- d %>% 
    mutate(
      y_new = lag(y),
      y_new = ifelse(is.na(y_new), y, y - y_new)
    )
  
  d %>% filter(y_new == max(y_new))
  d %>% arrange(desc(y_new))
  
  d_outlier <- d %>%
    filter(x == ymd("2020-07-17") | x == ymd("2020-06-07"))

  d <- d %>% anti_join(d_outlier, by = "x")
  
  d <- d %>% 
    mutate(
      y_movavg_7 = roll_mean(y_new, n = 7, fill = NA, align = "right"),
      y_movavg_7 = round(y_movavg_7, 0)
    )
  
  eventos <- left_join(EVENTOS, d %>% select(x, y = y_new), by = "x")
  eventos <- mutate(eventos, x = datetime_to_timestamp(x))
  
  titulo <- str_glue("Total de fallecidos diarios reportados a nivel nacional.")
  
  hchart(
    d %>% select(x, y = y_new),
    hcaes(x, y),
    type = "line",
    name = "Fallecimientos Diarios",
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
      name = "Datos de 2020-07-17 y  2020-06-07"
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

grafico_defunciones_por_edad <- function(){
  
  d <- get_data_producto_10()
  
  colores <- paleta_colores(nrow(d %>% count(grupo_de_edad)))
  
  d <- d %>% 
    select(x = dia, y = fallecimientos, group = grupo_de_edad )
  
  hchart(
    d,
    hcaes(x, y, group = group),
    type = "area",
    color = colores,
    stacking = "normal",
    fillColor = NA
  ) %>% 
    hc_tooltip(table = TRUE, valueDecimals = 0, valueSuffix =  "{value:.0f}", sort = TRUE) %>%
    hc_xAxis(title = list(text = "Fecha")) %>%
    hc_yAxis(title = list(text = "Porcentaje"), labels = list(format =  "{value:.0f}")) %>% 
    hc_subtitle(text = titulo) 
  
}

grafico_defunciones_por_region_100k_hab <- function(){
  
  dpoblacion <- get_data_producto_1()
  
  dpoblacion <- dpoblacion %>% 
    group_by(region = Region) %>% 
    summarise(poblacion_total = sum(Poblacion, na.rm = TRUE))
  
  d <- get_data_producto_14()
  
  d <- d %>% 
    left_join(dpoblacion, by = "region") %>% 
    mutate(fallecimientos = round(100000 * fallecimientos / poblacion_total)) 
  
  d <- d %>% 
    group_by(region) %>% 
    filter(wday(dia) == 2 | row_number() == dplyr::n())
   
  d <- d %>% 
    ungroup() %>% 
    mutate(region = factor(region, levels = PARS$region_levels))
  
  d %>% count(region)
  
  hchart(
    d %>% select(x = dia, y = fallecimientos, group = region),
    hcaes(x, y, group = group),
    type = "line",
    showInLegend = FALSE,
    lineWidth = 1,
    color = PARS$colors$gray,
    id = PARS$region_ids
  ) %>% 
    hc_elementId("hc_fallecimientos_region") %>% 
    hc_yAxis(endOnTick = FALSE)

}

grafico_activos_media_movil_7_dias_por_region_100k_hab <- function(){
  
  dpoblacion <- get_data_producto_1()
  
  dpoblacion <- dpoblacion %>% 
    group_by(region = Region) %>% 
    summarise(poblacion_total = sum(Poblacion, na.rm = TRUE))
  
  d <- get_data_producto_75()
  
  d <- d %>%
    select(-casos_media_movil_7_dias) %>% 
    filter(!str_detect(region, "Total")) %>% 
    filter(!is.na(activos_media_movil_7_dias)) %>% 
    left_join(dpoblacion, by = "region") %>% 
    mutate(activos_media_movil_7_dias = round(100000 * activos_media_movil_7_dias / poblacion_total)) 
  
  d <- d %>% 
    group_by(region) %>% 
    filter(wday(dia) == 2 | row_number() == dplyr::n())
  
  d <- d %>% 
    ungroup() %>% 
    mutate(region = factor(region, levels = PARS$region_levels))
  
  d %>% count(region)
  
  hchart(
    d %>% select(x = dia, y = activos_media_movil_7_dias, group = region),
    hcaes(x, y, group = group),
    type = "line",
    showInLegend = FALSE,
    lineWidth = 1,
    color = PARS$colors$gray,
    id = PARS$region_ids
  ) %>% 
    hc_elementId("hc_activos_media_movil_7_dias_region") %>% 
    hc_yAxis(endOnTick = FALSE)
  
}

grafico_activos_media_movil_7_dias_totales <- function(){
  
  d <- get_data_producto_75()
  
  d <- d %>%
    select(-casos_media_movil_7_dias) %>% 
    filter(str_detect(region, "Total")) %>%
    filter(!is.na(activos_media_movil_7_dias))
  
  d <- d %>% 
    select(x = dia, y = activos_media_movil_7_dias)
  
  eventos <- left_join(EVENTOS, d %>% select(x, y), by = "x")
  eventos <- mutate(eventos, x = datetime_to_timestamp(x))
  
  titulo <- "Promedio móvil de los últimos 7 días de casos <b>activos</b>  en todo el país."
  
  hchart(
    d,
    hcaes(x, y),
    type = "line",
    name = "Media Móvil 7 días",
    showInLegend = TRUE,
    color = "#eb3c46"
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
 
tabla_region <- function() {
  
  d <- get_data_consolidado_region()
  
  id_num <- d %>%
    map_lgl(is.numeric) %>% 
    which(1:length(.)) %>% 
    as.vector()
  
  DT::datatable(
    d, 
    elementId = "tabla_region",
    options = list(
      searching = FALSE,
      bPaginate = FALSE,
      bInfo = FALSE,
      columnDefs = list(
        list(visible = FALSE, targets = 0:1)
      )
      # initComplete = JS(
      #   "function(settings, json) {",
      #   "$('td').css({'cursor': 'pointer'});",
      #   "$('th').css({'cursor': 'pointer'});",
      # "$(this.api().tables().header()).css({'font-family': 'Alegreya Sans SC', sans-serif'});",
      #   "$(this.api().tables().body()).css({'font-size': '0.7em'});",
      #   "}")
    ),
    rownames = FALSE,
    selection = "single",
    extensions = "Responsive",
    callback =   JS("table.on('mouseover', 'td', function() {
      

      
      
      });"),
  ) 
  
  
  
  
}

tabla_region2 <- function() {
  
  d <- get_data_consolidado_region()
  
  d <- d %>% 
    select(-id0, -id_region) %>% 
    
    mutate(dosis_completa_pct = round(100 * (dosis_segunda + dosis_unica) / poblacion_total, 1)) %>% 
    select(-dosis_segunda, -dosis_unica, -dosis_primera) %>% 
    
    select(1:4, dosis_completa_pct) %>% 
    
    filter(TRUE)
  
  # Render a bar chart with a label on the left
  bar_chart <- function(label, width = "100%", height = "16px", fill = "#00bfc4", background = NULL) {
    
    bar <- div(style = list(background = fill, width = width, height = height))
    chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
    div(style = list(display = "flex", alignItems = "center"), label, chart)
    
  }
  bar_style <- function(width = 1, fill = "#e6e6e6", height = "75%", align = c("left", "right"), color = NULL) {
    align <- match.arg(align)
    if (align == "left") {
      position <- paste0(width * 100, "%")
      image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
    } else {
      position <- paste0(100 - width * 100, "%")
      image <- sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, fill)
    }
    list(
      backgroundImage = image,
      backgroundSize = paste("100%", height),
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center",
      color = color
    )
  }
  
  with_tooltip <- function(value, tooltip) {
    tags$abbr(style = "text-decoration: underline; text-decoration-color: #C0C0C0;text-decoration-style: dotted; cursor: help",
              title = tooltip, value)
  }
  
  reactable::reactable(
    d,
    pagination = FALSE, 
    sortable = TRUE, 
    highlight = TRUE,
    
    onClick = JS("function(rowInfo, colInfo) { myFunction(PARS.region_ids[rowInfo.index]);}"),
    
    columns = list(
      fallecimientos = colDef(
        # name = "Fallecimientos",
        header = with_tooltip("Fallecimientos", "Fallecimientos registrados desde 22 Marzo 2020"),
        align = "right",
        style = function(value) {
          bar_style(width = value / max(d$fallecimientos), fill = "hsl(208, 70%, 90%)", align = "right")
        },
        format = colFormat(digits = 0, separators = TRUE, locales = "es-CL")
        
      ),
      dosis_completa_pct = colDef(
        name = "% Dosis completa",
        align = "left",
        cell = function(value) {
          width <- paste0(value / 100 * 100, "%")
          bar_chart(value, width = width, fill = "#fc5185", background = "#e1e1e1")
          }
        )
      )
    )
  
}
