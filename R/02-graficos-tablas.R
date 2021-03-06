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

grafico_porcentaje_vacunacion <- function(){
  
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
    ungroup() %>% 
    group_by(group) %>% 
    summarise(prop = weighted.mean(prop, n)) %>% 
    mutate(prop2 = lag(prop) - prop) %>% 
    mutate(prop2 = coalesce(prop2, prop))
  
  d <- d %>% 
    add_row(group = "Sin vacunar", prop2 = 1 - sum(d$prop2)) %>% 
    select(-prop)
  
  d <- d %>% 
    mutate(n = round(100 * prop2))
  
  # forzar suma 100
  diff <- 100 - sum(d$n)
  
  d <- d %>% 
    mutate(n = ifelse(row_number() == 1, n + diff, n))
  
  titulo <- "Porcentaje de población vacunada por grupo de edad"
  
  hchart(
    d,
    "item",
    hcaes(name = group, y = n),
    name = "Vacunación",
    showInLegend = TRUE,
    marker = list(symbol = "square")
  ) %>% 
    hc_colors(as.vector(unlist(c(rev(PARS$colors[1:2]), PARS$colors["gray"])))) %>% 
    hc_plotOptions(
      # avoid hide series due bug
      series = list(point = list(events = list(legendItemClick = JS("function(e) {e.preventDefault() }"))))
    ) %>% 
    hc_legend(
      labelFormat =  '{name} <span style="opacity: 0.4">{y}</span>',
      # force squere legend 
      # https://jsfiddle.net/BlackLabel/kfmnjcdp
      symbolHeight = 11,
      symbolWidth = 11,
      symbolRadius = 0
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
    hc_yAxis(title = list(text = "Porcentaje"), labels = list(format =  "{value:.0f}%"), max = 100) %>% 
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
    hc_yAxis(title = list(text = "Porcentaje"), labels = list(format =  "{value:.0f}%"), max = 100) %>% 
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
      dnormal %>% filter(anio %in% 2019:2018) %>% select(x = fecha, y = nro_fallecidos),
      "line",
      hcaes(x, y),
      name = "Fallecimientos semanales 2018-2019",
      showInLegend = TRUE,
      visible = FALSE,
      color = PARS$colors$gray
      ) %>% 
    # hc_add_series(
    #   dnormal %>% filter(anio %in% 2010:2018) %>% select(x = fecha, y = nro_fallecidos),
    #   "line",
    #   hcaes(x, y),
    #   name = "Fallecimientos semanales 2010-2018",
    #   showInLegend = TRUE,
    #   visible = FALSE,
    #   color = PARS$colors$gray
    # ) %>% 
    hc_tooltip(shared = TRUE, valueDecimals = 0) %>% 
    hc_yAxis(title = list(text = "Número de fallecidos"), min = 0, endOndTick = FALSE) %>%
    hc_xAxis(title = list(text = "Fecha")) %>% 
    hc_subtitle(text = titulo)
  
}

grafico_defunciones_mensuales_pandemia <- function(){
  
  d <- get_data_producto_10() 
   
  d <- d %>% 
    group_by(dia) %>% 
    summarise(fallecimientos = sum(fallecimientos)) %>% 
    mutate(
      fallecimientos_dia = fallecimientos - lag(fallecimientos),
      fallecimientos_dia = ifelse(is.na(fallecimientos_dia), fallecimientos, fallecimientos_dia),
      anio_mes = floor_date(dia, unit = "month")
      ) %>% 
    group_by(anio_mes) %>% 
    summarise(fallecimientos_dia = sum(fallecimientos_dia))
  
  d <- d %>% 
    mutate(
      mes_lbl = month(
        anio_mes, 
        label = TRUE, 
        # locale = "Spanish_Spain.1252"
        abbr = FALSE
        ),
      mes = month(anio_mes) - 1,
      anio = year(anio_mes)
      )
  
  meses <- d %>% pull(mes_lbl) %>% levels()
  
  titulo <- "Defunciones por año"
  
  hchart(
    d %>% select(x = mes, y = fallecimientos_dia, group = anio),
    type = "area",
    fillColor = hex_to_rgba("#CCCCCC", 0.2),
    lineWidth = 5,
    hcaes(x, y, group = group)
  ) %>% 
    hc_xAxis(title = list(text = "Mes"), categories = meses) %>% 
    
    hc_tooltip(shared = TRUE, valueDecimals = 0) %>% 
    hc_yAxis(title = list(text = "Número de fallecidos"), min = 0, endOndTick = FALSE) %>%
    hc_subtitle(text = titulo)
  
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

grafico_activos_media_movil_7_dias <- function(){
  
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
    color = PARS$colors[[2]]
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

grafico_region_linea <- function(){
  
  hc <- highchart() %>% 
    hc_chart(variable = "casos_activos", region = "") %>% 
    hc_plotOptions(
      line = list(
        cursor = "pointer",
        showInLegend = FALSE,
        lineWidth = 1,
        color = PARS$colors$gray,
        events = list(
          legendItemClick = JS("function(event) { event.preventDefault();} "),
          click = JS("function(){
                        console.log('HC');
                        console.log('region ' + this.options.id);
                        console.log('col ' + this.chart.options.chart.variable);
                        myFunction('' + this.options.id, this.chart.options.chart.variable);
                        
                     }")
          )
        )
    ) %>% 
    hc_xAxis(type = "datetime") %>% 
    hc_elementId("hc_linea_region") %>% 
    hc_yAxis(endOnTick = FALSE)
  
  d <- jsonlite::read_json("data/data_regiones.json")
  
  # d$casos_activos
  
  hc$x$hc_opts$series <- d$casos_activos
  
  hc
  
}

grafico_region_mapa <- function(){
  
  # ruta_geojson <- dir("geojson", full.names = TRUE, pattern = "\\.json") %>% 
  #   sample(size = 1)
  # gjson <- jsonlite::fromJSON(ruta_geojson)
  # gjson <- geojsonio::as.json(gjson)
  
  hcm <- highchart(type = "map") %>%
    hc_chart(map = JS("Highcharts.maps['tarapaca']")) %>% 
    hc_add_series(
      id = "region",
      region = 'tarapaca',
      name = "Casos activos",
      data = JS("data_comunas['tarapaca']['casos_activos']")
      ) %>% 
    hc_colorAxis(
      stops = color_stops(n = 100, colors = paleta_colores(30)),
      startOnTick = TRUE,
      min = 0,
      endOnTick =  FALSE
    ) %>%
    hc_plotOptions(
      map = list(
        joinBy = c("codigo_comuna", "codigo_comuna"),
        borderColor = "#D3D3D3",
        borderWidth = 0.3,
        animation = list(duration = 250),
        tooltip = list(pointFormat = "<b>{point.comuna}</b>: {point.value}"),
        dataLabels = list(
          enabled = TRUE, 
          format = "{point.comuna}",
          color =  "white",
          style = list(fontSize = "12px", textOutline = "2px gray", fontWeight = "normal")
          )
        )
      ) %>% 
    hc_legend(symbolWidth = 400, align = "center", verticalAlign = "top") %>% 
    hc_elementId("hc_mapa_region") %>% 
    hc_responsive()
  
  hcm$x$fonts <- ""
  
  hcm
  # highchart(type = "map") 
}

tabla_region <- function() {
  
  d <- get_data_consolidado_region()
  
  d <- d %>% select(-id0, -codigo_region) 
  
  d <- d %>% relocate(poblacion, .after = last_col())
  
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
    
    onClick = JS("function(rowInfo, colInfo) { myFunction(rowInfo.row.id_region, colInfo.id); }"),
    
    defaultColDef = colDef(format = colFormat(digits = 0, separators = TRUE, locales = "es-CL")),
    
    columns = list(
      id_region = colDef(
        show = FALSE
      ),
      nro_fallecidos  = colDef(
        # name = "Fallecimientos",
        header = with_tooltip("Fallecimientos", "Fallecimientos registrados desde 22 Marzo 2020"),
        align = "right",
        style = function(value) {
          bar_style(width = value / max(d$nro_fallecidos), fill = "hsl(208, 70%, 90%)", align = "right")
        }
        )
      )
    )
  
}
