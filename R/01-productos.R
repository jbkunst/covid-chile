get_data_producto_1 <- function(){
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto1/Covid-19.csv",
    col_types =  cols(
      .default = col_double(),
      Region = col_character(),
      `Codigo region` = col_character(),
      Comuna = col_character(),
      `Codigo comuna` = col_character()
    )
  )

  d
  
}

get_data_producto_3 <- function(){

  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo.csv",
    col_types = cols(
      .default = col_double(),
      Region = col_character()
    )
  )
  
  d %>% count(Region)
  
  d <- d %>% 
    filter(Region != "Total") %>%
    gather(dia, nro_casos, -Region) %>% 
    mutate(dia = ymd(dia)) %>% 
    janitor::clean_names() 
  
  d
  
}

get_data_producto_5 <- function(){
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales_T.csv",
    col_types = cols(
      Fecha = col_date(),
      .default = col_double()
    )
  )
  
  d <- d %>% 
    janitor::clean_names() 
  
  d
  
}

get_data_producto_8 <- function(){
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto8/UCI_T.csv",
    col_types = cols(
      Region = col_character(),
      `Arica y Parinacota` = col_double(),
      `Tarapacá` = col_character(),
      Antofagasta = col_character(),
      Atacama = col_character(),
      Coquimbo = col_character(),
      Valparaíso = col_character(),
      Metropolitana = col_double(),
      `O’Higgins` = col_character(),
      Maule = col_character(),
      Ñuble = col_double(),
      Biobío = col_character(),
      Araucanía = col_character(),
      `Los Ríos` = col_double(),
      `Los Lagos` = col_double(),
      Aysén = col_double(),
      Magallanes = col_double()
    )
  )
  
  d
  
  d <- d %>% 
    filter(!Region %in% c("Codigo region", "Poblacion")) %>% 
    gather(ciudad, valor, -Region) %>% 
    janitor::clean_names() %>% 
    rename(dia = region, region = ciudad, nro_pacientes_uci = valor) %>% 
    mutate(
      dia = ymd(dia),
      nro_pacientes_uci = as.numeric(nro_pacientes_uci)
    )
  
  d
  
}

get_data_producto_10 <- function(){
  
  d <- read_csv(
    "https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto10/FallecidosEtario.csv",
    col_types = cols(
      .default = col_double(),
      `Grupo de edad` = col_character()
    )
  )
  
  d
  
  d <- d %>% 
    gather(dia, valor, -`Grupo de edad`) %>% 
    janitor::clean_names() %>% 
    rename(fallecimientos = valor) %>% 
    mutate(
      dia = ymd(dia),
      fallecimientos = as.numeric(fallecimientos)
    )
  
  d
  
}

get_data_producto_14 <- function(){
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto14/FallecidosCumulativo.csv",
    col_types = cols(
      .default = col_double(),
      Region = col_character()
      )
    )
  
  d
  
  d <- d %>% 
    filter(!Region %in% c("Total")) %>% 
    gather(ciudad, valor, -Region) %>% 
    janitor::clean_names() %>% 
    rename(dia = ciudad, fallecimientos = valor) %>% 
    mutate(
      dia = ymd(dia),
      fallecimientos = as.numeric(fallecimientos)
    )
  
  d
  
}

get_data_producto_19 <- function(){
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto19/CasosActivosPorComuna.csv",
    col_types = cols(
      Region = col_character(),
      Comuna = col_character(),
      .default = col_double()
    )
  )
  
  d %>% distinct(Comuna) 
  
  d <- d %>% 
    filter(!Comuna %in% c("Total")) %>% 
    filter(!str_detect(Comuna, "Desconocido")) %>% 
    select(-Poblacion, -`Codigo region`, -`Codigo comuna`) %>% 
    gather(fecha, activos,-Region, -Comuna) %>% 
    janitor::clean_names() %>% 
    mutate(
      fecha = ymd(fecha),
      activos = as.numeric(activos)
    )
  
  d
  
}

get_data_producto_25 <- function(){
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto25/CasosActualesPorComuna.csv",
    col_types = cols(
      Region = col_character(),
      Comuna = col_character(),
      .default = col_double()
    )
  )
  
  d <- d %>% 
    filter(!Comuna %in% c("Total")) %>% 
    filter(!str_detect(Comuna, "Desconocido")) %>% 
    gather(dia, casos_activos,-Region, -Comuna, -Poblacion, -`Codigo region`, -`Codigo comuna`) %>% 
    janitor::clean_names() %>% 
    mutate(
      dia = ymd(dia),
      casos_activos = as.numeric(casos_activos)
    )
  
  d
  
}

get_data_producto_32 <- function(){
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto32/Defunciones.csv",
    col_types = cols(
      .default = col_double(),
      Region = col_character(),
      Comuna = col_character()
    )
  )
  
  dim(d)
  
  d <- d %>% 
    gather(dia, nro_fallecidos, -Region, -`Codigo region`, -Comuna, -`Codigo comuna`) %>% 
    mutate(
      dia = ymd(dia),
      nro_semana = week(dia),
      anio = year(dia)
      ) %>% 
    janitor::clean_names()
  
  d
  
}

get_data_producto_38 <- function(){
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto38/CasosFallecidosPorComuna.csv",
    col_types = cols(
      Region = col_character(),
      Comuna = col_character(),
      .default = col_double()
    )
  )
  
  d <- d %>% 
    filter(!Comuna %in% c("Total")) %>% 
    filter(!str_detect(Comuna, "Desconocido")) %>% 
    gather(dia, nro_fallecidos,-Region, -Comuna, -Poblacion, -`Codigo region`, -`Codigo comuna`) %>% 
    janitor::clean_names() %>% 
    mutate(
      dia = ymd(dia),
      nro_fallecidos = as.numeric(nro_fallecidos)
    )
  
  d
  
}

get_data_producto_65 <- function(){
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto65/PositividadPorComuna.csv",
    col_types = cols(
      .default = col_double(),
      Region = col_character(),
      Comuna = col_character()
    )
  )
  
  d
   
  d <- d %>% 
    gather(dia, positividad, -Region, -`Codigo region`, -Comuna, -`Codigo comuna`, -Poblacion) %>% 
    mutate(
      dia = ymd(dia)
    ) %>% 
    group_by(Region, dia) %>% 
    summarise(positividad = weighted.mean(positividad, Poblacion, na.rm = TRUE)) %>% 
    janitor::clean_names() 
  
  d 
}

get_data_producto_65_comuna <- function(){
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto65/PositividadPorComuna.csv",
    col_types = cols(
      .default = col_double(),
      Region = col_character(),
      Comuna = col_character()
    )
  )
  
  d
  
  d <- d %>% 
    gather(dia, positividad, -Region, -`Codigo region`, -Comuna, -`Codigo comuna`, -Poblacion) %>% 
    mutate(
      dia = ymd(dia)
    ) %>% 
    janitor::clean_names() 
  
  d 
}

get_data_producto_75 <- function(){
  
  d1 <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto75/MediaMovil_casos_activos.csv",
    col_types = cols(
      Region = col_character(),
      .default = col_double()
    )
  )
  
  d2 <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto75/MediaMovil_casos_nuevos.csv",
    col_types = cols(
      Region = col_character(),
      .default = col_double()
    )
  )
  
  d1 <- d1 %>% 
    gather(dia, activos_media_movil_7_dias, -Region) %>% 
    mutate(
      dia = ymd(dia)
    ) %>% 
    janitor::clean_names()
  
  d2 <- d2 %>% 
    gather(dia, casos_media_movil_7_dias, -Region) %>% 
    mutate(
      dia = ymd(dia)
    ) %>% 
    janitor::clean_names()
  
  d <- left_join(d1, d2, by = c("region", "dia")) 
  
  d
  
}

get_data_producto_78_1ra_dosis <- function(){
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto78/vacunados_edad_fecha_1eraDosis.csv",
    col_types = cols(.default = col_double())
    )
  
  d <- d %>% 
    gather(dia, n, -Edad) %>% 
    janitor::clean_names() %>% 
    mutate(dia = ymd(dia), n = replace_na(n, 0))
  
  d
  
}

get_data_producto_78_2da_dosis <- function(){
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto78/vacunados_edad_fecha_2daDosis.csv",
    col_types = cols(.default = col_double())
  )
  
  d <- d %>% 
    gather(dia, n, -Edad) %>% 
    janitor::clean_names() %>% 
    mutate(dia = ymd(dia), n = replace_na(n, 0))
  
  d
  
}

get_data_producto_78_unica_dosis <- function(){
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto78/vacunados_edad_fecha_UnicaDosis.csv",
    col_types = cols(.default = col_double())
  )
  
  d <- d %>% 
    gather(dia, n, -Edad) %>% 
    janitor::clean_names() %>% 
    mutate(dia = ymd(dia), n = replace_na(n, 0))
  
  d
  
}

get_data_producto_76_1ra_2da_unica_dosis <- function(){
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto76/vacunacion_std.csv",
    col_types = cols(
      Region = col_character(),
      Dosis  = col_character(),
      Fecha = col_date(),
      .default = col_double())
  )
  
  d <- d %>% 
    filter(Region != "Total") %>% 
    janitor::clean_names() 
  
  d
  
}

get_data_ine_proyeccion_poblacion_2021 <- function(){
 
  fname <- "data/ine_proyeccion_poblacion_2021.txt"
  
  if(!file.exists(fname)){
    d <- read_delim(
      "https://www.ine.cl/docs/default-source/proyecciones-de-poblacion/cuadros-estadisticos/base-2017/ine_estimaciones-y-proyecciones-de-poblaci%C3%B3n-1992-2050_base-2017_base-de-datos.csv?sfvrsn=4022da86_11&download=true",
      delim = ";",
      skip = 1,
      n_max = 102,
      col_types = cols(
        .default = col_character(),
        EDAD = col_character()
      )
    )
    
    tail(d)
    
    d <- d %>% 
      filter(EDAD != "TOTAL") %>% 
      mutate(EDAD = str_remove(EDAD, "\\+")) %>% 
      gather(ano, poblacion, -EDAD) %>% 
      janitor::clean_names() %>% 
      mutate(
        edad = as.numeric(edad),
        poblacion = str_remove(poblacion, "\\."),
        poblacion = as.numeric(poblacion)
      )
    
    # d %>% filter(!complete.cases(.))
    
    d <- d %>% filter(ano == 2021)
    
    write_tsv(d, fname)
    
  } else {
    
    d <- read_tsv(
      fname,
      col_types = cols(
        edad = col_double(),
        ano = col_double(),
        poblacion = col_double()
        )
      )
  }
  
  d
  
}

get_data_consolidado_region_comuna_fecha <- function(){ 
  
  # diarios
  dcasos      <- get_data_producto_25()
  
  # acumulados
  dfallecidos <- get_data_producto_38() %>%
    select(-poblacion) %>% 
    arrange(dia) %>% 
    group_by(region, codigo_region, comuna, codigo_comuna) %>% 
    mutate(nro_fallecidos_ultimo = nro_fallecidos - lag(nro_fallecidos))
  
  dfallecidos %>% filter(comuna == "Santiago") %>% tail()
  
  d <- list(
    dcasos,
    dfallecidos
  ) %>%
    reduce(left_join, by = c("region", "codigo_region", "comuna", "codigo_comuna", "dia")) 
  
  d %>% filter(!complete.cases(.))
  
  d %>% filter(comuna == "Pica") %>% filter(complete.cases(.))
  
  d
  
}

get_data_consolidado_region_fecha <- function() {
  
  d <- get_data_consolidado_region_comuna_fecha()
  
  d <- d %>% 
    select(-comuna, -codigo_comuna) %>% 
    group_by(region, codigo_region, dia) %>% 
    summarise_all(sum, na.rm = TRUE) %>% 
    ungroup()
  
  d
  
}

get_data_consolidado_region <- function(){ 
  
  d <- get_data_consolidado_region_fecha()
  
  d <- d %>%
    filter(dia == max(dia)) %>% 
    select(-dia)
  
  d <- d %>% 
    mutate(region = region_to_factor(region)) %>% 
    mutate(id_region = str_make_id(region), .before = 1) %>% 
    mutate(id0 = row_number() - 1, .before = 1) %>% 
    arrange(region)
  
  d
  
}

