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
  
  # Datos acumulados en formato largo 
  # SIN Total
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo.csv",
    col_types = cols(
      .default = col_double(),
      Region = col_character()
    )
  )
  
  d %>% count(Region)
  
  d <- d %>% 
    rename_all(str_to_lower) %>% 
    filter(region != "Total") %>%
    gather(dia, nro_casos, -region) %>% 
    mutate(dia = ymd(dia)) 
  
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
    rename_all(str_to_lower) %>% 
    rename(dia = region, region = ciudad, nro_pascientes_uci = valor) %>% 
    mutate(
      dia = ymd(dia),
      nro_pascientes_uci = as.numeric(nro_pascientes_uci)
    )
  
  d
  
}

get_data_producto_78_1ra_dosis <- function(){
  
  d <- read_csv(
    "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto78/vacunados_edad_fecha_1eraDosis.csv",
    col_types = cols(.default = col_double())
    )
  
  d <- d %>% 
    gather(dia, n, -Edad) %>% 
    rename_all(str_to_lower) %>% 
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
    rename_all(str_to_lower) %>% 
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
    rename_all(str_to_lower) %>% 
    mutate(dia = ymd(dia), n = replace_na(n, 0))
  
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
      rename_all(str_to_lower) %>% 
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
