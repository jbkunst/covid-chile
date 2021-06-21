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
