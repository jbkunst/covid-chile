
# Inicio ------------------------------------------------------------------
# Confirmados -------------------------------------------------------------

vb_confirmados <- function(){
  
  d <- get_data_consolidado_region()
  
  rest <- d %>% 
    select_if(is.numeric) %>% 
    summarise_all(sum) %>% 
    select( casos_totales, casos_ultimos, casos_media_movil_7_dias) %>% 
    gather(tipo, descripcion) %>% 
    mutate(aux = c(
      "millones",
      "últimos confirmados",
      "media móvil ult. 7 días"
    )) %>% 
    mutate(descripcion = ifelse(
      tipo == "casos_totales", 
      paste0(comma_01(descripcion / 1e6), " ", aux),
      paste0(comma_1(descripcion), " ", aux)
    )) %>% 
    select(-aux)
  
  rest
}
         