
# Inicio ------------------------------------------------------------------
# Confirmados -------------------------------------------------------------

vb_confirmados <- function(){
  
  d <- hc_to_data_frame(grafico_confirmados_diarios())
  
  rest <- d %>% 
    group_by(serie) %>% 
    filter(x == max(x)) %>% 
    ungroup() %>% 
    filter(! str_detect(serie, "17")) %>% 
    mutate(
      tipo = c("últimos confirmados", "promedio últimos 7 días"), 
      descripcion = paste0(comma_1(y), " ", tipo)
    ) %>% 
    select(tipo, descripcion) %>% 
    bind_rows(
      d %>% 
        filter(str_detect(serie, "Diarios")) %>% 
        group_by(serie) %>% 
        summarise(y = sum(y)) %>% 
        ungroup() %>% 
        mutate(tipo = "total", descripcion = comma_01(y / 1e6)) %>% 
        select(tipo, descripcion)
    ) %>% 
    mutate_all(as.character)
  
  rest
}
         