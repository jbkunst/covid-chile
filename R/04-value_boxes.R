
# Inicio ------------------------------------------------------------------
# Confirmados -------------------------------------------------------------

vb_confirmados <- function(){
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
  
  d %>% 
    filter(x == max(x)) %>% 
    gather(tipo, valor, -x) %>% 
    mutate(
      tipo = c(
        "total",
        "ultimos confirmados",
        "medía móvil 7 días"
      )
    ) %>% 
    mutate(
      descripcion = ifelse(tipo == "total",
                           paste0(comma_01(valor / 1e6), " millones"),
                           paste0(comma_1(valor), " ", tipo)
      )
    )
}
         