valueBoxCaptions <- function(value, caption, info, icon = NULL, color = NULL, href = NULL) {
  
  # value <- "1.5 millones"
  # caption <- "Confirmados"
  # 
  # info <- c(
  # "Dato interesante 1",
  # "Dato interesante 2",
  # "Dato interesante 3"
  # )
  
  info <- purrr::map2(info, length(info):1, function(x, y){
    
    spn <- tags$span(x)
    
    if(y == 1) {
      return(spn)
    }
    shiny::tagList(spn, tags$br())
  }) 
  
  caption <- tags$span(
    tags$b(caption), tags$br(), shiny::tagList(info)
  )
  
  valueBox(
    value = value, 
    caption = caption, 
    icon = icon, 
    color = color, 
    href = href)
  
}

categorizar_edad <- function(d) {
  
  d <- d %>% 
    mutate(
      edad_c = santoku::chop(edad, PARS$edad, labels = santoku::lbl_discrete()),
      edad_c = forcats::fct_recode(edad_c, `80+` = "80 - Inf")   
    ) 
  
  d
  
}

