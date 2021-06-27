# parametros generales ----------------------------------------------------
PARS <- list(
  debug = FALSE,
  colors = list(
    sparkline = "#F4F6F9", # color de fondo de value boxes "blancos"
    primary = "#0f69b4", #"#007bff",
    secondary = "#eb3c46", #"#DC3545",
    gray = "#C0C0C0"
  ),
  edad_breaks = c(12, 18, 30, 40, 50, 60, 70, 80, Inf),
  region_levels = c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", 
                    "Coquimbo", "Valparaíso", "Metropolitana", "O’Higgins", "Maule", 
                    "Ñuble", "Biobío", "Araucanía", "Los Ríos", "Los Lagos", "Aysén", 
                    "Magallanes"),
  region_ids = c("arica_y_parinacota", "tarapaca", "antofagasta", "atacama", 
                 "coquimbo", "valparaiso", "metropolitana", "ohiggins", "maule", 
                 "nuble", "biobio", "araucania", "los_rios", "los_lagos", "aysen", 
                 "magallanes"),
  font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
)

# data --------------------------------------------------------------------
# from https://jkunst.com/blog/posts/2020-06-02-30diasdegraficos-parte-3/#d%C3%ADa-21-gr%C3%A1ficos-con-anotaciones
EVENTOS <- tibble(
  x = ymd(c("2020-05-15", "2021-05-9")),
  text = c("Primera cuarentena<br>en la RM", "Día de la madre")
) 

# css ---------------------------------------------------------------------
css <- readr::read_lines("css/style_template.css")

css %>% 
  stringr::str_replace("PRIMARYCOLOR", PARS$colors$primary) %>% 
  readr::write_lines("css/style.css")

# highcharts --------------------------------------------------------------
Sys.setlocale("LC_ALL", "Spanish_Spain.1252")
# Sys.setlocale("LC_ALL","English")
# f <- Sys.Date()
# dias <- weekdays((f - lubridate::days(lubridate::wday(f) - 1)) + lubridate::days(0:6))

newlang_opts <- getOption("highcharter.lang")

newlang_opts$thousandsSep <- "."
newlang_opts$decimalPoint <- ","
newlang_opts$weekdays     <- c("domingo", "lunes", "martes", "miércoles", 
                               "jueves", "viernes", "sábado")
newlang_opts$months       <- c("enero", "febrero", "marzo", "abril", "mayo",
                               "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
newlang_opts$shortMonths  <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", 
                               "ago", "sep", "oct", "nov", "dic")

options(
  highcharter.lang = newlang_opts,
  highcharter.google_fonts = FALSE,
  highcharter.theme = 
    hc_theme_smpl(
      
      colors = c(PARS$colors$primary, PARS$colors$secondary),
      
      title = list(
        style = list(fontSize = "1.2em", fontFamily = PARS$font)
      ),
      
      subtitle = list(
        style = list(fontFamily = PARS$font, fontSize = "0.85em")
      ),
      
      xAxis = list(
        title = list(
          align = "high",
          style = list(
            fontSize = "0.85em"
          )  
        )
      ),
      
      yAxis = list(
        opposite = TRUE,
        title = list(
          align = "high",
          style = list(
            fontSize = "0.85em"
          )  
        )
      ),
      
      chart = list(
        backgroundColor = "white",
        style = list(fontFamily = PARS$font, fontSize = "1.0em"),
        zoomType = "x"
      ),
      
      plotOptions = list(
        series = list(
          animation = list(duration = 2000),
          marker = list(symbol = "circle")
        ),
        line = list(
          lineWidth = 4
        ),
        area = list(
          fillColor = list(
            linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
            stops = list(
              list(0.0, hex_to_rgba(PARS$color$primary, alpha = 0.1)),
              list(1.0, PARS$color$sparkline)
            )
          )
        ),
        arearange = list(
          lineWidth = 1,
          fillOpacity = 0.25
        )
      ),
      
      exporting = list(
        enabled = FALSE,
        buttons = list(
          contextButton = list(
            symbol = 'url(https://www.iconsdb.com/icons/preview/gray/download-2-xxl.png)',
            symbolSize = 18,
            symbolX = 21,
            symbolY = 20,
            titleKey = "Descargar",
            y = -05
          )
        )
      ),
      
      tooltip = list(
        useHTML = TRUE
      ),
      
      legend = list(
        verticalAlign = "top",
        align = "left",
        itemStyle =  list(
          fontWeight = "normal"
        )
      ),
      
      responsive = list(
        rules = list(
          list(
            condition = list(maxWidth = 500, maxHeigth = 250),
            chartOptions = list(
              legend = list(enabled = FALSE),
              xAxis = list(title = NULL),
              yAxis = list(title = "", endOnTick = FALSE)
              # plotOptions = list(series = list(lineWidth = 1))
            )
          )
        )
      )
    )
)

rm(newlang_opts)
