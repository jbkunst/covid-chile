PARS <- list(
  debug = FALSE,
  colors = list(
    sparkline = "#F4F6F9", # color de fondo de value boxes "blancos"
    primary = "#007bff",
    danger = "#DC3545",
    gray = "#C0C0C0"
  ),
  top = 5,
  hc = list(
    duration = 2500
  ),
  font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
)

Sys.setlocale("LC_ALL", "Spanish_Spain.1252")
# Sys.setlocale("LC_ALL","English")
# f <- Sys.Date()
# dias <- weekdays((f - lubridate::days(lubridate::wday(f) - 1)) + lubridate::days(0:6))

newlang_opts <- getOption("highcharter.lang")

newlang_opts$weekdays <- c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado")
newlang_opts$months <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", 
                         "agosto", "septiembre", "octubre", "noviembre", "diciembre")
newlang_opts$shortMonths <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", 
                              "oct", "nov", "dic")

newlang_opts$thousandsSep <- "."

newlang_opts$decimalPoint <- ","

options(
  highcharter.lang = newlang_opts,
  highcharter.google_fonts = FALSE,
  highcharter.theme = 
    hc_theme_smpl(
      
      colors = c("#007bff", "#DC3545"),
      
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
        title = list(
          align = "high",
          style = list(
            fontSize = "0.85em"
          )  
        )
      ),
      
      chart = list(
        backgroundColor = "white",
        style = list(fontFamily = PARS$font, fontSize = "1.0em")
      ),
      
      plotOptions = list(
        series = list(
          dataLabels = list(color = "#222d32", style = list(fontWeight = "normal", textShadow = FALSE, textOutline = FALSE)),
          animation = list(duration = PARS$hc$duration)
        ),
        line = list(
          lineWidth = 4
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
      )
    )
)
