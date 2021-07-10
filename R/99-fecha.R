# hack para mostrar la fecha en espaniol
d <- Sys.Date()

mes <- c("enero", "febrero", "marzo",
         "abril", "mayo", "junio",
         "julio", "agosto", "septiembre",
         "octubre", "noviembre", "diciembre")[lubridate::month(d)]

fecha <- paste(
  lubridate::day(d),
  mes,
  lubridate::year(d)
)
