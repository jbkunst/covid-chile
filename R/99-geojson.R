library(tidyverse)
library(stringr)
library(highcharter)

source("R/03-funciones-auxiliares.R", encoding = "UTF-8")

regiones <-  1:16
archivo <- "geojson/regiones_geojson.js"

try(dir.create("geojson"))
try(file.remove(archivo))

d <- read_csv(
  "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto65/PositividadPorComuna.csv",
  col_types = cols(
    .default = col_double(),
    Region = col_character(),
    Comuna = col_character()
  )
)

d <- d %>% 
  select(1:4) %>% 
  janitor::clean_names() %>% 
  mutate(codigo_comuna = str_pad(codigo_comuna, width = 5, pad = "0")) %>% 
  mutate(id_region = str_make_id(region), .before = 1)

d

dr <- d %>% 
  distinct(id_region, region, codigo_region)

dr

walk(regiones, function(r = 6) {

  message(r)
    
  fl <- str_glue(
    "https://raw.githubusercontent.com/pachadotdev/chilemapas/master/data_geojson/comunas/r{ r }.geojson",
    r = str_pad(r, width = 2, pad = "0")
  )
  
  gjson <- jsonlite::fromJSON(fl)
  
  gjson$features$properties <- gjson$features$properties %>% 
    left_join(d %>% select(codigo_comuna, comuna), by = "codigo_comuna")
  
  if(r == 5) {
    
    ids <- which(gjson$features$properties$codigo_comuna %in% c("05201", "05104"))
    
    gjson$features <- gjson$features[-ids,]
    
    id <- which(gjson$features$properties$codigo_comuna == "05101")
    
    gjson$features$geometry$coordinates[[id]] <- gjson$features$geometry$coordinates[[id]][2]
    
  }
  
  geojson <- geojsonio::as.json(gjson)
  
  geojson
  
  hc <- highchart(type = "map") %>%
    hc_add_series(mapData = geojson)
  
  hc$x$fonts <- ""
  
  hc
  
  idr <- dr %>% 
    filter(codigo_region == r) %>% 
    pull(id_region)
  
  idr <- case_when(
    str_detect(idr, "magallanes") ~ "magallanes",
    str_detect(idr, "ohiggins") ~ "ohiggins",
    str_detect(idr, "araucania") ~ "araucania",
    
    TRUE ~ idr
  )
  
  fout <- fs::path("geojson", idr, ext = "json")
  
  
  json_string <- RJSONIO::toJSON(geojson)
  
  json_string %>% str_sub(0, 20)
  json_string %>% str_sub(-20, -1)
  
  json_string <- str_sub(json_string, 4, -4)
  
  json_string %>% str_sub(0, 40) %>% str_remove_all("\\\\")
  
  json_string <- json_string %>% str_remove_all("\\\\")
  
  write_lines(json_string, fout)
  
  json_string <- str_glue("Highcharts.maps[\"{ idr }\"] = ", idr = idr) %>% 
    str_c(json_string)
  
  write_lines(json_string, archivo, append = file.exists(archivo))
  
})

