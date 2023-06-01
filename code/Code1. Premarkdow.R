# Nicolas Lozano - 
# Samuel Gutiérrez Jaramillo - 202111674
# Problem set #3 Taller R Uniandes.
# R 4.2.2

# If not installed run next line:
# install.packages("pacman")
rm(list=ls())
require(pacman)
p_load(tidyverse, rio, janitor, ggplot2, skimr, rvest, dplyr, +
         tidyr, tibble, data.table, stargazer, outreg, coefplot, xlsx, +
         sf, leaflet,tmaptools, osmdata, ggsn, ggmap, wordcloud, tidytext, tm)
rm(list=ls())

#1.Regresiones.
#Cargar base de datos
datareg <- import("input/data_regresiones.rds")

#1.1. Estimaciones:
#Primer modelo. Regresión lineal simple, explicar precio de 
#la vivienda a partir de cuartos, baños y área.
m1 <- lm(price~rooms+bathrooms+surface_total, data=datareg)

#Segundo modelo
datareg <- mutate(datareg, tipo = ifelse(property_type == "Casa", 0, 1))
m2 <- lm(price~rooms+bathrooms+tipo+surface_total, data=datareg)

#Tercer modelo
m3 <- lm(price~tipo+bathrooms+rooms+surface_total+dist_cole+dist_park, data=datareg)

#1.2 Tabla resultados:
fitlist <- list(m1, m2, m3)
regs <- outreg(fitlist)

#coefplots:
plot <- multiplot(m1,m2,m3, title = "Estimación modelos",xlab = "Valor estimación del parámetro")
ggsave("output/plot_regresiones.png")

export(regs, "output/resultados_regresiones.xlsx")

#2. Datos espaciales:
#2.1 Descargar datos
osm <- opq("Ubate Colombia") %>%
  add_osm_feature(key="amenity" , value="restaurant") 
osm_sf <- osm %>% osmdata_sf()
restaurantes <- osm_sf$osm_points %>% select(osm_id,amenity)

osm <- opq("Ubate Colombia") %>%
  add_osm_feature(key="leisure" , value="park") 
osm_sf <- osm %>% osmdata_sf()
parques <- osm_sf$osm_polygons %>% select(osm_id,leisure)

#2.2 Visualizar
leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurantes, col="blue") %>% addPolygons(data=parques, col="green")

#2.3 Geocodificar sitio: Bicicleteria KIKO
bici <- geocode_OSM("Calle 6 %7% 7-6, Ubate Colombia", as.sf=T)

#2.4 Mapa
Ubate <- opq("Ubate Colombia") %>%
  add_osm_feature(key="amenity" , value="restaurant") %>% add_osm_feature(key="leisure", value="park") %>% osmdata_sf()


#3. Web Scrapping:

#3.1: Objeto HTML
pag <- "https://es.wikipedia.org/wiki/Departamentos_de_Colombia"
deptos <- read_html(pag)
  
#3.2. Titulo de la página:
titulo <- deptos %>% html_nodes(xpath='//*[@id="firstHeading"]') %>% html_text()

#3.3 Tabla con Deptos de Colombia:
tabla <- deptos %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[3]') %>% html_table()
tabla <- as.data.frame(tabla)
export(tabla, "output/tabla_departamento.xlsx")

#3.4: Nube de palabras
palabras <- deptos %>% html_nodes("p") %>% html_text()
png("output/nube_palabras.png")
nube <- wordcloud(palabras) 
dev.off()

