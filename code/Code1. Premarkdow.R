# Nicolas Lozano - 
# Samuel Gutiérrez Jaramillo - 202111674
# Problem set #3 Taller R Uniandes.
# R 4.2.2

# If not installed run next line:
# install.packages("pacman")

rm(list=ls())
require(pacman)
p_load(tidyverse, 
       rio, 
       janitor, 
       ggplot2, 
       skimr, 
       rvest, 
       dplyr,
       tidyr, 
       tibble, 
       data.table, 
       stargazer, 
       outreg, 
       coefplot, 
       sf, 
       leaflet,
       tmaptools, 
       osmdata, 
       ggsn, 
       ggmap, 
       modelsummary, 
       wordcloud, 
       tidytext, 
       tm)


#1.Regresiones.
#Cargar base de datos
datareg <- import("input/data_regresiones.rds")

#1.1. Estimaciones:
#Primer modelo. Regresión lineal simple, explicar precio de 
#la vivienda a partir de cuartos, baños y área.
m1 <- lm(price ~ rooms + bathrooms + surface_total, data = datareg)

#Segundo modelo
m2 <- lm(price ~ rooms + bathrooms + as.factor(property_type) + surface_total, data=datareg)

#Tercer modelo
m3 <- lm(price ~ as.factor(property_type) + bathrooms + rooms + surface_total + dist_cole + dist_park, data=datareg)

#1.2 Tabla resultados:
fitlist <- list("Modelo 1" = m1, "Modelo 2" = m2, "Modelo 3" = m3)
msummary(fitlist)

#modelsummary(fitlist, stars = TRUE, output = "output\\modelos.xlsx")

regs <- outreg(fitlist)
export(regs, "output/resultados_regresiones.xlsx")

#coefplots:

etiquetas <- c('dist_park' = 'Distancia Parques',
               'rooms' = 'Habitaciones',
               'bathrooms' = 'Baños',
               'surface_total' = 'Area',
               'dist_cole' = 'Distancia Colegios',
               'as.factor(property_type)Casa' = 'Tipo Inmueble',
               '(Intercept)' = 'Intercepto')


modelplot(fitlist, coef_map = etiquetas, facet = F) + 
  labs(title = "Coeficientes Estimados", subtitle = "Intervalos 95%", caption="", alt = "") +
  scale_x_continuous(labels = scales::comma) + 
  theme_grey() 
ggsave("output/plot_regresiones.png", width = 7, height = 5, dpi = 300, units = "in")


#2. Datos espaciales:
#2.1 Descargar datos
osm <- opq("Santa Marta Colombia") %>%
  add_osm_feature(key="amenity" , value="restaurant") 
osm_sf <- osm %>% osmdata_sf()
restaurantes <- osm_sf$osm_points %>% select(osm_id,amenity)

osm <- opq("Santa Marta Colombia") %>%
  add_osm_feature(key="leisure" , value="park") 
osm_sf <- osm %>% osmdata_sf()
parques <- osm_sf$osm_polygons %>% select(osm_id,leisure)

#2.2 Visualizar
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data=restaurantes, col="blue") %>% 
  addPolygons(data=parques, col="green")

#2.3 Geocodificar sitio: Bicicleteria KIKO
direccion <- geocode_OSM("Calle 14, Localidad 3 Turística - Perla del Caribe, Santa Marta, Magdalena, 005075, Colombia", as.sf=T)

#2.4 Mapa
santa_marta <- opq(bbox = getbb("Santa_Marta Colombia")) %>%
  add_osm_feature(key="boundary", value="administrative") %>%
  osmdata_sf()

santa_marta <- santa_marta$osm_multipolygons %>% subset(admin_level==8)

osm_layer <- get_stamenmap(bbox = as.vector(st_bbox(santa_marta)), 
                          maptype="toner", source="osm", zoom=13) 

mapa <- ggmap(osm_layer) + 
        geom_sf(data = santa_marta,alpha=0.3, inherit.aes=F) +
        geom_sf(data = parques, 
                colour = 'green',
                size = 15, 
                inherit.aes = FALSE) +
        geom_sf(data = restaurantes, 
                colour = 'blue',
                size = 1, 
                inherit.aes = FALSE) +
        geom_sf(data = direccion, 
                colour = 'yellow',
                size = 2, 
                inherit.aes = FALSE) +
        scale_fill_viridis(option = "D" , 
                           name = "Variable") +
        scalebar(data = santa_marta, 
                 dist = 5, 
                 transform =T, 
                 dist_unit = "km", 
                 box.fill=c("white","black"), 
                 st.bottom = F, 
                 st.color = "grey") +
        north(data = santa_marta, 
              location = "topright") + 
        theme_linedraw() +
        labs(x="" , y="")

mapa

ggsave("output/mapa_amenities.png", width = 7, height = 5, dpi = 300, units = "in")


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

