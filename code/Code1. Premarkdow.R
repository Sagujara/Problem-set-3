# Nicolas Lozano - 
# Samuel Gutiérrez Jaramillo - 202111674
# TITULO:
# R 4.2.2

# If not installed run next line:
# install.packages("pacman")
rm(list=ls())
require(pacman)
p_load(tidyverse, rio, janitor, ggplot2, skimr, rvest, dplyr, +
         tidyr, tibble, data.table, stargazer, outreg, coefplot, xlsx)
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

#2.