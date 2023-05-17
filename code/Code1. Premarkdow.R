# Samuel Gutiérrez Jaramillo
# TITULO:
# R 4.2.2

# If not installed run line 6.
# install.packages("pacman")

require(pacman)
p_load(tidyverse, rio, janitor, ggplot2, skimr, rvest, dplyr, +
         tidyr, tibble, data.table)

#1.Regresiones.
#Cargar base de datos
datareg <- import("input/data_regresiones.rds")

#1.1. Estimaciones:
#Primer modelo. Regresión lineal simple, explicar precio de 
#la vivienda a partir de cuartos, baños y área.
m1 <- lm(price~rooms+bathrooms+surface_total, data=datareg)

