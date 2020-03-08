library(readxl)
library(tidyr)
options(scipen = 999)
datos <- read.csv("bosques_energia.csv", header = T, sep = ",")
#datos <- datos[, colSums(is.na(datos)) != nrow(datos)]

years <- colnames(datos)[4:63]


wider <- pivot_wider(
  datos,
  names_from = Indicador,
  values_from = all_of(years) 
)


wider <- wider[, colSums(is.na(wider)) != nrow(wider)]

#######################################################



############################################################
library(tidyr)
library(dplyr)

longer <- pivot_longer(
  datos, 
  cols = -c(Pais,Codigo.Pais,Indicador),
  names_to = "Año",
  values_to = "Porcentaje",
  values_drop_na = T
)


consumo <- filter(
  longer,
  longer$Indicador == "Consumo de energía renovable (% del consumo total de energía final)"
)

grafico_a <- pivot_wider(
  consumo, 
  id_cols = Pais,
  names_from = Año, 
  values_from = Porcentaje
)




