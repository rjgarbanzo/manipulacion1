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
library(ggplot2)
library(scales)
longer <- pivot_longer(
  datos, 
  cols = -c(Pais,Codigo.Pais,Indicador),
  names_to = "Year",
  values_to = "Porcentaje",
  values_drop_na = T
)

consumo <- filter(
  longer,
  longer$Indicador == "Consumo de energía renovable (% del consumo total de energía final)"
)


grafico_A <- consumo %>% filter(Pais %in% c("Canada","Paraguay", "Peru", "China"))

grafico_A$Year <- gsub("[a-zA-Z ]", "", grafico_A$Year)
grafico_A$Year <- as.factor(grafico_A$Year)
str(grafico_A)



ggplot(grafico_A, mapping = aes(x=Porcentaje, y=Year, color = Pais))+
  geom_point()+
  labs(title = "Consumo de energía renovable")


##########################################################################

library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(forcats)

longer <- pivot_longer(
  datos, 
  cols = -c(Pais,Codigo.Pais,Indicador),
  names_to = "Year",
  values_to = "Porcentaje",
  values_drop_na = T
)


superficie <- filter(
  longer,
  longer$Indicador == "Consumo de energía renovable (% del consumo total de energía final)"
)

superficie$Year <- gsub("[a-zA-Z ]", "", superficie$Year)
superficie$Year <- as.factor(superficie$Year)
str(superficie)

tabla_b <- pivot_wider(
  superficie, 
  id_cols = Pais, # No toma en cuenta la variable pais
  names_from = Year, 
  values_from = Porcentaje
)

grafico_B <- tabla_b %>% mutate(mean_all = rowMeans(tabla_b[-1]))

grafico_B_top_10 <- grafico_B %>% 
  filter(rank(desc(mean_all))<=10)

grafico_B_top_10

ggplot(data = grafico_B_top_10,
       mapping = aes(x = Pais,
                     y = mean_all)) +
  geom_col(fill = "#07689f") +
  scale_y_continuous() +
  theme_linedraw() +
  cord_flip






  
