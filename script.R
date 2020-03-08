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
       mapping = aes(x = fct_reorder(Pais,mean_all),
                     y = mean_all)) +
  geom_col(fill = "#07689f") +
  scale_y_continuous() +
  theme_linedraw() +
  coord_flip()




###########################################################################
library(tidyr)

datos <- read.csv("DatosEducacion_V2.csv", header = T, sep = ",")

str(datos)

wider<- pivot_wider(
  datos,
  names_from = fecha, 
  values_from = valor,
  values_fill = list(valor = 0)
)

###########################################################################
library(tidyr)
library(dplyr)




suma <- wider %>% mutate(mean_all = rowMeans(wider[-c(1,2,3)]))
suma <- suma[-c(2,4:10)] 
#sumaPais <- suma %>% group_by(indicador)

suma_P <- pivot_wider(
  suma,
  names_from = indicador, 
  values_from = mean_all
)

#str(suma_P)


suma_P <- rename(
  suma_P, 
  "Repitentes" = "Cantidad de Estudiantes que repitieron", 
  "Duracion en primaria" = "Duracion de la educacion primaria", 
  "Alfabetizacion mayores a 15" = "Porcentaje de alfabetizacion de adultos para la poblacion mayor de 15 aÃ±os",
  "% PIB en educacion primaria" = "Gasto publico en educacion primaria como porcentaje del PIB",
  "Gasto educacion en millones USD" = "Gasto publico bruto en educacion primaria en millones de dolares",
  "% matricula" = "Porcentaje de matricula",
  "% no escolarizados" = "Porcentaje de niÃ±os no escolarizados",
  "% desercion" = "Porcentaje de desercion"
)


############################################

library("cluster")
library("factoextra")
library("FactoMineR")

row.names(suma_P) <- suma_P$pais
suma_P[1] <- NULL

suma_P[is.na(suma_P)] <- 0

matriz.distacias <- dist(suma_P)
modelo <- hclust(matriz.distacias, method = "complete")
plot(modelo)
rect.hclust(modelo, k = 3, border = "red")



################################################


library(fmsb)
# Función para encontrar el centroide de cada cluster
centroide <- function(num.cluster, datos, clusters) {
  ind <- (clusters == num.cluster)
  return(colMeans(datos[ind,]))
}

grupos <- cutree(modelo, k = 3)
grupos

centro.cluster1 <- centroide(1, suma_P, grupos)
centro.cluster1
centro.cluster2 <- centroide(2, suma_P, grupos)
centro.cluster2
centro.cluster3 <- centroide(3, suma_P, grupos)
centro.cluster3


centros <- rbind(centro.cluster1,
                 centro.cluster2,
                 centro.cluster3)

centros <- as.data.frame(centros)
maximos <- apply(centros, 2, max)
minimos <- apply(centros, 2, min)
centros <- rbind(minimos, centros)
centros <- rbind(maximos, centros)
str(centros)

color <- c("red","green","blue")
radarchart(as.data.frame(centros),
           maxmin=TRUE, axistype=4,
           axislabcol="slategray4",
           centerzero=FALSE, seg=8,
           cglcol="gray67",
           pcol=color,plty=1,
           plwd=5,
           title="Comparación de clústeres")

legenda <-legend(1.5,1,
                 legend=c("Cluster 1",
                          "Cluster 2",
                          "Cluster 3"),
                 seg.len=-1.4, title="Clústeres",
                 pch=21,bty="n", lwd=3,
                 y.intersp=1, horiz=FALSE,
                 col=color)
