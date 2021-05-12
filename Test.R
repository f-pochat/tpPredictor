#Test Git
#Preuba
#prueba mateo
library(ggplot2)
library(plotrix)
library(dplyr)

dataset <- read.csv("dataset.csv", sep=",")
View(dataset)

angioplastia <- dataset[which(dataset$PROCEDIMIENTO == "ANGIOPLASTIA"),]
cirugia <- dataset[which(dataset$PROCEDIMIENTO == "CIRUGIA"),]
endovalvula <- dataset[which(dataset$PROCEDIMIENTO == "ENDOVALVULA"),]

plot(factor(cirugia$EDAD),type = "h")

cirugia %>%
  ggplot( aes(x=EDAD))+
  geom_density(fill="#b3ffb3", color="#66ff66", alpha=0.8)

angioplastia %>%
  ggplot( aes(x=EDAD))+
  geom_density(fill="#b3ffb3", color="#66ff66", alpha=0.8)

endovalvula %>%
  ggplot( aes(x=EDAD))+
  geom_density(fill="#b3ffb3", color="#66ff66", alpha=0.8)

