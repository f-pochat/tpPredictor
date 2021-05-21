dataset <- read.csv("dataset.csv", sep=",")
library(ggplot2)
library(plotrix)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(tibble)
## Analizar y contar datos

#1 Promedio de edad para epoc obesidad diabetes y diálisis
epoc <- dataset[which(dataset$EPOC == 1),]
ageAverageWithEpoc <- mean(epoc$EDAD)
ageAverageWithEpoc <- round(ageAverageWithEpoc, 1)

obesity <- dataset[which(dataset$OBESIDAD.MORBIDA == 1),]
ageAverageWithObesity <- mean(obesity$EDAD)
ageAverageWithObesity <- round(ageAverageWithObesity, 1)

diabetes <- dataset[which(dataset$DIABETES == 1),]
ageAverageWithDiabetes <- mean(diabetes$EDAD)
ageAverageWithDiabetes <- round(ageAverageWithDiabetes, 1)

dialisis <- dataset[which(dataset$DIALISIS == 1),]
ageAverageWithDialisis <- mean(dialisis$EDAD)
ageAverageWithDialisis <- round(ageAverageWithDialisis, 1)

age <- c(ageAverageWithDiabetes,ageAverageWithDialisis,ageAverageWithEpoc,ageAverageWithObesity)
p <- barplot(age, ylab="Average Age", names=c("Diabetes", "Dialisis", "Epoc", "Obesidad"), col = rainbow(4), ylim = c(0, 70)) #No aporta mucha informacion
text(p, labels=round(age, 1), y=age+3)

#2 Porcentaje de Hombres y Mujeres y promedio de edad para cada una
numOfMale <- length(dataset$SEXO[which(dataset$SEXO == "MASC")])
numOfFemale <- length(dataset$SEXO[which(dataset$SEXO == "FEME")])
totalNum <- numOfFemale + numOfMale
percentageOfMale <- round(numOfMale / totalNum * 100, 2)
percentageOfFemale <- round(numOfFemale / totalNum * 100, 2)
percentages <- c(percentageOfFemale,percentageOfMale)

#Pie
pie3D(percentages,main = "Porcentaje de Hombres y Mujeres", labels = c(paste("Mujeres", percentageOfFemale,"%"), paste("Hombres", percentageOfMale,"%")), theta = 1, explode = 0.00)


ageAverageInMen <- mean(dataset[which(dataset$SEXO == "MASC"),]$EDAD)
ageAverageInWomen <- mean(dataset[which(dataset$SEXO == "FEME"),]$EDAD)

ages <- c(dataset[which(dataset$SEXO == "MASC"),]$EDAD,dataset[which(dataset$SEXO == "FEME"),]$EDAD)

#Histograma
ggplot(dataset, aes(x = EDAD, fill = SEXO)) +
  geom_density(alpha = 0.4) +
  scale_fill_discrete(name = "Gender", labels = c("Female","Male")) +
  theme(legend.position = "top")+
  geom_vline(data=dataset, aes(xintercept=ageAverageInMen, color = "Hombres"),
               linetype="dashed")+
  geom_vline(data=dataset, aes(xintercept=ageAverageInWomen, color = "Mujeres"),
             linetype="dashed")

#3 Porcentaje de personas que van a angioplastia, cirugia o endovalvula

percentageByProcedure <- prop.table(table(dataset$PROCEDIMIENTO), NULL)*100
percentageAngioplasty <- percentageByProcedure[1]
percentageSerguryAndEndovalve <- percentageByProcedure[3]
percentageEndovalve <- percentageByProcedure[4] + percentageSerguryAndEndovalve
percentageSurgery <- percentageByProcedure[2] + percentageSerguryAndEndovalve
percentageAngioplasty <- round(percentageAngioplasty, digits = 1)
percentageEndovalve <- round(percentageEndovalve, digits = 1)
percentageSurgery <- round(percentageSurgery, digits = 1)
#Pie 
percentageAngioplastyArray <- c(percentageAngioplasty[1],100-percentageAngioplasty[1])
pie3D(percentageAngioplastyArray, main = "Porcentaje angioplastia",
      labels = c(paste("Angioplastia", percentageAngioplastyArray[1],"%"), paste("No angioplastia", percentageAngioplastyArray[2],"%")),
      theta = 1,
      explode = 0.00)

percentageEndovalveArray <- c(percentageEndovalve[1],100-percentageEndovalve[1])
pie3D(percentageEndovalveArray, main = "Porcentaje endovalvula",
      labels = c(paste("Endovalvula", percentageEndovalveArray[1],"%"), paste("No endovalvula", percentageEndovalveArray[2],"%")),
      theta = 1,
      explode = 0.00)

percentageSurgeryArray <- c(percentageSurgery[1],100-percentageSurgery[1])
pie3D(percentageSurgeryArray, main = "Porcentaje cirugia",
      labels = c(paste("Cirugia", percentageSurgeryArray[1],"%"), paste("No cirugia", percentageSurgeryArray[2],"%")),
      theta = 1,
      explode = 0.00)


## Cruzar variabeles

#4 Promedio de lesiones con respecto a pacientes con epoc, obesidad mórbida, diálisis o diabetes
injuryAverageWithEpoc <- mean(as.numeric(epoc$NUMERO.DE.LESIONES))
injuryAverageWithEpoc

injuryAverageWithObesity <- mean(as.numeric(obesity$NUMERO.DE.LESIONES))
injuryAverageWithObesity

injuryAverageWithDialisis <- mean(as.numeric(dialisis$NUMERO.DE.LESIONES))
injuryAverageWithDialisis

injuryAverageWithDiabetes <- mean(na.omit(as.numeric(diabetes$NUMERO.DE.LESIONES)))
injuryAverageWithDiabetes

#Barplot
barplot(age, ylab="Average Age", col = rainbow(4)) #No aporta mucha informacion

#5 Porcentaje de pacientes masculinos y femeninos con con epoc, obesidad mórbida, diálisis o diabetes

#Masculino
menData <- dataset[which(dataset$SEXO == 'MASC'),]

percentageOfMenWithEpoc <- prop.table(table(menData$EPOC), NULL)*100
percentageOfMenWithEpoc <- round(percentageOfMenWithEpoc,1)

percentageOfMenWithObesity <- prop.table(table(menData$OBESIDAD.MORBIDA), NULL)*100
percentageOfMenWithObesity <- round(percentageOfMenWithObesity,1)

percentageOfMenWithDialisis <- prop.table(table(menData$DIALISIS), NULL)*100
percentageOfMenWithDialisis <- round(percentageOfMenWithDialisis,1)

percentageOfMenWithDiabetes <- prop.table(table(menData$DIABETES), NULL)*100
percentageOfMenWithDiabetes <- round(percentageOfMenWithDiabetes,1)

#Femenino
femaleData <- dataset[which(dataset$SEXO == 'FEME'),]

percentageOfWomenWithEpoc <-  prop.table(table(femaleData$EPOC), NULL)*100
percentageOfWomenWithEpoc <- round(percentageOfWomenWithEpoc,1)

percentageOfWomenWithObesity <- prop.table(table(femaleData$OBESIDAD.MORBIDA), NULL)*100
percentageOfWomenWithObesity <- round(percentageOfWomenWithObesity,1)

percentageOfWomenWithDialisis <- prop.table(table(femaleData$DIALISIS), NULL)*100
percentageOfWomenWithDialisis <- round(percentageOfWomenWithDialisis,1)

percentageOfWomenWithDiabetes <- prop.table(table(femaleData$DIABETES), NULL)*100
percentageOfWomenWithDiabetes <- round(percentageOfWomenWithDiabetes,1)

#Todos pie charts

porcentageOfWomenWithNotEpoc <- 100-percentageOfWomenWithEpoc[1]
porcentageOfWomenWithNotEpoc <- round(porcentageOfWomenWithNotEpoc,1)
womenWithEpoc <- c(percentageOfWomenWithEpoc[1], porcentageOfWomenWithNotEpoc)
womenWithEpoc[2] <- 0.000001
pie3D(womenWithEpoc, main = "Porcentajes de Mujeres con epoc",
      labels = c(paste("No epoc", womenWithEpoc[1],"%"), paste("Epoc", 0,"%")),
      theta = 1,
      explode = 0.00)

percentageOfMenWithNotEpoc <- 100 - percentageOfMenWithEpoc[2]
percentageOfMenWithNotEpoc <- round(percentageOfMenWithNotEpoc,1)
menWithEpoc <- c(percentageOfMenWithEpoc[2], percentageOfMenWithNotEpoc)
pie3D(menWithEpoc, main = "Porcentaje De Hombres con Epoc ",
      labels = c(paste("Con Epoc", menWithEpoc[1],"%"), paste("Sin epoc", menWithEpoc[2],"%")),
      theta = 1,
      explode = 0.00)

porcentageOfWomenWithNotObesity <- 100-percentageOfWomenWithObesity[2]
porcentageOfWomenWithNotObesity <- round(porcentageOfWomenWithNotObesity,1)
womenWithObesity <- c(percentageOfWomenWithObesity[2], porcentageOfWomenWithNotObesity)
pie3D(womenWithObesity, main = "Porcentajes de Mujeres con Obesidad",
      labels = c(paste("Con obesidad", womenWithObesity[1],"%"), paste("Sin Obesidad", womenWithObesity[2],"%")),
      theta = 1,
      explode = 0.00)

porcentageOfMenWithNotObesity <- 100-percentageOfMenWithObesity[2]
porcentageOfMenWithNotObesity <- round(porcentageOfMenWithNotObesity,1)
menWithObesity <- c(percentageOfMenWithObesity[2], porcentageOfMenWithNotObesity)
pie3D(menWithObesity, main = "Porcentajes de Hombres con Obesidad",
      labels = c(paste("Con Obesidad", menWithObesity[1],"%"), paste("Sin Obesidad", menWithObesity[2],"%")),
      theta = 1,
      explode = 0.00)

porcentageOfWomenWithNotDialisis <- 100-percentageOfWomenWithDialisis[2]
porcentageOfWomenWithNotDialisis <- round(porcentageOfWomenWithNotDialisis,1)
womenWithDialisis <- c(percentageOfWomenWithDialisis[2], porcentageOfWomenWithNotDialisis)
pie3D(womenWithDialisis, main = "Porcentajes de Mujeres con Dialisis",
      labels = c(paste("Con Dialisis", womenWithDialisis[1],"%"), paste("Sin Dialisis", womenWithDialisis[2],"%")),
      theta = 1,
      explode = 0.00)

porcentageOfMenWithNotDialisis <- 100-percentageOfMenWithDialisis[2]
porcentageOfMenWithNotDialisis <- round(porcentageOfMenWithNotDialisis,1)
menWithDialisis <- c(percentageOfMenWithDialisis[2], porcentageOfMenWithNotDialisis)
pie3D(menWithDialisis, main = "Porcentajes de Hombres con Dialisis",
      labels = c(paste("Con Dialisis", menWithDialisis[1],"%"), paste("Sin Dialisis", menWithDialisis[2],"%")),
      theta = 1,
      explode = 0.00)

porcentageOfWomenWithNotDiabetes <- 100-percentageOfWomenWithDiabetes[2]
porcentageOfWomenWithNotDiabetes <- round(porcentageOfWomenWithNotDiabetes,1)
womenWithDiabetes <- c(percentageOfWomenWithDiabetes[2], porcentageOfWomenWithNotDiabetes)
pie3D(womenWithDiabetes, main = "Porcentajes de Mujeres con Diabetes",
      labels = c(paste("Con Diabetes", womenWithDiabetes[1],"%"), paste("Sin Diabetes", womenWithDiabetes[2],"%")),
      theta = 1,
      explode = 0.00)

porcentageOfMenWithNotDiabetes <- 100-percentageOfMenWithDiabetes[2]
porcentageOfMenWithNotDiabetes <- round(porcentageOfMenWithNotDiabetes,1)
menWithDiabetes <- c(percentageOfMenWithDiabetes[2], porcentageOfMenWithNotDiabetes)
pie3D(menWithDiabetes, main = "Porcentajes de Hombres con Diabetes",
      labels = c(paste("Con Diabetes", menWithDiabetes[1],"%"), paste("Sin Diabetes", menWithDiabetes[2],"%")),
      theta = 1,
      explode = 0.00)

#6 Promedio de edad para cada procedimiento
surgery <- dataset[which(dataset$PROCEDIMIENTO == 'CIRUGIA'),]
ageAverageInSurgery <- mean(surgery$EDAD)

angioplasty <- dataset[which(dataset$PROCEDIMIENTO == 'ANGIOPLASTIA'),]
ageAverageInAngioplasty <- mean(angioplasty$EDAD)

age2 <- c(round(ageAverageInSurgery,1), round(ageAverageInAngioplasty, 1))
#Barplot
barplot6 <- barplot(age2, ylab="Average Age", names=c("Cirugia", "Angioplastia"), col = rainbow(4), ylim = c(0, 70)) #No aporta mucha informacion
text(barplot6, labels=age2, y=age2+3)

## gooood cruzando variables

#7 Porcentaje de riesgo y no de riesgo por procedimiento (sin endovalvula)

diabetesAndSurgery <- diabetes[which(diabetes$PROCEDIMIENTO == 'CIRUGIA'),]
diabetesAndAngioplasty <- diabetes[which(diabetes$PROCEDIMIENTO == 'ANGIOPLASTIA'),]

percentageOfSurgeryInDiabetic <- nrow(diabetesAndSurgery) / nrow(diabetes)
percentageOfAngioplastyInDiabetic <- nrow(diabetesAndAngioplasty) / nrow(diabetes)

epocAndSurgery <- epoc[which(epoc$PROCEDIMIENTO == 'CIRUGIA'),]
epocAndAngioplasty <- epoc[which(epoc$PROCEDIMIENTO == 'ANGIOPLASTIA'),]

percentageOfSurgeryInEpoc <- nrow(epocAndSurgery) / nrow(epoc)
percentageOfAngioplastyInEpoc <- nrow(epocAndAngioplasty) / nrow(epoc)

obesityAndSurgery <- obesity[which(obesity$PROCEDIMIENTO == 'CIRUGIA'),]
obesityAndAngioplasty <- obesity[which(obesity$PROCEDIMIENTO == 'ANGIOPLASTIA'),]

percentageOfSurgeryInObesity <- nrow(obesityAndSurgery) / nrow(obesity)
percentageOfAngioplastyInObesity <- nrow(obesityAndAngioplasty) / nrow(obesity)

dialisisAndSurgery <- dialisis[which(dialisis$PROCEDIMIENTO == 'CIRUGIA'),]
dialisisAndAngioplasty <- dialisis[which(dialisis$PROCEDIMIENTO == 'ANGIOPLASTIA'),]

percentageOfSurgeryInDialisis <- nrow(dialisisAndSurgery) / nrow(dialisis)
percentageOfAngioplastyInDialisis <- nrow(dialisisAndAngioplasty) / nrow(dialisis)
#PieCharts diabetes
percentagenonAngioplastyInDiabetic<-100- percentageOfAngioplastyInDiabetic*100
percentagenonSurgeryInDiabetic<-100- percentageOfSurgeryInDiabetic*100

percentagesSurgeriesInDiabetic <- c(percentagenonSurgeryInDiabetic,percentageOfSurgeryInDiabetic*100)
pie3D(percentagesSurgeriesInDiabetic, main = "Porcentaje de Cirugias en Diabeticos ",
      labels = c(paste("No va a cirugia", round( percentagenonSurgeryInDiabetic,digits=2),"%"), paste("Va a cirugia", round(percentageOfSurgeryInDiabetic*100,digits = 2),"%")),
      theta = 1,
      explode = 0.00)
percentagesAngioplastyInDiabetic <- c(percentageOfAngioplastyInDiabetic*100,percentagenonAngioplastyInDiabetic)
pie3D(percentagesSurgeriesInDiabetic, main = "Porcentaje de Angioplastias en Diabeticos ",
      labels = c(paste("Angioplastia",round(percentageOfAngioplastyInDiabetic*100,digits = 2) ,"%"), paste("No Angioplastia",round( percentagenonAngioplastyInDiabetic,digits = 2),"%")),
      theta = 1,
      explode = 0.00)

#pie charts epoc
percentagenonAngioplastyInEpoc<-100- percentageOfAngioplastyInEpoc*100
percentagenonSurgeryInEpoc<-100- percentageOfSurgeryInEpoc*100

percentagesSurgeriesInEpoc <- c(percentagenonSurgeryInEpoc,percentageOfSurgeryInEpoc*100)
pie3D(percentagesSurgeriesInEpoc, main = "Porcentaje de Cirugias en Epoc ",
      labels = c(paste("no Cirugia", percentagenonSurgeryInEpoc,"%"), paste("Cirugia", percentageOfSurgeryInEpoc*100,"%")),
      theta = 1,
      explode = 0.00)
percentagesAngioplastyInEpoc <- c(percentageOfAngioplastyInEpoc*100,percentagenonAngioplastyInEpoc)
pie3D(percentagesSurgeriesInEpoc, main = "Porcentaje de Angioplastias en Epoc ",
      labels = c(paste("Angioplastia",percentageOfAngioplastyInEpoc*100 ,"%"), paste("no Angioplastia", percentagenonAngioplastyInEpoc,"%")),
      theta = 1,
      explode = 0.00)

#pie charts obesidad
percentagenonAngioplastyInObesity<-100- percentageOfAngioplastyInObesity*100
percentagenonSurgeryInObesity<-100- percentageOfSurgeryInObesity*100

percentagesSurgeriesInObesity <- c(percentagenonSurgeryInObesity,percentageOfSurgeryInObesity*100)
pie3D(percentagesSurgeriesInObesity, main = "Porcentaje de Cirugias en Obesos Morbidos ",
      labels = c(paste("no Cirugia", percentagenonSurgeryInObesity,"%"), paste("Cirugia", percentageOfSurgeryInObesity*100,"%")),
      theta = 1,
      explode = 0.00)
percentagesAngioplastyInObesity <- c(percentageOfAngioplastyInObesity*100,percentagenonAngioplastyInObesity)
pie3D(percentagesSurgeriesInObesity, main = "Porcentaje de Angioplastias en Obesos Morbidos ",
      labels = c(paste("Angioplastia",round(percentageOfAngioplastyInObesity*100,digits = 2) ,"%"), paste("no Angioplastia",round( percentagenonAngioplastyInObesity,digits = 2),"%")),
      theta = 1,
      explode = 0.00)

#pie charts dialisis
percentagenonAngioplastyInDialisis<-100- percentageOfAngioplastyInDialisis*100
percentagenonSurgeryInDialisis<-100- percentageOfSurgeryInDialisis*100


percentagesSurgeriesInDialisis <- c(percentagenonSurgeryInDialisis,percentageOfSurgeryInDialisis*100)
pie3D(percentagesSurgeriesInDialisis, main = "Porcentaje de Cirugias en Dialisis ",
      labels = c(paste("no Cirugia",round( percentagenonSurgeryInDialisis,digits = 2),"%"), paste("Cirugia",round( percentageOfSurgeryInDialisis*100,digits = 2),"%")),
      theta = 1,
      explode = 0.00)
percentagesAngioplastyInDialisis <- c(percentageOfAngioplastyInDialisis*100,percentagenonAngioplastyInDialisis)
pie3D(percentagesSurgeriesInDialisis, main = "Porcentaje de Angioplastias en Dialisis ",
      labels = c(paste("Angioplastia",round(percentageOfAngioplastyInDialisis*100,digits = 2) ,"%"), paste("no Angioplastia",round( percentagenonAngioplastyInDialisis,digits = 2),"%")),
      theta = 1,
      explode = 0.00)

#8 promedio de complicaciones inmediatas y tardias con respecto a cada procedimiento

#En angioplastia usamos complicaciones angiplastia pq ninguno tenia complicaciones inmediata
percentageOfImmediateComplicationsAngioplasty <- prop.table(table(angioplasty$COMPLICACIONES.ANGIOPLASTIA), NULL)*100
percentageOfImmediateComplicationsAngioplasty

complicationsAngioplasty <- c(round(percentageOfImmediateComplicationsAngioplasty[1],digits = 2),round(100-percentageOfImmediateComplicationsAngioplasty[1],digits = 2))
pie3D(complicationsAngioplasty, main = "Complicaciones angioplastia",
      labels = c(paste("No complicaciones", complicationsAngioplasty[1],"%"), paste("Complicaciones", complicationsAngioplasty[2],"%")),
      theta = 1,
      explode = 0.00)

percentageOfImmediateComplicationsSurgery <- prop.table(table(surgery$COMPLICACIONES.INMEDIATAS), NULL)*100
percentageOfImmediateComplicationsSurgery

immediateComplicationsSurgery <- c(round(percentageOfImmediateComplicationsSurgery[1],digits = 2),round(100-percentageOfImmediateComplicationsSurgery[1],digits = 2))
pie3D(immediateComplicationsSurgery, main = "Complicaciones inmediatas cirugia",
      labels = c(paste("No complicaciones", immediateComplicationsSurgery[1],"%"), paste("Complicaciones", immediateComplicationsSurgery[2],"%")),
      theta = 1,
      explode = 0.00)

percentageOfLateComplicationsSurgery <- prop.table(table(surgery$COMPLICACIONES.TARDIAS), NULL)*100
percentageOfLateComplicationsSurgery

lateComplicationsSurgery <- c(round(percentageOfLateComplicationsSurgery[1],digits = 2),round(100-percentageOfLateComplicationsSurgery[1],digits = 2))
pie3D(lateComplicationsSurgery, main = "Complicaciones tardias cirugia",
      labels = c(paste("No complicaciones", lateComplicationsSurgery[1],"%"), paste("Complicaciones", lateComplicationsSurgery[2],"%")),
      theta = 1,
      explode = 0.00)

endovalve <- dataset[which(dataset$PROCEDIMIENTO == 'ENDOVALVULA'),]
percentageOfImmediateComplicationsEndovalve <- prop.table(table(endovalve$COMPLICACIONES.INMEDIATAS), NULL)*100
percentageOfImmediateComplicationsEndovalve

immediateComplicationsEndovalve <- c(round(percentageOfImmediateComplicationsEndovalve[1],digits = 2),round(100-percentageOfImmediateComplicationsEndovalve[1],digits = 2))
immediateComplicationsEndovalve[2] <- 0.00001
pie3D(immediateComplicationsEndovalve, main = "Complicaciones inmediatas endovalvula",
      labels = c(paste("No complicaciones", immediateComplicationsEndovalve[1],"%"), paste("Complicaciones", 0,"%")),
      theta = 1,
      explode = 0.00)

percentageOfLateComplicationsEndovalve <- prop.table(table(endovalve$COMPLICACIONES.TARDIAS), NULL)*100
percentageOfLateComplicationsEndovalve

lateComplicationsEndovalve <- c(round(percentageOfLateComplicationsEndovalve[1],digits = 2),round(100-percentageOfLateComplicationsEndovalve[1],digits = 2))
lateComplicationsEndovalve[2] <- 0.00001
pie3D(lateComplicationsEndovalve, main = "Complicaciones tardias endovalvula",
      labels = c(paste("No complicaciones", lateComplicationsEndovalve[1],"%"), paste("Complicaciones", 0,"%")),
      theta = 1,
      explode = 0.00)



