dataset <- read.csv("dataset.csv", sep=",")
library(ggplot2)
library(plotrix)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(tibble)
library(plotly)
library(gganimate)
library(VennDiagram)
library(RColorBrewer)
library("ggVennDiagram")
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

riskFactors <- dataset[which(dataset$EPOC == 1 | dataset$OBESIDAD.MORBIDA == 1 | dataset$DIABETES == 1 | dataset$DIALISIS == 1),]
riskFactors <- riskFactors[,c(2,17:20)]

riskFactors$EPOC <- factor(riskFactors$EPOC, levels = c(0,1), labels = c(0, "EPOC"))
riskFactors$DIABETES <- factor(riskFactors$DIABETES, levels = c(0,1), labels = c(0, "DIABETES"))
riskFactors$DIALISIS <- factor(riskFactors$DIALISIS, levels = c(0,1), labels = c(0, "DIALISIS"))
riskFactors$OBESIDAD.MORBIDA <- factor(riskFactors$OBESIDAD.MORBIDA, levels = c(0,1), labels = c(0, "OBESIDAD.MORBIDA"))
riskFactors <- na_if(riskFactors,0)
riskFactors <- mutate(riskFactors, factors = paste(riskFactors$EPOC,riskFactors$DIABETES,riskFactors$DIALISIS,riskFactors$OBESIDAD.MORBIDA, sep = " & "))
riskFactors$factors <- gsub(" & NA" , "",riskFactors$factors)
riskFactors$factors <- gsub("NA &" , "",riskFactors$factors)
riskFactors$factors <- gsub(" & NA & ","", riskFactors$factors)
riskFactors <- riskFactors[which(riskFactors$factors != "EPOC & DIABETES"),]
riskFactors <- riskFactors[which(riskFactors$factors != " DIABETES & OBESIDAD.MORBIDA"),]
riskFactors <- riskFactors[which(riskFactors$factors != "EPOC & DIABETES & OBESIDAD.MORBIDA"),]
riskFactors <- riskFactors[which(riskFactors$factors != "EPOC & OBESIDAD.MORBIDA"),]

View(riskFactors)

ggplot(riskFactors, aes(x = EDAD, fill = factors)) +
  geom_density(alpha = 0.4) +
  scale_fill_discrete(name = "Factors") +
  theme(legend.position = "top")

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
p <- ggplot(dataset, aes(x = EDAD, fill = SEXO)) +
  geom_density(alpha = 0.4) +
  scale_fill_discrete(name = "Gender", labels = c("Female","Male")) +
  theme(legend.position = "top")+
  geom_vline(data=dataset, aes(xintercept=ageAverageInMen, color = "Hombres"),
               linetype="dashed")+
  geom_vline(data=dataset, aes(xintercept=ageAverageInWomen, color = "Mujeres"),
             linetype="dashed")

p <- ggplotly(p)
p
#3 Porcentaje de personas que van a angioplastia, cirugia o endovalvula
peopleByProcedure <- dataset[which(dataset$PROCEDIMIENTO != "CIRUGIA, ENDOVALVULA"),] 
percentageByProcedure <- prop.table(table(peopleByProcedure$PROCEDIMIENTO), NULL)*100
percentageByProcedureArray <- c(percentageByProcedure[1],percentageByProcedure[2],percentageByProcedure[3])
pie3D(percentageByProcedureArray, main = "Percentage by procedure",
      labels = c(paste("Angioplastia", round(percentageByProcedure[1],digits=2),"%"), paste("Surgery", round(percentageByProcedure[2],digits=2),"%"), paste("Endovalvula", round(percentageByProcedure[3],digits=2),"%")),
      theta = 1,
      explode = 0.00)

## Cruzar variabeles

#4 Promedio de lesiones con respecto a pacientes con epoc, obesidad mórbida, diálisis o diabetes
#cambiar a grafico de densidad
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
#cambiar a diagrama de venn, uno masculino y otro femenino

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

#Venn

myCol <- c("#dbe2ef","#3f72af","#112d4e")

# Chart
venn.diagram(
  x = list(
    DIABETES = dataset$ï..[which(dataset$DIABETES == 1 & dataset$SEXO == "MASC")],
    OBESITY = dataset$ï..[which(dataset$OBESIDAD.MORBIDA == 1 & dataset$SEXO == "MASC")],
    EPOC = dataset$ï..[which(dataset$EPOC == 1 & dataset$SEXO == "MASC")]),
  category.names = c("DIABETES" , "OBESITY" , "COPD"),
  filename = 'venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)
venn.diagram(
  x = list(
    DIABETES = dataset$ï..[which(dataset$DIABETES == 1 & dataset$SEXO == "FEME")],
    OBESITY = dataset$ï..[which(dataset$OBESIDAD.MORBIDA == 1 & dataset$SEXO == "FEME")],
    EPOC = dataset$ï..[which(dataset$EPOC == 1 & dataset$SEXO == "FEME")]),
  category.names = c("DIABETES" , "OBESITY","COPD"),
  filename = 'venn_diagramm2.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)
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
#cambiar a densidad
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


diabetes7 <- c(percentageOfAngioplastyInDiabetic*100,percentageOfSurgeryInDiabetic*100)
dialisis7 <- c(percentageOfAngioplastyInDialisis*100,percentageOfSurgeryInDialisis*100)
epoc7 <- c(percentageOfAngioplastyInEpoc*100,percentageOfSurgeryInEpoc*100)
obesity7<-c(percentageOfAngioplastyInObesity*100,percentageOfSurgeryInObesity*100)

diabetesBar <- barplot(diabetes7,ylab="percentage of patients with diabetes who go to each procedure" , names=c("Angioplasty","Surgery"),col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6)),ylim=c(0,100))
text(diabetesBar, labels=paste("%", c(round(diabetes7,2))), y=diabetes7+5)

dialisisBar <- barplot(dialisis7,ylab="percentage of patients with dialisis who go to each procedure" , names=c("Angioplasty","Surgery"),col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6)),ylim=c(0,100))
text(dialisisBar, labels=paste("%", c(round(dialisis7,2))), y=dialisis7+5)

epocBar <- barplot(epoc7,ylab="percentage of patients with epoc who go to each procedure" , names=c("Angioplasty","Surgery"),col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6)),ylim=c(0,100))
text(epocBar, labels=paste("%", c(round(epoc7,2))), y=epoc7+5)

obesityBar <- barplot(obesity7,ylab="percentage of patients with morbid obesity who go to each procedure" , names=c("Angioplasty","Surgery"),col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6)),ylim=c(0,100))
text(obesityBar, labels=paste("%", c(round(obesity7,2))), y=obesity7+5)

#8 promedio de complicaciones inmediatas y tardias con respecto a cada procedimiento
#cambiar a un solo pie chart solo con los que tuvieron complicaciones  

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


#8 NUEVA
# un pie con angioplastia que divida entre los motivos de ingreso
# un pie con cirugia que divida entre los motivos de ingreso


#prog coronaria -> angioplatia la mayoria y aveces cirugia
#prog  valv -> Siempre cirugia
#prog tavi -> endovalvula siempre
#sca no sst

angioplastyPatients <- dataset[which(dataset$PROCEDIMIENTO == "ANGIOPLASTIA"),]
angioplastyCoronaryNumber <- length(angioplastyPatients$MOTIVO.DE.INGRESO[which(angioplastyPatients$MOTIVO.DE.INGRESO == "PROG CORONARIO")])
#Todos los de angioplastia vienen de prog coronario

surgeryPatients <- dataset[which(dataset$PROCEDIMIENTO == "CIRUGIA"), ]
surgeryCoronaryNumber <- length(surgeryPatients$MOTIVO.DE.INGRESO[which(surgeryPatients$MOTIVO.DE.INGRESO == "PROG CORONARIO")])
surgeryValvNumber <- length(surgeryPatients$MOTIVO.DE.INGRESO[which(surgeryPatients$MOTIVO.DE.INGRESO == "PROG VALV")])
surgerySCANumber <- length(surgeryPatients$MOTIVO.DE.INGRESO[which(surgeryPatients$MOTIVO.DE.INGRESO == "SCA no SST")])
surgeryOther <- length(surgeryPatients$MOTIVO.DE.INGRESO[which(surgeryPatients$MOTIVO.DE.INGRESO != "PROG CORONARIO" & surgeryPatients$MOTIVO.DE.INGRESO != "PROG VALV" & surgeryPatients$MOTIVO.DE.INGRESO != "SCA no SST")])
totalNumSurgery <- surgeryCoronaryNumber + surgeryValvNumber + surgerySCANumber + surgeryOther

percentageOfSurgeryCoronary <- round(surgeryCoronaryNumber / totalNumSurgery * 100, 2)
percentageOfSurgeryValv <- round(surgeryValvNumber / totalNumSurgery * 100, 2)
percentageOfSurgerySCA <- round(surgerySCANumber / totalNumSurgery * 100, 2)
percentageOfSurgeryOther <- round(surgeryOther / totalNumSurgery * 100, 2)
percentagesSurgery <- c(percentageOfSurgeryCoronary, percentageOfSurgeryValv, percentageOfSurgerySCA, percentageOfSurgeryOther)

#Pie surgery
progsAngioplasty <- c("Prog coronaria", "Prog valv", "SCA no SST", "Other")
dataSurgery <- data.frame(progsAngioplasty, percentagesSurgery)
colors <- c('rgb(219, 226, 239))', 'rgb(63, 114, 175))')
fig <- plot_ly(data = df, labels = ~progsAngioplasty, values = ~percentagesSurgery, type = 'pie',marker = list(colors = colors))
fig <- fig %>% layout(
  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



#Pie
progsAngioplasty <- c("Prog coronaria", "Prog valv", "Prog tavi", "SCA no SST", "Other")
df <- data.frame(sex,percentages)
colors <- c('rgb(219, 226, 239))', 'rgb(63, 114, 175))')
fig <- plot_ly(data = df, labels = ~sex, values = ~percentages, type = 'pie',marker = list(colors = colors))
fig <- fig %>% layout(
  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



