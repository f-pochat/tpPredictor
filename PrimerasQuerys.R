dataset <- read.csv("dataset.csv", sep=",")

## Analizar y contar datos

#Promedio de edad para epoc obesidad diabetes y diálisis
epoc <- dataset[which(dataset$EPOC == 1),]
ageAverageWithEpoc <- mean(epoc$EDAD)
ageAverageWithEpoc

obesity <- dataset[which(dataset$OBESIDAD.MORBIDA == 1),]
ageAverageWithObesity <- mean(obesity$EDAD)
ageAverageWithObesity

diabetes <- dataset[which(dataset$DIABETES == 1),]
ageAverageWithDiabetes <- mean(diabetes$EDAD)
ageAverageWithDiabetes

dialisis <- dataset[which(dataset$DIALISIS == 1),]
ageAverageWithDialisis <- mean(dialisis$EDAD)
ageAverageWithDialisis

#Porcentaje de Hombres y Mujeres y promedio de edad para cada una
numOfMale <- length(dataset$SEXO[which(dataset$SEXO == "MASC")])
numOfFemale <- length(dataset$SEXO[which(dataset$SEXO == "FEME")])
totalNum <- numOfFemale + numOfMale
percentageOfMale <- round(numOfMale / totalNum * 100, 2)
percentageOfFemale <- round(numOfFemale / totalNum * 100, 2)

ageAverageInMen <- mean(dataset[which(dataset$SEXO == "MASC"),]$EDAD)
ageAverageInWomen <- mean(dataset[which(dataset$SEXO == "FEME"),]$EDAD)

#Porcentaje de personas que van a angioplastia, cirugia o endovalvula

percentageByProcedure <- prop.table(table(dataset$PROCEDIMIENTO), NULL)*100
porcentageAngioplasty <- percentageByProcedure[1]
percentageSerguryAndEndovalve <- percentageByProcedure[3]
percentageEndovalve <- percentageByProcedure[4] + percentageSerguryAndEndovalve
percentageSurgery <- percentageByProcedure[2] + percentageSerguryAndEndovalve
porcentageAngioplasty
percentageEndovalve
percentageSurgery

## Cruzar variabeles

#Promedio de lesiones con respecto a pacientes con epoc, obesidad mórbida, diálisis o diabetes
injuryAverageWithEpoc <- mean(as.numeric(epoc$NUMERO.DE.LESIONES))
injuryAverageWithEpoc

injuryAverageWithObesity <- mean(as.numeric(obesity$NUMERO.DE.LESIONES))
injuryAverageWithObesity

injuryAverageWithDialisis <- mean(as.numeric(dialisis$NUMERO.DE.LESIONES))
injuryAverageWithDialisis

injuryAverageWithDiabetes <- mean(na.omit(as.numeric(diabetes$NUMERO.DE.LESIONES)))
injuryAverageWithDiabetes

#Porcentaje de pacientes masculinos y femeninos con con epoc, obesidad mórbida, diálisis o diabetes

                    #Masculino
menData <- dataset[which(dataset$SEXO == 'MASC'),]

percentageOfMenWithEpoc <- prop.table(table(menData$EPOC), NULL)*100
percentageOfMenWithEpoc

percentageOfMenWithObesity <- prop.table(table(menData$OBESIDAD.MORBIDA), NULL)*100
percentageOfMenWithObesity

percentageOfMenWithDialisis <- prop.table(table(menData$DIALISIS), NULL)*100
percentageOfMenWithDialisis

percentageOfMenWithDiabetes <- prop.table(table(menData$DIABETES), NULL)*100
percentageOfMenWithDiabetes

                    #Femenino
femaleData <- dataset[which(dataset$SEXO == 'FEME'),]

percentageOfWomenWithEpoc <-  prop.table(table(femaleData$EPOC), NULL)*100
percentageOfWomenWithEpoc

percentageOfWomenWithObesity <- prop.table(table(femaleData$OBESIDAD.MORBIDA), NULL)*100
percentageOfWomenWithObesity

percentageOfWomenWithDialisis <- prop.table(table(femaleData$DIALISIS), NULL)*100
percentageOfWomenWithDialisis

percentageOfWomenWithDiabetes <- prop.table(table(femaleData$DIABETES), NULL)*100
percentageOfWomenWithDiabetes

#Promedio de edad para cada procedimiento
surgery <- dataset[which(dataset$PROCEDIMIENTO == 'CIRUGIA'),]
ageAverageInSurgery <- mean(surgery$EDAD)

angioplasty <- dataset[which(dataset$PROCEDIMIENTO == 'ANGIOPLASTIA'),]
ageAverageInAngioplasty <- mean(angioplasty$EDAD)

## gooood cruzando variables

#Porcentaje de riesgo y no de riesgo por procedimiento (sin endovalvula)

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

#promedio de complicaciones inmediatas y tardias con respecto a cada procedimiento

#En angioplastia usamos complicaciones angiplastia pq ninguno tenia complicaciones inmediata
percentageOfImmediateComplicationsAngioplasty <- prop.table(table(angioplasty$COMPLICACIONES.ANGIOPLASTIA), NULL)*100
percentageOfImmediateComplicationsAngioplasty

percentageOfImmediateComplicationsSurgery <- prop.table(table(surgery$COMPLICACIONES.INMEDIATAS), NULL)*100
percentageOfImmediateComplicationsSurgery

percentageOfLateComplicationsSurgery <- prop.table(table(surgery$COMPLICACIONES.TARDIAS), NULL)*100
percentageOfLateComplicationsSurgery

endovalve <- dataset[which(dataset$PROCEDIMIENTO == 'ENDOVALVULA'),]
percentageOfImmediateComplicationsEndovalve <- prop.table(table(endovalve$COMPLICACIONES.INMEDIATAS), NULL)*100
percentageOfImmediateComplicationsEndovalve

percentageOfLateComplicationsEndovalve <- prop.table(table(endovalve$COMPLICACIONES.TARDIAS), NULL)*100
percentageOfLateComplicationsEndovalve





