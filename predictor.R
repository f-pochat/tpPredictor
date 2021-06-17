dataset <- read.csv("dataset.csv", sep=",")

## 70% of the sample size
smp_size <- floor(0.7 * nrow(dataset))

## set the seed to make your partition reproducible
set.seed(123) 
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
train_ind <- train_ind[which(train)]

train <- dataset[train_ind, ]
test <- dataset[-train_ind, ]


hyp.out <- glm(PROCEDIMIENTO~EDAD+VALVULOPATIA, data= train, family="binomial")
summary(hyp.out)

#edad, motivo de ingreso, distintos factores de riesgo, numero de lesiones
#---------------------------------------------------------------------------------

testIndio <- test[which(test$hispan_i != "12 Not Hispanic/Spanish origin"),]
predDat <- with(testIndio,
                expand.grid(age_p = c(25,50),
                            sex = "1 Male",
                            bmi = mean(bmi,na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predecir

preds <- predict(hyp.out, type = "response",
                 se.fit = TRUE, interval="confidence",
                 newdata = predDat)
cbind(predDat, preds)