library(randomForest)
library(kableExtra)
library(ROCR)
dataset <- read.csv("dataset.csv", sep=",")


## 70% of the sample size
smp_size <- floor(0.7 * nrow(dataset))

## set the seed to make your partition reproducible

dataset3 <- subset(dataset,dataset$MOTIVO.DE.INGRESO == "PROG VALV" |
                     dataset$MOTIVO.DE.INGRESO == "PROG CORONARIO" |
                     dataset$MOTIVO.DE.INGRESO == "SCA no SST" |
                     dataset$MOTIVO.DE.INGRESO == "PROGRAMADO TAVI")

dataset2 <- subset(dataset3, dataset3$PROCEDIMIENTO != "CIRUGIA, ENDOVALVULA")
dataset2 <- subset(dataset2, dataset2$PROCEDIMIENTO != "ENDOVALVULA")
smp_size <- floor(0.7 * nrow(dataset2))

## set the seed to make your partition reproducible


dataset2$EDAD <- as.numeric(dataset2$EDAD)
dataset2$EDAD <- cut(dataset2$EDAD,c(0,30,55,100),labels = c("young adults","adults","elderly"))
dataset2$EDAD <- as.factor(dataset2$EDAD)

dataset2$NUMERO.DE.LESIONES <- as.numeric(dataset2$NUMERO.DE.LESIONES)
dataset2$NUMERO.DE.LESIONES[is.na(dataset2$NUMERO.DE.LESIONES)]<- 1000
dataset2$NUMERO.DE.LESIONES <- as.factor(dataset2$NUMERO.DE.LESIONES)

dataset2$PROCEDIMIENTO <- as.factor(dataset2$PROCEDIMIENTO)
dataset2$NUMERO.DE.LESIONES <- as.factor(dataset2$NUMERO.DE.LESIONES)
dataset2$EDAD <- as.factor(dataset2$EDAD)
dataset2$MOTIVO.DE.INGRESO <- as.factor(dataset2$MOTIVO.DE.INGRESO)
dataset2$DIABETES <- as.factor(dataset2$DIABETES)
dataset2$EPOC <- as.factor(dataset2$EPOC)
dataset2$DIALISIS <- as.factor(dataset2$DIALISIS)
dataset2$OBESIDAD.MORBIDA <- as.factor(dataset2$OBESIDAD.MORBIDA)

set.seed(-123)
train_ind <- sample(seq_len(nrow(dataset2)), size = smp_size)

train <- dataset2[train_ind, ]
test <- dataset2[-train_ind, ]

modelo<-randomForest(PROCEDIMIENTO~EDAD+DIALISIS+MOTIVO.DE.INGRESO+OBESIDAD.MORBIDA+DIABETES+EPOC+NUMERO.DE.LESIONES,
                     data=train,mtry=3,na.action =na.roughfix)

predictor<-predict(modelo,test,type = "class")
predictor
table <-table(test$PROCEDIMIENTO,predictor)
round(sum(diag(table))/sum(table)*100,2)
table %>%
  kbl() %>%
  kable_material(c("striped", "hover"))

plotROC <- function(pred){
  perf<- performance(pred,"tpr","fpr")
  plot(perf)
  AUC<-performance(pred,"auc")@y.values[[1]]
  grid()
  text(.6,.2,sprintf("AUC=%0.3f", AUC))
  abline(0,1,col="red", lty = 2)
}

predaux<-prediction(as.numeric(predictor),test$PROCEDIMIENTO)
perf <- performance(predaux,"auc")
perf@y.values[[1]]

plotROC(predaux)
