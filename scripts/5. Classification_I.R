############################################################
#         Problem Set 3. Predicting Poverty
#           Big data and Machine Learning
#             Universidad de los Andes
##########################################################

#Authors:

#- Lucia Fillippo
#- Miguel Angel Victoria Simbaqueva 
#- Irina Andrea Vélez López  
#- Daniel Casas Bautista  

# Initial configuration ---------------------------------------------------

rm(list = ls())

path_sript <- rstudioapi::getActiveDocumentContext()$path
path_folder <- dirname(path_sript)
setwd(path_folder)

# Libraries
require(pacman)
library(tidyverse)

p_load(tidyverse,
       rio,
       sf,
       rstudioapi,
       stargazer,
       glmnet,
       MLmetrics,
       caret)


# Importing from Github
test_hogares <- readRDS("../stores/test_hogares.rds")
p_train_hogares <- readRDS("../stores/p_train_hogares.rds")

#############################################################################
###########################     Clasificación     ###########################
#############################################################################


table(p_train_hogares$pobre)

library(stargazer)

p_train_hogares<-p_train_hogares %>% mutate(Pobre_dummy=factor(pobre,levels=c(1,0), labels=c("Si", "No")))
table(p_train_hogares$Pobre_dummy)    #Aquí vemos 33.024 pobres y 131.936 no pobres

#Dividimos la base de train para obtener: tres bases que utilizaremos luego
#Una mini train, una mini test y la de evaluación para calcular el ROC

require(caret)
set.seed(10101)
Split_1 <- createDataPartition(p_train_hogares$pobre, p = .7) [[1]]
length(Split_1)

### Mini train
other_ <- p_train_hogares[-Split_1,]
p_train_hogares_mini<- p_train_hogares[ Split_1,] #Base mini train
#Esta partición nos deja 49.488 observaciones


set.seed(10101)
### ROC
Split_2<- createDataPartition(other_$pobre, p = 1/3) [[1]]
Evaluation_H <- other_[ Split_2,] #Base evaluacion para ROC
#Esta partición nos deja 16.496 observaciones

### Minitest
Testing_H <- other_[-Split_2,] #Base mini test
#Esta partición nos deja 32.992 observaciones


#Se comprueba la proporción de la variable  "Pobre" en la base de datos
prop.table(table(p_train_hogares_mini$Pobre_dummy)) #Para la base mini train
#0.2006201 (SI) y 0.7993799 (NO)
prop.table(table(Evaluation_H$Pobre_dummy))         #Para la base ROC
#0.2016853 (SI) y 0.7983147 (NO)
prop.table(table(Testing_H$Pobre_dummy))            #Para la base mini test
#0.1979571 (SI) y 0.8020429 (NO)


#Se realiza el K-fold como método de control del modelo

Varios_parametros<-function(...)c(twoClassSummary(...), defaultSummary(...))

ctrl_def_modg <- trainControl(method = "cv",
                              number = 5,
                              summaryFunction = Varios_parametros,
                              classProbs = TRUE,
                              verbose=FALSE,
                              savePredictions = T)


############################################################
##########      Con la Mini Train (p_train_hogares_mini)
############################################################


##### Las variables que vamos a utilizar en el modelo son:

#Se realiza el modelo de clasificacón con la base de control 
#El modelo contiene las siguientes variables
#Número de personas por unidad de gasto (Npersug)
#Línea de pobreza (Lp)
#Tipo de vivienda (Tipo_vivienda)
#Dominio/Ciudad (Dominio)
#Número de cuartos (Num_cuartos)


###LOGIT

set.seed(10101)
logit_caret_modg <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos,
  data = p_train_hogares_mini,
  method = "glm", #Para logit
  trControl = ctrl_def_modg,
  family = "binomial",
  preProcess = c("center", "scale"))

logit_caret_modg



### LASSO
#Prueba tomando como métrica la Sensibilidad


#Lambdas para Lasso
lambdas <- 10^seq(-4, 0.01, length = 200)

set.seed(10101)
logit_lasso_acc <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos,
  data = p_train_hogares_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale"))

logit_lasso_acc
#lambda = 0.009435694

### LASSO
#Prueba tomando como métrica el ROC

set.seed(10101)
logit_lasso_roc <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos,
  data = p_train_hogares_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale"))

logit_lasso_roc    #0.009883815

#Calcularemos la regla para realizar la clasificación (Cut off)

Eval_Resultados <- data.frame(Pobre = Evaluation_H$Pobre_dummy)
Eval_Resultados$Roc <- predict(logit_lasso_roc,
                               newdata = Evaluation_H,
                               type = "prob")[,1]

library(pROC)
#Se calcula el ROC para la regresión
rf_ROC <- roc(Eval_Resultados$Pobre, Eval_Resultados$Roc, levels = rev(levels(Eval_Resultados$Pobre)))

rf_ROC      #En este caso la curva ROC El 0.7758

#La AUC se encuentra en un rango de 0 a 1, donde un valor de 0.5 indica que el modelo no es 
#mejor que una clasificación aleatoria, mientras que un valor cercano a 1 indica un buen poder
#de discriminación (el modelo es capaz de distinguir efectivamente entre las clases positiva y
#negativa). En este caso, el valor de AUC de 0.7758 sugiere que el modelo tiene una capacidad
#razonable para distinguir entre las dos clases.

#Se calcula el Cut off
rf_Thresh <- coords(rf_ROC, x = "best", best.method = "closest.topleft")
rf_Thresh

#En resumen, el resultado muestra que utilizando un umbral de clasificación de 0.1945946, 
#el modelo de clasificación binaria alcanzó una especificidad de 0.6925714 (tasa de verdaderos
#negativos) y una sensibilidad de 0.721151 (tasa de verdaderos positivos). Estas métricas son
#importantes para evaluar el rendimiento del modelo, ya que indican qué tan bien el modelo 
#puede clasificar correctamente las instancias de ambas clases (positivas y negativas). 
#Un alto valor de sensibilidad y especificidad es deseable para un buen modelo de clasificación

#Se evalúan los resultados
Eval_Resultados<-Eval_Resultados %>% mutate(hat_def_05=ifelse(Eval_Resultados$Roc>0.5,"Si","No"),
                                            hat_def_rf_Thresh=ifelse(Eval_Resultados$Roc>rf_Thresh$threshold,"Si","No"))

#Cuando el threshold es igual a 0.5 (regla de Bayes)
with(Eval_Resultados,table(Pobre,hat_def_05))
#Cuando el threshold es obtenido del ROC
with(Eval_Resultados,table(Pobre,hat_def_rf_Thresh))


#Se realiza evaluación de los resultados del modelo de clasificación binaria utilizando 
#diferentes umbrales de clasificación para dos umbrales de clasificación diferentes: 
#0.5 (regla de Bayes) y el umbral obtenido de la curva ROC

#Las tablas de contingencia muestran:
#Pobre    No    Si
#Si  2769   602
#No 12819   306

############################################################
##########      Logit Lasso Up-sampling
############################################################


#Up-sampling
set.seed(10101)
upSampled_Train_H <- upSample(x = p_train_hogares_mini,
                              y = p_train_hogares_mini$Pobre_dummy,
                              ## Mantener la variable de clasificación con el mismo nombre:
                              yname = "Pobre_dummy")

dim(upSampled_Train_H)
table(upSampled_Train_H$Pobre_dummy)

set.seed(10101)
logit_lasso_upsample <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos,
  data = upSampled_Train_H,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale")
)
logit_lasso_upsample
#0.01367666


############################################################
##########      Logit Lasso Down-sampling
############################################################


set.seed(10101)
downSampled_Train_H <- downSample(x = p_train_hogares_mini,
                                  y = p_train_hogares_mini$Pobre_dummy,
                                  ## keep the class variable name the same:
                                  yname = "Pobre_dummy")

table(downSampled_Train_H$Pobre_dummy)

set.seed(10101)
logit_lasso_downsample <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos,
  data = downSampled_Train_H,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale")
)

logit_lasso_downsample     
#0.01305657



predictors<-c("Npersug","Lp","Tipo_vivienda", "Dominio","Num_cuartos") 

head(p_train_hogares_mini[predictors])
testResults <- data.frame(Pobre = Testing_H$Pobre_dummy)
testResults$logit<- predict(logit_caret_modg,
                            newdata = Testing_H, 
                            type = "prob")[,1]
testResults$lasso<- predict(logit_lasso_roc,
                            newdata = Testing_H,
                            type = "prob")[,1]
testResults$lasso_thresh<- predict(logit_lasso_roc,
                                   newdata = Testing_H,
                                   type = "prob")[,1]
testResults$lasso_upsample<- predict(logit_lasso_upsample,
                                     newdata = Testing_H,
                                     type = "prob")[,1]
testResults$mylogit_lasso_downsample<- predict(logit_lasso_downsample,
                                               newdata = Testing_H,
                                               type = "prob")[,1]

testResults<-testResults %>%
  mutate(logit=ifelse(logit>0.5,"Si","No"),
         lasso_SENS=ifelse(lasso>0.5,"Si","No"),
         lasso_ROC=ifelse(lasso_thresh>rf_Thresh$threshold,"Si","No"),
         lasso_upsample=ifelse(lasso_upsample>0.5,"Si","No"),
         mylogit_lasso_downsample=ifelse(mylogit_lasso_downsample>0.5,"Si","No")
  )

with(testResults,table(Pobre,logit))
with(testResults,table(Pobre,lasso_SENS))
with(testResults,table(Pobre,lasso_ROC))
with(testResults,table(Pobre,lasso_upsample))
with(testResults,table(Pobre,mylogit_lasso_downsample))








