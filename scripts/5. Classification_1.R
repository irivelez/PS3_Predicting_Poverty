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


# Importing DBs from Github ----
test_hogares <- readRDS("../stores/test_hogares.rds")
p_train_hogares <- readRDS("../stores/p_train_hogares.rds")

ilasso_test_hogares <- readRDS("../stores/ilasso_test_hogares.rds")
i_train_hogares <- readRDS("../stores/i_test_hogares.rds")

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
##########      Análisis con el Modelo 1
############################################################


##### Las variables que vamos a utilizar en el modelo son:

#Se realiza el modelo de clasificacón con la base de control 
#El modelo contiene las siguientes variables
#Número de personas por unidad de gasto (Npersug)
#Línea de pobreza (Lp)
#Tipo de vivienda (Tipo_vivienda)
#Dominio/Ciudad (Dominio)
#Número de cuartos (Num_cuartos)


###LOGIT -----

set.seed(10101)
logit1 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos,
  data = p_train_hogares_mini,
  method = "glm", #Para logit
  trControl = ctrl_def_modg,
  family = "binomial",
  preProcess = c("center", "scale"))

logit1


### LASSO -----
#Prueba tomando como métrica la Sensibilidad


#Lambdas para Lasso
lambdas <- 10^seq(-4, 0.01, length = 200)

set.seed(10101)
lasso_sens1 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos,
  data = p_train_hogares_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale"))

lasso_sens1
#lambda = 0.009435694

### LASSO
#Prueba tomando como métrica el ROC

set.seed(10101)
lasso_roc1 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos,
  data = p_train_hogares_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale"))

lasso_roc1    
#0.009883815

#Calcularemos la regla para realizar la clasificación (Cut off)

Eval_Resultados <- data.frame(Pobre = Evaluation_H$Pobre_dummy)
Eval_Resultados$Roc <- predict(lasso_roc1,
                               newdata = Evaluation_H,
                               type = "prob")[,1]

library(pROC)
#Se calcula el ROC para la regresión
rf_ROC <- roc(Eval_Resultados$Pobre, Eval_Resultados$Roc, levels = rev(levels(Eval_Resultados$Pobre)))

rf_ROC
#En este caso la curva ROC El 0.7799

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
lasso_upsample1 <- upSample(x = p_train_hogares_mini,
                              y = p_train_hogares_mini$Pobre_dummy,
                              ## Mantener la variable de clasificación con el mismo nombre:
                              yname = "Pobre_dummy")

dim(lasso_upsample1)
table(lasso_upsample1$Pobre_dummy)

set.seed(10101)
lasso_upsample1 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos,
  data = lasso_upsample1,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale")
)
lasso_upsample1
#0.01367666


############################################################
##########      Logit Lasso Down-sampling
############################################################


set.seed(10101)
lasso_downsample1 <- downSample(x = p_train_hogares_mini,
                                  y = p_train_hogares_mini$Pobre_dummy,
                                  ## keep the class variable name the same:
                                  yname = "Pobre_dummy")

table(lasso_downsample1$Pobre_dummy)

set.seed(10101)
lasso_downsample1 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos,
  data = lasso_downsample1,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale")
)

lasso_downsample1
#0.01305657


############################################################
##########                Elastic net
############################################################


set.seed(10101)

elasticnet1 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos,
  data = p_train_hogares_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  preProcess = c("center", "scale")
)

elasticnet1
#lambda = 0.01921286

##############################################
#########      Tablas de contingencia
##############################################

predictors<-c("Npersug","Lp","Tipo_vivienda", "Dominio","Num_cuartos") 

head(p_train_hogares_mini[predictors])
testResults <- data.frame(Pobre = Testing_H$Pobre_dummy)

testResults$logit1<- predict(logit1,
                             newdata = Testing_H, 
                             type = "prob")[,1]
testResults$lasso_sens1<- predict(lasso_sens1,
                                  newdata = Testing_H,
                                  type = "prob")[,1]
testResults$lasso_roc1<- predict(lasso_roc1,
                                 newdata = Testing_H,
                                 type = "prob")[,1]
testResults$lasso_upsample1<- predict(lasso_upsample1,
                                      newdata = Testing_H,
                                      type = "prob")[,1]
testResults$lasso_downsample1<- predict(lasso_downsample1,
                                        newdata = Testing_H,
                                        type = "prob")[,1]
testResults$elasticnet1<- predict(elasticnet1,
                                  newdata = Testing_H,
                                  type = "prob")[,1]


testResults<-testResults %>%
  mutate(logit1=ifelse(logit1>0.5,"Si","No"),
         lasso_sens1=ifelse(lasso_sens1>0.5,"Si","No"),
         lasso_roc1=ifelse(lasso_roc1>0.5,"Si","No"),
         lasso_upsample1=ifelse(lasso_upsample1>0.5,"Si","No"),
         lasso_downsample1=ifelse(lasso_downsample1>0.5,"Si","No"),
         elasticnet1=ifelse(elasticnet1>0.5,"Si","No")
  )


#Tabla de contingencia para el modelo 1
with(testResults,table(Pobre,logit1))
with(testResults,table(Pobre,lasso_sens1))
with(testResults,table(Pobre,lasso_roc1))
with(testResults,table(Pobre,lasso_upsample1))
with(testResults,table(Pobre,lasso_downsample1))
with(testResults,table(Pobre,elasticnet1))



############################################################
##########      Análisis con el Modelo 2
############################################################


##### Las variables que vamos a utilizar en el modelo son:

#Se realiza el modelo de clasificacón con la base de control 
#El modelo contiene las siguientes variables
#Número de personas por unidad de gasto (Npersug)
#Línea de pobreza (Lp)
#Tipo de vivienda (Tipo_vivienda)
#Dominio/Ciudad (Dominio)
#Número de cuartos (Num_cuartos)

#Máximo nivel de educación (max_educ_h)
#Número de habitaciones (P5010)
#Número de personas por cuarto (Num_personas_cuarto)



###LOGIT

set.seed(10101)
logit2 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos+
    max_educ_h+P5010+Num_personas_cuarto,
  data = p_train_hogares_mini,
  method = "glm", #Para logit
  trControl = ctrl_def_modg,
  family = "binomial",
  preProcess = c("center", "scale"))

logit2


### LASSO
#Prueba tomando como métrica la Sensibilidad


#Lambdas para Lasso
lambdas <- 10^seq(-4, 0.01, length = 200)

set.seed(10101)
lasso_sens2 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos+
    max_educ_h+P5010+Num_personas_cuarto,
  data = p_train_hogares_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale"))

lasso_sens2
#lambda = 0.0124646

### LASSO
#Prueba tomando como métrica el ROC

set.seed(10101)
lasso_roc2 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos+
    max_educ_h+P5010+Num_personas_cuarto,
  data = p_train_hogares_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale"))

lasso_roc2    
#0.01189947

#Calcularemos la regla para realizar la clasificación (Cut off)

Eval_Resultados <- data.frame(Pobre = Evaluation_H$Pobre_dummy)
Eval_Resultados$Roc <- predict(lasso_roc2,
                               newdata = Evaluation_H,
                               type = "prob")[,1]

library(pROC)
#Se calcula el ROC para la regresión
rf_ROC <- roc(Eval_Resultados$Pobre, Eval_Resultados$Roc, levels = rev(levels(Eval_Resultados$Pobre)))

rf_ROC
#En este caso la curva ROC El 0.8148

#La AUC se encuentra en un rango de 0 a 1, donde un valor de 0.5 indica que el modelo no es 
#mejor que una clasificación aleatoria, mientras que un valor cercano a 1 indica un buen poder
#de discriminación (el modelo es capaz de distinguir efectivamente entre las clases positiva y
#negativa). En este caso, el valor de AUC de 0.8148 sugiere que el modelo tiene una capacidad
#razonable para distinguir entre las dos clases.

#Se calcula el Cut off
rf_Thresh <- coords(rf_ROC, x = "best", best.method = "closest.topleft")
rf_Thresh

#En resumen, el resultado muestra que utilizando un umbral de clasificación de 0.1992515, 
#el modelo de clasificación binaria alcanzó una especificidad de 0.6925714 (tasa de verdaderos
#negativos) y una sensibilidad de 0.7406072 (tasa de verdaderos positivos). Estas métricas son
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
lasso_upsample2 <- upSample(x = p_train_hogares_mini,
                            y = p_train_hogares_mini$Pobre_dummy,
                            ## Mantener la variable de clasificación con el mismo nombre:
                            yname = "Pobre_dummy")

dim(lasso_upsample2)
table(lasso_upsample2$Pobre_dummy)

set.seed(10101)
lasso_upsample2 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos+
    max_educ_h+P5010+Num_personas_cuarto,
  data = lasso_upsample2,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale")
)
lasso_upsample2
#0.01571926


############################################################
##########      Logit Lasso Down-sampling
############################################################


set.seed(10101)
lasso_downsample2 <- downSample(x = p_train_hogares_mini,
                                y = p_train_hogares_mini$Pobre_dummy,
                                ## keep the class variable name the same:
                                yname = "Pobre_dummy")

table(lasso_downsample2$Pobre_dummy)

set.seed(10101)
lasso_downsample2 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos+
    max_educ_h+P5010+Num_personas_cuarto,
  data = lasso_downsample2,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale")
)

lasso_downsample2
#0.01571926


############################################################
##########                Elastic net
############################################################


set.seed(10101)

elasticnet2 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos+
    max_educ_h+P5010+Num_personas_cuarto,
  data = p_train_hogares_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  preProcess = c("center", "scale")
)

elasticnet2
#lambda = 0.002504042

##############################################
#########      Tablas de contingencia
##############################################

predictors<-c("Npersug","Lp","Tipo_vivienda", "Dominio","Num_cuartos") 

head(p_train_hogares_mini[predictors])
testResults <- data.frame(Pobre = Testing_H$Pobre_dummy)

testResults$logit2<- predict(logit2,
                             newdata = Testing_H, 
                             type = "prob")[,1]
testResults$lasso_sens2<- predict(lasso_sens2,
                                  newdata = Testing_H,
                                  type = "prob")[,1]
testResults$lasso_roc2<- predict(lasso_roc2,
                                 newdata = Testing_H,
                                 type = "prob")[,1]
testResults$lasso_upsample2<- predict(lasso_upsample2,
                                      newdata = Testing_H,
                                      type = "prob")[,1]
testResults$lasso_downsample2<- predict(lasso_downsample2,
                                        newdata = Testing_H,
                                        type = "prob")[,1]
testResults$elasticnet2<- predict(elasticnet2,
                                  newdata = Testing_H,
                                  type = "prob")[,1]


testResults<-testResults %>%
  mutate(logit2=ifelse(logit2>0.5,"Si","No"),
         lasso_sens2=ifelse(lasso_sens2>0.5,"Si","No"),
         lasso_roc2=ifelse(lasso_roc2>0.5,"Si","No"),
         lasso_upsample2=ifelse(lasso_upsample2>0.5,"Si","No"),
         lasso_downsample2=ifelse(lasso_downsample2>0.5,"Si","No"),
         elasticnet2=ifelse(elasticnet2>0.5,"Si","No")
  )


#Tabla de contingencia para el modelo 2
with(testResults,table(Pobre,logit2))
with(testResults,table(Pobre,lasso_sens2))
with(testResults,table(Pobre,lasso_roc2))
with(testResults,table(Pobre,lasso_upsample2))
with(testResults,table(Pobre,lasso_downsample2))
with(testResults,table(Pobre,elasticnet2))


############################################################
##########      Análisis con el Modelo 3
############################################################


##### Las variables que vamos a utilizar en el modelo son:

#Se realiza el modelo de clasificacón con la base de control 
#El modelo contiene las siguientes variables
#Número de personas por unidad de gasto (Npersug)
#Línea de pobreza (Lp)
#Tipo de vivienda (Tipo_vivienda)
#Dominio/Ciudad (Dominio)
#Número de cuartos (Num_cuartos)

#Máximo nivel de educación (max_educ_h)
#Número de habitaciones (P5010)
#Número de personas por cuarto (Num_personas_cuarto)

#Acceso a salud: (max_health_h)
#Edad promedio: (edad_prom_h)
#Horas trabajadas: (horastrab_prom_h)


# LOGIT -----

set.seed(10101)
logit3 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos+
    max_educ_h+P5010+Num_personas_cuarto+
    max_health_h+edad_prom_h+horastrab_prom_h,
  data = p_train_hogares_mini,
  method = "glm", #Para logit
  trControl = ctrl_def_modg,
  family = "binomial",
  preProcess = c("center", "scale"))

logit3

## Predicción modelo Logit ####

y_hat_out_logit <- predict(logit3, newdata = test_hogares, type = "raw")
resultado_logit <- data.frame(id = test_hogares$id, pobre = y_hat_out_logit)

resultado_logit$pobre <- ifelse(resultado_logit$pobre == "No", 0, 1)
write.csv(resultado_logit, "../outputs/resultado_logit.csv", row.names = FALSE)


### LASSO
#Prueba tomando como métrica la Sensibilidad


#Lambdas para Lasso
lambdas <- 10^seq(-4, 0.01, length = 200)

set.seed(10101)
lasso_sens3 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos+
    max_educ_h+P5010+Num_personas_cuarto+
    max_health_h+edad_prom_h+horastrab_prom_h,
  data = p_train_hogares_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale"))

lasso_sens3
#lambda = 0.01189947

### LASSO
#Prueba tomando como métrica el ROC

set.seed(10101)
lasso_roc3 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos+
    max_educ_h+P5010+Num_personas_cuarto+
    max_health_h+edad_prom_h+horastrab_prom_h,
  data = p_train_hogares_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale"))

lasso_roc3    
#0.0124646

#Calcularemos la regla para realizar la clasificación (Cut off)

Eval_Resultados <- data.frame(Pobre = Evaluation_H$Pobre_dummy)
Eval_Resultados$Roc <- predict(lasso_roc3,
                               newdata = Evaluation_H,
                               type = "prob")[,1]

library(pROC)
#Se calcula el ROC para la regresión
rf_ROC <- roc(Eval_Resultados$Pobre, Eval_Resultados$Roc, levels = rev(levels(Eval_Resultados$Pobre)))

rf_ROC
#En este caso la curva ROC El 0.8696

#La AUC se encuentra en un rango de 0 a 1, donde un valor de 0.5 indica que el modelo no es 
#mejor que una clasificación aleatoria, mientras que un valor cercano a 1 indica un buen poder
#de discriminación (el modelo es capaz de distinguir efectivamente entre las clases positiva y
#negativa). En este caso, el valor de AUC de 0.8696 sugiere que el modelo tiene una capacidad
#razonable para distinguir entre las dos clases.

#Se calcula el Cut off
rf_Thresh <- coords(rf_ROC, x = "best", best.method = "closest.topleft")
rf_Thresh

#En resumen, el resultado muestra que utilizando un umbral de clasificación de 0.2011781, 
#el modelo de clasificación binaria alcanzó una especificidad de 0.7879869 (tasa de verdaderos
#negativos) y una sensibilidad de 0.7965134 (tasa de verdaderos positivos). Estas métricas son
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
lasso_upsample3 <- upSample(x = p_train_hogares_mini,
                            y = p_train_hogares_mini$Pobre_dummy,
                            ## Mantener la variable de clasificación con el mismo nombre:
                            yname = "Pobre_dummy")

dim(lasso_upsample3)
table(lasso_upsample3$Pobre_dummy)

set.seed(10101)
lasso_upsample3 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos+
    max_educ_h+P5010+Num_personas_cuarto+
    max_health_h+edad_prom_h+horastrab_prom_h,
  data = lasso_upsample3,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale")
)

lasso_upsample3
#0.01571926


############################################################
#               Lasso Down-sampling.                ########
############################################################

set.seed(10101)
lasso_downsample3 <- downSample(x = p_train_hogares_mini,
                                y = p_train_hogares_mini$Pobre_dummy,
                                ## keep the class variable name the same:
                                yname = "Pobre_dummy")

table(lasso_downsample3$Pobre_dummy)

set.seed(10101)
lasso_downsample3 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos+
    max_educ_h+P5010+Num_personas_cuarto+
    max_health_h+edad_prom_h+horastrab_prom_h,
  data = lasso_downsample3,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale")
)

lasso_downsample3
#0.01571926

### Predicciones modelo lasso down-sample ----
y_hat_out_lassodown <- predict(lasso_downsample3, newdata = test_hogares, type = "raw")
resultado_lassodown <- data.frame(id = test_hogares$id, pobre = y_hat_out_lassodown)

resultado_lassodown$pobre <- ifelse(resultado_lassodown$pobre == "No", 0, 1)
write.csv(resultado_lassodown, "../outputs/resultado_lassodown.csv", row.names = FALSE)

############################################################
#                         Elastic net                   ####
############################################################

## Modelo 3 EN ####
set.seed(10101)

elasticnet3 <- train(
  Pobre_dummy ~Npersug+Lp +factor(Tipo_vivienda)+ factor(Dominio)+Num_cuartos+
    max_educ_h+P5010+Num_personas_cuarto+
    max_health_h+edad_prom_h+horastrab_prom_h,
  data = p_train_hogares_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  preProcess = c("center", "scale")
)

elasticnet3
#lambda = 0.002504042

### Predicciones modelo 3 EN ----
y_hat_out_EN3 <- predict(elasticnet3, newdata = test_hogares, type = "raw")
resultado_EN3 <- data.frame(id = test_hogares$id, pobre = y_hat_out_EN3)

resultado_EN3$pobre <- ifelse(resultado_EN3$pobre == "No", 0, 1)
write.csv(resultado_EN3, "../outputs/resultado_EN3.csv", row.names = FALSE)


## Modelo 4 EN ####
set.seed(10101)

elasticnet4 <- train(
  Pobre_dummy ~ Npersug + Lp + Tipo_vivienda + factor(Dominio) + Num_cuartos +
    max_educ_h + P5010 + Num_personas_cuarto +
    max_health_h + edad_prom_h + horastrab_prom_h +
    Nper + Li,
  data = p_train_hogares_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  preProcess = c("center", "scale")
)

elasticnet4
#lambda = 0.002504042

### Predicciones modelo 4 EN ----
y_hat_out_EN4 <- predict(elasticnet4, newdata = test_hogares, type = "raw")
resultado_EN4 <- data.frame(id = test_hogares$id, pobre = y_hat_out_EN4)

resultado_EN4$pobre <- ifelse(resultado_EN4$pobre == "No", 0, 1)
write.csv(resultado_EN4, "../outputs/resultado_EN4.csv", row.names = FALSE)


## Modelo 5 EN ####
## En este modelo vamos a usar las bases de datos creadas a partir 
## de la predicción de income, especificamente los resultados obtenidos con Lasso

set.seed(10101)

elasticnet5 <- train(
  Pobre_dummy ~ factor(Dominio) + Num_cuartos + P5010 + Tipo_vivienda +
    Nper + Npersug + Li + Lp + Num_personas_cuarto +
    edad_prom_h + horastrab_prom_h + max_educ_h + max_health_h + tot_income_h,
  data = p_train_hogares_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  preProcess = c("center", "scale")
)

elasticnet5
#lambda = 0.0002504042

## Predicciones modelo 5 EN ----
y_hat_out_EN5 <- predict(elasticnet5, newdata = ilasso_test_hogares, type = "raw")
resultado_EN5 <- data.frame(id = ilasso_test_hogares$id, pobre = y_hat_out_EN5)

resultado_EN5$pobre <- ifelse(resultado_EN5$pobre == "No", 0, 1)
write.csv(resultado_EN5, "../outputs/resultado_EN5.csv", row.names = FALSE)

## Modelo 6 EN ####

## Estandarizar variable edad ####
mu <- mean(p_train_hogares_mini$edad_prom_h)
sigma <- sd(p_train_hogares_mini$edad_prom_h)
p_train_hogares_mini$estedad_prom_h <- (p_train_hogares_mini$edad_prom_h - mu)/sigma
test_hogares$estedad_prom_h <- (test_hogares$edad_prom_h - mu)/sigma

## Estandarizar variable horas_trabajadas ####
mu <- mean(p_train_hogares_mini$horastrab_prom_h)
sigma <- sd(p_train_hogares_mini$horastrab_prom_h)
p_train_hogares_mini$esthorastrab_prom_h <- (p_train_hogares_mini$horastrab_prom_h - mu)/sigma
test_hogares$esthorastrab_prom_h <- (test_hogares$horastrab_prom_h - mu)/sigma

## Crear log_income
p_train_hogares_mini$tot_income_h <- ifelse(p_train_hogares_mini$tot_income_h == 0, 0.1, p_train_hogares_mini$tot_income_h)
p_train_hogares_mini$log_income <- log(p_train_hogares_mini$tot_income_h)

ien_test_hogares$tot_income_h <- ifelse(ien_test_hogares$tot_income_h == 0, 0.1, ien_test_hogares$tot_income_h)
ien_test_hogares$log_income <- log(ien_test_hogares$tot_income_h)

set.seed(10101)

elasticnet6 <- train(
  Pobre_dummy ~ factor(Dominio) + Num_cuartos + P5010 + Tipo_vivienda +
    Nper + Npersug + Li + Lp + Num_personas_cuarto +
    estedad_prom_h + esthorastrab_prom_h + max_educ_h + max_health_h,
  data = p_train_hogares_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  preProcess = c("center", "scale")
)

elasticnet6
#lambda = 0.0002504042

## Predicciones modelo 6 EN ----
y_hat_out_EN6 <- predict(elasticnet6, newdata = test_hogares, type = "raw")
resultado_EN6 <- data.frame(id = test_hogares$id, pobre = y_hat_out_EN6)

resultado_EN6$pobre <- ifelse(resultado_EN6$pobre == "No", 0, 1)
write.csv(resultado_EN6, "../outputs/resultado_EN6.csv", row.names = FALSE)

##############################################
#          Tablas de contingencia      #####
##############################################

predictors<-c("Npersug","Lp","Tipo_vivienda", "Dominio","Num_cuartos",
              "max_educ_h", "P5010", "Num_personas_cuarto",
              "max_health_h", "edad_prom_h", "horastrab_prom_h") 

head(p_train_hogares_mini[predictors])
testResults <- data.frame(Pobre = Testing_H$Pobre_dummy)

testResults$logit3<- predict(logit3,
                             newdata = Testing_H, 
                             type = "prob")[,1]
testResults$lasso_sens3<- predict(lasso_sens3,
                                  newdata = Testing_H,
                                  type = "prob")[,1]
testResults$lasso_roc3<- predict(lasso_roc3,
                                 newdata = Testing_H,
                                 type = "prob")[,1]
testResults$lasso_upsample3<- predict(lasso_upsample3,
                                      newdata = Testing_H,
                                      type = "prob")[,1]
testResults$lasso_downsample3<- predict(lasso_downsample3,
                                        newdata = Testing_H,
                                        type = "prob")[,1]
testResults$elasticnet3<- predict(elasticnet3,
                                  newdata = Testing_H,
                                  type = "prob")[,1]


testResults<-testResults %>%
  mutate(logit3=ifelse(logit3>0.5,"Si","No"),
         lasso_sens3=ifelse(lasso_sens3>0.5,"Si","No"),
         lasso_roc3=ifelse(lasso_roc3>0.5,"Si","No"),
         lasso_upsample3=ifelse(lasso_upsample3>0.5,"Si","No"),
         lasso_downsample3=ifelse(lasso_downsample3>0.5,"Si","No"),
         elasticnet3=ifelse(elasticnet3>0.5,"Si","No")
  )


#Tabla de contingencia para el modelo 3
with(testResults,table(Pobre,logit3))
with(testResults,table(Pobre,lasso_sens3))
with(testResults,table(Pobre,lasso_roc3))
with(testResults,table(Pobre,lasso_upsample3))
with(testResults,table(Pobre,lasso_downsample3))
with(testResults,table(Pobre,elasticnet3))

