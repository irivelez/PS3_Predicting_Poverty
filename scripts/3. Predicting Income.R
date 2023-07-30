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

# Libraries -----
if(!require(pacman)) install.packages("pacman")
require(pacman)

p_load(tidyverse,rio,
       sf,
       rstudioapi,
       stargazer,
       glmnet,
       MLmetrics)
p_load(caret)


# Importing databases ----
test_hogares <- readRDS("../stores/test_hogares.rds")
p_train_hogares <- readRDS("../stores/p_train_hogares.rds")


# Preparing the data -----
## Creación log_income ####
p_train_hogares$tot_income_h <- ifelse(p_train_hogares$tot_income_h == 0, 0.1, p_train_hogares$tot_income_h)
p_train_hogares$log_income <- log(p_train_hogares$tot_income_h)


## Volviendo factor la variable Dominio
test_hogares$Dominio <- factor(test_hogares$Dominio)
class(test_hogares$Dominio)

p_train_hogares$Dominio <- factor(p_train_hogares$Dominio)
class(p_train_hogares$Dominio)

## Estandarizar variable edad ####
mu <- mean(p_train_hogares$edad_prom_h)
sigma <- sd(p_train_hogares$edad_prom_h)
p_train_hogares$estedad_prom_h <- (p_train_hogares$edad_prom_h - mu)/sigma
test_hogares$estedad_prom_h <- (test_hogares$edad_prom_h - mu)/sigma

## Estandarizar variable horas_trabajadas ####
mu <- mean(p_train_hogares$horastrab_prom_h)
sigma <- sd(p_train_hogares$horastrab_prom_h)
p_train_hogares$esthorastrab_prom_h <- (p_train_hogares$horastrab_prom_h - mu)/sigma
test_hogares$esthorastrab_prom_h <- (test_hogares$horastrab_prom_h - mu)/sigma

## Deleting Depto var
p_train_hogares <- subset(p_train_hogares, select = -Depto)
test_hogares <- subset(test_hogares, select = -Depto)

# Predicting Income -----
# Selección de la penalización
set.seed(9873)
block_folds <- trainControl(method = "cv", number = 5)

## Elastic Net ####
### EN Model 1 ####
EN <- train(log_income ~ Dominio+Num_cuartos+P5010+Tipo_vivienda+Nper+Npersug+Li+Lp+pobre+
              Num_personas_cuarto+estedad_prom_h+esthorastrab_prom_h+max_educ_h+max_health_h,
            data = p_train_hogares,
            method = 'glmnet', 
            trControl = block_folds,
            tuneGrid = expand.grid(alpha = seq(0,1,0.1),
                                   lambda = seq(0.001,0.2,0.01))
)
plot(EN)
mejor_lambda_EN <- EN$bestTune

## Predecir income
y_hat_in_EN <- predict(EN, newdata = p_train_hogares, s = mejor_lambda_EN)
y_hat_in_EN_cop <- exp(y_hat_in_EN)

y_hat_out_EN <- predict(EN, newdata = test_hogares, s = mejor_lambda_EN)
y_hat_out_EN_cop <- exp(y_hat_out_EN)

options(scipen = 999)
summary(y_hat_out_EN_cop)

## Métricas dentro de muestra
r2_in_EN <- R2_Score(y_pred = y_hat_in_EN_cop, y_true = p_train_hogares$tot_income_h)

error_EN <- y_hat_in_EN_cop - p_train_hogares$tot_income_h
RMSE_in_EN <- sqrt(mean(error_EN^2))

resultados1 <- data.frame(Modelo = "ENStd", 
                          Muestra = "Dentro",
                          R2_Score = r2_in_EN, 
                          RMSE = RMSE_in_EN)

### EN Model 2 ####
EN2 <- train(log_income ~ Dominio+Num_cuartos+P5010+Tipo_vivienda+Nper+Npersug+Li+Lp+pobre+
              Num_personas_cuarto+edad_prom_h+horastrab_prom_h+max_educ_h+max_health_h,
            data = p_train_hogares,
            method = 'glmnet', 
            trControl = block_folds,
            tuneGrid = expand.grid(alpha = seq(0,1,length.out = 20),
                                   lambda = seq(0.001,0.2,length.out = 50))
)
plot(EN2)
mejor_lambda_EN2 <- EN2$bestTune

## Predecir income
y_hat_in_EN <- predict(EN2, newdata = p_train_hogares, s = mejor_lambda_EN2)
y_hat_in_EN_cop <- exp(y_hat_in_EN)

y_hat_out_EN <- predict(EN2, newdata = test_hogares, s = mejor_lambda_EN2)
y_hat_out_EN_cop <- exp(y_hat_out_EN)

summary(y_hat_out_EN_cop)

## Métricas dentro de muestra
r2_in_EN2 <- R2_Score(y_pred = y_hat_in_EN_cop, y_true = p_train_hogares$tot_income_h)

error_EN2 <- y_hat_in_EN_cop - p_train_hogares$tot_income_h
RMSE_in_EN2 <- sqrt(mean(error_EN2^2))

resultados2 <- data.frame(Modelo = "EN", 
                          Muestra = "Dentro",
                          R2_Score = r2_in_EN2, 
                          RMSE = RMSE_in_EN2)

### Results EN #####
resultados <- rbind(resultados1, resultados2)
resultados
predict_en <- data.frame(id = test_hogares$id, tot_income_h = y_hat_out_EN_cop)

saveRDS(predict_en, "../stores/predict_en.rds")

## Lasso ####
X_train <- select(p_train_hogares, -c(log_income, tot_income_h, id, estedad_prom_h, esthorastrab_prom_h))
X_trainstd <- select(p_train_hogares, -c(log_income, tot_income_h, id, edad_prom_h, horastrab_prom_h))

X_train <- as.matrix(X_train)
X_trainstd <- as.matrix(X_trainstd)
y <- p_train_hogares$log_income

### Lasso Model 1 ####
set.seed(9873)
block_folds <- trainControl(method = "cv", number = 5)

lasso0 <- glmnet(
  x = X_trainstd,
  y = y,
  alpha = 1 #lasso
)

plot(lasso0, xvar = "lambda")

lasso <- train(log_income ~ Dominio+Num_cuartos+P5010+Tipo_vivienda+Nper+Npersug+Li+Lp+pobre+
                 Num_personas_cuarto+estedad_prom_h+esthorastrab_prom_h+max_educ_h+max_health_h,
             data = p_train_hogares,
             method = 'glmnet', 
             trControl = block_folds,
             tuneGrid = expand.grid(alpha = 1, #lasso
                                    lambda = lasso0$lambda)
) 

mejor_lambda_lasso <- lasso$bestTune

## Predecir income
y_hat_in_lasso <- predict(lasso, newdata = p_train_hogares, s = mejor_lambda_lasso)
y_hat_in_lasso_cop <- exp(y_hat_in_lasso)

y_hat_out_lasso <- predict(lasso, newdata = test_hogares, s = mejor_lambda_lasso)
y_hat_out_lasso_cop <- exp(y_hat_out_lasso)

summary(y_hat_out_lasso_cop)

## Métricas dentro de muestra
r2_in_lasso <- R2_Score(y_pred = y_hat_in_lasso_cop, y_true = p_train_hogares$tot_income_h)

error_lasso <- y_hat_in_lasso_cop - p_train_hogares$tot_income_h
RMSE_in_lasso <- sqrt(mean(error_lasso^2))

resultados3 <- data.frame(Modelo = "LassoStd", 
                          Muestra = "Dentro",
                          R2_Score = r2_in_lasso, 
                          RMSE = RMSE_in_lasso)

### Lasso Model 2 ####
set.seed(9873)
block_folds <- trainControl(method = "cv", number = 5)

lasso0 <- glmnet(
  x = X_train,
  y = y,
  alpha = 1 #lasso
)

plot(lasso0, xvar = "lambda")

lasso2 <- train(log_income ~ Dominio+Num_cuartos+P5010+Tipo_vivienda+Nper+Npersug+Li+Lp+pobre+
                 Num_personas_cuarto+edad_prom_h+horastrab_prom_h+max_educ_h+max_health_h,
               data = p_train_hogares,
               method = 'glmnet', 
               trControl = block_folds,
               tuneGrid = expand.grid(alpha = 1, #lasso
                                      lambda = lasso0$lambda)
) 

mejor_lambda_lasso2 <- lasso2$bestTune

## Predecir income
y_hat_in_lasso <- predict(lasso2, newdata = p_train_hogares, s = mejor_lambda_lasso2)
y_hat_in_lasso_cop <- exp(y_hat_in_lasso)

y_hat_out_lasso <- predict(lasso2, newdata = test_hogares, s = mejor_lambda_lasso2)
y_hat_out_lasso_cop <- exp(y_hat_out_lasso)

## Métricas dentro de muestra
r2_in_lasso2 <- R2_Score(y_pred = y_hat_in_lasso_cop, y_true = p_train_hogares$tot_income_h)

error_lasso2 <- y_hat_in_lasso_cop - p_train_hogares$tot_income_h
RMSE_in_lasso2 <- sqrt(mean(error_lasso2^2))

resultados4 <- data.frame(Modelo = "Lasso", 
                          Muestra = "Dentro",
                          R2_Score = r2_in_lasso2, 
                          RMSE = RMSE_in_lasso2)
### Results lasso #####
resultados <- rbind(resultados1, resultados2, resultados3, resultados4)
resultados

predict_lasso <- data.frame(id = test_hogares$id, tot_income_h = y_hat_out_lasso_cop)
saveRDS(predict_lasso, "../stores/predict_lasso.rds")

## Ridge ####
### Ridge Model 1 ####
set.seed(9873)
block_folds <- trainControl(method = "cv", number = 5)

lasso0 <- glmnet(
  x = X_trainstd,
  y = y,
  alpha = 1 #lasso
)

plot(lasso0, xvar = "lambda")

ridge <- train(log_income ~ Dominio+Num_cuartos+P5010+Tipo_vivienda+Nper+Npersug+Li+Lp+pobre+
                 Num_personas_cuarto+estedad_prom_h+esthorastrab_prom_h+max_educ_h+max_health_h,
               data = p_train_hogares,
               method = 'glmnet', 
               trControl = block_folds,
               tuneGrid = expand.grid(alpha = 0, #ridge
                                      lambda = lasso0$lambda)
) 

mejor_lambda_ridge <- ridge$bestTune

## Predecir income
y_hat_in_ridge <- predict(ridge, newdata = p_train_hogares, s = mejor_lambda_ridge)
y_hat_in_ridge_cop <- exp(y_hat_in_ridge)

y_hat_out_ridge <- predict(ridge, newdata = test_hogares, s = mejor_lambda_ridge)
y_hat_out_ridge_cop <- exp(y_hat_out_ridge)

summary(y_hat_out_ridge_cop)

## Métricas dentro de muestra
r2_in_ridge <- R2_Score(y_pred = y_hat_in_ridge_cop, y_true = p_train_hogares$tot_income_h)

error_ridge <- y_hat_in_ridge_cop - p_train_hogares$tot_income_h
RMSE_in_ridge <- sqrt(mean(error_ridge^2))

resultados5 <- data.frame(Modelo = "RidgeStd", 
                          Muestra = "Dentro",
                          R2_Score = r2_in_ridge, 
                          RMSE = RMSE_in_ridge)

### Ridge Model 2 ####
set.seed(9873)
block_folds <- trainControl(method = "cv", number = 5)

lasso0 <- glmnet(
  x = X_train,
  y = y,
  alpha = 1 #lasso
)

plot(lasso0, xvar = "lambda")

ridge2 <- train(log_income ~ Dominio+Num_cuartos+P5010+Tipo_vivienda+Nper+Npersug+Li+Lp+pobre+
                 Num_personas_cuarto+edad_prom_h+horastrab_prom_h+max_educ_h+max_health_h,
               data = p_train_hogares,
               method = 'glmnet', 
               trControl = block_folds,
               tuneGrid = expand.grid(alpha = 0, #ridge
                                      lambda = lasso0$lambda)
) 

mejor_lambda_ridge2 <- ridge2$bestTune

## Predecir income
y_hat_in_ridge <- predict(ridge2, newdata = p_train_hogares, s = mejor_lambda_ridge2)
y_hat_in_ridge_cop <- exp(y_hat_in_ridge)

y_hat_out_ridge <- predict(ridge2, newdata = test_hogares, s = mejor_lambda_ridge2)
y_hat_out_ridge_cop <- exp(y_hat_out_ridge)

summary(y_hat_out_ridge_cop)

## Métricas dentro de muestra
r2_in_ridge2 <- R2_Score(y_pred = y_hat_in_ridge_cop, y_true = p_train_hogares$tot_income_h)

error_ridge2 <- y_hat_in_ridge_cop - p_train_hogares$tot_income_h
RMSE_in_ridge2 <- sqrt(mean(error_ridge2^2))

resultados6 <- data.frame(Modelo = "Ridge", 
                          Muestra = "Dentro",
                          R2_Score = r2_in_ridge2, 
                          RMSE = RMSE_in_ridge2)

### Results ridge #####
resultados <- rbind(resultados1, resultados2, resultados3, resultados4, resultados5, resultados6)
resultados

predict_ridge <- data.frame(id = test_hogares$id, tot_income_h = y_hat_out_ridge_cop)
saveRDS(predict_ridge, "../stores/predict_ridge.rds")

# Databases with predicted income -----
ien_test_hogares <- left_join(test_hogares, predict_en)
ilasso_test_hogares <- left_join(test_hogares, predict_lasso)
iridge_test_hogares <- left_join(test_hogares, predict_ridge)
i_train_hogares <- subset(p_train_hogares,select = -log_income)

# Saving DBs
saveRDS(ien_test_hogares, "../stores/ien_test_hogares.rds") 
saveRDS(ilasso_test_hogares, "../stores/ilasso_test_hogares.rds") 
saveRDS(iridge_test_hogares, "../stores/iridge_test_hogares.rds") 
saveRDS(i_train_hogares, "../stores/i_train_hogares.rds")
