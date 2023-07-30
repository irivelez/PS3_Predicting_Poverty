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
# EN Model 1
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
y_hat_in_EN <- predict(EN, newx = X_trainstd, s = mejor_lambda_EN)
y_hat_in_EN_cop <- exp(y_hat_in_EN)

## Métricas dentro de muestra
r2_in_EN <- R2_Score(y_pred = y_hat_in_EN_cop, y_true = p_train_hogares$tot_income_h)

errores_EN <- y_hat_in_EN_cop - p_train_hogares$tot_income_h
rmse_in_EN <- sqrt(mean(errores_EN^2))

resultados1 <- data.frame(Modelo = "ENStd", 
                          Muestra = "Dentro",
                          R2_Score = r2_in_EN, RMSE = rmse_in_EN)

# EN Model 2
EN2 <- train(log_income ~ Dominio+Num_cuartos+P5010+Tipo_vivienda+Nper+Npersug+Li+Lp+pobre+
              Num_personas_cuarto+edad_prom_h+horastrab_prom_h+max_educ_h+max_health_h,
            data = p_train_hogares,
            method = 'glmnet', 
            trControl = block_folds,
            tuneGrid = expand.grid(alpha = seq(0,1,length.out = 20),
                                   lambda = seq(0.001,0.2,length.out = 50))
)
plot(EN2)
mejor_lambda_EN <- EN2$bestTune

## Predecir income
y_hat_in_EN <- predict(EN2, newx = X_train, s = mejor_lambda_EN)
y_hat_in_EN_cop <- exp(y_hat_in_EN)

y_hat_out_EN <- predict(EN2, newx = test_hogares, s = mejor_lambda_EN)
y_hat_out_EN_cop <- exp(y_hat_out_EN)

summary(y_hat_out_EN_cop)

predict_1 <- data.frame(id = test_hogares$id, tot_income_h = y_hat_outsample_EN_cop)

## Métricas dentro de muestra
r2_in_EN1 <- R2_Score(y_pred = y_hat_in_EN_cop, y_true = p_train_hogares$tot_income_h)

errores_EN1 <- y_hat_in_EN_cop - p_train_hogares$tot_income_h
rmse_in_EN1 <- sqrt(mean(errores_EN1^2))

resultados2 <- data.frame(Modelo = "ENS", 
                          Muestra = "Dentro",
                          R2_Score = r2_in_EN1, RMSE = rmse_in_EN1)

## Lasso ####
X_train <- select(p_train_hogares, -c(log_income, tot_income_h, id, estedad_prom_h, esthorastrab_prom_h))
X_trainstd <- select(p_train_hogares, -c(log_income, tot_income_h, id, edad_prom_h, horastrab_prom_h))

X_train <- as.matrix(X_train)
X_trainstd <- as.matrix(X_trainstd)
y <- p_train_hogares$log_income

# Lasso Model 1
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
y_hat_in_lasso <- predict(lasso, newx = X_trainstd, s = mejor_lambda_lasso)
y_hat_in_lasso_cop <- exp(y_hat_in_lasso)

## Métricas dentro de muestra
r2_in_lasso <- R2_Score(y_pred = y_hat_in_lasso_cop, y_true = p_train_hogares$tot_income_h)

errores <- y_hat_in_lasso_cop - p_train_hogares$tot_income_h
rmse_in_lasso <- sqrt(mean(errores^2))

resultados3 <- data.frame(Modelo = "LassoStd", 
                          Muestra = "Dentro",
                          R2_Score = r2_in_lasso, RMSE = rmse_in_lasso)

# Lasso Model 2
set.seed(9873)
block_folds <- trainControl(method = "cv", number = 5)

lasso0 <- glmnet(
  x = X_train,
  y = y,
  alpha = 1 #lasso
)

plot(lasso0, xvar = "lambda")

lasso1 <- train(log_income ~ Dominio+Num_cuartos+P5010+Tipo_vivienda+Nper+Npersug+Li+Lp+pobre+
                 Num_personas_cuarto+edad_prom_h+horastrab_prom_h+max_educ_h+max_health_h,
               data = p_train_hogares,
               method = 'glmnet', 
               trControl = block_folds,
               tuneGrid = expand.grid(alpha = 1, #lasso
                                      lambda = lasso0$lambda)
) 

mejor_lambda_lasso <- lasso1$bestTune

## Predecir income
y_hat_in_lasso <- predict(lasso1, newx = X_train, s = mejor_lambda_lasso)
y_hat_in_lasso_cop <- exp(y_hat_in_lasso)

## Métricas dentro de muestra
r2_in_lasso1 <- R2_Score(y_pred = y_hat_in_lasso_cop, y_true = p_train_hogares$tot_income_h)

errores1 <- y_hat_in_lasso_cop - p_train_hogares$tot_income_h
rmse_in_lasso1 <- sqrt(mean(errores^2))

resultados4 <- data.frame(Modelo = "Lasso", 
                          Muestra = "Dentro",
                          R2_Score = r2_in_lasso1, RMSE = rmse_in_lasso1)

resultados <- rbind(resultados1, resultados2, resultados3, resultados4)

# Bases de datos
i_test_hogares <- left_join(test_hogares, predict_1) 
i_train_hogares <- subset(p_train_hogares,select = -log_income)

# Guardar bases
saveRDS(i_test_hogares, "../stores/i_test_hogares.rds") 
saveRDS(i_train_hogares, "../stores/i_train_hogares.rds") 




