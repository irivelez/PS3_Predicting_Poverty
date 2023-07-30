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
       leaflet,
       tmaptools,
       osmdata,
       nngeo,
       magrittr,
       rgeos,
       rio,
       rstudioapi,
       stargazer,
       glmnet)

# Importing databases

test_hogares <- readRDS("../stores/test_hogares.rds")
test_hogares$Dominio <- factor(test_hogares$Dominio)
class(test_hogares$Dominio)


p_train_hogares <- readRDS("../stores/p_train_hogares.rds")
p_train_hogares$Dominio <- factor(p_train_hogares$Dominio)
class(p_train_hogares$Dominio)

# Creac log_income
p_train_hogares$log_income <- log(p_train_hogares$tot_income_h)


## Predicting Income
p_load(caret)
block_folds <- trainControl(method = "CV", number = 5)

set.seed(9873)
EN <- train(log_income ~ Dominio+Num_cuartos+P5010+Tipo_vivienda+Nper+Npersug+Li+Lp+pobre+
              Num_personas_cuarto+edad_prom_h+horastrab_prom_h+max_educ_h+max_health_h,
            data=p_train_hogares,
            method = 'glmnet', 
            trControl = block_folds,
            tuneGrid = expand.grid(alpha =seq(0,1,length.out = 20),
                                   lambda = seq(0.001,0.2,length.out = 50))
)

plot(EN)


## Predecir datos
# Verificar si hay que crear una variable con el mismo nombre en el test
y_hat_outsample_EN <- predict(EN,test_hogares)

y_hat_outsample_EN_cop <- exp(y_hat_outsample_EN)



summary(y_hat_outsample_EN)
summary(p_train_hogares$tot_income_h)


