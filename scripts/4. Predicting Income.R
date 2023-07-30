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
p_train_hogares <- readRDS("../stores/p_train_hogares.rds")

## Predicting Income

block_folds <- trainControl(method = "CV", number = 5)



set.seed(9873)
EN <- train(tot_income_h ~ surface_covered_new + rooms + bedrooms + bathrooms +
              parqueaderoT + ascensorT + bañoprivado + balcon+vista + remodelado +
              Es_apartamento + distancia_parque + area_parque + distancia_sport_centre +
              distancia_swimming_pool,
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