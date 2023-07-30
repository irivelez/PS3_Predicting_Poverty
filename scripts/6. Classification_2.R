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
       caret,
       dplyr)

# Importing from Github



# Bases
ien_test_hogares <- readRDS("../stores/ien_test_hogares.rds")
ilasso_test_hogares <- readRDS("../stores/ilasso_test_hogares.rds")
iridge_test_hogares <- readRDS("../stores/iridge_test_hogares.rds")

i_train_hogares <- readRDS("../stores/i_test_hogares.rds")


# Clasificación de 1 y 0 con income predicho EN
ien_test_hogares$pobre_p_1 <- ifelse(ien_test_hogares$tot_income_h >= ien_test_hogares$Lp, 0, 1)

# Clasificación de 1 y 0 con income predicho Lasso
ilasso_test_hogares$pobre_p_1 <- ifelse(ilasso_test_hogares$tot_income_h >= ilasso_test_hogares$Lp, 0, 1)

# Clasificación de 1 y 0 con income predicho Ridge
iridge_test_hogares$pobre_p_1 <- ifelse(iridge_test_hogares$tot_income_h >= iridge_test_hogares$Lp, 0, 1)


# Guardar base para subir predicción
p_load(dplyr)

# EN
predicciones_EN_1_income <- ien_test_hogares[,c(1,19)]
predicciones_EN_1_income <- predicciones_EN_1_income %>% rename(pobre = pobre_p_1)

write.csv(predicciones_EN_1_income, "../outputs/predicciones_EN_1_income.csv", row.names = FALSE)


# Lasso
predicciones_Lasso_1_income <- ilasso_test_hogares[,c(1,19)]
predicciones_Lasso_1_income <- predicciones_Lasso_1_income %>% rename(pobre = pobre_p_1)

write.csv(predicciones_Lasso_1_income, "../outputs/predicciones_Lasso_1_income.csv", row.names = FALSE)


# Ridge
predicciones_Ridge_1_income <- iridge_test_hogares[,c(1,19)]
predicciones_Ridge_1_income <- predicciones_Ridge_1_income %>% rename(pobre = pobre_p_1)

write.csv(predicciones_Ridge_1_income, "../outputs/predicciones_Ridge_1_income.csv", row.names = FALSE)









