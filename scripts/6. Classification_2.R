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

# Importing from Github #AJUSTAR 
i_test_hogares <- readRDS("../stores/i_test_hogares.rds")
i_train_hogares <- readRDS("../stores/i_train_hogares.rds")


# Clasificación de 1 y 0 con income predicho 1
i_test_hogares$pobre_p_1 <- if(Li)




# Clasificación de 1 y 0 con income predicho 2




