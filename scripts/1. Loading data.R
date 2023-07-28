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

## llamado librerías de la sesión
require(pacman)

# Loading data -------------------------------------------------------------
train_personas <- read.csv("/Users/irina/Downloads/uniandes-bdml-202313-ps31/train_personas.csv")
train_hogares <- read.csv("/Users/irina/Downloads/uniandes-bdml-202313-ps31/train_hogares.csv")
test_personas <- read.csv("/Users/irina/Downloads/uniandes-bdml-202313-ps31/test_personas.csv")
test_hogares <- read.csv("/Users/irina/Downloads/uniandes-bdml-202313-ps31/test_hogares.csv")

sample_submission <- read.csv("/Users/irina/Downloads/uniandes-bdml-202313-ps31/sample_submission.csv")

saveRDS(train_personas, "../stores/train_personas.rds")
saveRDS(train_hogares, "../stores/train_hogares.rds")
saveRDS(test_personas, "../stores/test_personas.rds")
saveRDS(test_hogares, "../stores/test_hogares.rds")
