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

p_load(tidyverse,rio,
       sf,
       leaflet,
       tmaptools,
       osmdata,
       nngeo,
       magrittr,
       rgeos,
       rio,
       rstudioapi)


# Import data -------------------------------------------------------------

## Personas
### Train
train_personas <- read.csv("../stores/train_personas.csv")

### Test
test_personas <- read.csv("../stores/test_personas.csv")

## Hogares
### Train
train_hogares <- read.csv("../stores/train_hogares.csv")

### Test
test_hogares <- read.csv("../stores/test_hogares.csv")


# View 1 data -------------------------------------------------------------
## Personas ####
## Train
names(train_personas)
glimpse(train_personas)
summary(train_personas)

## Test
names(test_personas)
glimpse(test_personas)
summary(test_personas)

## Hogares ####
### Train
names(train_hogares)
glimpse(train_hogares)
summary(train_hogares)

### Test
names(test_hogares)
glimpse(test_hogares)
summary(test_hogares)















