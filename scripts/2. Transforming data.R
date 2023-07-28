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

p_load(tidyverse)

# Importing db from Github
train_personas <- readRDS("../stores/train_personas.rds")
test_personas <- readRDS("../stores/test_personas.rds")
train_hogares <- readRDS("../stores/train_hogares.rds")
test_hogares <- readRDS("../stores/test_hogares.rds")

sample_submission <- readRDS("../stores/sample_submission.rds")

# Reviewing the data -----

# Personas
names(train_personas)
glimpse(train_personas)

names(test_personas)
glimpse(test_personas)

# Hogares
names(train_hogares)
glimpse(train_hogares)

names(test_hogares)
glimpse(test_hogares)

# Leave both db with the same variables for train and test
diff_personas <- setdiff(names(train_personas), names(test_personas))
length(diff_personas)

diff_hogares <- setdiff(names(train_hogares), names(test_hogares))
length(diff_hogares)

n_train_personas <- train_personas %>% select(-all_of(diff_personas))
n_train_hogares <- train_hogares %>% select(-all_of(diff_hogares))  

saveRDS(n_train_personas, "../stores/n_train_personas.rds")
saveRDS(n_train_hogares, "../stores/n_train_hogares.rds")


# Reviewing the data filtered and identifying na values
## Train ##
names(n_train_personas)
glimpse(n_train_personas)

na_count_p <- colSums(is.na(n_train_personas))
na_count_p <- sort(na_count_p, decreasing = TRUE)
proportion_na_p <- na_count_p/nrow(n_train_personas)

diff_train <- setdiff(names(n_train_personas), names(n_train_hogares))
diff_train

na_count_p <- colSums(is.na(n_train_personas[diff_train]))
na_count_p <- sort(na_count_p, decreasing = TRUE)
proportion_na_p <- na_count_p/nrow(n_train_personas)

common_var_train <- intersect(names(n_train_personas), names(n_train_hogares))
common_var_train

colnames(n_train_hogares)
colnames(n_train_personas)

sum_income <- n_train_personas %>% group_by(id) %>% summarize(sum(ingtot, na.rm = TRUE))

summarize(Ingtot_hogar=sum(Ingtot,na.rm = TRUE)) 
summary(n_train_hogares)
summary(n_train_personas)

Ingtotob
Ingtotes

names(test_personas)
glimpse(test_personas)
summary(test_personas)

## Hogares ##


names(test_hogares)
glimpse(test_hogares)
summary(test_hogares)


na_count <- colSums(is.na(train_personas[diff_personas]))
na_count <- sort(na_count, decreasing = TRUE)
