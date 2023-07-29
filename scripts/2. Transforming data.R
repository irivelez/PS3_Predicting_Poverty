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

common_var_train <- intersect(names(train_personas), names(train_hogares))
common_var_train

# Creating the variable income ----
sum_income <- train_personas %>% group_by(id) %>% summarize(tot_income_h = sum(Ingtot, na.rm = TRUE))
summary(sum_income)

# Joining the new variable tot_income_h to train_hogares
train_hogares <- left_join(train_hogares, sum_income)
colnames(train_hogares)

head(train_hogares[c("id","Ingtotug","tot_income_h")])

# Poverty calculation
table(train_hogares$Pobre)
train_hogares <- train_hogares %>% mutate(Pobre_h = ifelse(Ingpcug < Lp, 1, 0))
table(train_hogares$Pobre_h, train_hogares$Pobre)

train_hogares <- train_hogares %>% mutate(Pobre_h2 = ifelse(Ingtotugarr < Lp*Npersug, 1, 0))
table(train_hogares$Pobre, train_hogares$Pobre_h2)

# Leave both db with the same variables for train and test
# Personas
diff_personas <- setdiff(names(train_personas), names(test_personas))
diff_personas <- diff_personas[diff_personas != "Ingtot"]
length(diff_personas)
n_train_personas <- train_personas %>% select(-all_of(diff_personas))

# Hogares
diff_hogares <- setdiff(names(train_hogares), names(test_hogares))
length(diff_hogares)
diff_hogares <- diff_hogares[diff_hogares != "Pobre"]
p_train_hogares <- train_hogares %>% select(-all_of(diff_hogares))  ## Dependent variable Pobre

diff_hogares <- setdiff(names(train_hogares), names(test_hogares))
diff_hogares_i <- diff_hogares[diff_hogares != "tot_income_h"]
i_train_hogares <- train_hogares %>% select(-all_of(diff_hogares_i))  ## Dependent variable tot_income_h

saveRDS(n_train_personas, "../stores/n_train_personas.rds")
saveRDS(p_train_hogares, "../stores/p_train_hogares.rds")
saveRDS(i_train_hogares, "../stores/i_train_hogares.rds")

# Check if any test IDs are in the train dataset
test_id <- unique(test_hogares$id)
test_id_in_train <- test_id %in% train_hogares$id
test_id[test_id_in_train] ## There is no ID test on train

## No data available for dependent variables such as income(ingtot) or poor (pobre)
## in the test datasets
## ¿How can we calculate the RMSE without the real value of the dependent variables
## in the test datasets?

# Transforming variables ------
## Hogares ####
## The db of interest is the household db, so we are going to name these variables

### Test ####
names(test_hogares)[names(test_hogares) == "P5000"] <- "Num_cuartos"
names(test_hogares)[names(test_hogares) == "P5100"] <- "Cuota_amortización"
names(test_hogares)[names(test_hogares) == "P5140"] <- "Arriendo"

names(test_hogares)[names(test_hogares) == "P5090"] <- "Tipo_vivienda"
test_hogares$Tipo_vivienda <- factor(test_hogares$Tipo_vivienda)

test_hogares <- test_hogares %>% mutate(Num_personas_cuarto = Nper/P5010)

### Train income independent var ####
names(i_train_hogares)[names(i_train_hogares) == "P5000"] <- "Num_cuartos"
names(i_train_hogares)[names(i_train_hogares) == "P5100"] <- "Cuota_amortización"
names(i_train_hogares)[names(i_train_hogares) == "P5140"] <- "Arriendo"

names(i_train_hogares)[names(i_train_hogares) == "P5090"] <- "Tipo_vivienda"
i_train_hogares$Tipo_vivienda <- factor(i_train_hogares$Tipo_vivienda)

i_train_hogares <- i_train_hogares %>% mutate(Num_personas_cuarto = Nper/P5010)

### Train poor independent var ####
names(p_train_hogares)[names(p_train_hogares) == "P5000"] <- "Num_cuartos"
names(p_train_hogares)[names(p_train_hogares) == "P5100"] <- "Cuota_amortización"
names(p_train_hogares)[names(p_train_hogares) == "P5140"] <- "Arriendo"

names(p_train_hogares)[names(p_train_hogares) == "P5090"] <- "Tipo_vivienda"
p_train_hogares$Tipo_vivienda <- factor(p_train_hogares$Tipo_vivienda)

p_train_hogares <- p_train_hogares %>% mutate(Num_personas_cuarto = Nper/P5010)

## Personas ####
### Test ####
names(test_personas)[names(test_personas) == "P6020"] <- "mujer"
test_personas$mujer <- test_personas$mujer  - 1

names(test_personas)[names(test_personas) == "P6040"] <- "Edad"

names(test_personas)[names(test_personas) == "P6210"] <- "Nivel_educ"
test_personas$Nivel_educ <- factor(test_personas$Nivel_educ)

### Train ####
names(n_train_personas)[names(n_train_personas) == "P6020"] <- "mujer"
n_train_personas$mujer <- n_train_personas$mujer  - 1

names(n_train_personas)[names(n_train_personas) == "P6040"] <- "Edad"

names(n_train_personas)[names(n_train_personas) == "P6210"] <- "Nivel_educ"
n_train_personas$Nivel_educ <- factor(n_train_personas$Nivel_educ)
                                      
                     
                                      
# Reviewing the data filtered and identifying na values. Pendiente revisar
## Train ##
names(n_train_personas)
glimpse(n_train_personas)

na_count_p <- colSums(is.na(n_train_personas))
na_count_p <- sort(na_count_p, decreasing = TRUE)
proportion_na_p <- na_count_p/nrow(n_train_personas)

##diff_train <- setdiff(names(n_train_personas), names(n_train_hogares))
##diff_train

##na_count_p <- colSums(is.na(n_train_personas[diff_train]))
na_count_p <- sort(na_count_p, decreasing = TRUE)
proportion_na_p <- na_count_p/nrow(n_train_personas)

##common_var_train <- intersect(names(n_train_personas), names(n_train_hogares))
##common_var_train

#colnames(n_train_hogares)
colnames(n_train_personas)


na_count <- colSums(is.na(train_personas[diff_personas]))
na_count <- sort(na_count, decreasing = TRUE)
