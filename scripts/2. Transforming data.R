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

# Creating variables ----- 

# Check if any test IDs are in others dataset
test_id <- unique(test_hogares$id)
test_id_in_train <- test_id %in% train_hogares$id
test_id[test_id_in_train] ## There is no ID test on train

test_id <- unique(test_hogares$id)
test_id_in_trainp <- test_id %in% train_personas$id
test_id[test_id_in_trainp] ## There is no ID test on train

test_id_in_sub <- test_id %in% sample_submission$id
test_id[test_id_in_sub]

## Pobre ####
test_hogares <- left_join(test_hogares, sample_submission)
colnames(test_hogares)

# Poverty calculation
table(train_hogares$Pobre)
train_hogares <- train_hogares %>% mutate(Pobre_h = ifelse(Ingpcug < Lp, 1, 0))
table(train_hogares$Pobre_h, train_hogares$Pobre)

train_hogares <- train_hogares %>% mutate(Pobre_h2 = ifelse(Ingtotugarr < Lp*Npersug, 1, 0))
table(train_hogares$Pobre, train_hogares$Pobre_h2)

## Income ####
sum_income <- train_personas %>% group_by(id) %>% summarize(tot_income_h = sum(Ingtot, na.rm = TRUE))
summary(sum_income)


# Leave both db with the same variables for train and test -----
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

# Naming variables ------

## Hogares ####
### Test ####
names(test_hogares)[names(test_hogares) == "P5000"] <- "Num_cuartos"
names(test_hogares)[names(test_hogares) == "P5100"] <- "Cuota_amortización"
names(test_hogares)[names(test_hogares) == "P5140"] <- "Arriendo"

names(test_hogares)[names(test_hogares) == "P5090"] <- "Tipo_vivienda"
test_hogares$Tipo_vivienda <- factor(test_hogares$Tipo_vivienda)

test_hogares <- test_hogares %>% mutate(Num_personas_cuarto = Nper/P5010)

saveRDS(test_hogares, "../stores/test_hogares.rds")

### Train income independent var ####
names(i_train_hogares)[names(i_train_hogares) == "P5000"] <- "Num_cuartos"
names(i_train_hogares)[names(i_train_hogares) == "P5100"] <- "Cuota_amortización"
names(i_train_hogares)[names(i_train_hogares) == "P5140"] <- "Arriendo"

names(i_train_hogares)[names(i_train_hogares) == "P5090"] <- "Tipo_vivienda"
i_train_hogares$Tipo_vivienda <- factor(i_train_hogares$Tipo_vivienda)

i_train_hogares <- i_train_hogares %>% mutate(Num_personas_cuarto = Nper/P5010)

saveRDS(i_train_hogares, "../stores/i_train_hogares.rds")

### Train poor independent var ####
names(p_train_hogares)[names(p_train_hogares) == "P5000"] <- "Num_cuartos"
names(p_train_hogares)[names(p_train_hogares) == "P5100"] <- "Cuota_amortización"
names(p_train_hogares)[names(p_train_hogares) == "P5140"] <- "Arriendo"

names(p_train_hogares)[names(p_train_hogares) == "P5090"] <- "Tipo_vivienda"
p_train_hogares$Tipo_vivienda <- factor(p_train_hogares$Tipo_vivienda)

p_train_hogares <- p_train_hogares %>% mutate(Num_personas_cuarto = Nper/P5010)

saveRDS(p_train_hogares, "../stores/p_train_hogares.rds")

## Personas ####
### Test ####
names(test_personas)[names(test_personas) == "P6020"] <- "mujer"
test_personas$mujer <- test_personas$mujer  - 1

names(test_personas)[names(test_personas) == "P6040"] <- "Edad"

names(test_personas)[names(test_personas) == "P6210"] <- "Nivel_educ"
test_personas$Nivel_educ <- factor(test_personas$Nivel_educ)

saveRDS(test_personas, "../stores/test_personas.rds")


### Train ####
names(n_train_personas)[names(n_train_personas) == "P6020"] <- "mujer"
n_train_personas$mujer <- n_train_personas$mujer  - 1

names(n_train_personas)[names(n_train_personas) == "P6040"] <- "Edad"

names(n_train_personas)[names(n_train_personas) == "P6210"] <- "Nivel_educ"
n_train_personas$Nivel_educ <- factor(n_train_personas$Nivel_educ)

saveRDS(n_train_personas, "../stores/n_train_personas.rds")


# Adding variables from personas to hogares db ----

## Promedio Edad ####
# Test
sum_edad <- test_personas %>% group_by(id) %>% summarize(edad_prom_h = mean(Edad, na.rm = TRUE))
summary(sum_edad)

test_hogares <- left_join(test_hogares, sum_edad)
colnames(test_hogares)

# Train
sum_edad_train <- train_personas %>% group_by(id) %>% summarize(edad_prom_h = mean(P6040, na.rm = TRUE))
summary(sum_edad_train)

p_train_hogares <- left_join(p_train_hogares, sum_edad_train)
colnames(p_train_hogares)

## Promedio Horas trabajadas ####
# Test
sum_horastrab <- test_personas %>% group_by(id) %>% summarize(horastrab_prom_h = mean(P6800, na.rm = TRUE))
summary(sum_horastrab)

test_hogares <- left_join(test_hogares, sum_horastrab)
colnames(test_hogares)

na_count <- colSums(is.na(test_hogares))
na_count <- sort(na_count, decreasing = TRUE)

test_hogares$horastrab_prom_h <- ifelse(is.na(test_hogares$horastrab_prom_h), 
                                        ifelse(test_hogares$pobre == 0, 
                                               mean(test_hogares$horastrab_prom_h, na.rm = TRUE), 
                                               0), 
                                        test_hogares$horastrab_prom_h)
# Train
names(p_train_hogares)[names(p_train_hogares) == "Pobre"] <- "pobre"  # Rename Pobre variable

sum_horastrab_train <- train_personas %>% group_by(id) %>% summarize(horastrab_prom_h = mean(P6800, na.rm = TRUE))
summary(sum_horastrab_train)

p_train_hogares <- left_join(p_train_hogares, sum_horastrab_train)
colnames(p_train_hogares)

na_count <- colSums(is.na(p_train_hogares))
na_count <- sort(na_count, decreasing = TRUE)

p_train_hogares$horastrab_prom_h <- ifelse(is.na(p_train_hogares$horastrab_prom_h), 
                                           ifelse(p_train_hogares$pobre == 0, 
                                                  mean(p_train_hogares$horastrab_prom_h, na.rm = TRUE), 
                                                  0), 
                                           p_train_hogares$horastrab_prom_h)

## Deleting var with missing values and unneeded variables ####
test_hogares <- subset(test_hogares, select = -Cuota_amortización)
p_train_hogares <- subset(p_train_hogares, select = -Cuota_amortización)

na_count <- colSums(is.na(test_hogares))
na_count <- sort(na_count, decreasing = TRUE)

test_hogares <- subset(test_hogares, select = -Arriendo)
p_train_hogares <- subset(p_train_hogares, select = -Arriendo)

test_hogares <- subset(test_hogares, select = -P5130)
p_train_hogares <- subset(p_train_hogares, select = -P5130)

test_hogares <- subset(test_hogares, select = -c(Clase, Fex_c, Fex_dpto))
p_train_hogares<- subset(p_train_hogares, select = -c(Clase, Fex_c, Fex_dpto))

## Transforming variable P6210 - Education level ####
# Train
train_personas$P6210 <- ifelse(is.na(train_personas$P6210), 
                               0, train_personas$P6210)
                               
train_personas$P6210 <- ifelse(train_personas$P6210 == 9, 
                               0, train_personas$P6210)                               

sum_educ <- train_personas %>% group_by(id) %>% summarize(max_educ_h = max(P6210, na.rm = TRUE))
summary(sum_educ)

p_train_hogares <- left_join(p_train_hogares, sum_educ)
colnames(p_train_hogares)
table(p_train_hogares$max_educ_h)

p_train_hogares$max_educ_h <- factor(p_train_hogares$max_educ_h)

# Test   
test_personas$Nivel_educ <- ifelse(is.na(test_personas$Nivel_educ), 
                               0, test_personas$Nivel_educ)

test_personas$Nivel_educ <- ifelse(test_personas$Nivel_educ == 7, 
                               0, test_personas$Nivel_educ) 

sum_educ_test <- test_personas %>% group_by(id) %>% summarize(max_educ_h = max(Nivel_educ, na.rm = TRUE))
summary(sum_educ_test)

test_hogares <- left_join(test_hogares, sum_educ_test)
colnames(test_hogares)
table(test_hogares$max_educ_h)

test_hogares$max_educ_h <- factor(test_hogares$max_educ_h)


## Transforming variable P6090 - Health affiliation ####
# Train
train_personas$P6090 <- ifelse(is.na(train_personas$P6090), 
                               0, train_personas$P6090)

train_personas$P6090 <- ifelse(train_personas$P6090 != 1, 
                               0, train_personas$P6090)                               

sum_health <- train_personas %>% group_by(id) %>% summarize(max_health_h = max(P6090, na.rm = TRUE))
summary(sum_health)

p_train_hogares <- left_join(p_train_hogares, sum_health)
colnames(p_train_hogares)
table(p_train_hogares$max_health_h)

# Test
test_personas$P6090 <- ifelse(is.na(test_personas$P6090), 
                               0, test_personas$P6090)

test_personas$P6090 <- ifelse(test_personas$P6090 != 1, 
                               0, test_personas$P6090)  

sum_health_test <- test_personas %>% group_by(id) %>% summarize(max_health_h = max(P6090, na.rm = TRUE))
summary(sum_health_test)

test_hogares <- left_join(test_hogares, sum_health_test)
colnames(test_hogares)
table(test_hogares$max_health_h)

# Variable income -----

# Joining the new variable tot_income_h to train_hogares
p_train_hogares <- left_join(p_train_hogares, sum_income)
colnames(p_train_hogares)

saveRDS(p_train_hogares, "../stores/p_train_hogares.rds")
saveRDS(test_hogares, "../stores/test_hogares.rds")
