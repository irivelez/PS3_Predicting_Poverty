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

# Importing databases ----------------------------------------------------

i_test_hogares <- readRDS("../stores/i_test_hogares.rds")
i_train_hogares <- readRDS("../stores/i_train_hogares.rds")


# View data ---------------------------------------------------------------
## General ----
p_load(GGally)
i_train_hogares$pobre <- factor(i_train_hogares$pobre)
ggpairs(i_train_hogares, columns = 10:17, ggplot2::aes(colour = pobre)) +
  theme_bw()


## tot_income_h ----
summary(i_train_hogares$tot_income_h) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

ggplot(i_train_hogares, aes(x = tot_income_h)) +
  geom_histogram(fill = "lightblue") +
  theme_bw() +
  labs(x = "Salario", y = "Cantidad")


## pobre ----
#Creamos un data frame con los datos 
table(i_train_hogares$pobre)

# Creamos el data.frame con los datos proporcionados
datos_pob <- data.frame(
  Categoría = c("Pobreza", "No Pobreza"),
  Cantidad = c(33024, 131936)
)

# Calculamos el porcentaje de cada categoría
datos_pob$Porcentaje <- datos_pob$Cantidad / sum(datos_pob$Cantidad) * 100

# Creamos el gráfico de torta usando ggplot2
grafico_torta <- ggplot(datos_pob, aes(x = "", y = Porcentaje, fill = Categoría)) +
  geom_bar(stat = "identity", width = 1) +
  theme_minimal() +
  coord_polar("y", start = 0) +
  theme_void() +  # Eliminamos los ejes y el fondo para mejorar el aspecto
  labs(title = "Distribución de Pobreza",
       subtitle = "Porcentaje de personas en categorías de pobreza y no pobreza",
       caption = "Fuente: DANE") +
  scale_fill_manual(values = c("lightblue", "lightgreen"), 
                    labels = c("No Pobreza", "Pobreza"), 
                    name = "Categoría") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption = element_text(hjust = 0, size = 8)) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), position = position_stack(vjust = 0.5))

# Mostramos el gráfico
print(grafico_torta)


## Dominio ----
table(i_train_hogares$Dominio)

# Creamos el data.frame con los datos proporcionados
datos_ciudades <- data.frame(
  Ciudad = c("ARMENIA", "BARRANQUILLA", "BOGOTA", "BUCARAMANGA", "CALI", "CARTAGENA", "CUCUTA", "FLORENCIA", "IBAGUE",
             "MANIZALES", "MEDELLIN", "MONTERIA", "NEIVA", "PASTO", "PEREIRA", "POPAYAN", "QUIBDO", "RESTO URBANO",
             "RIOHACHA", "RURAL", "SANTA MARTA", "SINCELEJO", "TUNJA", "VALLEDUPAR", "VILLAVICENCIO"),
  Cantidad = c(5547, 6568, 10567, 5197, 6666, 5335, 4677, 4880, 5286, 6108, 8921, 5017, 5546, 4924, 5545, 5938,
               4075, 17049, 5473, 15472, 5879, 5321, 4648, 5030, 5291)
)

# Creamos el gráfico de barras usando ggplot2
grafico_barras <- ggplot(datos_ciudades, aes(x = Ciudad, y = Cantidad, fill = Ciudad)) +
  geom_bar(stat = "identity") +
  theme_minimal() +  # Aplicamos un tema más minimalista
  labs(title = "Distribución de Datos por Ciudad",
       x = "Ciudad",
       y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Mostramos el gráfico
print(grafico_barras)

## Num_cuartos ----
summary(i_train_hogares$Num_cuartos)
table(i_train_hogares$Num_cuartos)

# Creamos el data.frame con los datos proporcionados
datos_cuartos <- data.frame(
  Cuartos = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 43, 98),
  Frecuencia = c(11064, 22129, 56051, 52336, 16728, 4779, 1287, 401, 106, 48, 16, 6, 1, 3, 1, 1, 1, 1, 1)
)

# Creamos el gráfico de barras usando ggplot2 con paleta de colores predeterminada
grafico_barras <- ggplot(datos_cuartos, aes(x = factor(Cuartos), y = Frecuencia, fill = factor(Cuartos))) +
  geom_bar(stat = "identity") +
  scale_fill_discrete() +  # Utilizamos la paleta de colores predeterminada de ggplot2
  theme_minimal() +  # Aplicamos un tema más minimalista
  labs(title = "Distribución del Número de Cuartos",
       x = "Número de Cuartos",
       y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mostramos el gráfico
print(grafico_barras)


## Tipo_vivienda ----
table(i_train_hogares$Tipo_vivienda)

# Creamos el data.frame con los datos proporcionados
datos_vivienda <- data.frame(
  Tipo_Vivienda = c("Propia", "Propia en deuda", "En arriendo", "Usufracto", "Posesión sin título", "Otra"),
  Cantidad = c(62276, 5626, 64453, 24865, 7574, 166)
)

# Creamos una paleta de colores personalizada
paleta_colores <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3")

# Creamos el gráfico de torta usando ggplot2 con paleta de colores y etiquetas
grafico_torta <- ggplot(datos_vivienda, aes(x = "", y = Cantidad, fill = Tipo_Vivienda)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +  
  labs(title = "Distribución de Tipos de Vivienda",
       caption = "Fuente: DANE") +
  scale_fill_manual(values = paleta_colores) +  # Utilizamos la paleta de colores personalizada
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 8)) +
  geom_text(aes(label = Cantidad), 
            position = position_stack(vjust = 0.5))

# Mostramos el gráfico
print(grafico_torta)


## Li ----
summary(i_train_hogares$Li) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

ggplot(i_train_hogares, aes(x = Li)) +
  geom_histogram(fill = "lightblue") +
  theme_bw() +
  labs(x = "Línea de indigencia", y = "Cantidad")

## Lp ----
summary(i_train_hogares$Lp) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

ggplot(i_train_hogares, aes(x = Lp)) +
  geom_histogram(fill = "lightgreen") +
  theme_bw() +
  labs(x = "Línea de pobreza", y = "Cantidad")

## Num_personas_cuarto ----
table(i_train_hogares$Num_personas_cuarto)

ggplot(i_train_hogares, aes(x = Num_personas_cuarto)) +
  geom_histogram(fill = "lightblue") +
  theme_bw() +
  labs(x = "Personas por cuarto", y = "Cantidad")


## edad_prom_h ----
summary(i_train_hogares$edad_prom_h) %>%
  as.matrix() %>%
  as.data.frame() 

ggplot(i_train_hogares, aes(x = edad_prom_h)) +
  geom_histogram(fill = "lightgreen") +
  theme_bw() +
  labs(x = "Edad promedio por hogar", y = "Cantidad")


## horastrab_prom_h ----
summary(i_train_hogares$horastrab_prom_h) %>%
  as.matrix() %>%
  as.data.frame() 

ggplot(i_train_hogares, aes(x = horastrab_prom_h)) +
  geom_histogram(fill = "lightblue") +
  theme_bw() +
  labs(x = "Horas de trabajo promedio por hogar", y = "Cantidad")



## max_educ_h ----
table(i_train_hogares$max_educ_h)

# Creamos el data.frame con los datos proporcionados
datos_educacion <- data.frame(
  Nivel_Educacion = c("No informa", "Ninguno", "Preescolar", "Básica primaria",
                      "Básica secundaria", "Media", "Universitaria"),
  Cantidad = c(1, 2445, 25, 16492, 18111, 47501, 80385)
)

# Creamos una paleta de colores personalizada
paleta_colores <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b2df8a")

# Creamos el gráfico de barras usando ggplot2 con la paleta de colores personalizada
grafico_barras <- ggplot(datos_educacion, aes(x = Nivel_Educacion, y = Cantidad, fill = Nivel_Educacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = paleta_colores) +  # Utilizamos la paleta de colores personalizada
  theme_minimal() +  # Aplicamos un tema más minimalista
  labs(title = "Distribución del Nivel Máximo de Educación en un Hogar",
       x = "Nivel de Educación",
       y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mostramos el gráfico
print(grafico_barras)



## max_health_h ----
table(i_train_hogares$max_health_h)

# Creamos el data.frame con los datos proporcionados
datos_health <- data.frame(
  Categoría = c("Afiliado", "No Afiliado"),
  Cantidad = c(159508, 5452)
)

# Calculamos el porcentaje de cada categoría
datos_health$Porcentaje <- datos_health$Cantidad / sum(datos_health$Cantidad) * 100

# Creamos el gráfico de torta usando ggplot2
grafico_torta <- ggplot(datos_health, aes(x = "", y = Porcentaje, fill = Categoría)) +
  geom_bar(stat = "identity", width = 1) +
  theme_minimal() +
  coord_polar("y", start = 0) +
  theme_void() +  
  labs(title = "Afiliado seguridad social",
       subtitle = "Porcentaje de personas afiliadas a seguridad social",
       caption = "Fuente: DANE") +
  scale_fill_manual(values = c("lightblue", "lightgreen"), 
                    labels = c("Afiliado", "No Afiliado"), 
                    name = "Categoría") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption = element_text(hjust = 0, size = 8)) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), position = position_stack(vjust = 0.5))

# Mostramos el gráfico
print(grafico_torta)















































