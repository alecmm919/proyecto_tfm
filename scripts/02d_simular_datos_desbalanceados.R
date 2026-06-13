# Título: 02d_simular_datos_desbalanceados.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico de variables homocedásticas, pero no normales. En este caso, los grupos tienen distinto número de datos. Estos datos tienen únicamente objetivos exploratorios, así que se han desbalanceado arbitrariamente.

#Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)
library(nortest)

# Generamos grupos desbalanceados
n <- c(20, 25, 30, 15)
names(n) <- c("V1804D", "E1794D", "Q1785H", "R1753T") # Nombre de los grupos.

datos_reales <- read_csv("datos/reales/02a_brca_tidy.csv")
estadisticos_real <- read_csv("datos/reales/estadisticos/02a_estadisticos.csv")

# Buscamos generar datos no normales, pero sí homocedásticas, que mantengan la mediana y el RI de los originales.

ri_global <- IQR(datos_reales$valor)
desviacion <- (datos_reales$valor - median(datos_reales$valor)) / ri_global  # Las desviaciones la normalizamos con respecto a la mediana y al RI.

# Procedemos a simular.
simulados <- datos_reales %>% # Separamos por tipo.
    distinct(grupo) %>%
    group_by(grupo) %>%
    reframe(
        {
            med_tipo <- median(datos_reales$valor[datos_reales$grupo == first(grupo)]) # Sacamos la mediana del tipo.
            desv <- sample(desviacion, size = n[grupo], replace = TRUE) # Tomamos unas desviaciones pseudoaleatorias.
            desv <- desv - median(desv) # La centramos.
            desv <- desv / IQR(desv) # La normalizamos a RI = 1.
            valor <- med_tipo + desv * ri_global # Generamos el valor.
            tibble(valor = valor)
        }
    )

# Comprobamos que los supuestos paramétricos se respetan con respecto a los datos originales.
simulados$grupo <- factor(simulados$grupo)
comprobaciones_02(simulados, datos_reales, "datos/simulados/02d_brca_desbalanceados.csv", "datos/simulados/estadisticos/02d_brca_desbalanceados.csv")