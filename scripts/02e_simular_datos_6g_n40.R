# Título: 02e_simular_datos_6g_n40.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico de variables homocedásticas, pero no normales. Se generan dos grupos extra con 40 datos, de la misma forma que se realizó en el 02c.

#Librerías y carga:
source("scripts/00_funciones.R")

n <- 40

library(tidyverse)
library(car)
library(nortest)

datos_reales <- read_csv("datos/reales/02a_brca_tidy.csv")
estadisticos_real <- read_csv("datos/reales/estadisticos/02a_estadisticos.csv")


ri_global <- IQR(datos_reales$valor)
desviacion <- (datos_reales$valor - median(datos_reales$valor)) / ri_global  # Las desviaciones la normalizamos con respecto a la mediana y al RI.
ri_medio <- mean(estadisticos_real$RI)
mediana_glob <- median(datos_reales$valor)

# Procedemos a simular. Simulamos el resto de grupos con la misma filosofía que el 'script' 2b.
grupos_extra <- c("G5", "G6")
medianas_extra <- c(median(datos_reales$valor), median(datos_reales[datos_reales$grupo == "R1753T", ]$valor) + median(datos_reales[datos_reales$grupo == "Q1785H", ]$valor)) # Ponemos como medianas la mediana de todos los datos (G5), y la suma de las medianas de dos grupos (G6).

medianas_objetivo <- datos_reales %>%
    group_by(grupo) %>%
    summarise(med_tipo = median(valor), .groups = "drop") %>%
    bind_rows(
        tibble(
            grupo = grupos_extra,
            med_tipo = medianas_extra
        )
    )

simulados <- medianas_objetivo %>%
    group_by(grupo) %>%
    reframe(
        {
            med_tipo <- first(med_tipo)
            desv <- sample(desviacion, size = n, replace = TRUE)
            desv <- desv - median(desv)
            desv <- desv / IQR(desv)
            valor <- med_tipo + desv * ri_global
            tibble(valor = valor)
        }
    )

# Comprobamos los supuestos.

simulados$grupo <- factor(simulados$grupo)
comprobaciones_02(simulados, datos_reales, "datos/simulados/02e_brca_6g_n40.csv", "datos/simulados/estadisticos/02e_brca_6g_n40.csv")