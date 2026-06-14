# Título: g_prueba_kde.
#
# Autor: Alejandro M.
#
# Descripción: Esta prueba comprueba el uso de la función rnorm para compararla con kde.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)
library(cramer)
n = 15 # Número de datos.

# Cargamos los datos y convertimos en factores.
datos_reales <- read_csv("datos/reales/03a_datos_vih_tidy.csv") %>%
    mutate(
        origenes = factor(origenes),
        tipo_paciente = factor(tipo_paciente),
        int = interaction(origenes, tipo_paciente, drop = TRUE)
    )

estadisticos_real <- read_csv("datos/reales/estadisticos/03a_estadisticos.csv")

# Simulamos los grupos por sus distintos estadísticos.
simulados <- datos_reales %>%
    group_by(origenes, tipo_paciente, int) %>%
    group_modify(~ {
        mu <- mean(.x$valor)
        s  <- sd(.x$valor)
        
        tibble(
            valor = rnorm(n, mean = mu, sd = s)
        )
    }) %>%
    ungroup()

# Hacemos las comprobaciones: se mantiene la falta de parametricidad y los estadísticos.
comprobar_normalidad_shapiro(simulados, c("origenes", "tipo_paciente"), "valor")
leveneTest(valor ~ origenes * tipo_paciente, data = simulados)

estadisticos_real <- generar_estadisticos(datos_reales, c("origenes", "tipo_paciente"))
estadisticos_sim <- generar_estadisticos(simulados, c("origenes", "tipo_paciente"))

# Mostramos los estadísticos..
estadisticos_real
estadisticos_si