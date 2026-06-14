# Título: m_caso4_jitter.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de esta prueba es generar datos para el caso 04, pero con la función 'jitter'.

#Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)
library(nortest)
library(ks)

datos_reales <- read_csv("datos/reales/04a_hepatitis_tidy.csv")
estadisticos_real <- read_csv("datos/reales/estadisticos/04a_estadisticos.csv")

# Buscamos generar datos no normales, pero sí homocedásticas, que mantengan la mediana y el RI de los originales.

desviacion <- datos_reales %>%
    mutate(grupo = factor(grupo)) %>%
    group_by(grupo) %>%
    mutate(
        desviacion = (valor - median(valor)) / IQR(valor)
    ) %>%
    ungroup()

# Simulamos.
simulados <- datos_reales %>%
    group_by(grupo) %>%
    mutate(
        amount_jitter = 0.01 * IQR(valor)
    ) %>%
    ungroup()

# Con este bucle, comprobamos que no hay valores negativos.
for (i in seq_len(nrow(simulados))) {
    nuevo <- jitter(simulados$valor[i], amount = simulados$amount_jitter[i])
}

simulados <- simulados %>%
    select(-amount_jitter)

# Comprobamos que los supuestos paramétricos se respetan con respecto a los datos originales.

simulados$grupo <- factor(simulados$grupo)

comprobar_normalidad_shapiro(simulados, "grupo", "valor")
leveneTest(valor ~ grupo, data=simulados)

estadisticos_real <- generar_estadisticos_np(datos_reales, "grupo")
estadisticos_sim  <- generar_estadisticos_np(simulados, "grupo")

estadisticos_real
estadisticos_sim