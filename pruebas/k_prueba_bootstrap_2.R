# Título: k_prueba_bootstrap_2.R
#
# Autor: Alejandro M.
#
# Descripción: Esta prueba intenta buscar la generación de datos del caso 02 por bootstrap.

#Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)
library(nortest)

datos_reales <- read_csv("datos/reales/02a_brca_tidy.csv")
estadisticos_real <- read_csv("datos/reales/estadisticos/02a_estadisticos.csv")

simulados <- datos_reales %>%
    group_by(grupo) %>%
    mutate(
        valor = simular_normal_exacto(valor)
    ) %>%
    ungroup()

comprobar_normalidad_shapiro(simulados, "grupo", "valor")
leveneTest(valor ~ grupo, data = simulados)

estadisticos_real <- generar_estadisticos_np(datos_reales, "grupo")
estadisticos_sim  <- generar_estadisticos_np(simulados, "grupo")

estadisticos_real
estadisticos_sim