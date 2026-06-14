# Título: e_prueba_simulacion.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico de los eosinófilos/piel atópica y fumar. Se trata de un contexto homocedástico, pero no normal.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)
set.seed(123)
n = 1000 # Número de datos.

# Cargamos los datos.
datos_reales <- read_csv("datos/reales/02a_eosinofilos_tidy.csv")
estadisticos_real <- read_csv("datos/reales/estadisticos/02a_estadisticos.csv")

# Generamos un 'tibble' para poder meter los datos simulados.
simulados <- tibble(
    tipo = rep(c("hsa", "hsna", "nhsa", "nhsna"), each = n)
)

datos_sim <- datos_reales %>%
    group_by(tipo) %>%
    summarise(
        valor = list(sample(valor, size = n, replace = TRUE)),
        .groups = "drop"
    )