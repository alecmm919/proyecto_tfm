# Título: 03b_simular_datos.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del estrés en ratas. Se generan 4 grupos de n = 20, n = 30 y n = 40 cada uno.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)

# Cargamos los datos y convertimos en factores.
datos_reales <- read_csv("datos/reales/03a_estres_tidy.csv") %>%
    mutate(
        grupo = factor(grupo)
    )

estadisticos_real <- read_csv("datos/reales/estadisticos/03a_estadisticos.csv")

# Simulamos los grupos por sus distintos estadísticos.
for (n in c(20, 30, 40)){
    simulados <- datos_reales %>%
        group_by(grupo) %>%
        reframe(
            valor = simular_normal_exacto(valor, n)
        )
    
    # Pasamos a factores los simulados y comprobamos.
    simulados$grupo <- factor(simulados$grupo)
    comprobaciones_03(simulados, datos_reales, paste0("datos/simulados/03b_estres_", n, ".csv"), paste0("datos/simulados/estadisticos/03b_estres_", n, ".csv"))
}