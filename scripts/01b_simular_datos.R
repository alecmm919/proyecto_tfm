# Título: 01b_simular_datos.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico normal y homocedástico. Los datos son independientes. Se generan 20, 30 y 40 datos.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)

datos_reales <- read.csv("datos/reales/01a_corazon_tidy.csv")
estadisticos_real <- read.csv("datos/reales/estadisticos/01a_estadisticos.csv")

# Pasamos a factor (los datos provienen de los .csv.)
estadisticos_real %>%
    mutate(
        grupo = factor(grupo)
    )

# Simulamos.
for (n in c(20, 30, 40)){
    simulados <- datos_reales %>%
        group_by(grupo) %>%
        reframe(
            valor = simular_normal_exacto(valor, n)
        )
    
    # Pasamos a factores los simulados.
    simulados$gravedad <- factor(simulados$grupo)
    
    # Guardamos.
    comprobaciones_01(simulados, paste0("datos/simulados/01b_corazon_", n, ".csv"), paste0("datos/simulados/estadisticos/01b_corazon_", n, ".csv"))
}
