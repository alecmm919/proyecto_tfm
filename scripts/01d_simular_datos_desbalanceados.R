# Título: 01d_simular_datos_desbalanceados.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico normal y homocedástico. Los datos son independientes. En este caso, los grupos tienen distinto número de datos. Estos datos tienen únicamente objetivos exploratorios, así que se han desbalanceado arbitrariamente.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)

# Generamos grupos desbalanceados
n <- c(15, 20, 25, 30)
names(n) <- c("C", "P", "I", "B") # Asignamos un n a cada grupo.

datos_reales <- read.csv("datos/reales/01a_corazon_tidy.csv")
estadisticos_real <- read.csv("datos/reales/estadisticos/01a_estadisticos.csv")

# Pasamos a factor.
estadisticos_real %>%
    mutate(
        grupo = factor(grupo)
    )

simulados <- datos_reales %>%
    group_by(grupo) %>%
    reframe(
        valor = simular_normal_exacto(valor, n[first(grupo)])
    )

# Pasamos a factores.
simulados$gravedad <- factor(simulados$grupo)

# Guardamos.
comprobaciones_01(simulados, "datos/simulados/01d_corazon_desbalanceados.csv", "datos/simulados/estadisticos/01c_corazon_desbalanceados.csv")