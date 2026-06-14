# Título: h_prueba_3_anterior.R [DESCARTADO]
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del VIH, siguiendo la Bibliografía y los casos que buscamos. Se trata de un contexto normal y heterocedástico.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
n = 10 # Número de datos.

# Cargamos los estadísticos
estadisticos_real <- read_csv("datos/reales/estadisticos/03a_estadisticos.csv")
datos_reales <- read_csv("datos/reales/03a_datos_vih_tidy.csv")

# Pasamos a factor (los datos provienen de los .csv.)
estadisticos_real %>%
    mutate(
        origenes = factor(origenes),
        tipo_paciente = factor(tipo_paciente)
    )

# Generamos un 'tibble' con 4 grupos y 1000 muestras por grupo. Hay que generar este 'tibble' en el orden de los estadísticos.
simulados <- tibble(
    origenes = rep(c("on", "pre"), each = 2*n),
    tipo_paciente = rep(c("IR", "ISR"), each = n, times = 2)
)

# Pasamos a factores.
simulados$origenes <- factor(simulados$origenes)
simulados$tipo_paciente <- factor(simulados$tipo_paciente)

# Añadimos las simulaciones a la columna correspondiente.
simulados$valor <- simular_con_estadisticos(4, estadisticos_real, simulados)

# Revisamos que se siguen cumpliendo los supuestos de los que partimos. Primero, la normalidad. Al haber n grandes, usamos Kolmogorov-Smirnov. Siguen siendo datos normales y heterocedásticos.
comprobar_normalidad_kolmogorov(simulados, c("origenes", "tipo_paciente"), "valor")
bartlett.test(valor ~ interaction(origenes, tipo_cd4, tipo_paciente), data=simulados)

# Comprobamos que los estadísticos se han respetado entre los datos normales y los simulados.
estadisticos_sim <- generar_estadisticos(simulados, c("origenes", "tipo_paciente"))

# CONTEXTO DESCARTADO.