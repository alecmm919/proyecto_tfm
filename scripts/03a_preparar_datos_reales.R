# Título: 03a_preparar_datos_reales.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' pretende hacer un análisis exploratorio de variables normales y heterocedásticas. Se usará el contexto del estrés en ratas. Para ello, se medirá la concentración de receptor de N-metil-d-aspartato 2. Los grupos son 1: control; 2: sometidos a estrés durante 5 días; 3: 14 y 4: 21 días, respectivamente. No se trata de las mismas ratas, por lo que los datos son independientes.
#
# Referencia: Han et al., 2017.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)

# Cargamos los datos. Como ya venían en formato 'tidy', fue muy sencillo.
datos <- read.csv("datos/reales/03_estres.csv")

datos$grupo <- factor(datos$GROUP)
datos$valor <- as.numeric(gsub(",", ".", datos$NR2A)) # Cambiamos a punto decimal.

datos$NR2A <- NULL
datos$GROUP <- NULL

#. Comprobamos parametricidad, y vemos que, efectivamente, la variable es normal, pero heterocedástica.
comprobar_normalidad_shapiro(datos, "grupo", "valor")
bartlett.test(valor ~ grupo, data=datos)

# Guardamos los datos.
write_csv(datos, "datos/reales/03a_estres_tidy.csv")

# Planteamos una exploración estadística básica. Nos servirá para las simulaciones posteriores. Los guardamos en un .csv.
estadisticos_03 <- generar_estadisticos(datos, "grupo")
write_csv(estadisticos_03, "datos/reales/estadisticos/03a_estadisticos.csv")