# Título: 01a_preparar_datos_reales.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' pretende hacer un análisis exploratorio de variables paramétricas. Para ello, se midieron los LVIDs (left ventricular internal diameter) tras tres semanas con los ratones sometidos a un tratamiento: control, piridostigmina, isoprenalina o ambos a la vez. Normalmente se asocia su aumento con la presencia de una patología cardíaca. En el artículo de referencia, existe un .csv con datos y en este 'scripts' se pretenden disponer en formato 'tidy' para posteriores análisis.
#
# Referencia: Marinkovic et al., 2026.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)

# Cargamos los datos y los limpiamos.
datos <- read.csv("datos/reales/01_corazon.csv", sep = ";")

datos <- datos[c(3:8, 11:16, 19:22, 24, 28:32), c("X.1", "X.2")]

names(datos) <- c("grupo", "valor")

datos$grupo[18:22] <- "B"

datos$grupo <- lapply(datos$grupo, function(x) substr(x, 1, 1))

datos$grupo <- unlist(datos$grupo)
datos$grupo <- factor(datos$grupo)

#. Comprobamos parametricidad, y vemos que, efectivamente, la variable es normal y homocedástica.
comprobar_normalidad_shapiro(datos, "grupo", "valor")
bartlett.test(valor ~ grupo, data=datos)

# Llamamos 'valor' a nuestra variable para ser consistente con otros datos del proyecto y los guardamos.
write_csv(datos, "datos/reales/01a_corazon_tidy.csv")

# Ahora, calculamos los estadísticos.
estadisticos_01 <- generar_estadisticos(datos, "grupo")

# Guardamos los estadísticos.
write_csv(estadisticos_01, "datos/reales/estadisticos/01a_estadisticos.csv")