# Título: 02a_preparar_datos_reales.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' pretende hacer una disposición en formato 'tidy' de datos procedentes de variables no normales, pero sí homocedásticas. Para ello, se mide la expresión del gen BRCA1 en 4 de sus posibles variantes en el extremo N-terminal. Se mide con respecto a la actividad del alelo silvestre.
#
# Referencia: Carvalho et al., 2014.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)
library(stringr)

# Importamos los datos y los limpiamos. Los del 'paper' pusieron un punto separador del mil y también del decimal.
datos <- read.csv("datos/reales/02_brca.csv", sep = ";")

datos$ratio[nchar(datos$ratio) >= 7 & str_count(datos$ratio, "\\.") == 2] <-
    sub("\\.", "", datos$ratio[nchar(datos$ratio) >= 7 & str_count(datos$ratio, "\\.") == 2])

datos$variant <- factor(datos$variant)

names(datos) <- c("grupo", "valor")

datos$valor <- as.numeric(datos$valor)

datos <- datos[datos$grupo == "Q1785H" | datos$grupo == "R1753T" | datos$grupo == "E1794D" | datos$grupo == "V1804D",]

# No son normales, pero sí homocedásticos para la variable 3.
comprobar_normalidad_shapiro(datos, "grupo", "valor")
leveneTest(valor ~ grupo, data = datos)

estadisticos_02 <- generar_estadisticos_np(datos, "grupo")

write_csv(estadisticos_02, "datos/reales/estadisticos/02a_estadisticos.csv")
write_csv(datos, "datos/reales/02a_brca_tidy.csv")