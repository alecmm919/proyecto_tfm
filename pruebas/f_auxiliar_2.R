# Título: f_auxiliar_2.R [DESCARTADO]
#
# Autor: Alejandro M.
#
# Descripción: Este 'script' pretende buscar una alternativa más fiable para los datos del caso 2.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)

datos <- read.csv("datos/reales/A_datos_dias.csv", sep = ";")

# Eliminamos y limpiamos los datos.
datos["X"] <- NULL
datos <- datos[-7,]

# Cambiamos la etiqueta según el paciente.
names(datos) <- c("hora", rep("sano", times = 10), rep("asma", times = 20))

# Separamos los datos.
datos <- datos[1:6,]

datos <- pivot_longer(datos, cols = -hora, names_to = c("estado", "replica"), names_sep = "_", values_to = "valor")

datos["replica"] <- NULL

datos$valor <- as.numeric(gsub(",", ".", datos$valor))

datos$hora <- as.factor(datos$hora)
datos$estado <- as.factor(datos$estado)

# Sacamos algunos grupos. Tomaremos las horas de las 8:00 y 20:00.
datos <- datos[datos$hora == "08:00" | datos$hora == "20:00",]

comprobar_normalidad_shapiro(datos, c("hora", "estado"), "valor")
leveneTest(valor ~ hora*estado, data = datos)