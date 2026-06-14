# Título: q_posible_nuevo_4.R [DESCARTADO]
#
# Autor: Alejandro M.
#
# Descripción: Esta prueba busca un posible caso 04 nuevo que reemplace al anterior escogido.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)

# Se toman los datos desde los 5 csv publicados en el 'paper'. Y tomamos únicamente las variables que nos interesan.
datos <- read.csv("datos/reales/prueba.csv")

# Ahora, convertimos los datos a los tipos que nos interesan.

datos <- datos[,c("grupo", "var4")]

datos$grupo <- factor(datos$grupo)
datos$valor <- as.numeric(gsub(",",".", datos$var4))

datos <- datos[,c("grupo", "valor")]

write_csv(datos, "datos/reales/04a_hepatitis_tidy.csv")

# Comprobamos la parametricidad. Vemos que los datos no son normales ni homocedásticos. En este caso, tenemos 4 'ties' con valor 0, por lo que Kolmogorov no puede usarse. Tampoco podemos usar Shapiro porque hay 70 individuos por grupo. Por ello, se usará Cramer-Von-Mises para este contraste.
comprobar_normalidad_shapiro(datos, "grupo", "valor")
leveneTest(valor ~ grupo, data = datos)