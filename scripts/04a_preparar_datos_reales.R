# Título: 04a_preparar_datos_reales.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' pretende hacer un análisis exploratorio de variables no normales y heterocedásticos. Se parte de los datos del paper Zhen et al., 2024. Se pretende predecir el estadío de la fibrosis en la hepatitis B en función de la concentración de GPR.
#
# Referencia: Zhen et al., 2024

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)

# Se toman los datos desde los 5 csv publicados en el 'paper'. Y tomamos únicamente las variables que nos interesan.
datos_0 <- read.csv("datos/reales/04_f0.csv", sep = ";")
datos_1 <- read.csv("datos/reales/04_f1.csv", sep = ";")
datos_2 <- read.csv("datos/reales/04_f2.csv", sep = ";")
datos_3 <- read.csv("datos/reales/04_f3.csv", sep = ";")
datos_4 <- read.csv("datos/reales/04_f4.csv", sep = ";")

datos_0 <- datos_0[c("name", "GPR", "age", "sex", "grade")]
datos_1 <- datos_1[c("name", "GPR", "age", "sex", "grade")]
datos_2 <- datos_2[c("name", "GPR", "age", "sex", "grade")]
datos_3 <- datos_3[c("name", "GPR", "age", "sex", "grade")]
datos_4 <- datos_4[c("name", "GPR", "age", "sex", "grade")]

datos_0 <- datos_0[datos_0$age %in% 40:50 & datos_0$sex == "male", ]
datos_0 <- datos_0[,c(1,2)]
datos_1 <- datos_1[datos_1$age %in% 40:50 & datos_1$sex == "male", ]
datos_1 <- datos_1[,c(1,2)]
datos_2 <- datos_2[datos_2$age %in% 40:50 & datos_2$sex == "male", ]
datos_2 <- datos_2[,c(1,2)]
datos_3 <- datos_3[datos_3$age %in% 40:50 & datos_3$sex == "male", ]
datos_3 <- datos_3[,c(1,2)]
datos_4 <- datos_4[datos_4$age %in% 40:50 & datos_4$sex == "male", ]
datos_4 <- datos_4[,c(1,2)]

# Para unir las 5 tablas en una sola, marcamos la grupo.
datos_0 <- datos_0 %>%
    mutate(grupo = "GPR_0") %>%
    dplyr::select(grupo, valor = GPR) # En este caso, hay que poner dplyr:: porque la función select estaba dando problemas.

datos_1 <- datos_1 %>%
    mutate(grupo = "GPR_1") %>%
    dplyr::select(grupo, valor = GPR)

datos_2 <- datos_2 %>%
    mutate(grupo = "GPR_2") %>%
    dplyr::select(grupo, valor = GPR)

datos_3 <- datos_3 %>%
    mutate(grupo = "GPR_3") %>%
    dplyr::select(grupo, valor = GPR)

datos_4 <- datos_4 %>%
    mutate(grupo = "GPR_4") %>%
    dplyr::select(grupo, valor = GPR)

datos <- bind_rows(datos_0, datos_1, datos_2, datos_3, datos_4)

datos <- datos[datos$grupo != "GPR_1",]

# Ahora, convertimos los datos a los tipos que nos interesan.
datos$grupo <- factor(datos$grupo, ordered = TRUE) # En este caso, los factores deben ir ordenados.
datos$valor <- as.numeric(gsub(",",".", datos$valor))

write_csv(datos, "datos/reales/04a_hepatitis_tidy.csv")

# Comprobamos la parametricidad. Vemos que los datos no son normales ni homocedásticos. En este caso, tenemos 4 'ties' con valor 0, por lo que Kolmogorov no puede usarse. Tampoco podemos usar Shapiro porque hay 70 individuos por grupo. Por ello, se usará Cramer-Von-Mises para este contraste.
comprobar_normalidad_shapiro(datos, "grupo", "valor")
leveneTest(valor ~ grupo, data = datos) # Usamos Levene porque los datos no son normales.

estadisticos_04 <- generar_estadisticos_np(datos, "grupo")
write_csv(estadisticos_04, "datos/reales/estadisticos/04a_estadisticos.csv")