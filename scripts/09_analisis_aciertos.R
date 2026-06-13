# Título: 09_analisis_aciertos.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se analizan los resultados de la comparación de aciertos entre árboles y estadística clásica.

# Librerías y carga:
source("scripts/00_funciones.R")

library(rstatix)
library(dplyr)
library(ggplot2)
library(car)

# Cargamos los datos.
datos <- list.files(path = "resultados/resultados_exploratorios/tablas_comparativas", pattern = "\\.csv$", full.names = TRUE)

# Ahora, calculamos los aciertos en función del tipo de datos.
for (i in 1:6){
    # Título. Importante diferenciar los casos con 6 grupos y con 20 y 40 datos.
    tit <- sacar_titulo(datos[i])
    
    calcular_aciertos(datos[c(i, 6+i, 12+i, 18+i)], paste0("resultados/resultados_exploratorios/analisis_aciertos/09_aciertos_", tit, "_.csv"))
}

# Análisis 1: Generamos un gráfico de barras de aciertos.
datos_20 <- read.csv("resultados/resultados_exploratorios/analisis_aciertos/09_aciertos_4g_20n_.csv")
datos_30 <- read.csv("resultados/resultados_exploratorios/analisis_aciertos/09_aciertos_4g_30n_.csv")
datos_40 <- read.csv("resultados/resultados_exploratorios/analisis_aciertos/09_aciertos_4g_40n_.csv")

# Sacamos el n_grupo.
datos_20$X <- NULL
datos_20$n_grupo <- 20
datos_30$X <- NULL
datos_30$n_grupo <- 30
datos_40$X <- NULL
datos_40$n_grupo <- 40

datos <- bind_rows(datos_20, datos_30, datos_40)

datos$grupo <- as.factor(datos$grupo)
datos$n_grupo <- as.factor(datos$n_grupo)

p <- ggplot(datos, aes(x = grupo, y = aciertos, fill = n_grupo)) +
    geom_col(width = 0.75, position = "dodge") +
    labs(
        title = "Aciertos por grupo y n",
        x = "",
        y = "Número de aciertos",
    ) +
    theme_minimal()

print(p)

ggsave(filename = "resultados/resultados_exploratorios/analisis_aciertos/09_cambio_n.png", plot = p, width = 10, height = 6, dpi = 300)

# Análisis 2: Miramos si el cambio en k tiene algún impacto. Limpiamos los datos.
datos_6g_20n <- read.csv("resultados/resultados_exploratorios/analisis_aciertos/09_aciertos_6g_20n_.csv")
datos_6g_40n <- read.csv("resultados/resultados_exploratorios/analisis_aciertos/09_aciertos_6g_40n_.csv")

datos <- bind_rows(datos_20, datos_40, datos_6g_20n, datos_6g_40n)

datos$X <- NULL

datos <- datos %>%
    mutate(
        n_grupo = factor(rep(c(20, 40), each = 4, times = 2)),
        k = factor(rep(c(4, 6), each = 8)),
        aciertos = aciertos / (ifelse(k == 6, 15, 6)) # Proporción de aciertos (no aciertos absolutos.)
    )

datos$grupo <- as.factor(datos$grupo)
datos$k <- as.factor(datos$k)

p <- ggplot(datos, aes(x = grupo, y = aciertos, fill = k)) +
    geom_col(width = 0.75, position = "dodge") +
    labs(
        title = "Aciertos por grupo y k",
        x = "",
        y = "Proporción de aciertos",
    ) +
    theme_minimal()

print(p)

ggsave(filename = "resultados/resultados_exploratorios/analisis_aciertos/09_cambio_k.png", plot = p, width = 10, height = 6, dpi = 300)

# Análisis 3: Queremos comprobar si el tamaño del efecto afecta a la probabilidad de acertar.

# Unimos todo en un solo data.frame.
datos <- list.files(path = "resultados/resultados_exploratorios/tablas_comparativas", pattern = "\\.csv$", full.names = TRUE)

tabla <- data.frame()

for (i in datos) {
    i <- read.csv(i)
    tabla <- bind_rows(tabla, i)
}

# Limpiamos la tabla.
tabla_limpia <- tabla %>%
    mutate(
        grupo = factor(rep(c("1", "2", "3", "4"), each = 54)),
        acierto = ifelse(separado_clasica == separado_arbol, 1, 0),
        magnitude = magnitude
    )

tabla_limpia <- tabla_limpia[c("grupo", "acierto", "magnitude")]

# Realizamos una prueba de homogeneidad chi cuadrado.
tabla <- table(tabla_limpia$magnitude, tabla_limpia$acierto)
chisq.test(tabla)
chisq.test(tabla)$stdres

# No parece que haya una implicación directa, revisamos por grupo.
tabla_1 <- table(tabla_limpia$magnitude[tabla_limpia$grupo == 1], tabla_limpia$acierto[tabla_limpia$grupo == 1])
tabla_2 <- table(tabla_limpia$magnitude[tabla_limpia$grupo == 2], tabla_limpia$acierto[tabla_limpia$grupo == 2])
tabla_3 <- table(tabla_limpia$magnitude[tabla_limpia$grupo == 3], tabla_limpia$acierto[tabla_limpia$grupo == 3])
tabla_4 <- table(tabla_limpia$magnitude[tabla_limpia$grupo == 4], tabla_limpia$acierto[tabla_limpia$grupo == 4])

chisq.test(tabla_1)
chisq.test(tabla_1)$stdres
chisq.test(tabla_2)
chisq.test(tabla_2)$stdres
chisq.test(tabla_3)
chisq.test(tabla_3)$stdres
chisq.test(tabla_4)
chisq.test(tabla_4)$stdres
# Parece haber una relación, pero en todos los casos cambia de un tamaño de efecto al otro, así que no lo consideraremos como significativo.