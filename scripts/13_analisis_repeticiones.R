# Título: 13_analisis_repeticiones.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es repetir muchas veces la simulación de datos, comparación de la separación por estadística clásica y árboles y recuento de aciertos para buscar tendencias estadísticas ('scripts' 1-4, 8 y 9). Para ello, tomamos los 60 datos generados por los apartados c y e de los primeros 4 'scripts' y los combinamos pseudoaleatoriamente para poder extraer muestras de distintos subgrupos de datos.

#Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(purrr)
library(rstatix)

# Cargamos los datos.
archivos_1 <- list.files("datos/simulados", pattern = "^01[c-e]_corazon_6g_n[0-9][0-9].csv$", full.names = TRUE)
datos_1 <- map_dfr(archivos_1, read_csv)

archivos_2 <- list.files("datos/simulados", pattern = "^02[c-e]_brca_6g_n[0-9][0-9].csv$", full.names = TRUE)
datos_2 <- map_dfr(archivos_2, read_csv)

archivos_3 <- list.files("datos/simulados", pattern = "^03[c-e]_estres_6g_n[0-9][0-9].csv$", full.names = TRUE)
datos_3 <- map_dfr(archivos_3, read_csv)

archivos_4 <- list.files("datos/simulados", pattern = "^04[c-e]_hepatitis_6g_n[0-9][0-9].csv$", full.names = TRUE)
datos_4 <- map_dfr(archivos_4, read_csv)

# Limpiamos.
datos_1$gravedad <- NULL
datos_1$grupo <- factor(datos_1$grupo)
datos_2$grupo <- factor(datos_2$grupo)
datos_3$grupo <- factor(datos_3$grupo)
datos_4$grupo <- factor(datos_4$grupo)

for (i in 1:500){
    contador <- 1 # Para adaptar el bucle.
    # Sacamos las subdivisiones de datos.
    for (n in c(20, 30, 40)){
        for (caso in list(datos_1, datos_2, datos_3, datos_4)){
            datos_corte <- caso %>%
                group_by(grupo) %>%
                slice_sample(n = n)
            
            # La función de comparación da error si se le pasa un tibble como argumento.
            datos_corte <- as.data.frame(datos_corte)
            
            archivo <- paste0("resultados/resultados_finales/repeticiones/13_comparacion_cla_arb_0", contador, "_n", n, ".csv")
            
            if (contador == 1) {
                
                # Separación clásica.
                clasica <- as.data.frame(TukeyHSD(aov(valor ~ grupo, data = datos_corte))$grupo)
                
                # Identificamos los grupos separados.
                clasica$separado <- ifelse(clasica$`p adj` < 0.05, TRUE, FALSE)
                clasica$grupo <- rownames(clasica)
                clasica <- clasica[, c("grupo", "separado")]
                
                # Aplicamos la comparación y guardamos.
                escribir_resultado(datos_corte, clasica, archivo, homocedastico = TRUE)
                contador <- contador + 1
            }
            
            else if (contador == 2 | contador == 4) {
                
                # Separación clásica.
                clasica <- as.data.frame(dunn_test(datos_corte, valor ~ grupo))[c(2, 3, 8)]
                
                # Pegamos los nombres de los grupos.
                clasica$grupo <- paste0(clasica$group1, "-", clasica$group2)
                rownames(clasica) <- clasica$grupo
                
                clasica$separado <- ifelse(clasica$p.adj < 0.05, TRUE, FALSE)
                clasica <- clasica[, c("grupo", "separado")]
                
                # Aplicamos la comparación y guardamos.
                escribir_resultado(datos_corte, clasica, archivo, homocedastico = FALSE)
                
                if (contador == 4) {
                    contador <- 1
                }
                else {
                    contador <- contador + 1
                }
            }
            
            else { # Caso 3.
                clasica <- as.data.frame(games_howell_test(datos_corte, valor ~ grupo))[c(2, 3, 7)]
                
                # Pegamos los nombres de los grupos.
                clasica$grupo <- paste0(clasica$group1, "-", clasica$group2)
                rownames(clasica) <- clasica$grupo
                
                clasica$separado <- ifelse(clasica$p.adj < 0.05, TRUE, FALSE)
                clasica <- clasica[, c("grupo", "separado")]
                
                # Aplicamos la comparación y guardamos.
                escribir_resultado(datos_corte, clasica, archivo, homocedastico = TRUE)
                
                contador <- contador + 1
            }
        }
    }
}

# Ahora, calculamos los aciertos en función del tipo de datos.
datos <- list.files(path = "resultados/resultados_finales/repeticiones/", pattern = "^13_comparacion", full.names = TRUE)

# Ahora, calculamos los aciertos en función del tipo de datos.
for (i in 1:3){ # Este bucle, por n = 20, n = 30 y n = 40.
    for (k in 4:6){ # Este bucle itera por k.
        calcular_aciertos(datos[c(i, 3+i, 6+i, 9+i)], paste0("resultados/resultados_finales/repeticiones/13_aciertos_n", (1+i)*10, "_g", k, ".csv"), n_grupo = k)
    }
}

# Repetimos el mismo proceso que en 'script' 09.
datos_20_4 <- read.csv("resultados/resultados_finales/repeticiones/13_aciertos_n20_g4.csv")
datos_20_5 <- read.csv("resultados/resultados_finales/repeticiones/13_aciertos_n20_g5.csv")
datos_20_6 <- read.csv("resultados/resultados_finales/repeticiones/13_aciertos_n20_g6.csv")
datos_30_4 <- read.csv("resultados/resultados_finales/repeticiones/13_aciertos_n30_g4.csv")
datos_30_5 <- read.csv("resultados/resultados_finales/repeticiones/13_aciertos_n30_g5.csv")
datos_30_6 <- read.csv("resultados/resultados_finales/repeticiones/13_aciertos_n30_g6.csv")
datos_40_4 <- read.csv("resultados/resultados_finales/repeticiones/13_aciertos_n40_g4.csv")
datos_40_5 <- read.csv("resultados/resultados_finales/repeticiones/13_aciertos_n40_g5.csv")
datos_40_6 <- read.csv("resultados/resultados_finales/repeticiones/13_aciertos_n40_g6.csv")

# Sacamos el n_grupo.
datos_20_4$X <- NULL
datos_20_4$n_grupo <- 20
datos_30_4$X <- NULL
datos_30_4$n_grupo <- 30
datos_40_4$X <- NULL
datos_40_4$n_grupo <- 40
datos_20_5$X <- NULL
datos_20_5$n_grupo <- 20
datos_30_5$X <- NULL
datos_30_5$n_grupo <- 30
datos_40_5$X <- NULL
datos_40_5$n_grupo <- 40
datos_20_6$X <- NULL
datos_20_6$n_grupo <- 20
datos_30_6$X <- NULL
datos_30_6$n_grupo <- 30
datos_40_6$X <- NULL
datos_40_6$n_grupo <- 40

datos_4 <- bind_rows(datos_20_4, datos_30_4, datos_40_4)
datos_5 <- bind_rows(datos_20_5, datos_30_5, datos_40_5)
datos_6 <- bind_rows(datos_20_6, datos_30_6, datos_40_6)

datos_4$grupo <- as.factor(datos_4$grupo)
datos_4$n_grupo <- as.factor(datos_4$n_grupo)

datos_5$grupo <- as.factor(datos_5$grupo)
datos_5$n_grupo <- as.factor(datos_5$n_grupo)

datos_6$grupo <- as.factor(datos_6$grupo)
datos_6$n_grupo <- as.factor(datos_6$n_grupo)

p <- ggplot(datos_4, aes(x = grupo, y = aciertos, fill = n_grupo)) +
    geom_col(width = 0.75, position = "dodge") +
    labs(
        title = "Aciertos por grupo y n (k = 4)",
        x = "",
        y = "Número de aciertos",
    ) +
    theme_minimal()

print(p)

ggsave(filename = "resultados/resultados_finales/repeticiones/13_cambio_n_4k.png", plot = p, width = 10, height = 6, dpi = 300)

p <- ggplot(datos_5, aes(x = grupo, y = aciertos, fill = n_grupo)) +
    geom_col(width = 0.75, position = "dodge") +
    labs(
        title = "Aciertos por grupo y n (k = 5)",
        x = "",
        y = "Número de aciertos",
    ) +
    theme_minimal()

print(p)

ggsave(filename = "resultados/resultados_finales/repeticiones/13_cambio_n_5k.png", plot = p, width = 10, height = 6, dpi = 300)

p <- ggplot(datos_6, aes(x = grupo, y = aciertos, fill = n_grupo)) +
    geom_col(width = 0.75, position = "dodge") +
    labs(
        title = "Aciertos por grupo y n (k = 6)",
        x = "",
        y = "Número de aciertos",
    ) +
    theme_minimal()

print(p)

ggsave(filename = "resultados/resultados_finales/repeticiones/13_cambio_n_6k.png", plot = p, width = 10, height = 6, dpi = 300)