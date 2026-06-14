# Título: 05_informe_simulacion.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' devuelve un informe que muestra los resultados exploratorios de las comparaciones entre estadísticos reales y simulados. El objetivo es comprobar si se han respetado los supuestos.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)

# Cargamos los datos reales.
real_1 <- read.csv("datos/reales/estadisticos/01a_estadisticos.csv")
real_2 <- read.csv("datos/reales/02a_brca_tidy.csv")
real_3 <- read.csv("datos/reales/estadisticos/03a_estadisticos.csv")
real_4 <- read.csv("datos/reales/04a_hepatitis_tidy.csv")

real_1$grupo <- as.character(real_1$grupo)
real_2$grupo <- as.character(real_2$grupo)
real_3$grupo <- as.character(real_3$grupo)
real_4$grupo <- as.character(real_4$grupo)

# Tomamos todos los archivos necesarios.
archivos_sim_1 <- list.files("datos/simulados/estadisticos", pattern = "^01.*\\.csv$", full.names = TRUE)
archivos_sim_2 <- list.files("datos/simulados", pattern = "^02.*\\.csv$", full.names = TRUE)
archivos_sim_3 <- list.files("datos/simulados/estadisticos", pattern = "^03.*\\.csv$", full.names = TRUE)
archivos_sim_4 <- list.files("datos/simulados", pattern = "^04.*\\.csv$", full.names = TRUE)

# Para los casos normales, podemos buscar la comparación mediante el cálculo de los errores estándar de la media. Hay que hacer esto con todos los simulados de la ruta especificada.
comparacion_1_normal_homo <- lapply(archivos_sim_1, function(archivo) { # Definimos un bucle.
    sim_1 <- read.csv(archivo) # Los simulados a usar.
    sim_1$grupo <- as.character(sim_1$grupo)
    comparar_estadisticos_reales_simulados(real_1, sim_1, nrow(real_1), nrow(sim_1), "grupo") # Comparamos.
})
names(comparacion_1_normal_homo) <- basename(archivos_sim_1)

# Igual para el caso 03.
comparacion_3_normal_hetero <- lapply(archivos_sim_3, function(archivo) {
    sim_3 <- read.csv(archivo)
    sim_3$grupo <- as.character(sim_3$grupo)
    comparar_estadisticos_reales_simulados(real_3, sim_3, nrow(real_3), nrow(sim_3), "grupo")
})

names(comparacion_3_normal_hetero) <- basename(archivos_sim_3)

# En los casos no-normales, realizaremos una prueba de Mann-Whitney para comprobar la igualdad de distribuciones. Lo haremmos para cada factor por separado.
niveles <- c("Q1785H", "R1753T", "E1794D", "V1804D")

comparacion_2_no_normal_homo <- lapply(archivos_sim_2, function(archivo) {
    sim_2 <- read.csv(archivo)
    sim_2$grupo <- as.character(sim_2$grupo)
    
    do.call(
        rbind,
        lapply(niveles, function(n) {
            test <- wilcox.test(
                real_2$valor[real_2[["grupo"]] == n],
                sim_2$valor[sim_2[["grupo"]] == n],
                exact = FALSE
            )
            data.frame(
                nivel = n,
                W = unname(test$statistic),
                p_value = test$p.value
            )
        })
    )
})
names(comparacion_2_no_normal_homo) <- basename(archivos_sim_2)

# Para el caso 04.
niveles <- c("GPR_0", "GPR_2", "GPR_3", "GPR_4")

comparacion_4_no_normal_hetero <- lapply(archivos_sim_4, function(archivo) {
    sim_4 <- read.csv(archivo)
    sim_4$grupo <- as.character(sim_4$grupo)
    
    do.call(
        rbind,
        lapply(niveles, function(n) {
            test <- wilcox.test(
                real_4$valor[real_4[["grupo"]] == n],
                sim_4$valor[sim_4[["grupo"]] == n],
                exact = FALSE
            )
            data.frame(
                nivel = n,
                W = unname(test$statistic),
                p_value = test$p.value
            )
        })
    )
})
names(comparacion_4_no_normal_hetero) <- basename(archivos_sim_4)

# Mostramos el informe de estadísticos. Quitamos las columnas que no interesan, resultado de la fusión de las tablas.
informe_1_normal_homo <- bind_rows(lapply(comparacion_1_normal_homo, function(x) x[-5]), .id = "archivo")
informe_2_no_normal_homo <- bind_rows(comparacion_2_no_normal_homo, .id = "archivo")
informe_3_normal_hetero <- bind_rows(lapply(comparacion_3_normal_hetero, function(x) x[-5]), .id = "archivo")
informe_4_no_normal_hetero <- bind_rows(comparacion_4_no_normal_hetero, .id = "archivo")

# Ajustamos los p-valores, puesto que se han realizado muchas comparaciones.
informe_1_normal_homo$p_media_ajustado <- p.adjust(informe_1_normal_homo$p_valor_media, method = "holm")
informe_1_normal_homo$p_var_ajustado <- p.adjust(informe_1_normal_homo$p_valor_var, method = "holm")
informe_2_no_normal_homo$p_ajustado <- p.adjust(informe_2_no_normal_homo$p_value, method = "holm")
informe_3_normal_hetero$p_media_ajustado <- p.adjust(informe_3_normal_hetero$p_valor_media, method = "holm")
informe_3_normal_hetero$p_var_ajustado <- p.adjust(informe_3_normal_hetero$p_valor_var, method = "holm")
informe_4_no_normal_hetero$p_ajustado <- p.adjust(informe_4_no_normal_hetero$p_value, method = "holm")

# Se guardan los informes.
write.csv(informe_1_normal_homo, file = "resultados/resultados_exploratorios/informes_comparativos/05_informe_caso1.csv")
write.csv(informe_2_no_normal_homo, file = "resultados/resultados_exploratorios/informes_comparativos/05_informe_caso2.csv")
write.csv(informe_3_normal_hetero, file = "resultados/resultados_exploratorios/informes_comparativos/05_informe_caso3.csv")
write.csv(informe_4_no_normal_hetero, file = "resultados/resultados_exploratorios/informes_comparativos/05_informe_caso4.csv")

# Podemos ver que se conservan los estadísticos y distribuciones.