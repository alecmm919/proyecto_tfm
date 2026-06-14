# Título: k_prueba_metodo_2_en_4.R [DESCARTADO]
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es usar el 'método de simulación' del script 02b en el caso 4.
source("scripts/00_funciones.R")
library(tidyverse)
library(car)
library(ks)
library(cramer)

datos_reales <- read_csv("datos/reales/04a_hepatitis_tidy.csv")
estadisticos_real <- read_csv("datos/reales/estadisticos/04a_estadisticos.csv")

estadisticos_real <- estadisticos_real %>%
    mutate(
        fase = factor(fase)
    )

datos_reales <- datos_reales %>%
    mutate(
        fase = factor(fase)
    )

# 1) Medianas, tamaños y escala por grupo
info_grupos <- datos_reales %>%
    group_by(fase) %>%
    summarise(
        mediana = median(valor),
        n = n(),
        escala = IQR(valor),
        .groups = "drop"
    )

# 2) Residuos centrados por grupo
residuos_df <- datos_reales %>%
    left_join(info_grupos, by = "fase") %>%
    mutate(
        residuo = valor - mediana
    )

# 3) Estandarizamos dentro de cada grupo
residuos_std <- residuos_df %>%
    mutate(
        residuo_std = residuo / escala
    )

# 4) Simulación por grupo manteniendo la heterocedasticidad
simulados <- info_grupos %>%
    rowwise() %>%
    do({
        residuos_grupo <- residuos_std %>%
            filter(fase == .$fase) %>%
            pull(residuo_std)
        
        tibble(
            fase = .$fase,
            valor = .$mediana + sample(residuos_grupo, size = .$n, replace = TRUE) * .$escala
        )
    }) %>%
    ungroup()

simulados$fase <- factor(simulados$fase, levels = levels(datos_reales$fase))

# Hacemos las comprobaciones: se mantiene la falta de parametricidad y los estadísticos.
comprobar_normalidad_kolmogorov(simulados, "fase", "valor")
leveneTest(valor ~ fase, data = simulados)

estadisticos_real <- generar_estadisticos_np(datos_reales, "fase")
estadisticos_sim <- generar_estadisticos_np(simulados, "fase")

# Mostramos los estadísticos. Al no ser normales, no realizamos la prueba t.
estadisticos_real
estadisticos_sim