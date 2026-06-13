# Título: 04c_simular_datos_6g_n20.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del GPR. Se generan dos grupos extra siguiendo los mismos criterios que en el 'script' 01c.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)
library(ks)

n <- 20

# Cargamos los datos y convertimos en factores.
datos_reales <- read_csv("datos/reales/04a_hepatitis_tidy.csv")
estadisticos_real <- read_csv("datos/reales/estadisticos/04a_estadisticos.csv")

estadisticos_real <- estadisticos_real %>%
    mutate(
        grupo = factor(grupo)
    )

datos_reales <- datos_reales %>%
    mutate(
        grupo = factor(grupo)
    )

# En este caso, al usar la función 'ks', tenemos que partir de una distribución original. Por ello, primero generamos los datos simualdos.
simulados <- datos_reales %>%
    group_by(grupo) %>%
    group_modify(~ {
        x <- .x$valor
        dens <- kde(x)
        data.frame(
            valor = rkde(dens, n = n) # Esta función calcula la distribución.
        )
    }) %>%
    ungroup()

# Simulamos el grupo G5 con las distribuciones de GPR_2 y GPR_3. Simulamos G6 con las dos restantes.
x_gpr2 <- simulados %>%
    filter(grupo == "GPR_2") %>%
    pull(valor)

x_gpr3 <- simulados %>%
    filter(grupo == "GPR_3") %>%
    pull(valor)

dens_gpr2 <- kde(x_gpr2) # Sacamos las densidades.
dens_gpr3 <- kde(x_gpr3)

sim_G5 <- tibble(
    valor = (rkde(dens_gpr2, n = n) + rkde(dens_gpr3, n = n)) / 2,
    grupo = "G5"
)

# Generamos G6 como la suma de las distribuciones de GPR_0 y GPR_4.
x_gpr0 <- simulados %>%
    filter(grupo == "GPR_0") %>%
    pull(valor)

x_gpr4 <- simulados %>%
    filter(grupo == "GPR_4") %>%
    pull(valor)

dens_gpr0 <- kde(x_gpr0)
dens_gpr4 <- kde(x_gpr4)

sim_G6 <- tibble(
    valor = rkde(dens_gpr0, n = n) + rkde(dens_gpr4, n = n),
    grupo = "G6"
)

# Añadimos G5 y G6 a los simulados.
simulados <- bind_rows(simulados, sim_G5, sim_G6)

# Reajustamos los niveles del factor.
simulados$grupo <- factor(simulados$grupo)

# Hacemos las comprobaciones: se mantiene la falta de parametricidad y los estadísticos.
comprobaciones_04(simulados, datos_reales, "datos/simulados/04c_hepatitis_6g_n20.csv", "datos/simulados/estadisticos/04c_hepatitis_6g_n20.csv")