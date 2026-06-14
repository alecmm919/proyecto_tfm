# Título: j_prueba_fosfolipasa.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' pretende sustituir los datos del grupo 4 por unos de fosfolipasa

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)

# Se toman los datos desde los 5 csv publicados en el 'paper'. Y tomamos únicamente las variables que nos interesan.
datos <- read.csv("datos/reales/prueba.csv", sep = ";")



# Pasamos a formado 'tidy' mediante un 'pivot_longer'.
datos <- datos %>%
    pivot_longer(
        cols = starts_with("GPR"),
        names_to = "fase",
        values_to = "valor"
    )

# Ahora, convertimos los datos a los tipos que nos interesan.
datos$name <- factor(datos$name)
datos$fase <- factor(datos$fase, ordered = TRUE) # En este caso, los factores deben ir ordenados.
datos$valor <- as.numeric(gsub(",",".", datos$valor))

write_csv(datos, "datos/reales/04a_hepatitis_tidy.csv")

# Comprobamos la parametricidad. Vemos que los datos no son normales ni homocedásticos. En este caso, tenemos 4 'ties' con valor 0, por lo que Kolmogorov no puede usarse. Tampoco podemos usar Shapiro porque hay 70 individuos por grupo. Por ello, se usará Cramer-Von-Mises para este contraste.
comprobar_normalidad_cramer(datos, "fase", "valor")
leveneTest(valor ~ fase, data = datos) # Usamos Levene porque los datos no son normales.

estadisticos_04 <- generar_estadisticos_np(datos, "fase")
write_csv(estadisticos_04, "datos/reales/estadisticos/04a_estadisticos.csv")

# Título: 04b_simular_datos.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del GPR como predictor de la hepatitis B, siguiendo la Bibliografía y los casos que buscamos. En este caso, se trata de un contexto no normal y heterocedástico.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)
library(ks)
library(cramer)
n = 71 # Número de datos.

# Cargamos los datos y convertimos en factores.
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

# Creamos un 'tibble' para meter los datos. Usaremos una función ks, se trata de una función de densidad calculada en base a los datos originales y replicada en la simulación.
simulados <- datos_reales %>%
    group_by(fase) %>%
    group_modify(~ {
        x <- .x$valor
        n_grupo <- length(x)
        dens <- kde(x)
        
        sim_boot <- sample(x, size = n_grupo, replace = TRUE)
        sim_kde <- as.numeric(rkde(dens, n = n_grupo))
        sim <- 0.8 * sim_boot + 0.2 * sim_kde
        
        med_real <- median(x, na.rm = TRUE)
        iqr_real <- IQR(x, na.rm = TRUE)
        med_sim <- median(sim, na.rm = TRUE)
        iqr_sim <- IQR(sim, na.rm = TRUE)
        
        if (is.finite(iqr_real) && iqr_real > 0 && is.finite(iqr_sim) && iqr_sim > 0) {
            sim <- (sim - med_sim) / iqr_sim * iqr_real + med_real
        } else {
            sim <- sim - med_sim + med_real
        }
        
        sim <- pmax(pmin(sim, max(x, na.rm = TRUE)), min(x, na.rm = TRUE))
        
        data.frame(
            valor = sim # Esta función calcula la distribución.
        )
    }) %>%
    ungroup()

simulados$fase <- factor(simulados$fase, levels = levels(datos_reales$fase))

# Hacemos las comprobaciones: se mantiene la falta de parametricidad y los estadísticos.
comprobar_normalidad_cramer(simulados, "fase", "valor")
leveneTest(valor ~ fase, data=simulados)

estadisticos_real <- generar_estadisticos_np(datos_reales, "fase")
estadisticos_sim <- generar_estadisticos_np(simulados, "fase")

# Mostramos los estadísticos. Al no ser normales, no realizamos la prueba t.
estadisticos_real
estadisticos_sim