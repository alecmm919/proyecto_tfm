# Título: 02b_simular_datos.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico de variables homocedásticas, pero no normales. Para ello, se toma la desviación de cada dato y se genera un nuevo dato tomando estas desviaciones pseudoaleatoriamente. Se generan 4 grupos de n = 20, n = 30 y n = 40.

#Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)
library(nortest)

datos_reales <- read_csv("datos/reales/02a_brca_tidy.csv")
estadisticos_real <- read_csv("datos/reales/estadisticos/02a_estadisticos.csv")

# Buscamos generar datos no normales, pero sí homocedásticas, que mantengan la mediana y el RI de los originales.

#Sacamos el RI global. Como mencionamos en el 'script' 02a, si los datos siguen distribuciones 'más o menos parecidas', las desviaciones estándar son proporcionales al RI.

ri_global <- IQR(datos_reales$valor)
desviacion <- (datos_reales$valor - median(datos_reales$valor)) / ri_global  # Las desviaciones la normalizamos con respecto a la mediana y al RI.

# Procedemos a simular.
for (n in c(20, 30, 40)){
    simulados <- datos_reales %>% # Separamos por tipo.
        distinct(grupo) %>%
        group_by(grupo) %>%
        reframe(
            {
                med_tipo <- median(datos_reales$valor[datos_reales$grupo == first(grupo)]) # Sacamos la mediana del tipo.
                desv <- sample(desviacion, size = n, replace = TRUE) # Tomamos unas desviaciones pseudoaleatorias.
                desv <- desv - median(desv) # La centramos.
                desv <- desv / IQR(desv) # La normalizamos a RI = 1.
                valor <- med_tipo + desv * ri_global # Generamos el valor.
                tibble(valor = valor)
            }
        )
    
    # Comprobamos que los supuestos paramétricos se respetan con respecto a los datos originales.
    simulados$grupo <- factor(simulados$grupo)
    comprobaciones_02(simulados, datos_reales, paste0("datos/simulados/02b_brca_", n, ".csv"), paste0("datos/simulados/estadisticos/02b_brca_", n, ".csv"))
}