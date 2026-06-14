# Título: t_prueba_aciertos.R
#
# Autor: Alejandro M.
#
# Descripción: En este archivo se hace una prueba para definir la función 'calcular_aciertos_profundidad'.

# Librerías y carga:
source("scripts/00_funciones.R")

library(rstatix)
library(dplyr)
library(ggplot2)
library(car)
library(stringr)

# Cargamos los datos de la etapa 4.
datos5 <- list.files(path = "salidas/resultados/etapa4", pattern = "\\G5.csv$", full.names = TRUE)

datos6 <- list.files(path = "salidas/resultados/etapa4", pattern = "\\G6.csv$", full.names = TRUE)

calcular_aciertos_profundidad <- function(lista = datos5, k, salida){ # Función similar a la anterior pero con profundidades. Nótese que es independiente del número de grupos excepto por la profundidad.
    aciertos <- c()
    for (ruta in lista){
        tabla <- read.csv(ruta)
        # Sacamos los vectores lógicos de separación.
        clas <- unlist(c(tabla["separado_clasica"]))
        arbol <- unlist(c(tabla["separado_arbol"]))
        # Los comparamos y contamos
        aciertos <- append(aciertos, sum(clas == arbol))
    }
    ncomb <- length(aciertos)
    
    vec_caso <- c(rep(c("1", "2", "3", "4"), each = 4))
    vec_prof <- c(rep((k-3):k, times = 4))
    
    resultado <- data.frame(
        caso = vec_caso,
        profundidad = vec_prof,
        aciertos = aciertos
    )
    write.csv(resultado, file = salida)
}