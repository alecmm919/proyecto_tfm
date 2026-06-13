# Título: 07_arboles_e1.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se pretende generar los árboles de decisión y sus matrices de confusión para los datos generados.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

# Cargamos los datos simulados.
lista_1 <- list.files(path = "datos/simulados", pattern = "^01", full.names = TRUE)
lista_2 <- list.files(path = "datos/simulados", pattern = "^02", full.names = TRUE)
lista_3 <- list.files(path = "datos/simulados", pattern = "^03", full.names = TRUE)
lista_4 <- list.files(path = "datos/simulados", pattern = "^04", full.names = TRUE)

# Generamos los árboles de decisión.
for (i in 1:length(lista_1)){
    # Cargamos.
    caso_1 <- read.csv(lista_1[i])
    caso_1$grupo <- as.factor(caso_1$grupo)
    caso_1$valor <- as.numeric(caso_1$valor)
    
    caso_2 <- read.csv(lista_2[i])
    caso_2$grupo <- as.factor(caso_2$grupo)
    caso_2$valor <- as.numeric(caso_2$valor)
    
    caso_3 <- read.csv(lista_3[i])
    caso_3$grupo <- as.factor(caso_3$grupo)
    caso_3$valor <- as.numeric(caso_3$valor)
    
    caso_4 <- read.csv(lista_4[i])
    caso_4$grupo <- as.factor(caso_4$grupo)
    caso_4$valor <- as.numeric(caso_4$valor)
    
    # Sacamos el título y simulamos.
    tit <- sacar_titulo(lista_1[i])
    
    generar_arbol(caso_1, paste0("resultados/resultados_exploratorios/arboles/07_arbol_01_", tit, ".png"), paste("Contexto paramétrico (", tit, ")"))
    generar_arbol(caso_2, paste0("resultados/resultados_exploratorios/arboles/07_arbol_02_", tit, ".png"), paste("Contexto no normal, homocedástico (", tit, ")"))
    generar_arbol(caso_3, paste0("resultados/resultados_exploratorios/arboles/07_arbol_03_", tit, ".png"), paste("Contexto normal, heterocedástico (", tit, ")"))
    generar_arbol(caso_4, paste0("resultados/resultados_exploratorios/arboles/07_arbol_04_", tit, ".png"), paste("Contexto no normal, heterocedástico (", tit, ")"))
    
    # Por otro lado, generamos las matrices de confusión.
    matriz_confusion(caso_1, paste0("resultados/resultados_exploratorios/arboles/07_matriz_01_", tit, ".png"))
    matriz_confusion(caso_2, paste0("resultados/resultados_exploratorios/arboles/07_matriz_02_", tit, ".png"))
    matriz_confusion(caso_3, paste0("resultados/resultados_exploratorios/arboles/07_matriz_03_", tit, ".png"))
    matriz_confusion(caso_4, paste0("resultados/resultados_exploratorios/arboles/07_matriz_04_", tit, ".png"))
}