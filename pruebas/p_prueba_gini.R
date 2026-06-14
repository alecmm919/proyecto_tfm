# Título: p_prueba_gini.R
#
# Autor: Alejandro M.
#
# Descripción: Esta prueba busca obtener el índice de Gini de un árbol.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(rpart)

datos <- read.csv("datos/simulados/01d_corazon_e3.csv")

model <- rpart(valor ~ grupo, data = datos, method = "class")

frame <- model$frame

# Hojas
leaves <- frame[frame$var == "<leaf>", ]

# Columnas de probabilidades
prob_cols <- grep("^prob", colnames(leaves$yval2))

# Mantener estructura matricial
probs <- leaves$yval2[, prob_cols, drop = FALSE]

# Gini por hoja
gini_leaf <- 1 - rowSums(probs^2)

# Gini ponderado total
gini_total <- weighted.mean(gini_leaf, leaves$n)

gini_total