# Título: o_arboles_poisson.R
#
# Autor: Alejandro M.
#
# Descripción: En esta prueba, haremos árboles de Poisson para ver si pueden separar los aciertos en función de H.

# Librerías y carga:
source("scripts/00_funciones.R")

library(rpart)
library(rpart.plot)

# Cargamos.

datos <- read_csv("salidas/resultados/10a_entropia_shannon.csv")

for (i in 1:4){
    datos_caso <- datos[datos$caso == i,]
    
    # Árbol.
    rpart.plot(
        rpart(n_aciertos ~ H, data = datos_caso, method = "poisson")
    )
}