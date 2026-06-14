# Título: 08_comparacion_separaciones.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se pretende comparar los resultados para las distintos grupos de datos simulados del proyecto. Es decir, compara, para cada caso y cada grupo, qué se ha separado por estadística clásica y qué no se ha separado. Lo mismo se realiza con los árboles de decisión. Los datos se dejan preparados para el 'script' 09.

# Librerías y carga:
source("scripts/00_funciones.R")

library(rstatix)
library(dplyr)
library(rpart)

# Cargamos los archivos con un bucle, cogiendo solo los .csv. Calculamos los estadísticos d de Cohen. Sacamos las columnas que interesan.
archivos <- list.files(path = "datos/simulados", pattern = "\\.csv$", full.names = TRUE)

for (i in archivos) { # Bucle para cada contexto. Generaremos una tabla por grupo de datos.
    
    n <- sacar_titulo(i)
    
    datos <- read.csv(i)
    datos$grupo <- as.factor(datos$grupo)
    
    if (substr(i, 18, 18) == "1") {
        # Separación clásica.
        clasica <- as.data.frame(TukeyHSD(aov(valor ~ grupo, data = datos))$grupo)
        # Identificamos los grupos separados.
        clasica$separado <- ifelse(clasica$`p adj` < 0.05, TRUE, FALSE)
        clasica$grupo <- rownames(clasica)
        clasica <- clasica[, c("grupo", "separado")]
        
        # Aplicamos la comparación y guardamos.
        comparar_clasica_arbol_tamano(datos, clasica, paste0("resultados/resultados_exploratorios/tablas_comparativas/08_comparacion_cla_arb_0", substr(i, 18, 18), "_", n, ".csv"), homocedastico = TRUE)
    }
    
    else if (substr(i, 18, 18) == "2" | substr(i, 18, 18) == "4") { # Mismo proceso, pero adaptado a la prueba de Dunn, para los casos 2 y 4.
        # Separación clásica.
        clasica <- as.data.frame(dunn_test(datos, valor ~ grupo))[c(2, 3, 8)]
        
        # Pegamos los nombres de los grupos.
        clasica$grupo <- paste0(clasica$group1, "-", clasica$group2)
        rownames(clasica) <- clasica$grupo # Para que aparezcan en el .csv.
        clasica$separado <- ifelse(clasica$p.adj < 0.05, TRUE, FALSE)
        clasica <- clasica[,c("grupo", "separado")]
        
        # Aplicamos la comparación y guardamos.
        comparar_clasica_arbol_tamano(datos, clasica, paste0("resultados/resultados_exploratorios/tablas_comparativas/08_comparacion_cla_arb_0", substr(i, 18, 18), "_", n, ".csv"), homocedastico = FALSE)
    }
    
    else { # Para el caso 3.
        
        clasica <- as.data.frame(games_howell_test(datos, valor ~ grupo))[c(2, 3, 7)]
        
        # Pegamos los nombres de los grupos.
        clasica$grupo <- paste0(clasica$group1, "-", clasica$group2)
        rownames(clasica) <- clasica$grupo
        clasica$separado <- ifelse(clasica$p.adj < 0.05, TRUE, FALSE)
        clasica <- clasica[,c("grupo", "separado")]
        
        # Aplicamos la comparación y guardamos.
        comparar_clasica_arbol_tamano(datos, clasica, paste0("resultados/resultados_exploratorios/tablas_comparativas/08_comparacion_cla_arb_0", substr(i, 18, 18), "_", n, ".csv"), homocedastico = TRUE)
    }
}