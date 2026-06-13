# Título: 06_exploracion_general.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se pretende hacer un análisis exploratorio de los datos simulados generados en los 'scripts' 1-4, de tal forma que se puedan guardar los resultados de los distintos estudios.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(FSA)
library(rstatix)

# Importamos todos los datos simulados.
lista_1 <- list.files(path = "datos/simulados", pattern = "^01", full.names = TRUE)
lista_2 <- list.files(path = "datos/simulados", pattern = "^02", full.names = TRUE)
lista_3 <- list.files(path = "datos/simulados", pattern = "^03", full.names = TRUE)
lista_4 <- list.files(path = "datos/simulados", pattern = "^04", full.names = TRUE)

# Planteamos un bucle para todo.
for (i in 1:length(lista_1)){
    # Situación 1.
    # ANOVA.
    caso_1 <- read.csv(lista_1[i])
    summary(m1 <- aov(valor ~ grupo, data = caso_1))
    TukeyHSD(m1)
    
    # Situación 2: Kruskal - Wallis + Dunn.
    caso_2 <- read.csv(lista_2[i])
    caso_2$grupo <- factor(caso_2$grupo)
    
    kruskal_test(caso_2, valor ~ grupo)
    dunnTest(valor ~ grupo, data = caso_2)
    
    # Situación 3: ANOVA de Welch + Games-Howell.
    caso_3 <- read.csv(lista_3[i])
    oneway.test(valor ~ grupo, data = caso_3, var.equal = FALSE)
    games_howell_test(caso_3, valor ~ grupo)
    
    # Situación 4: Kruskal-Wallis + Dunn.
    caso_4 <- read.csv(lista_4[i])
    caso_4$grupo <- factor(caso_4$grupo)
    
    kruskal_test(caso_4, valor ~ grupo)
    dunnTest(valor ~ grupo, data = caso_4)
    
    # Boxplots.
    # Sacamos el título. Es fundamental distinguir los casos de 6 grupos con 20 y 40 datos respectivamente.
    tit <- sacar_titulo(lista_1[i])
    
    plot_contrastes(caso_1, paste("Contexto paramétrico (", tit, ")"), "tukey", paste0("resultados/resultados_exploratorios/comparaciones_clasica_ad/06_boxplot_01_", tit, ".png"))
    
    plot_contrastes(caso_2, paste("Contexto paramétrico (", tit, ")"), "dunn", paste0("resultados/resultados_exploratorios/comparaciones_clasica_ad/06_boxplot_02_", tit, ".png"))
    
    plot_contrastes(caso_3, paste("Contexto paramétrico (", tit, ")"), "games-howell", paste0("resultados/resultados_exploratorios/comparaciones_clasica_ad/06_boxplot_03_", tit, ".png"))
    
    plot_contrastes(caso_4, paste("Contexto paramétrico (", tit, ")"), "dunn", paste0("resultados/resultados_exploratorios/comparaciones_clasica_ad/06_boxplot_04_", tit, ".png"))
}