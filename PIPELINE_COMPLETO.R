# Título: PIPELINE_COMPLETO.R
#
# Autor: Alejandro M.
#
# Descripción: Este archivo contiene todo el proyecto.

# Librerías, carga y configuraciones generales ----
set.seed(117) # Semilla para la reproducibilidad.

library(dplyr)
library(nortest)
library(readr)
library(utils)
library(car)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(rpart)
library(grDevices)
library(rpart.plot)
library(caret)
library(stringr)

# Parte 0: Definición de funciones ----
simular_normal_exacto <- function(x, n, mu = NULL, s = NULL){ # Esta función genera valores normales a partir de una muestra normal.
    
    if (is.null(mu) || is.null(s)) { # Si se dan estos argumentos, se usan los parámetros pasados por el usuario. Si no, se calculan automáticamente.
        mu <- mean(x)
        s <- sd(x)
    }
    
    sim <- rnorm(n, mean = mu, sd = s)
    
    return(sim)
}

comprobar_normalidad_shapiro <- function(dat, grupos, resultados){ # Hace una prueba de normalidad de Shapiro-Wilk. Devuelve solo el p-valor.
    
    dat %>%
        group_by(across(all_of(grupos))) %>%
        summarise(
            n = n(),
            p_value = shapiro.test(.data[[resultados]])$p.value,
            .groups = "drop",
        )
}

comprobar_normalidad_kolmogorov <- function(dat, grupos, resultados){ # Hace una prueba de normalidad de Kolmogorov-Smirnof. Devuelve solo el p-valor.
    
    dat %>%
        group_by(across(all_of(grupos))) %>%
        summarise(
            n = n(),
            p_value = ks.test(.data[[resultados]], "pnorm", mean(.data[[resultados]]), sd(.data[[resultados]]))$p.value,
            .groups = "drop"
        )
}

comprobar_normalidad_cramer <- function(dat, grupos, resultados){ # Hace una prueba de normalidad de Cramer-Von-Misses. Devuelve solo el p-valor.
    
    suppressWarnings( # Eliminamos las advertencias porque esta función devuelve un 'warning' si el p-valor es demasiado pequeño.
        dat %>%
            group_by(across(all_of(grupos))) %>%
            summarise(
                n = n(),
                p_value = cvm.test(.data[[resultados]])$p.value,
                .groups = "drop"
            )
    )
}

generar_estadisticos <- function(dat, grupos){ # Devuelve los estadísticos paramétricos (media y desviación estándar) de un 'tibble' en función de los factores.
    
    salida <- dat %>%
        group_by(across(all_of(grupos))) %>%
        summarise(
            media = mean(valor),
            dest = sd(valor)
        )
}

generar_estadisticos_np <- function(dat, grupos){ # Devuelve los estadísticos no paramétricos (mediana y rango intercuartílico) de un 'tibble' en función de los factores.
    
    salida <- dat %>%
        group_by(across(all_of(grupos))) %>%
        summarise(
            mediana = median(valor),
            RI = IQR(valor)
        )
}

comparar_estadisticos_reales_simulados <- function(tabla1, tabla2, n_1, n_2, factores){ # Compara los estadísticos de dos tablas de datos (reales y simulados) y hace una prueba t de Student para la igualdad de medias y una F para el cociente de varianzas.
    
    inner_join(
        tabla1, tabla2,
        by = factores,
        suffix = c("_real", "_sim") # Unimos las tablas etiquetando las columnas.
    ) %>%
        mutate(
            # Comparación de medias con prueba t.
            dif_medias = media_real - media_sim,
            error_estandar = sqrt((dest_real^2)/n_1 + (dest_sim^2)/n_2),
            z = dif_medias / error_estandar,
            p_valor_media = 2 * pnorm(-abs(z)),
            ic_inf = dif_medias - 1.96 * error_estandar,
            ic_sup = dif_medias + 1.96 * error_estandar,
            
            # Comparación de varianzas con prueba F.
            var_real = dest_real^2,
            var_sim = dest_sim^2,
            F_var = var_real / var_sim,
            df1 = n_1 - 1,
            df2 = n_2 - 1,
            p_valor_var = 2 * pmin(pf(F_var, df1, df2), pf(1 / F_var, df2, df1))
        ) %>%
        select(
            -z, -error_estandar, -df1, -df2, # Quitamos lo que no es necesario.
        )
}

comprobaciones_01 <- function(simulados, salida_datos, salida_estadisticos){ # Comprueba que los datos simulados del caso 01 respetan los supuestos estadísticos de los reales.
    
    n_min <- min(table(simulados$grupo))
    
    # Revisamos que se siguen cumpliendo los supuestos de los que partimos.
    print(comprobar_normalidad_shapiro(simulados, "grupo", "valor"))
    print(bartlett.test(valor ~ grupo, data=simulados))
    
    # Comprobamos que los estadísticos se han respetado entre los datos normales y los simulados.
    estadisticos_sim <- generar_estadisticos(simulados, "grupo")
    
    print(estadisticos_sim)
    
    write_csv(simulados, salida_datos)
    write.csv(estadisticos_sim, salida_estadisticos)
}

comprobaciones_02 <- function(simulados, datos_reales, salida_datos, salida_estadisticos){ # Comprueba que los datos simulados del caso 02 respetan los supuestos estadísticos de los reales. La filosofía es la misma que la de la función anterior.
    
    n_min <- min(table(simulados$grupo))
    
    print(comprobar_normalidad_shapiro(simulados, "grupo", "valor"))
    print(fligner.test(valor ~ grupo, data=simulados)) # Usamos Fligner porque estos datos presentan demasiados valores atípicos.
    
    estadisticos_real <- generar_estadisticos_np(datos_reales, "grupo")
    estadisticos_sim  <- generar_estadisticos_np(simulados, "grupo")
    
    print(estadisticos_real)
    print(estadisticos_sim)
    
    write_csv(simulados, salida_datos)
    write.csv(estadisticos_sim, salida_estadisticos)
}

comprobaciones_03 <- function(simulados, datos_reales, salida_datos, salida_estadisticos){ # Comprueba que los datos simulados del caso 03 respetan los supuestos estadísticos de los reales.
    
    n_min <- min(table(simulados$grupo))
    
    print(comprobar_normalidad_shapiro(simulados, "grupo", "valor"))
    print(leveneTest(valor ~ grupo, data=simulados)) # Los datos no son normales.
    
    estadisticos_real <- generar_estadisticos(datos_reales, "grupo")
    estadisticos_sim <- generar_estadisticos(simulados, "grupo")
    print(estadisticos_real)
    print(estadisticos_sim)
    
    estadisticos_sim <- generar_estadisticos(simulados, "grupo")
    
    write_csv(simulados, salida_datos)
    write.csv(estadisticos_sim, salida_estadisticos)
}

comprobaciones_04 <- function(simulados, datos_reales, salida_datos, salida_estadisticos){ # Comprueba que los datos simulados del caso 04 respetan los supuestos estadísticos de los reales.
    
    n_min <- min(table(simulados$grupo))
    
    print(comprobar_normalidad_shapiro(simulados, "grupo", "valor"))
    print(fligner.test(valor ~ grupo, data=simulados))
    
    estadisticos_real <- generar_estadisticos_np(datos_reales, "grupo")
    estadisticos_sim <- generar_estadisticos_np(simulados, "grupo")
    
    print(estadisticos_real)
    print(estadisticos_sim)
    
    write_csv(simulados, salida_datos)
    write.csv(estadisticos_sim, salida_estadisticos)
}

plot_contrastes <- function(data, titulo, metodo_posthoc = c("tukey", "dunn", "games-howell"), archivo_salida) { # Genera 'boxplots' e indica las diferencias entre grupos en función de la prueba 'post hoc' planteada.
    
    metodo_posthoc <- match.arg(metodo_posthoc)
    
    yvar <- "valor"
    
    data$.grupo <- as.factor(data$grupo)
    
    g <- ggplot(data, aes(x = .grupo, y = .data[[yvar]], fill = .grupo)) +
        geom_boxplot(alpha = 0.8, width = 0.65, outlier.shape = 21, outlier.fill = "white", outlier.size = 2) +
        scale_fill_discrete() +
        theme_bw() +
        theme(
            plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            axis.title = element_text(face = "bold", size = 12),
            axis.text = element_text(size = 10, colour = "black"),
            legend.position = "none",
            panel.grid.major = element_line(colour = "grey"),
            panel.grid.minor = element_blank()
        ) +
        labs(title = titulo, x = "grupo", y = yvar)
    
    formula_posthoc <- as.formula("valor ~ .grupo")
    
    post <- switch( # Elegimos el método 'post hoc'.
        metodo_posthoc,
        
        "tukey" = tukey_hsd(data, formula_posthoc),
        
        "dunn" = dunn_test(data, formula_posthoc, p.adjust.method = "holm"),
        
        "games-howell" = games_howell_test(data, formula_posthoc)
    )
    
    post_sig <- post[post$p.adj < 0.05, ]
    
    if (nrow(post_sig) > 0) {
        
        niveles <- levels(data$.grupo)
        ymax <- max(data[[yvar]], na.rm = TRUE)
        
        post_sig$xmin <- match(post_sig$group1, niveles)
        post_sig$xmax <- match(post_sig$group2, niveles)
        
        post_sig$y.position <- seq(from = ymax * 1.05, by = ymax * 0.08, length.out = nrow(post_sig))
        
        g <- g +
            stat_pvalue_manual(post_sig, xmin = "xmin", xmax = "xmax", y.position = "y.position", label = "p.adj.signif", hide.ns = TRUE)
    }
    
    ggsave(archivo_salida, g, width = 8, height = 6, dpi = 300)
    
    return(g)
}

generar_arbol <- function(dat, salida, titulo, profundidad = NULL) { # Genera un árbol de decisión. Permite seleccionar una profundidad determinada.
    
    # Para ignorar la profundidad si no se determina en la llamada.
    if (is.null(profundidad)) {
        ctrl <- rpart.control()
    } else {
        ctrl <- rpart.control(maxdepth = profundidad)
    }
    
    modelo <- rpart(valor ~ ., data = dat, method = "anova", control = ctrl)
    
    png(salida, width = 1600, height = 1600, res = 300)
    
    rpart.plot(modelo, type = 3, fallen.leaves = TRUE, nn = FALSE, main = titulo, box.palette = "RdYlGn", branch.lty = 1, branch.lwd = 2, tweak = 1.2, faclen = 0, roundint = FALSE)
    
    dev.off()
    
    # Generamos otra vez lo mismo para que aparezca en el visor de RStudio.
    rpart.plot(modelo, type = 3, fallen.leaves = TRUE, nn = FALSE, main = titulo, box.palette = "RdYlGn", branch.lty = 1, branch.lwd = 2, tweak = 1.2, faclen = 0, roundint = FALSE)
    
}

matriz_confusion <- function(dat, salida){ # Genera la matriz de confusión.
    modelo <- rpart(grupo ~ ., data = dat, method = "class")
    pred <- predict(modelo, dat, type = "class")
    conf <- confusionMatrix(data = pred, reference = dat$grupo)
    tabla <- as.data.frame(conf$table)
    
    p <- ggplot(tabla, aes(x = Reference, y = Prediction, fill = Freq)) +
        geom_tile(color = "white", linewidth = 0.8) +
        geom_text(aes(label = Freq), colour = "white", fontface = "bold", size = 5) +
        scale_fill_gradient(low = "red", high = "green") +
        labs(title = "Matriz de confusión", x = "Clase real", y = "Clase predicha") +
        theme_minimal() +
        theme(
            plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(colour = "black", size = 11),
            panel.grid = element_blank()
        )
    
    ggsave(filename = salida, plot = p, width = 6, height = 5, dpi = 300)
    
    p
    conf
}

comparar_clasica_arbol_tamano <- function(datos, clasica, salida, homocedastico = TRUE, profundidad = NULL){ # Genera el árbol de decisión y lo compara con una salida clásica. De esta forma, se obtienen los aciertos.
    
    # Para ignorar la profundidad si no se determina en la llamada.
    if (is.null(profundidad)) {
        ctrl <- rpart.control()
    } else {
        ctrl <- rpart.control(maxdepth = profundidad)
    }
    
    # Separación por árbol.
    # Calculamos n por grupo y k.
    k <- length(levels(datos$grupo))
    
    # Modelizamos los árboles.
    arbol <- as.data.frame(rpart(valor ~ grupo, data = datos, method = "anova", control = ctrl)$where) # Indica en qué rama termina cada observación.
    names(arbol) <- "grupo"
    
    # Hojas alcanzadas por cada grupo.
    hojas_por_grupo <- split(arbol$grupo, datos$grupo)
    
    # Comprobamos qué grupos están separados.
    separado <- c()
    
    for (j in 1:(k - 1)) {
        for (l in (j + 1):k) {
            
            # TRUE si no comparten ninguna hoja terminal.
            separado <- c(separado, length(intersect(hojas_por_grupo[[j]], hojas_por_grupo[[l]])) == 0)
        }
    }
    
    # Añadimos esto a un data.frame.
    sep_arbol <- data.frame(
        row.names = rownames(clasica), # Compartimos nombres.
        separado = separado
    )
    
    # Finalmente, calculamos el valor d de Cohen.
    # Limpiamos.
    datos <- datos[!is.na(datos$grupo) & !is.na(datos$valor), ]
    datos$grupo <- droplevels(datos$grupo)
    tabla_grupos <- table(datos$grupo)
    datos <- datos[datos$grupo %in% names(tabla_grupos[tabla_grupos >= 2]), ]
    
    cohen <- as.data.frame(cohens_d(datos, valor ~ grupo, var.equal = homocedastico)[c(2, 3, 7)])
    
    # Pegamos los nombre de los grupos para que sean coherentes con lo anterior.
    rownames(cohen) <- rownames(clasica)
    cohen$group1 <- NULL
    cohen$group2 <- NULL
    
    # Unimos la tabla.
    clasica$grupo <- rownames(clasica)
    rownames(clasica) <- NULL
    names(clasica) <- c("grupo", "separado_clasica")
    sep_arbol$grupo <- rownames(sep_arbol)
    rownames(sep_arbol) <- NULL
    names(sep_arbol) <- c("separado_arbol", "grupo")
    cohen$grupo <- rownames(cohen)
    rownames(cohen) <- NULL
    
    resultado_1 <- inner_join(clasica, sep_arbol, by = "grupo")
    resultado <- inner_join(resultado_1, cohen, by= "grupo")
    
    resultado <- relocate(resultado, c("grupo", "separado_clasica", "separado_arbol", "magnitude"))
    
    # Limpieza de columnas.
    resultado <- resultado[,c("grupo", "separado_clasica", "separado_arbol", "magnitude")]
    
    write.csv(resultado, file = salida)
}

calcular_aciertos <- function(lista, salida, n_grupos = 6){ # Calcula los aciertos de un modelo en el sentido de que se calculan cuántos grupos deberían separarse (según la estadística clásica) y cuántos separa el árbol. Hay que dar el argumento 'list' como la lista de rutas a los archivos. Hay que darle el número de etapas y grupos por etapa. Si dos grupos no existen y el árbol no los separa, también cuenta como un acierto.
    aciertos <- c()
    for (ruta in lista){
        # Leemos el archivo.
        tabla <- read.csv(ruta)
        
        # Hacemos los cortes de grupo si en la llamada se especifica.
        if (n_grupos == 5){
            tabla <- tabla[!apply(tabla, 1, function(fila) any(grepl("G6", fila))), ]
        }
        else if (n_grupos == 4){
            tabla <- tabla[!apply(tabla, 1, function(fila) any(grepl("G6|G5", fila))), ]
        }
        
        # Sacamos las separaciones en forma de vector lógico.
        clas <- as.logical(tabla$separado_clasica)
        arbol <- as.logical(tabla$separado_arbol)
        
        # Sumamos los que coinciden.
        aciertos <- append(aciertos, sum(!is.na(clas) & !is.na(arbol) & clas == arbol))
    }
    
    # Creamos un 'data frame'.
    vec_grupos <- seq_along(lista)
    resultado <- data.frame(
        grupo = vec_grupos,
        aciertos = aciertos
    )
    
    write.csv(resultado, file = salida)
}

calcular_shannon <- function(vec){ # Calcula la entropía de Shannon y la normaliza.
    suma <- sum(vec)
    p <- vec / suma
    H <- -sum(p * log(p, base = 2))
    return(H / log(length(vec), base = 2)) # Normaliza para que H máxima sea 1.
}

analizar_arboles_shannon <- function(datos_base, muestras_vec, nombres, metodo_clasico, columnas){ # Con esta función, analizamos los datos en función de su entropía de Shannon
    
    # Sacamos una muestra pseudoaleatoria.
    datos <- bind_rows(
        lapply(
            split(datos_base, datos_base$grupo),
            function(x) {
                g <- as.character(unique(x$grupo))
                n <- muestras_vec[seq_along(g)]
                
                x[sample(nrow(x), n, replace = FALSE), ]
            }
        )
    )
    
    datos$grupo <- as.factor(datos$grupo)
    
    # Varía el método de separación.
    if (metodo_clasico == "tukey") {
        clasica <- as.data.frame(TukeyHSD(aov(valor ~ grupo, data = datos))$grupo)
    }
    
    if (metodo_clasico == "dunn") {
        clasica <- as.data.frame(dunn_test(datos, valor ~ grupo))[columnas]
    }
    
    if (metodo_clasico == "games") {
        clasica <- as.data.frame(games_howell_test(datos, valor ~ grupo))[columnas]
    }
    
    col_p_adj <- grep("adj", names(clasica), ignore.case = TRUE, value = TRUE)[1] # En la prueba de Tukey y en Dunn las columnas se llaman diferente. Por eso es necesario hacer un 'grep'.
    clasica$separado <- ifelse(clasica[[col_p_adj]] < 0.05, TRUE, FALSE)
    clasica$grupo <- rownames(clasica)
    clasica <- clasica[, c("grupo", "separado")]
    
    modelo_arbol <- rpart(valor ~ grupo, data = datos, method = "anova")
    arbol <- data.frame(nodo = modelo_arbol$where)
    arbol$grupo_real <- datos$grupo
    
    grupos <- sapply(split(arbol$nodo, arbol$grupo_real), `[`, 1)
    
    separado <- c()
    for (j in 1:length(grupos)) {
        if (j < length(grupos)) {
            for (l in 1:(length(grupos) - j)) {
                separado <- c(separado, grupos[j] != grupos[j + l])
            }
        }
    }
    
    sep_arbol <- data.frame(
        row.names = rownames(clasica),
        separado = separado
    )
    
    clas <- unlist(c(clasica["separado"]))
    arbol <- unlist(c(sep_arbol["separado"]))
    
    return(sum(clas == arbol))
}

sacar_titulo <- function(cadena){ # Saca el título para poder ser utilizado en un gráfico. Si es el grupo de desbalanceados, se queda con el título 'desbalanceados'.
    if (grepl("desbalanceados", cadena)){
        tit <- "desbalanceados"
    }
    else {
        n <- gsub("\\D", "", sub(".*_([^.]+)\\..*$", "\\1", cadena)) # Sacamos el n.
        g <- 4
        if (grepl("6g", substr(cadena, nchar(cadena) - 9, nchar(cadena)))){
            g <- 6
        }
        tit <- paste0(g, "g_", n, "n")
    }
    return(tit)
}

hacer_regresion_binomial <- function(datos_regresion, max_aciertos, titulo_grafico, salida, color_caso = FALSE){ # Realiza una regresión COM de Poisson y devuelve el gráfico.
    
    modelo <- glm(
        cbind(n_aciertos, max_aciertos - n_aciertos) ~ H,
        data = datos_regresion,
        family = binomial(link = "logit")
    )
    
    print(summary(modelo))
    
    pred <- predict(modelo, type = "response")
    
    ord <- order(datos_regresion$H)
    
    # Abrir dispositivo gráfico para guardar
    png(filename = salida, width = 800, height = 600, res = 120)
    
    if (color_caso) {
        cols <- as.numeric(factor(datos_regresion$caso))
        
        plot(datos_regresion$H, datos_regresion$n_aciertos,
             col = cols,
             pch = 16,
             xlab = "H",
             ylab = "Número de aciertos",
             main = titulo_grafico,
             ylim = c(0, max_aciertos))
        
        legend("topleft", legend = levels(factor(datos_regresion$caso)),
               col = 1:length(unique(cols)), pch = 16, title = "Caso")
        
    } else {
        
        plot(datos_regresion$H, datos_regresion$n_aciertos,
             pch = 16,
             xlab = "H",
             ylab = "Número de aciertos",
             main = titulo_grafico,
             ylim = c(0, max_aciertos))
    }
    
    # Línea del modelo
    lines(datos_regresion$H[ord], pred[ord] * max_aciertos,
          col = "blue", lwd = 2)
    
    dev.off()
}


escribir_resultado <- function(datos_corte, clasica, archivo, homocedastico) { # Compara árboles y estadística clásica pero va guardando los resultados de las iteraciones.
    temporal <- tempfile(fileext = ".csv")
    comparar_clasica_arbol_tamano(datos_corte, clasica, temporal, homocedastico = homocedastico)
    
    resultado <- read_csv(temporal, show_col_types = FALSE)
    
    write_csv(resultado, archivo, append = file.exists(archivo), col_names = !file.exists(archivo))
}

message("Funciones cargadas.")

# Parte 1: Simulación de datos ----

message("Iniciando simulaciones.")

## Caso 1 ----

# Cargamos los datos y los limpiamos.
datos <- read.csv("datos/reales/01_corazon.csv", sep = ";")

datos <- datos[c(3:8, 11:16, 19:22, 24, 28:32), c("X.1", "X.2")]

names(datos) <- c("grupo", "valor")

datos$grupo[18:22] <- "B"

datos$grupo <- lapply(datos$grupo, function(x) substr(x, 1, 1))

datos$grupo <- unlist(datos$grupo)
datos$grupo <- factor(datos$grupo)

#. Comprobamos parametricidad, y vemos que, efectivamente, la variable es normal y homocedástica.
comprobar_normalidad_shapiro(datos, "grupo", "valor")
bartlett.test(valor ~ grupo, data=datos)

# Llamamos 'valor' a nuestra variable para ser consistente con otros datos del proyecto y los guardamos.
write_csv(datos, "datos/reales/01a_corazon_tidy.csv")

# Ahora, calculamos los estadísticos.
estadisticos_01 <- generar_estadisticos(datos, "grupo")

# Guardamos los estadísticos.
write_csv(estadisticos_01, "datos/reales/estadisticos/01a_estadisticos.csv")


datos_reales <- read.csv("datos/reales/01a_corazon_tidy.csv")
estadisticos_real <- read.csv("datos/reales/estadisticos/01a_estadisticos.csv")

# Pasamos a factor (los datos provienen de los .csv.)
estadisticos_real %>%
    mutate(
        grupo = factor(grupo)
    )

# Simulamos.
for (n in c(20, 30, 40)){
    simulados <- datos_reales %>%
        group_by(grupo) %>%
        reframe(
            valor = simular_normal_exacto(valor, n)
        )
    
    # Pasamos a factores los simulados.
    simulados$gravedad <- factor(simulados$grupo)
    
    # Guardamos.
    comprobaciones_01(simulados, paste0("datos/simulados/01b_corazon_", n, ".csv"), paste0("datos/simulados/estadisticos/01b_corazon_", n, ".csv"))
}


n <- 20

datos_reales <- read.csv("datos/reales/01a_corazon_tidy.csv")
estadisticos_real <- read.csv("datos/reales/estadisticos/01a_estadisticos.csv")

# Pasamos a factor (los datos provienen de los .csv.)
estadisticos_real %>%
    mutate(
        grupo = factor(grupo)
    )

# Simulamos los originales.
simulados <- datos_reales %>%
    group_by(grupo) %>%
    reframe(
        valor = simular_normal_exacto(valor, n)
    )

filas <- nrow(datos_reales)
filas_simulados <- nrow(simulados)
for (i in 1:n){ # Creamos nuevas filas.
    simulados[i+filas_simulados, ] <- list("G5", 0)
    datos_reales[i+filas, ] <- list("G5", 0)
}

# Repetimos para el 6.
filas <- nrow(datos_reales)
filas_simulados <- nrow(simulados)
for (i in 1:n){ # Creamos nuevas filas.
    simulados[i+filas_simulados, ] <- list("G6", 0)
    datos_reales[i+filas, ] <- list("G6", 0)
}

# Sacamos estadísticos.
media <- mean(estadisticos_real$media)
dest <- mean(estadisticos_real$dest)

# Simulamos el nuevo grupo.
simulados[simulados$grupo == "G5", ] <- datos_reales[datos_reales$grupo == "G5", ] %>%
    reframe(
        grupo = "G5",
        valor = simular_normal_exacto(valor, n, mu = media, s = dest) # Simulamos un grupo 'promedio' en sus dos estadísticos.
    )

simulados[simulados$grupo == "G6", ] <- datos_reales[datos_reales$grupo == "G6", ] %>%
    reframe(
        grupo = "G6",
        valor = simular_normal_exacto(valor, n, mu = mean(c(estadisticos_real[2,2], estadisticos_real[4,2])), s = dest) # Simulamos un grupo suma de otros dos. Debe ser homocedástico al resto.
    )

# Pasamos a factores.
simulados$gravedad <- factor(simulados$grupo)

# Guardamos.
comprobaciones_01(simulados, "datos/simulados/01c_corazon_6g_n20.csv", "datos/simulados/estadisticos/01c_corazon_6g_n20.csv")

# Generamos grupos desbalanceados
n <- c(15, 20, 25, 30)
names(n) <- c("C", "P", "I", "B") # Asignamos un n a cada grupo.

datos_reales <- read.csv("datos/reales/01a_corazon_tidy.csv")
estadisticos_real <- read.csv("datos/reales/estadisticos/01a_estadisticos.csv")

# Pasamos a factor.
estadisticos_real %>%
    mutate(
        grupo = factor(grupo)
    )

simulados <- datos_reales %>%
    group_by(grupo) %>%
    reframe(
        valor = simular_normal_exacto(valor, n[first(grupo)])
    )

# Pasamos a factores.
simulados$gravedad <- factor(simulados$grupo)

# Guardamos.
comprobaciones_01(simulados, "datos/simulados/01d_corazon_desbalanceados.csv", "datos/simulados/estadisticos/01c_corazon_desbalanceados.csv")


n <- 40 # General para todos los grupos.

library(tidyverse)

datos_reales <- read.csv("datos/reales/01a_corazon_tidy.csv")
estadisticos_real <- read.csv("datos/reales/estadisticos/01a_estadisticos.csv")

estadisticos_real %>%
    mutate(
        grupo = factor(grupo)
    )

simulados <- datos_reales %>%
    group_by(grupo) %>%
    reframe(
        valor = simular_normal_exacto(valor, n)
    )

# Creamos el grupo 5.
filas <- nrow(datos_reales)
filas_simulados <- nrow(simulados)
for (i in 1:n){
    simulados[i+filas_simulados, ] <- list("G5", 0)
    datos_reales[i+filas, ] <- list("G5", 0)
}

# Creamos el grupoo 6.
filas <- nrow(datos_reales)
filas_simulados <- nrow(simulados)
for (i in 1:n){
    simulados[i+filas_simulados, ] <- list("G6", 0)
    datos_reales[i+filas, ] <- list("G6", 0)
}

# Sacamos estadísticos.
media <- mean(estadisticos_real$media)
dest <- mean(estadisticos_real$dest)

# Simulamos el nuevo grupo.
simulados[simulados$grupo == "G5", ] <- datos_reales[datos_reales$grupo == "G5", ] %>%
    reframe(
        grupo = "G5",
        valor = simular_normal_exacto(valor, n, mu = media, s = dest) # Simulamos un grupo 'promedio' en sus dos estadísticos.
    )

simulados[simulados$grupo == "G6", ] <- datos_reales[datos_reales$grupo == "G6", ] %>%
    reframe(
        grupo = "G6",
        valor = simular_normal_exacto(valor, n, mu = mean(c(estadisticos_real[2,2], estadisticos_real[4,2])), s = dest) # Simulamos un grupo suma de otros dos. Debe ser homocedástico al resto.
    )

# Pasamos a factores.
simulados$gravedad <- factor(simulados$grupo)

# Guardamos.
comprobaciones_01(simulados, "datos/simulados/01e_corazon_6g_n40.csv", "datos/simulados/estadisticos/01e_corazon_6g_n40.csv")

## Caso 2 ----

# Importamos los datos y los limpiamos. Los del 'paper' pusieron un punto separador del mil y también del decimal.
datos <- read.csv("datos/reales/02_brca.csv", sep = ";")

datos$ratio[nchar(datos$ratio) >= 7 & str_count(datos$ratio, "\\.") == 2] <-
    sub("\\.", "", datos$ratio[nchar(datos$ratio) >= 7 & str_count(datos$ratio, "\\.") == 2])

datos$variant <- factor(datos$variant)

names(datos) <- c("grupo", "valor")

datos$valor <- as.numeric(datos$valor)

datos <- datos[datos$grupo == "Q1785H" | datos$grupo == "R1753T" | datos$grupo == "E1794D" | datos$grupo == "V1804D",]

# No son normales, pero sí homocedásticos para la variable 3.
comprobar_normalidad_shapiro(datos, "grupo", "valor")
leveneTest(valor ~ grupo, data = datos)

estadisticos_02 <- generar_estadisticos_np(datos, "grupo")

write_csv(estadisticos_02, "datos/reales/estadisticos/02a_estadisticos.csv")
write_csv(datos, "datos/reales/02a_brca_tidy.csv")


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


n <- 20

datos_reales <- read_csv("datos/reales/02a_brca_tidy.csv")
estadisticos_real <- read_csv("datos/reales/estadisticos/02a_estadisticos.csv")

# Sacamos los estadísticos ANTES de crear los grupos nuevos.
ri_global <- IQR(datos_reales$valor)
desviacion <- (datos_reales$valor - median(datos_reales$valor)) / ri_global  # Las desviaciones la normalizamos con respecto a la mediana y al RI.
ri_medio <- mean(estadisticos_real$RI)
mediana_glob <- median(datos_reales$valor)

# Procedemos a simular. Simulamos el resto de grupos con la misma filosofía que el 'script' 2b.
grupos_extra <- c("G5", "G6")
medianas_extra <- c(median(datos_reales$valor), median(datos_reales[datos_reales$grupo == "R1753T", ]$valor) + median(datos_reales[datos_reales$grupo == "Q1785H", ]$valor)) # Ponemos como medianas la mediana de todos los datos (G5), y la suma de las medianas de dos grupos (G6).

medianas_objetivo <- datos_reales %>%
    group_by(grupo) %>%
    summarise(med_tipo = median(valor), .groups = "drop") %>%
    bind_rows(
        tibble(
            grupo = grupos_extra,
            med_tipo = medianas_extra
        )
    )

simulados <- medianas_objetivo %>%
    group_by(grupo) %>%
    reframe(
        {
            med_tipo <- first(med_tipo)
            desv <- sample(desviacion, size = n, replace = TRUE)
            desv <- desv - median(desv)
            desv <- desv / IQR(desv)
            valor <- med_tipo + desv * ri_global
            tibble(valor = valor)
        }
    )

# Comprobamos los supuestos.
simulados$grupo <- factor(simulados$grupo)
comprobaciones_02(simulados, datos_reales, "datos/simulados/02c_brca_6g_n20.csv", "datos/simulados/estadisticos/02c_brca_6g_n20.csv")

# Generamos grupos desbalanceados
n <- c(20, 25, 30, 15)
names(n) <- c("V1804D", "E1794D", "Q1785H", "R1753T") # Nombre de los grupos.

datos_reales <- read_csv("datos/reales/02a_brca_tidy.csv")
estadisticos_real <- read_csv("datos/reales/estadisticos/02a_estadisticos.csv")

# Buscamos generar datos no normales, pero sí homocedásticas, que mantengan la mediana y el RI de los originales.

ri_global <- IQR(datos_reales$valor)
desviacion <- (datos_reales$valor - median(datos_reales$valor)) / ri_global  # Las desviaciones la normalizamos con respecto a la mediana y al RI.

# Procedemos a simular.
simulados <- datos_reales %>% # Separamos por tipo.
    distinct(grupo) %>%
    group_by(grupo) %>%
    reframe(
        {
            med_tipo <- median(datos_reales$valor[datos_reales$grupo == first(grupo)]) # Sacamos la mediana del tipo.
            desv <- sample(desviacion, size = n[grupo], replace = TRUE) # Tomamos unas desviaciones pseudoaleatorias.
            desv <- desv - median(desv) # La centramos.
            desv <- desv / IQR(desv) # La normalizamos a RI = 1.
            valor <- med_tipo + desv * ri_global # Generamos el valor.
            tibble(valor = valor)
        }
    )

# Comprobamos que los supuestos paramétricos se respetan con respecto a los datos originales.
simulados$grupo <- factor(simulados$grupo)
comprobaciones_02(simulados, datos_reales, "datos/simulados/02d_brca_desbalanceados.csv", "datos/simulados/estadisticos/02d_brca_desbalanceados.csv")



# Guardado de información ----
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")