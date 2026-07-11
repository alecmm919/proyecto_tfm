# Título: PIPELINE_COMPLETO.R
#
# Autor: Alejandro M.
#
# Descripción: Este 'script' contiene el 'pipeline' completo del proyecto. Se han fusionado los distintos 'scripts' en uno solo.
#
# Tiempo de ejecución aproximado: 

# TODO: limpiar librerías, comprobar funcionamiento y resultados.

inicio <- Sys.time()

set.seed(117) # Semilla para la reproducibilidad.

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
        dplyr::group_by(dplyr::across(dplyr::all_of(grupos))) %>%
        dplyr::summarise(
            n = dplyr::n(),
            p_value = shapiro.test(.data[[resultados]])$p.value,
            .groups = "drop",
        )
}

comprobar_normalidad_kolmogorov <- function(dat, grupos, resultados){ # Hace una prueba de normalidad de Kolmogorov-Smirnof. Devuelve solo el p-valor.
    
    dat %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grupos))) %>%
        dplyr::summarise(
            n = dplyr::n(),
            p_value = ks.test(.data[[resultados]], "pnorm", mean(.data[[resultados]]), sd(.data[[resultados]]))$p.value,
            .groups = "drop"
        )
}

comprobar_normalidad_cramer <- function(dat, grupos, resultados){ # Hace una prueba de normalidad de Cramer-Von-Misses. Devuelve solo el p-valor.
    
    suppressWarnings( # Eliminamos las advertencias porque esta función devuelve un 'warning' si el p-valor es demasiado pequeño.
        dat %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(grupos))) %>%
            dplyr::summarise(
                n = dplyr::n(),
                p_value = nortest::cvm.test(.data[[resultados]])$p.value,
                .groups = "drop"
            )
    )
}

generar_estadisticos <- function(dat, grupos){ # Devuelve los estadísticos paramétricos (media y desviación estándar) de un 'tibble' en función de los factores.
    
    salida <- dat %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grupos))) %>%
        dplyr::summarise(
            media = mean(valor),
            dest = sd(valor)
        )
}

generar_estadisticos_np <- function(dat, grupos){ # Devuelve los estadísticos no paramétricos (mediana y rango intercuartílico) de un 'tibble' en función de los factores.
    
    salida <- dat %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grupos))) %>%
        dplyr::summarise(
            mediana = median(valor),
            RI = IQR(valor)
        )
}

comparar_estadisticos_reales_simulados <- function(tabla1, tabla2, n_1, n_2, factores){ # Compara los estadísticos de dos tablas de datos (reales y simulados) y hace una prueba t de Student para la igualdad de medias y una F para el cociente de varianzas.
    
    dplyr::inner_join(
        tabla1, tabla2,
        by = factores,
        suffix = c("_real", "_sim") # Unimos las tablas etiquetando las columnas.
    ) %>%
        dplyr::mutate(
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
        dplyr::select(
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
    
    readr::write_csv(simulados, salida_datos)
    utils::write.csv(estadisticos_sim, salida_estadisticos)
}

comprobaciones_02 <- function(simulados, datos_reales, salida_datos, salida_estadisticos){ # Comprueba que los datos simulados del caso 02 respetan los supuestos estadísticos de los reales. La filosofía es la misma que la de la función anterior.
    
    n_min <- min(table(simulados$grupo))
    
    print(comprobar_normalidad_shapiro(simulados, "grupo", "valor"))
    print(fligner.test(valor ~ grupo, data=simulados)) # Usamos Fligner porque estos datos presentan demasiados valores atípicos.
    
    estadisticos_real <- generar_estadisticos_np(datos_reales, "grupo")
    estadisticos_sim  <- generar_estadisticos_np(simulados, "grupo")
    
    print(estadisticos_real)
    print(estadisticos_sim)
    
    readr::write_csv(simulados, salida_datos)
    utils::write.csv(estadisticos_sim, salida_estadisticos)
}

comprobaciones_03 <- function(simulados, datos_reales, salida_datos, salida_estadisticos){ # Comprueba que los datos simulados del caso 03 respetan los supuestos estadísticos de los reales.
    
    n_min <- min(table(simulados$grupo))
    
    print(comprobar_normalidad_shapiro(simulados, "grupo", "valor"))
    print(car::leveneTest(valor ~ grupo, data=simulados)) # Los datos no son normales.
    
    estadisticos_real <- generar_estadisticos(datos_reales, "grupo")
    estadisticos_sim <- generar_estadisticos(simulados, "grupo")
    print(estadisticos_real)
    print(estadisticos_sim)
    
    estadisticos_sim <- generar_estadisticos(simulados, "grupo")
    
    readr::write_csv(simulados, salida_datos)
    utils::write.csv(estadisticos_sim, salida_estadisticos)
}

comprobaciones_04 <- function(simulados, datos_reales, salida_datos, salida_estadisticos){ # Comprueba que los datos simulados del caso 04 respetan los supuestos estadísticos de los reales.
    
    n_min <- min(table(simulados$grupo))
    
    print(comprobar_normalidad_shapiro(simulados, "grupo", "valor"))
    print(fligner.test(valor ~ grupo, data=simulados))
    
    estadisticos_real <- generar_estadisticos_np(datos_reales, "grupo")
    estadisticos_sim <- generar_estadisticos_np(simulados, "grupo")
    
    print(estadisticos_real)
    print(estadisticos_sim)
    
    readr::write_csv(simulados, salida_datos)
    utils::write.csv(estadisticos_sim, salida_estadisticos)
}

plot_contrastes <- function(data, titulo, metodo_posthoc = c("tukey", "dunn", "games-howell"), archivo_salida) { # Genera 'boxplots' e indica las diferencias entre grupos en función de la prueba 'post hoc' planteada.
    
    metodo_posthoc <- match.arg(metodo_posthoc)
    
    yvar <- "valor"
    
    data$.grupo <- as.factor(data$grupo)
    
    g <- ggplot2::ggplot(data, ggplot2::aes(x = .grupo, y = .data[[yvar]], fill = .grupo)) +
        ggplot2::geom_boxplot(alpha = 0.8, width = 0.65, outlier.shape = 21, outlier.fill = "white", outlier.size = 2) +
        ggplot2::scale_fill_discrete() +
        ggplot2::theme_bw() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 16),
            axis.title = ggplot2::element_text(face = "bold", size = 12),
            axis.text = ggplot2::element_text(size = 10, colour = "black"),
            legend.position = "none",
            panel.grid.major = ggplot2::element_line(colour = "grey"),
            panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::labs(title = titulo, x = "grupo", y = yvar)
    
    formula_posthoc <- as.formula("valor ~ .grupo")
    
    post <- switch( # Elegimos el método 'post hoc'.
        metodo_posthoc,
        
        "tukey" = rstatix::tukey_hsd(data, formula_posthoc),
        
        "dunn" = rstatix::dunn_test(data, formula_posthoc, p.adjust.method = "holm"),
        
        "games-howell" = rstatix::games_howell_test(data, formula_posthoc)
    )
    
    post_sig <- post[post$p.adj < 0.05, ]
    
    if (nrow(post_sig) > 0) {
        
        niveles <- levels(data$.grupo)
        ymax <- max(data[[yvar]], na.rm = TRUE)
        
        post_sig$xmin <- match(post_sig$group1, niveles)
        post_sig$xmax <- match(post_sig$group2, niveles)
        
        post_sig$y.position <- seq(from = ymax * 1.05, by = ymax * 0.08, length.out = nrow(post_sig))
        
        g <- g +
            ggpubr::stat_pvalue_manual(post_sig, xmin = "xmin", xmax = "xmax", y.position = "y.position", label = "p.adj.signif", hide.ns = TRUE)
    }
    
    ggplot2::ggsave(archivo_salida, g, width = 8, height = 6, dpi = 300)
    
    return(g)
}

generar_arbol <- function(dat, salida, titulo, profundidad = NULL) { # Genera un árbol de decisión. Permite seleccionar una profundidad determinada.
    
    # Para ignorar la profundidad si no se determina en la llamada.
    if (is.null(profundidad)) {
        ctrl <- rpart::rpart.control()
    } else {
        ctrl <- rpart::rpart.control(maxdepth = profundidad)
    }
    
    modelo <- rpart::rpart(valor ~ ., data = dat, method = "anova", control = ctrl)
    
    grDevices::png(salida, width = 1600, height = 1600, res = 300)
    
    rpart.plot::rpart.plot(modelo, type = 3, fallen.leaves = TRUE, nn = FALSE, main = titulo, box.palette = "RdYlGn", branch.lty = 1, branch.lwd = 2, tweak = 1.2, faclen = 0, roundint = FALSE)
    
    grDevices::dev.off()
    
    # Generamos otra vez lo mismo para que aparezca en el visor de RStudio.
    rpart.plot::rpart.plot(modelo, type = 3, fallen.leaves = TRUE, nn = FALSE, main = titulo, box.palette = "RdYlGn", branch.lty = 1, branch.lwd = 2, tweak = 1.2, faclen = 0, roundint = FALSE)
    
}

matriz_confusion <- function(dat, salida){ # Genera la matriz de confusión.
    modelo <- rpart::rpart(grupo ~ ., data = dat, method = "class")
    pred <- predict(modelo, dat, type = "class")
    conf <- caret::confusionMatrix(data = pred, reference = dat$grupo)
    tabla <- as.data.frame(conf$table)
    
    p <- ggplot2::ggplot(tabla, ggplot2::aes(x = Reference, y = Prediction, fill = Freq)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.8) +
        ggplot2::geom_text(ggplot2::aes(label = Freq), colour = "white", fontface = "bold", size = 5) +
        ggplot2::scale_fill_gradient(low = "red", high = "green") +
        ggplot2::labs(title = "Matriz de confusión", x = "Clase real", y = "Clase predicha") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 16),
            axis.title = ggplot2::element_text(face = "bold"),
            axis.text = ggplot2::element_text(colour = "black", size = 11),
            panel.grid = ggplot2::element_blank()
        )
    
    ggplot2::ggsave(filename = salida, plot = p, width = 6, height = 5, dpi = 300)
    
    p
    conf
}

comparar_clasica_arbol_tamano <- function(datos, clasica, salida, homocedastico = TRUE, profundidad = NULL){ # Genera el árbol de decisión y lo compara con una salida clásica. De esta forma, se obtienen los aciertos.
    
    # Para ignorar la profundidad si no se determina en la llamada.
    if (is.null(profundidad)) {
        ctrl <- rpart::rpart.control()
    } else {
        ctrl <- rpart::rpart.control(maxdepth = profundidad)
    }
    
    # Separación por árbol.
    # Calculamos n por grupo y k.
    k <- length(levels(datos$grupo))
    
    # Modelizamos los árboles.
    arbol <- as.data.frame(rpart::rpart(valor ~ grupo, data = datos, method = "anova", control = ctrl)$where) # Indica en qué rama termina cada observación.
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
    
    cohen <- as.data.frame(rstatix::cohens_d(datos, valor ~ grupo, var.equal = homocedastico)[c(2, 3, 7)])
    
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
    
    resultado_1 <- dplyr::inner_join(clasica, sep_arbol, by = "grupo")
    resultado <- dplyr::inner_join(resultado_1, cohen, by= "grupo")
    
    resultado <- dplyr::relocate(resultado, c("grupo", "separado_clasica", "separado_arbol", "magnitude"))
    
    # Limpieza de columnas.
    resultado <- resultado[,c("grupo", "separado_clasica", "separado_arbol", "magnitude")]
    
    utils::write.csv(resultado, file = salida)
}

calcular_aciertos <- function(lista, salida, n_grupos = 6){ # Calcula los aciertos de un modelo en el sentido de que se calculan cuántos grupos deberían separarse (según la estadística clásica) y cuántos separa el árbol. Hay que dar el argumento 'list' como la lista de rutas a los archivos. Hay que darle el número de etapas y grupos por etapa. Si dos grupos no existen y el árbol no los separa, también cuenta como un acierto.
    aciertos <- c()
    for (ruta in lista){
        # Leemos el archivo.
        tabla <- utils::read.csv(ruta)
        
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
    
    utils::write.csv(resultado, file = salida)
}

calcular_shannon <- function(vec){ # Calcula la entropía de Shannon y la normaliza.
    suma <- sum(vec)
    p <- vec / suma
    H <- -sum(p * log(p, base = 2))
    return(H / log(length(vec), base = 2)) # Normaliza para que H máxima sea 1.
}

analizar_arboles_shannon <- function(datos_base, muestras_vec, nombres, metodo_clasico, columnas){ # Con esta función, analizamos los datos en función de su entropía de Shannon
    
    # Sacamos una muestra pseudoaleatoria.
    datos <- dplyr::bind_rows(
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
        clasica <- as.data.frame(rstatix::dunn_test(datos, valor ~ grupo))[columnas]
    }
    
    if (metodo_clasico == "games") {
        clasica <- as.data.frame(rstatix::games_howell_test(datos, valor ~ grupo))[columnas]
    }
    
    col_p_adj <- grep("adj", names(clasica), ignore.case = TRUE, value = TRUE)[1] # En la prueba de Tukey y en Dunn las columnas se llaman diferente. Por eso es necesario hacer un 'grep'.
    clasica$separado <- ifelse(clasica[[col_p_adj]] < 0.05, TRUE, FALSE)
    clasica$grupo <- rownames(clasica)
    clasica <- clasica[, c("grupo", "separado")]
    
    modelo_arbol <- rpart::rpart(valor ~ grupo, data = datos, method = "anova")
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

# Título: 01a_preparar_datos_reales.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' pretende hacer un análisis exploratorio de variables paramétricas. Para ello, se midieron los LVIDs (left ventricular internal diameter) tras tres semanas con los ratones sometidos a un tratamiento: control, piridostigmina, isoprenalina o ambos a la vez. Normalmente se asocia su aumento con la presencia de una patología cardíaca. En el artículo de referencia, existe un .csv con datos y en este 'scripts' se pretenden disponer en formato 'tidy' para posteriores análisis.
#
# Referencia: Marinkovic et al., 2026.

# Librerías y carga:

library(tidyverse)

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

# Título: 01b_simular_datos.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico normal y homocedástico. Los datos son independientes. Se generan 20, 30 y 40 datos.

# Librerías y carga:

library(tidyverse)

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


# Título: 01c_simular_datos_6g_n20.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico normal y homocedástico. Los datos son independientes. Se generan 20 datos en 6 grupos. Los dos grupos extra se generarán en todos los casos con los mismos criterios: el grupo 5 es la media de todos los grupos, el grupo 6 es la media de los dos grupos con media más baja.

# Librerías y carga:

library(tidyverse)

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

# Título: 01d_simular_datos_desbalanceados.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico normal y homocedástico. Los datos son independientes. En este caso, los grupos tienen distinto número de datos. Estos datos tienen únicamente objetivos exploratorios, así que se han desbalanceado arbitrariamente.

# Librerías y carga:

library(tidyverse)

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

# Título: 01e_simular_datos_6g_n40.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos de la misma forma que en el 'script' 01c, pero con un valor de n = 40.

# Librerías y carga:

library(tidyverse)

n <- 40 # General para todos los grupos.

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

# Título: 02a_preparar_datos_reales.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' pretende hacer una disposición en formato 'tidy' de datos procedentes de variables no normales, pero sí homocedásticas. Para ello, se mide la expresión del gen BRCA1 en 4 de sus posibles variantes en el extremo N-terminal. Se mide con respecto a la actividad del alelo silvestre.
#
# Referencia: Carvalho et al., 2014.

# Librerías y carga:

library(tidyverse)
library(car)
library(stringr)

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

# Título: 02b_simular_datos.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico de variables homocedásticas, pero no normales. Para ello, se toma la desviación de cada dato y se genera un nuevo dato tomando estas desviaciones pseudoaleatoriamente. Se generan 4 grupos de n = 20, n = 30 y n = 40.

#Librerías y carga:

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

# Título: 02c_simular_datos_6g_n20.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico de variables homocedásticas, pero no normales. Se generan dos grupos extra con los mismos criterios que en el caso del 'script' 01c.

#Librerías y carga:

library(tidyverse)
library(car)
library(nortest)

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

# Título: 02d_simular_datos_desbalanceados.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico de variables homocedásticas, pero no normales. En este caso, los grupos tienen distinto número de datos. Estos datos tienen únicamente objetivos exploratorios, así que se han desbalanceado arbitrariamente.

#Librerías y carga:

library(tidyverse)
library(car)
library(nortest)

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

# Título: 02e_simular_datos_6g_n40.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico de variables homocedásticas, pero no normales. Se generan dos grupos extra con 40 datos, de la misma forma que se realizó en el 02c.

#Librerías y carga:

library(tidyverse)
library(car)
library(nortest)

n <- 40

datos_reales <- read_csv("datos/reales/02a_brca_tidy.csv")
estadisticos_real <- read_csv("datos/reales/estadisticos/02a_estadisticos.csv")


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
comprobaciones_02(simulados, datos_reales, "datos/simulados/02e_brca_6g_n40.csv", "datos/simulados/estadisticos/02e_brca_6g_n40.csv")

# Título: 03a_preparar_datos_reales.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' pretende hacer un análisis exploratorio de variables normales y heterocedásticas. Se usará el contexto del estrés en ratas. Para ello, se medirá la concentración de receptor de N-metil-d-aspartato 2. Los grupos son 1: control; 2: sometidos a estrés durante 5 días; 3: 14 y 4: 21 días, respectivamente. No se trata de las mismas ratas, por lo que los datos son independientes.
#
# Referencia: Han et al., 2017.

# Librerías y carga:

library(tidyverse)

# Cargamos los datos. Como ya venían en formato 'tidy', fue muy sencillo.
datos <- read.csv("datos/reales/03_estres.csv")

datos$grupo <- factor(datos$GROUP)
datos$valor <- as.numeric(gsub(",", ".", datos$NR2A)) # Cambiamos a punto decimal.

datos$NR2A <- NULL
datos$GROUP <- NULL

#. Comprobamos parametricidad, y vemos que, efectivamente, la variable es normal, pero heterocedástica.
comprobar_normalidad_shapiro(datos, "grupo", "valor")
bartlett.test(valor ~ grupo, data=datos)

# Guardamos los datos.
write_csv(datos, "datos/reales/03a_estres_tidy.csv")

# Planteamos una exploración estadística básica. Nos servirá para las simulaciones posteriores. Los guardamos en un .csv.
estadisticos_03 <- generar_estadisticos(datos, "grupo")
write_csv(estadisticos_03, "datos/reales/estadisticos/03a_estadisticos.csv")

# Título: 03b_simular_datos.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del estrés en ratas. Se generan 4 grupos de n = 20, n = 30 y n = 40 cada uno.

# Librerías y carga:

library(tidyverse)
library(car)

# Cargamos los datos y convertimos en factores.
datos_reales <- read_csv("datos/reales/03a_estres_tidy.csv") %>%
    mutate(
        grupo = factor(grupo)
    )

estadisticos_real <- read_csv("datos/reales/estadisticos/03a_estadisticos.csv")

# Simulamos los grupos por sus distintos estadísticos.
for (n in c(20, 30, 40)){
    simulados <- datos_reales %>%
        group_by(grupo) %>%
        reframe(
            valor = simular_normal_exacto(valor, n)
        )
    
    # Pasamos a factores los simulados y comprobamos.
    simulados$grupo <- factor(simulados$grupo)
    comprobaciones_03(simulados, datos_reales, paste0("datos/simulados/03b_estres_", n, ".csv"), paste0("datos/simulados/estadisticos/03b_estres_", n, ".csv"))
}

# Título: 03c_simular_datos_6g_n20.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del estrés en ratas. Se generan dos grupos extra bajo los mismos criterios que en el 'script' 01c.

# Librerías y carga:

library(tidyverse)
library(car)

n <- 20 # Número de datos.

# Cargamos los datos y convertimos en factores.
datos_reales <- read_csv("datos/reales/03a_estres_tidy.csv")

estadisticos_real <- read_csv("datos/reales/estadisticos/03a_estadisticos.csv")

datos_reales$grupo <- as.character(datos_reales$grupo)

# Generamos dos grupos extra.
filas <- nrow(datos_reales)
for (i in 1:n){
    datos_reales[i+filas, ] <- list("G5", 0)
}

filas <- nrow(datos_reales)
for (i in 1:n){
    datos_reales[i+filas, ] <- list("G6", 0)
}

# Simulamos los grupos por sus distintos estadísticos.
simulados <- datos_reales %>%
    group_by(grupo) %>%
    reframe(
        valor = simular_normal_exacto(valor, n)
    )

# Simulamos los grupos nuevos.
media <- mean(datos_reales$valor, na.rm = TRUE)

# Simulamos los nuevos grupos usando los estadísticos.
simulados[simulados$grupo == "G5", ] <- datos_reales[datos_reales$grupo == "G5", ] %>%
    reframe(
        grupo = "G5",
        valor = simular_normal_exacto(valor, n, mu = media, s = mean(c(as.numeric(estadisticos_real[1,3]), as.numeric(estadisticos_real[2,3]), as.numeric(estadisticos_real[3,3]), as.numeric(estadisticos_real[4,3])))) # Simulamos un grupo 'promedio' en sus dos estadísticos.
    )

simulados[simulados$grupo == "G6", ] <- datos_reales[datos_reales$grupo == "G6", ] %>%
    reframe(
        grupo = "G6",
        valor = simular_normal_exacto(valor, n, mu = as.numeric(estadisticos_real[2,2]) + as.numeric(estadisticos_real[3,2]), s = mean(c(as.numeric(estadisticos_real[3,2]), as.numeric(estadisticos_real[3,3])))) # Simulamos la suma de dos grupos.
    )

# Hacemos las comprobaciones: se mantiene la falta de parametricidad y los estadísticos.
simulados$grupo <- factor(simulados$grupo)
comprobaciones_03(simulados, datos_reales, "datos/simulados/03c_estres_6g_n20.csv", "datos/simulados/estadisticos/03c_estres_6g_n20.csv")

# Título: 03d_simular_datos_desbalanceados.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del estrés en ratas. En este caso, los grupos tienen distinto número de datos. Estos grupos desbalanceados se utilizan únicamente con fines exploratorios, por lo que se han desbalanceado arbitrariamente.

# Librerías y carga:

library(tidyverse)
library(car)

# Generamos grupos desbalanceados
n <- c(25, 30, 15, 20)

# Cargamos los datos y convertimos en factores.
datos_reales <- read_csv("datos/reales/03a_estres_tidy.csv") %>%
    mutate(
        grupo = factor(grupo)
    )

estadisticos_real <- read_csv("datos/reales/estadisticos/03a_estadisticos.csv")

# Simulamos los grupos por sus distintos estadísticos.
simulados <- datos_reales %>%
    group_by(grupo) %>%
    reframe(
        valor = simular_normal_exacto(valor, n[first(grupo)])
    )

# Hacemos las comprobaciones: se mantiene la falta de parametricidad y los estadísticos.
simulados$grupo <- factor(simulados$grupo)
comprobaciones_03(simulados, datos_reales, "datos/simulados/03d_estres_desbalanceados.csv", "datos/simulados/estadisticos/03d_estres_desbalanceados.csv")

# Título: 03e_simular_datos_6g_n40.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del estrés en ratas. Se generan dos grupos extra con 40 datos de la misma forma que en el 03c.

# Librerías y carga:

library(tidyverse)
library(car)

n <- 40 # Número de datos general.

# Cargamos los datos y convertimos en factores.
datos_reales <- read_csv("datos/reales/03a_estres_tidy.csv")

estadisticos_real <- read_csv("datos/reales/estadisticos/03a_estadisticos.csv")

datos_reales$grupo <- as.character(datos_reales$grupo)

# Generamos dos grupos extra.
filas <- nrow(datos_reales)
for (i in 1:n){
    datos_reales[i+filas, ] <- list("G5", 0)
}

filas <- nrow(datos_reales)
for (i in 1:n){
    datos_reales[i+filas, ] <- list("G6", 0)
}

# Simulamos los grupos por sus distintos estadísticos.
simulados <- datos_reales %>%
    group_by(grupo) %>%
    reframe(
        valor = simular_normal_exacto(valor, n)
    )

# Simulamos los grupos nuevos.
media <- mean(datos_reales$valor, na.rm = TRUE)

# Simulamos los nuevos grupos usando los estadísticos.
simulados[simulados$grupo == "G5", ] <- datos_reales[datos_reales$grupo == "G5", ] %>%
    reframe(
        grupo = "G5",
        valor = simular_normal_exacto(valor, n, mu = media, s = mean(c(as.numeric(estadisticos_real[1,3]), as.numeric(estadisticos_real[2,3]), as.numeric(estadisticos_real[3,3]), as.numeric(estadisticos_real[4,3])))) # Simulamos un grupo 'promedio' en sus dos estadísticos.
    )

simulados[simulados$grupo == "G6", ] <- datos_reales[datos_reales$grupo == "G6", ] %>%
    reframe(
        grupo = "G6",
        valor = simular_normal_exacto(valor, n, mu = as.numeric(estadisticos_real[2,2]) + as.numeric(estadisticos_real[3,2]), s = mean(c(as.numeric(estadisticos_real[3,2]), as.numeric(estadisticos_real[3,3])))) # Simulamos la suma de dos grupos.
    )

# Hacemos las comprobaciones: se mantiene la falta de parametricidad y los estadísticos.
simulados$grupo <- factor(simulados$grupo)
comprobaciones_03(simulados, datos_reales, "datos/simulados/03e_estres_6g_n40.csv", "datos/simulados/estadisticos/03e_estres_6g_n40.csv")

# Título: 04a_preparar_datos_reales.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' pretende hacer un análisis exploratorio de variables no normales y heterocedásticos. Se parte de los datos del paper Zhen et al., 2024. Se pretende predecir el estadío de la fibrosis en la hepatitis B en función de la concentración de GPR.
#
# Referencia: Zhen et al., 2024

# Librerías y carga:

library(tidyverse)
library(car)

# Se toman los datos desde los 5 csv publicados en el 'paper'. Y tomamos únicamente las variables que nos interesan.
datos_0 <- read.csv("datos/reales/04_f0.csv", sep = ";")
datos_1 <- read.csv("datos/reales/04_f1.csv", sep = ";")
datos_2 <- read.csv("datos/reales/04_f2.csv", sep = ";")
datos_3 <- read.csv("datos/reales/04_f3.csv", sep = ";")
datos_4 <- read.csv("datos/reales/04_f4.csv", sep = ";")

datos_0 <- datos_0[c("name", "GPR", "age", "sex", "grade")]
datos_1 <- datos_1[c("name", "GPR", "age", "sex", "grade")]
datos_2 <- datos_2[c("name", "GPR", "age", "sex", "grade")]
datos_3 <- datos_3[c("name", "GPR", "age", "sex", "grade")]
datos_4 <- datos_4[c("name", "GPR", "age", "sex", "grade")]

datos_0 <- datos_0[datos_0$age %in% 40:50 & datos_0$sex == "male", ]
datos_0 <- datos_0[,c(1,2)]
datos_1 <- datos_1[datos_1$age %in% 40:50 & datos_1$sex == "male", ]
datos_1 <- datos_1[,c(1,2)]
datos_2 <- datos_2[datos_2$age %in% 40:50 & datos_2$sex == "male", ]
datos_2 <- datos_2[,c(1,2)]
datos_3 <- datos_3[datos_3$age %in% 40:50 & datos_3$sex == "male", ]
datos_3 <- datos_3[,c(1,2)]
datos_4 <- datos_4[datos_4$age %in% 40:50 & datos_4$sex == "male", ]
datos_4 <- datos_4[,c(1,2)]

# Para unir las 5 tablas en una sola, marcamos la grupo.
datos_0 <- datos_0 %>%
    mutate(grupo = "GPR_0") %>%
    dplyr::select(grupo, valor = GPR) # En este caso, hay que poner dplyr:: porque la función select estaba dando problemas.

datos_1 <- datos_1 %>%
    mutate(grupo = "GPR_1") %>%
    dplyr::select(grupo, valor = GPR)

datos_2 <- datos_2 %>%
    mutate(grupo = "GPR_2") %>%
    dplyr::select(grupo, valor = GPR)

datos_3 <- datos_3 %>%
    mutate(grupo = "GPR_3") %>%
    dplyr::select(grupo, valor = GPR)

datos_4 <- datos_4 %>%
    mutate(grupo = "GPR_4") %>%
    dplyr::select(grupo, valor = GPR)

datos <- bind_rows(datos_0, datos_1, datos_2, datos_3, datos_4)

datos <- datos[datos$grupo != "GPR_1",]

# Ahora, convertimos los datos a los tipos que nos interesan.
datos$grupo <- factor(datos$grupo, ordered = TRUE) # En este caso, los factores deben ir ordenados.
datos$valor <- as.numeric(gsub(",",".", datos$valor))

write_csv(datos, "datos/reales/04a_hepatitis_tidy.csv")

# Comprobamos la parametricidad. Vemos que los datos no son normales ni homocedásticos. En este caso, tenemos 4 'ties' con valor 0, por lo que Kolmogorov no puede usarse. Tampoco podemos usar Shapiro porque hay 70 individuos por grupo. Por ello, se usará Cramer-Von-Mises para este contraste.
comprobar_normalidad_shapiro(datos, "grupo", "valor")
leveneTest(valor ~ grupo, data = datos) # Usamos Levene porque los datos no son normales.

estadisticos_04 <- generar_estadisticos_np(datos, "grupo")
write_csv(estadisticos_04, "datos/reales/estadisticos/04a_estadisticos.csv")

# Título: 04b_simular_datos.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del GPR como predictor de la hepatitis B. Se generan 4 grupos de n = 20, n = 30 y n = 40 usando la función kde.

# Librerías y carga:

library(tidyverse)
library(car)
library(ks)

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

# Creamos un 'tibble' para meter los datos. Usaremos una función ks, se trata de una función de densidad calculada en base a los datos originales y replicada en la simulación.
for (n in c(20, 30, 40)){
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
    
    simulados$grupo <- factor(simulados$grupo, levels = levels(datos_reales$grupo))
    comprobaciones_04(simulados, datos_reales, paste0("datos/simulados/04b_hepatitis_", n, ".csv"), paste0("datos/simulados/estadisticos/04b_hepatitis_", n, ".csv"))
}

# Título: 04c_simular_datos_6g_n20.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del GPR. Se generan dos grupos extra siguiendo los mismos criterios que en el 'script' 01c.

# Librerías y carga:

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

# Título: 04d_simular_datos_desbalanceados.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del GPR como predictor de la hepatitis B. En este caso, los grupos tienen distinto número de datos. Como se trata de una exploración, los grupos se han desbalanceado arbitrariamente.

# Librerías y carga:

library(tidyverse)
library(car)
library(ks)

# Generamos números de datos pseudoaleatorios.
var <- c(20, 25, 30, 15)
names(var) <- c("GPR_0", "GPR_2", "GPR_3", "GPR_4")
n <- c(var["GPR_0"], var["GPR_2"], var["GPR_3"], var["GPR_4"])

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

# Creamos un 'tibble' para meter los datos. Usaremos una función ks, se trata de una función de densidad calculada en base a los datos originales y replicada en la simulación.
simulados <- datos_reales %>%
    group_by(grupo) %>%
    group_modify(~ {
        x <- .x$valor
        g <- .y$grupo
        dens <- kde(x)
        data.frame(
            valor = rkde(dens, n = n[as.character(g)])
        )
    }) %>%
    ungroup()

simulados$grupo <- factor(simulados$grupo, levels = levels(datos_reales$grupo))

# Hacemos las comprobaciones: se mantiene la falta de parametricidad y los estadísticos.
comprobaciones_04(simulados, datos_reales, "datos/simulados/04d_hepatitis_desbalanceados.csv", "datos/simulados/estadisticos/04d_hepatitis_desbalanceados.csv")

# Título: 04e_simular_datos_6g_n40.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del GPR como predictor de la hepatitis B. Se generan dos grupos extra con 40 datos de la misma forma que en 04c.

# Librerías y carga:

library(tidyverse)
library(car)
library(ks)

n <- 40

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

# Simulamos el grupo G5 con la suma de las dos distribuciones de mayor media. Si sumamos las 4, se obtiene una distribución normal, por lo que cogemos GPR_3 y GPR_4, que son las no usadas en G6. De esta forma, obtenemos distribuciones no normales.
x_gpr0 <- simulados %>%
    filter(grupo == "GPR_0") %>%
    pull(valor)

x_gpr2 <- simulados %>%
    filter(grupo == "GPR_2") %>%
    pull(valor)

x_gpr3 <- simulados %>%
    filter(grupo == "GPR_3") %>%
    pull(valor)

x_gpr4 <- simulados %>%
    filter(grupo == "GPR_4") %>%
    pull(valor)

# Sacamos las densidades.
dens_gpr0 <- kde(x_gpr0)
dens_gpr2 <- kde(x_gpr2)
dens_gpr3 <- kde(x_gpr3)
dens_gpr4 <- kde(x_gpr4)

sim_G5 <- tibble(
    valor = (rkde(dens_gpr3, n = n) + rkde(dens_gpr4, n = n)) / 2,
    grupo = "G5"
)

# Generamos G6 como la suma de las distribuciones de GPR_0 y GPR_2 (las de menor media).
sim_G6 <- tibble(
    valor = rkde(dens_gpr0, n = n) + rkde(dens_gpr2, n = n),
    grupo = "G6"
)

# Añadimos G5 y G6 a los simulados.
simulados <- bind_rows(simulados, sim_G5, sim_G6)

# Reajustamos los niveles del factor.
simulados$grupo <- factor(simulados$grupo)

# Hacemos las comprobaciones: se mantiene la falta de parametricidad y los estadísticos.
comprobaciones_04(simulados, datos_reales, "datos/simulados/04e_hepatitis_6g_n40.csv", "datos/simulados/estadisticos/04e_hepatitis_6g_n40.csv")

# Título: 05_informe_simulacion.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' devuelve un informe que muestra los resultados exploratorios de las comparaciones entre estadísticos reales y simulados. El objetivo es comprobar si se han respetado los supuestos.

# Librerías y carga:

library(tidyverse)

# Cargamos los datos reales.
real_1 <- read.csv("datos/reales/estadisticos/01a_estadisticos.csv")
real_2 <- read.csv("datos/reales/02a_brca_tidy.csv")
real_3 <- read.csv("datos/reales/estadisticos/03a_estadisticos.csv")
real_4 <- read.csv("datos/reales/04a_hepatitis_tidy.csv")

real_1$grupo <- as.character(real_1$grupo)
real_2$grupo <- as.character(real_2$grupo)
real_3$grupo <- as.character(real_3$grupo)
real_4$grupo <- as.character(real_4$grupo)

# Tomamos todos los archivos necesarios.
archivos_sim_1 <- list.files("datos/simulados/estadisticos", pattern = "^01.*\\.csv$", full.names = TRUE)
archivos_sim_2 <- list.files("datos/simulados", pattern = "^02.*\\.csv$", full.names = TRUE)
archivos_sim_3 <- list.files("datos/simulados/estadisticos", pattern = "^03.*\\.csv$", full.names = TRUE)
archivos_sim_4 <- list.files("datos/simulados", pattern = "^04.*\\.csv$", full.names = TRUE)

# Para los casos normales, podemos buscar la comparación mediante el cálculo de los errores estándar de la media. Hay que hacer esto con todos los simulados de la ruta especificada.
comparacion_1_normal_homo <- lapply(archivos_sim_1, function(archivo) { # Definimos un bucle.
    sim_1 <- read.csv(archivo) # Los simulados a usar.
    sim_1$grupo <- as.character(sim_1$grupo)
    comparar_estadisticos_reales_simulados(real_1, sim_1, nrow(real_1), nrow(sim_1), "grupo") # Comparamos.
})
names(comparacion_1_normal_homo) <- basename(archivos_sim_1)

# Igual para el caso 03.
comparacion_3_normal_hetero <- lapply(archivos_sim_3, function(archivo) {
    sim_3 <- read.csv(archivo)
    sim_3$grupo <- as.character(sim_3$grupo)
    comparar_estadisticos_reales_simulados(real_3, sim_3, nrow(real_3), nrow(sim_3), "grupo")
})

names(comparacion_3_normal_hetero) <- basename(archivos_sim_3)

# En los casos no-normales, realizaremos una prueba de Mann-Whitney para comprobar la igualdad de distribuciones. Lo haremmos para cada factor por separado.
niveles <- c("Q1785H", "R1753T", "E1794D", "V1804D")

comparacion_2_no_normal_homo <- lapply(archivos_sim_2, function(archivo) {
    sim_2 <- read.csv(archivo)
    sim_2$grupo <- as.character(sim_2$grupo)
    
    do.call(
        rbind,
        lapply(niveles, function(n) {
            test <- wilcox.test(
                real_2$valor[real_2[["grupo"]] == n],
                sim_2$valor[sim_2[["grupo"]] == n],
                exact = FALSE
            )
            data.frame(
                nivel = n,
                W = unname(test$statistic),
                p_value = test$p.value
            )
        })
    )
})
names(comparacion_2_no_normal_homo) <- basename(archivos_sim_2)

# Para el caso 04.
niveles <- c("GPR_0", "GPR_2", "GPR_3", "GPR_4")

comparacion_4_no_normal_hetero <- lapply(archivos_sim_4, function(archivo) {
    sim_4 <- read.csv(archivo)
    sim_4$grupo <- as.character(sim_4$grupo)
    
    do.call(
        rbind,
        lapply(niveles, function(n) {
            test <- wilcox.test(
                real_4$valor[real_4[["grupo"]] == n],
                sim_4$valor[sim_4[["grupo"]] == n],
                exact = FALSE
            )
            data.frame(
                nivel = n,
                W = unname(test$statistic),
                p_value = test$p.value
            )
        })
    )
})
names(comparacion_4_no_normal_hetero) <- basename(archivos_sim_4)

# Mostramos el informe de estadísticos. Quitamos las columnas que no interesan, resultado de la fusión de las tablas.
informe_1_normal_homo <- bind_rows(lapply(comparacion_1_normal_homo, function(x) x[-5]), .id = "archivo")
informe_2_no_normal_homo <- bind_rows(comparacion_2_no_normal_homo, .id = "archivo")
informe_3_normal_hetero <- bind_rows(lapply(comparacion_3_normal_hetero, function(x) x[-5]), .id = "archivo")
informe_4_no_normal_hetero <- bind_rows(comparacion_4_no_normal_hetero, .id = "archivo")

# Ajustamos los p-valores, puesto que se han realizado muchas comparaciones.
informe_1_normal_homo$p_media_ajustado <- p.adjust(informe_1_normal_homo$p_valor_media, method = "holm")
informe_1_normal_homo$p_var_ajustado <- p.adjust(informe_1_normal_homo$p_valor_var, method = "holm")
informe_2_no_normal_homo$p_ajustado <- p.adjust(informe_2_no_normal_homo$p_value, method = "holm")
informe_3_normal_hetero$p_media_ajustado <- p.adjust(informe_3_normal_hetero$p_valor_media, method = "holm")
informe_3_normal_hetero$p_var_ajustado <- p.adjust(informe_3_normal_hetero$p_valor_var, method = "holm")
informe_4_no_normal_hetero$p_ajustado <- p.adjust(informe_4_no_normal_hetero$p_value, method = "holm")

# Se guardan los informes.
write.csv(informe_1_normal_homo, file = "resultados/resultados_exploratorios/informes_comparativos/05_informe_caso1.csv")
write.csv(informe_2_no_normal_homo, file = "resultados/resultados_exploratorios/informes_comparativos/05_informe_caso2.csv")
write.csv(informe_3_normal_hetero, file = "resultados/resultados_exploratorios/informes_comparativos/05_informe_caso3.csv")
write.csv(informe_4_no_normal_hetero, file = "resultados/resultados_exploratorios/informes_comparativos/05_informe_caso4.csv")

# Podemos ver que se conservan los estadísticos y distribuciones.

# Título: 06_exploracion_general.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se pretende hacer un análisis de los datos simulados generados en los 'scripts' 1-4 mediante técnicas de estadística clásica en función de las propiedades de cada caso.

# Librerías y carga:

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
    
    plot_contrastes(caso_2, paste("Contexto no normal homocedástico (", tit, ")"), "dunn", paste0("resultados/resultados_exploratorios/comparaciones_clasica_ad/06_boxplot_02_", tit, ".png"))
    
    plot_contrastes(caso_3, paste("Contexto normal heterocedástico (", tit, ")"), "games-howell", paste0("resultados/resultados_exploratorios/comparaciones_clasica_ad/06_boxplot_03_", tit, ".png"))
    
    plot_contrastes(caso_4, paste("Contexto no normal heterocedástico (", tit, ")"), "dunn", paste0("resultados/resultados_exploratorios/comparaciones_clasica_ad/06_boxplot_04_", tit, ".png"))
}

# Título: 07_arboles_e1.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se pretende generar los árboles de decisión y sus matrices de confusión para los datos generados.

# Librerías y carga:

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

# Título: 08_comparacion_separaciones.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se pretende comparar los resultados para las distintos grupos de datos simulados del proyecto. Es decir, compara, para cada caso y cada grupo, qué se ha separado por estadística clásica y qué no se ha separado. Lo mismo se realiza con los árboles de decisión. Los datos se dejan preparados para el 'script' 09.

# Librerías y carga:


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

# Título: 09_analisis_aciertos.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se analizan los resultados de la comparación de aciertos entre árboles y estadística clásica. Se cuenta el número de aciertos en cada caso.

# Librerías y carga:


library(rstatix)
library(dplyr)
library(ggplot2)
library(car)

# Cargamos los datos.
datos <- list.files(path = "resultados/resultados_exploratorios/tablas_comparativas", pattern = "\\.csv$", full.names = TRUE)

# Ahora, calculamos los aciertos en función del tipo de datos.
for (i in 1:6){
    # Título. Importante diferenciar los casos con 6 grupos y con 20 y 40 datos.
    tit <- sacar_titulo(datos[i])
    
    calcular_aciertos(datos[c(i, 6+i, 12+i, 18+i)], paste0("resultados/resultados_exploratorios/analisis_aciertos/09_aciertos_", tit, "_.csv"))
}

# Análisis 1: Generamos un gráfico de barras de aciertos.
datos_20 <- read.csv("resultados/resultados_exploratorios/analisis_aciertos/09_aciertos_4g_20n_.csv")
datos_30 <- read.csv("resultados/resultados_exploratorios/analisis_aciertos/09_aciertos_4g_30n_.csv")
datos_40 <- read.csv("resultados/resultados_exploratorios/analisis_aciertos/09_aciertos_4g_40n_.csv")

# Sacamos el n_grupo.
datos_20$X <- NULL
datos_20$n_grupo <- 20
datos_30$X <- NULL
datos_30$n_grupo <- 30
datos_40$X <- NULL
datos_40$n_grupo <- 40

datos <- bind_rows(datos_20, datos_30, datos_40)

datos$grupo <- as.factor(datos$grupo)
datos$n_grupo <- as.factor(datos$n_grupo)

p <- ggplot(datos, aes(x = grupo, y = aciertos, fill = n_grupo)) +
    geom_col(width = 0.75, position = "dodge") +
    labs(
        title = "Aciertos por grupo y n",
        x = "",
        y = "Número de aciertos",
    ) +
    theme_minimal()

print(p)

ggsave(filename = "resultados/resultados_exploratorios/analisis_aciertos/09_cambio_n.png", plot = p, width = 10, height = 6, dpi = 300)

# Análisis 2: Miramos si el cambio en k tiene algún impacto. Limpiamos los datos.
datos_6g_20n <- read.csv("resultados/resultados_exploratorios/analisis_aciertos/09_aciertos_6g_20n_.csv")
datos_6g_40n <- read.csv("resultados/resultados_exploratorios/analisis_aciertos/09_aciertos_6g_40n_.csv")

datos <- bind_rows(datos_20, datos_40, datos_6g_20n, datos_6g_40n)

datos$X <- NULL

datos <- datos %>%
    mutate(
        n_grupo = factor(rep(c(20, 40), each = 4, times = 2)),
        k = factor(rep(c(4, 6), each = 8)),
        aciertos = aciertos / (ifelse(k == 6, 15, 6)) # Proporción de aciertos (no aciertos absolutos.)
    )

datos$grupo <- as.factor(datos$grupo)
datos$k <- as.factor(datos$k)

p <- ggplot(datos, aes(x = grupo, y = aciertos, fill = k)) +
    geom_col(width = 0.75, position = "dodge") +
    labs(
        title = "Aciertos por grupo y k",
        x = "",
        y = "Proporción de aciertos",
    ) +
    theme_minimal()

print(p)

ggsave(filename = "resultados/resultados_exploratorios/analisis_aciertos/09_cambio_k.png", plot = p, width = 10, height = 6, dpi = 300)

# Análisis 3: Queremos comprobar si el tamaño del efecto afecta a la probabilidad de acertar.

# Unimos todo en un solo data.frame.
datos <- list.files(path = "resultados/resultados_exploratorios/tablas_comparativas", pattern = "\\.csv$", full.names = TRUE)

tabla <- data.frame()

for (i in datos) {
    i <- read.csv(i)
    tabla <- bind_rows(tabla, i)
}

# Limpiamos la tabla.
tabla_limpia <- tabla %>%
    mutate(
        grupo = factor(rep(c("1", "2", "3", "4"), each = 54)),
        acierto = ifelse(separado_clasica == separado_arbol, 1, 0),
        magnitude = magnitude
    )

tabla_limpia <- tabla_limpia[c("grupo", "acierto", "magnitude")]

# Separamos por grupo.
tabla_1 <- table(tabla_limpia$magnitude[tabla_limpia$grupo == 1], tabla_limpia$acierto[tabla_limpia$grupo == 1])
tabla_2 <- table(tabla_limpia$magnitude[tabla_limpia$grupo == 2], tabla_limpia$acierto[tabla_limpia$grupo == 2])
tabla_3 <- table(tabla_limpia$magnitude[tabla_limpia$grupo == 3], tabla_limpia$acierto[tabla_limpia$grupo == 3])
tabla_4 <- table(tabla_limpia$magnitude[tabla_limpia$grupo == 4], tabla_limpia$acierto[tabla_limpia$grupo == 4])

tablas <- list(tabla_1, tabla_2, tabla_3, tabla_4)
niveles <- c("negligible", "small", "moderate", "large")
scores <- 1:4

# Sacamos el índice rho de Spearman.
resultados <- lapply(tablas, function(tabla) {
    for (n in setdiff(niveles, rownames(tabla))) {
        tabla <- rbind(tabla, setNames(c(0, 0), colnames(tabla)))
        rownames(tabla)[nrow(tabla)] <- n
    }
    tabla <- tabla[niveles, ]
    prop_acierto <- prop.table(tabla, margin = 1)[, "1"]
    
    validos <- !is.na(prop_acierto)
    
    cor.test(scores[validos], prop_acierto[validos], method = "spearman", exact = FALSE)
})

resultados

# Título: 10a_grupos_shannon.R
#
# Autor: Alejandro M.
#
# Descripción: En el 'script' 09, parece que el desbalanceo de los grupos afecta al rendimiento de los árboles en todos los casos. En este 'script' se pretende hacer un nuevo análisis en el que se buscará la probabilidad de acierto del árbol en función de la entropía de Shannon de los grupos. Como se va a intentar un número relativamente alto de intentos, no se representarán árboles con rpart.plot.

# Librerías y carga:

library(tidyverse)
library(rpart)

# Para contar con más margen, partimos de los datos de la etapa 3 con n = 40.
caso_1 <- read.csv("datos/simulados/01b_corazon_40.csv")
caso_2 <- read.csv("datos/simulados/02b_brca_40.csv")
caso_3 <- read.csv("datos/simulados/03b_estres_40.csv")
caso_4 <- read.csv("datos/simulados/04b_hepatitis_40.csv")

# Igualamos los grupos para evitar errores.
caso_1$gravedad <- NULL
caso_1 <- caso_1 %>%
    mutate(
        grupo = ifelse(grupo == "B", "1", ifelse(grupo == "C", "2", ifelse(grupo == "I", "3", "4")))
    )

caso_2$X <- NULL
caso_2 <- caso_2 %>%
    mutate(
        grupo = ifelse(grupo == "E1794D", "1", ifelse(grupo == "Q1785H", "2", ifelse(grupo == "R1753T", "3", "4")))
    )

caso_3$X <- NULL

caso_4$X <- NULL
caso_4 <- caso_4 %>%
    mutate(
        grupo = ifelse(grupo == "GPR_0", "1", ifelse(grupo == "GPR_2", "2", ifelse(grupo == "GPR_3", "3", "4")))
    )

# Ahora, repetimos 500 veces el mismo experimento: tomar valores pseudoaleatorios y hacer la predicción.
resultados <- data.frame(
    H <- numeric(),
    n_aciertos <- numeric(),
    caso <- character()
)


for (i in 1:500){
    vec <- c(sample(5:35, size = 16)) # Decidimos los 4 tamaños muestrales.
    
    # Caso 1.
    n_aciertos <- analizar_arboles_shannon(
        caso_1,
        vec[1:4],
        c("B", "C", "I", "P"),
        "tukey",
        NULL
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "1",
            H = calcular_shannon(vec[1:4]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 2.
    n_aciertos <- analizar_arboles_shannon(
        caso_2,
        vec[5:8],
        c("E1794D", "Q1785H", "R1753T", "V1804D"),
        "dunn",
        c(2, 3, 8)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "2",
            H = calcular_shannon(vec[5:8]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 3.
    n_aciertos <- analizar_arboles_shannon(
        caso_3,
        vec[9:12],
        c("1", "2", "3", "4"),
        "games",
        c(2, 3, 7)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "3",
            H = calcular_shannon(vec[9:12]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 4.
    n_aciertos <- analizar_arboles_shannon(
        caso_4,
        vec[13:16],
        c("GPR_0", "GPR_2", "GPR_3", "GPR_4"),
        "dunn",
        c(2, 3, 8)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "4",
            H = calcular_shannon(vec[13:16]),
            n_aciertos = n_aciertos
        )
    )
}

write_csv(resultados, file = "resultados/resultados_finales/shannon/10a_entropia_shannon.csv")

# Título: 10b_regresion_shannon.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' realizaremos una regresión COM-Poisson para buscar relaciones entre H y la probabilidad de acierto.

# Librerías y carga:


library(nnet)
library(tidyverse)
library(car)
library(lattice)
library(ggplot2)
library(MASS)
library(RVAideMemoire)

datos <- read_csv("resultados/resultados_finales/shannon/10a_entropia_shannon.csv")

# Al tratarse de una variable de conteo, utilizaremos la regresión COM-Poisson porque la media es mucho mayor que la varianza:
mean(datos[datos$caso == 1, ]$H)
mean(datos[datos$caso == 2, ]$H)
mean(datos[datos$caso == 3, ]$H)
mean(datos[datos$caso == 4, ]$H)
var(datos[datos$caso == 1, ]$H)
var(datos[datos$caso == 2, ]$H)
var(datos[datos$caso == 3, ]$H)
var(datos[datos$caso == 4, ]$H)

for (i in 1:4){
    
    datos_corte <- datos %>%
        filter(
            caso == i,
            !is.na(n_aciertos),
            !is.na(H),
            n_aciertos >= 0,
            n_aciertos <= 6
        )
    
    hacer_regresion_binomial(
        datos_regresion = datos_corte,
        max_aciertos = 6,
        titulo_grafico = paste0("Regresión CMP, caso ", i),
        salida = paste0(
            "resultados/resultados_finales/shannon/10b_regresion_binomial_0",
            i,
            ".png"
        )
    )
}

# Hacemos también una regresión global.
datos_global <- datos %>%
    filter(
        !is.na(n_aciertos),
        !is.na(H),
        n_aciertos >= 0,
        n_aciertos <= 6
    )

hacer_regresion_binomial(
    datos_regresion = datos_global,
    max_aciertos = 6,
    titulo_grafico = "Regresión CMP global",
    salida = "resultados/resultados_finales/shannon/10b_regresion_global.png",
    color_caso = TRUE
)

# Estudiamos la correlación.
for (i in 1:4){
    print(spearman.ci(datos[datos$caso == i, ]$H, datos[datos$caso == i, ]$n_aciertos, nrep = 1000, conf.level = 0.95))
}

# Título: 11a_grupos_shannon_6g.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se repetirá lo mismo que en el 10a y 10b, pero con 6 grupos. La Entropía de Shannon máxima depende de k, así que debe normalizarse.

# Librerías y carga:

library(tidyverse)

caso_1 <- read.csv("datos/simulados/01e_corazon_6g_n40.csv")
caso_2 <- read.csv("datos/simulados/02e_brca_6g_n40.csv")
caso_3 <- read.csv("datos/simulados/03e_estres_6g_n40.csv")
caso_4 <- read.csv("datos/simulados/04e_hepatitis_6g_n40.csv")

# Igualamos los grupos para evitar errores.
caso_1$gravedad <- NULL
caso_1 <- caso_1 %>%
    mutate(
        grupo = ifelse(grupo == "B", "1", ifelse(grupo == "C", "2", ifelse(grupo == "I", "3", ifelse(grupo == "P", "4", ifelse(grupo == "G5", "5", "6")))))
    )

caso_2$X <- NULL
caso_2 <- caso_2 %>%
    mutate(
        grupo = ifelse(grupo == "E1794D", "1", ifelse(grupo == "Q1785H", "2", ifelse(grupo == "R1753T", "3", ifelse(grupo == "V1804D", "4", ifelse(grupo == "G5", "5", "6")))))
    )

caso_3$X <- NULL

caso_3 <- caso_3 %>%
    mutate(
        grupo = ifelse(grupo == "G5", "5", ifelse(grupo == "G6", "6", grupo))
    )

caso_4$X <- NULL
caso_4 <- caso_4 %>%
    mutate(
        grupo = ifelse(grupo == "GPR_0", "1", ifelse(grupo == "GPR_2", "2", ifelse(grupo == "GPR_3", "3", ifelse(grupo == "GPR_4", "4", ifelse(grupo == "G5", "5", "6")))))
    )

# Ahora, repetimos 500 veces el mismo experimento: tomar valores pseudoaleatorios y hacer la predicción.
resultados <- data.frame(
    H <- numeric(),
    n_aciertos <- numeric(),
    caso <- character()
)


for (i in 1:500){
    vec <- c(sample(5:35, size = 24)) # Decidimos los 6 tamaños muestrales. Tomamos hasta 20 datos para evitar grandes desbalanceos.
    
    # Caso 1.
    n_aciertos <- analizar_arboles_shannon(
        caso_1,
        vec[1:6],
        c("B", "C", "I", "P", "G5", "G6"),
        "tukey",
        NULL
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "1",
            H = calcular_shannon(vec[1:6]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 2.
    n_aciertos <- analizar_arboles_shannon(
        caso_2,
        vec[7:12],
        c("E1794D", "Q1785H", "R1753T", "V1804D", "G5", "G6"),
        "dunn",
        c(2, 3, 8)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "2",
            H = calcular_shannon(vec[7:12]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 3.
    n_aciertos <- analizar_arboles_shannon(
        caso_3,
        vec[13:18],
        c("1", "2", "3", "4", "G5", "G6"),
        "games",
        c(2, 3, 7)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "3",
            H = calcular_shannon(vec[13:18]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 4.
    n_aciertos <- analizar_arboles_shannon(
        caso_4,
        vec[19:24],
        c("GPR_0", "GPR_2", "GPR_3", "GPR_4", "G5", "G6"),
        "dunn",
        c(2, 3, 8)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "4",
            H = calcular_shannon(vec[19:24]),
            n_aciertos = n_aciertos
        )
    )
}

write_csv(resultados, file = "resultados/resultados_finales/shannon/11a_entropia_shannon_6g.csv")

# Título: 11b_regresion_shannon_6g.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' realizaremos una regresión para buscar relaciones entre H y la probabilidad de acierto, esta vez con 6 grupos.

# Librerías y carga:


library(nnet)
library(tidyverse)
library(car)
library(lattice)
library(ggplot2)
library(MASS)

datos <- read_csv("resultados/resultados_finales/shannon/11a_entropia_shannon_6g.csv")

for (i in 1:4){
    
    datos_corte <- datos %>%
        filter(
            caso == i,
            !is.na(n_aciertos),
            !is.na(H),
            n_aciertos >= 0,
            n_aciertos <= 15
        )
    
    hacer_regresion_binomial(
        datos_regresion = datos_corte,
        max_aciertos = 15,
        titulo_grafico = paste0("Regresión CMP, caso ", i),
        salida = paste0(
            "resultados/resultados_finales/shannon/11b_regresion_binomial_6grupos_0",
            i,
            ".png"
        )
    )
}

# Hacemos también una regresión global.
datos_global <- datos %>%
    filter(
        !is.na(n_aciertos),
        !is.na(H),
        n_aciertos >= 0,
        n_aciertos <= 15
    )

hacer_regresion_binomial(
    datos_regresion = datos_global,
    max_aciertos = 15,
    titulo_grafico = "Regresión CMP global",
    salida = "resultados/resultados_finales/shannon/11b_regresion_global_6grupos.png",
    color_caso = TRUE
)

# Estudiamos la correlación.
for (i in 1:4){
    print(spearman.ci(datos[datos$caso == i, ]$H, datos[datos$caso == i, ]$n_aciertos, nrep = 1000, conf.level = 0.95))
}

# Título: 12a_grupos_shannon_5g.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se repetirá lo mismo que en el 10a y 10b, pero con 5 grupos. La Entropía de Shannon depende de k, así que debe normalizarse.

# Librerías y carga:

library(tidyverse)

caso_1 <- read.csv("datos/simulados/01e_corazon_6g_n40.csv")
caso_2 <- read.csv("datos/simulados/02e_brca_6g_n40.csv")
caso_3 <- read.csv("datos/simulados/03e_estres_6g_n40.csv")
caso_4 <- read.csv("datos/simulados/04e_hepatitis_6g_n40.csv")

# Igualamos los grupos para evitar errores.
caso_1$gravedad <- NULL
caso_1 <- caso_1 %>%
    mutate(
        grupo = ifelse(grupo == "B", "1", ifelse(grupo == "C", "2", ifelse(grupo == "I", "3", ifelse(grupo == "P", "4", "5"))))
    )

caso_2$X <- NULL
caso_2 <- caso_2 %>%
    mutate(
        grupo = ifelse(grupo == "E1794D", "1", ifelse(grupo == "Q1785H", "2", ifelse(grupo == "R1753T", "3", ifelse(grupo == "V1804D", "4", "5"))))
    )

caso_3$X <- NULL

caso_3 <- caso_3 %>%
    mutate(
        grupo = ifelse(grupo == "G5", "5", grupo)
    )

caso_4$X <- NULL
caso_4 <- caso_4 %>%
    mutate(
        grupo = ifelse(grupo == "GPR_0", "1", ifelse(grupo == "GPR_2", "2", ifelse(grupo == "GPR_3", "3", ifelse(grupo == "GPR_4", "4", "5"))))
    )

# Ahora, repetimos 500 veces el mismo experimento: tomar valores pseudoaleatorios y hacer la predicción.
resultados <- data.frame(
    H <- numeric(),
    n_aciertos <- numeric(),
    caso <- character()
)


for (i in 1:500){
    vec <- c(sample(5:35, size = 20)) # Decidimos los 6 tamaños muestrales. Tomamos hasta 20 datos para evitar grandes desbalanceos.
    
    # Caso 1.
    n_aciertos <- analizar_arboles_shannon(
        caso_1,
        vec[1:5],
        c("B", "C", "I", "P", "G5"),
        "tukey",
        NULL
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "1",
            H = calcular_shannon(vec[1:5]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 2.
    n_aciertos <- analizar_arboles_shannon(
        caso_2,
        vec[6:10],
        c("E1794D", "Q1785H", "R1753T", "V1804D", "G5"),
        "dunn",
        c(2, 3, 8)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "2",
            H = calcular_shannon(vec[6:10]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 3.
    n_aciertos <- analizar_arboles_shannon(
        caso_3,
        vec[11:15],
        c("1", "2", "3", "4", "G5"),
        "games",
        c(2, 3, 7)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "3",
            H = calcular_shannon(vec[11:15]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 4.
    n_aciertos <- analizar_arboles_shannon(
        caso_4,
        vec[16:20],
        c("GPR_0", "GPR_2", "GPR_3", "GPR_4", "G5"),
        "dunn",
        c(2, 3, 8)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "4",
            H = calcular_shannon(vec[16:20]),
            n_aciertos = n_aciertos
        )
    )
}

write_csv(resultados, file = "resultados/resultados_finales/shannon/12a_entropia_shannon_5g.csv")

# Título: 12b_regresion_shannon_5g.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' realizaremos una regresión para buscar relaciones entre H y la probabilidad de acierto, esta vez con 5 grupos.

# Librerías y carga:


library(nnet)
library(tidyverse)
library(car)
library(lattice)
library(ggplot2)
library(MASS)

datos <- read_csv("resultados/resultados_finales/shannon/12a_entropia_shannon_5g.csv")

for (i in 1:4){
    
    datos_corte <- datos %>%
        filter(
            caso == i,
            !is.na(n_aciertos),
            !is.na(H),
            n_aciertos >= 0,
            n_aciertos <= 10
        )
    
    hacer_regresion_binomial(
        datos_regresion = datos_corte,
        max_aciertos = 10,
        titulo_grafico = paste0("Regresión CMP, caso ", i),
        salida = paste0(
            "resultados/resultados_finales/shannon/12b_regresion_binomial_5grupos_0",
            i,
            ".png"
        )
    )
}

# Hacemos también una regresión global.
datos_global <- datos %>%
    filter(
        !is.na(n_aciertos),
        !is.na(H),
        n_aciertos >= 0,
        n_aciertos <= 10
    )

hacer_regresion_binomial(
    datos_regresion = datos_global,
    max_aciertos = 10,
    titulo_grafico = "Regresión CMP global",
    salida = "resultados/resultados_finales/shannon/12b_regresion_global_5grupos.png",
    color_caso = TRUE
)

# Estudiamos la correlación.
for (i in 1:4){
    print(spearman.ci(datos[datos$caso == i, ]$H, datos[datos$caso == i, ]$n_aciertos, nrep = 1000, conf.level = 0.95))
}

# Título: 13a_grupos_shannon.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se repetirá lo mismo que en el 10a y 10b, pero con 3 grupos. La Entropía de Shannon depende de k, así que debe normalizarse. Eliminamos uno de los dos grupos más similares de cada caso.

# Librerías y carga:

library(tidyverse)
library(rpart)

# Para contar con más margen, partimos de los datos de la etapa 3 con n = 40.
caso_1 <- read.csv("datos/simulados/01b_corazon_40.csv")
caso_2 <- read.csv("datos/simulados/02b_brca_40.csv")
caso_3 <- read.csv("datos/simulados/03b_estres_40.csv")
caso_4 <- read.csv("datos/simulados/04b_hepatitis_40.csv")

# Igualamos los grupos para evitar errores.
caso_1$gravedad <- NULL
caso_1 <- caso_1 %>%
    mutate(
        grupo = ifelse(grupo == "B", "1", ifelse(grupo == "C", "2", ifelse(grupo == "I", "3", "4")))
    )

caso_1 <- subset(caso_1, grupo != "2")

caso_2$X <- NULL
caso_2 <- caso_2 %>%
    mutate(
        grupo = ifelse(grupo == "E1794D", "1", ifelse(grupo == "Q1785H", "2", ifelse(grupo == "R1753T", "3", "4")))
    )

caso_2 <- subset(caso_2, grupo != "2")

caso_3$X <- NULL

caso_3 <- subset(caso_3, grupo != "3")

caso_4$X <- NULL
caso_4 <- caso_4 %>%
    mutate(
        grupo = ifelse(grupo == "GPR_0", "1", ifelse(grupo == "GPR_2", "2", ifelse(grupo == "GPR_3", "3", "4")))
    )

caso_4 <- subset(caso_4, grupo != "2")

# Ahora, repetimos 500 veces el mismo experimento: tomar valores pseudoaleatorios y hacer la predicción.
resultados <- data.frame(
    H <- numeric(),
    n_aciertos <- numeric(),
    caso <- character()
)


for (i in 1:500){
    vec <- c(sample(5:35, size = 12)) # Decidimos los 4 tamaños muestrales.
    
    # Caso 1.
    n_aciertos <- analizar_arboles_shannon(
        caso_1,
        vec[1:3],
        c("B", "I", "P"),
        "tukey",
        NULL
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "1",
            H = calcular_shannon(vec[1:3]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 2.
    n_aciertos <- analizar_arboles_shannon(
        caso_2,
        vec[4:6],
        c("E1794D", "R1753T", "V1804D"),
        "dunn",
        c(2, 3, 8)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "2",
            H = calcular_shannon(vec[4:6]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 3.
    n_aciertos <- analizar_arboles_shannon(
        caso_3,
        vec[7:9],
        c("1", "2", "4"),
        "games",
        c(2, 3, 7)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "3",
            H = calcular_shannon(vec[7:9]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 4.
    n_aciertos <- analizar_arboles_shannon(
        caso_4,
        vec[10:12],
        c("GPR_0", "GPR_3", "GPR_4"),
        "dunn",
        c(2, 3, 8)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "4",
            H = calcular_shannon(vec[10:12]),
            n_aciertos = n_aciertos
        )
    )
}

write_csv(resultados, file = "resultados/resultados_finales/shannon/13a_entropia_shannon_3g.csv")

# Título: 13b_regresion_shannon.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' realizaremos una regresión para buscar relaciones entre H y la probabilidad de acierto.

# Librerías y carga:


library(nnet)
library(tidyverse)
library(car)
library(lattice)
library(ggplot2)
library(MASS)

datos <- read_csv("resultados/resultados_finales/shannon/13a_entropia_shannon_3g.csv")

for (i in 1:4){
    
    datos_corte <- datos %>%
        filter(
            caso == i,
            !is.na(n_aciertos),
            !is.na(H),
            n_aciertos >= 0,
            n_aciertos <= 3
        )
    
    hacer_regresion_binomial(
        datos_regresion = datos_corte,
        max_aciertos = 3,
        titulo_grafico = paste0("Regresión CMP, caso ", i),
        salida = paste0(
            "resultados/resultados_finales/shannon/13b_regresion_binomial_3grupos_0",
            i,
            ".png"
        )
    )
}

# Hacemos también una regresión global.
datos_global <- datos %>%
    filter(
        !is.na(n_aciertos),
        !is.na(H),
        n_aciertos >= 0,
        n_aciertos <= 3
    )

hacer_regresion_binomial(
    datos_regresion = datos_global,
    max_aciertos = 3,
    titulo_grafico = "Regresión CMP global",
    salida = "resultados/resultados_finales/shannon/13b_regresion_global_3grupos.png",
    color_caso = TRUE
)

# Estudiamos la correlación.
for (i in 1:4){
    print(spearman.ci(datos[datos$caso == i, ]$H, datos[datos$caso == i, ]$n_aciertos, nrep = 1000, conf.level = 0.95))
}

# Título: 14_analisis_repeticiones.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es repetir muchas veces la simulación de datos, comparación de la separación por estadística clásica y árboles y recuento de aciertos para buscar tendencias estadísticas ('scripts' 1-4, 8 y 9). Para ello, tomamos los 60 datos generados por los apartados c y e de los primeros 4 'scripts' y los combinamos pseudoaleatoriamente para poder extraer muestras de distintos subgrupos de datos.

#Librerías y carga:

library(tidyverse)
library(purrr)
library(rstatix)

# Cargamos los datos.
archivos_1 <- list.files("datos/simulados", pattern = "^01[c-e]_corazon_6g_n[0-9][0-9].csv$", full.names = TRUE)
datos_1 <- map_dfr(archivos_1, read_csv)

archivos_2 <- list.files("datos/simulados", pattern = "^02[c-e]_brca_6g_n[0-9][0-9].csv$", full.names = TRUE)
datos_2 <- map_dfr(archivos_2, read_csv)

archivos_3 <- list.files("datos/simulados", pattern = "^03[c-e]_estres_6g_n[0-9][0-9].csv$", full.names = TRUE)
datos_3 <- map_dfr(archivos_3, read_csv)

archivos_4 <- list.files("datos/simulados", pattern = "^04[c-e]_hepatitis_6g_n[0-9][0-9].csv$", full.names = TRUE)
datos_4 <- map_dfr(archivos_4, read_csv)

# Limpiamos.
datos_1$gravedad <- NULL
datos_1$grupo <- factor(datos_1$grupo)
datos_2$grupo <- factor(datos_2$grupo)
datos_3$grupo <- factor(datos_3$grupo)
datos_4$grupo <- factor(datos_4$grupo)

for (i in 1:500){
    contador <- 1 # Para adaptar el bucle.
    # Sacamos las subdivisiones de datos.
    for (n in c(20, 30, 40)){
        for (caso in list(datos_1, datos_2, datos_3, datos_4)){
            datos_corte <- caso %>%
                group_by(grupo) %>%
                slice_sample(n = n)
            
            # La función de comparación da error si se le pasa un tibble como argumento.
            datos_corte <- as.data.frame(datos_corte)
            
            archivo <- paste0("resultados/resultados_finales/repeticiones/14_comparacion_cla_arb_0", contador, "_n", n, ".csv")
            
            if (contador == 1) {
                
                # Separación clásica.
                clasica <- as.data.frame(TukeyHSD(aov(valor ~ grupo, data = datos_corte))$grupo)
                
                # Identificamos los grupos separados.
                clasica$separado <- ifelse(clasica$`p adj` < 0.05, TRUE, FALSE)
                clasica$grupo <- rownames(clasica)
                clasica <- clasica[, c("grupo", "separado")]
                
                # Aplicamos la comparación y guardamos.
                escribir_resultado(datos_corte, clasica, archivo, homocedastico = TRUE)
                contador <- contador + 1
            }
            
            else if (contador == 2 | contador == 4) {
                
                # Separación clásica.
                clasica <- as.data.frame(dunn_test(datos_corte, valor ~ grupo))[c(2, 3, 8)]
                
                # Pegamos los nombres de los grupos.
                clasica$grupo <- paste0(clasica$group1, "-", clasica$group2)
                rownames(clasica) <- clasica$grupo
                
                clasica$separado <- ifelse(clasica$p.adj < 0.05, TRUE, FALSE)
                clasica <- clasica[, c("grupo", "separado")]
                
                # Aplicamos la comparación y guardamos.
                escribir_resultado(datos_corte, clasica, archivo, homocedastico = FALSE)
                
                if (contador == 4) {
                    contador <- 1
                }
                else {
                    contador <- contador + 1
                }
            }
            
            else { # Caso 3.
                clasica <- as.data.frame(games_howell_test(datos_corte, valor ~ grupo))[c(2, 3, 7)]
                
                # Pegamos los nombres de los grupos.
                clasica$grupo <- paste0(clasica$group1, "-", clasica$group2)
                rownames(clasica) <- clasica$grupo
                
                clasica$separado <- ifelse(clasica$p.adj < 0.05, TRUE, FALSE)
                clasica <- clasica[, c("grupo", "separado")]
                
                # Aplicamos la comparación y guardamos.
                escribir_resultado(datos_corte, clasica, archivo, homocedastico = TRUE)
                
                contador <- contador + 1
            }
        }
    }
}

# Ahora, calculamos los aciertos en función del tipo de datos.
datos <- list.files(path = "resultados/resultados_finales/repeticiones/", pattern = "^14_comparacion", full.names = TRUE)

# Ahora, calculamos los aciertos en función del tipo de datos.
for (i in 1:3){ # Este bucle, por n = 20, n = 30 y n = 40.
    for (k in 4:6){ # Este bucle itera por k.
        calcular_aciertos(datos[c(i, 3+i, 6+i, 9+i)], paste0("resultados/resultados_finales/repeticiones/14_aciertos_n", (1+i)*10, "_g", k, ".csv"), n_grupo = k)
    }
}

# Repetimos el mismo proceso que en 'script' 09.
datos_20_4 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n20_g4.csv")
datos_20_5 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n20_g5.csv")
datos_20_6 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n20_g6.csv")
datos_30_4 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n30_g4.csv")
datos_30_5 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n30_g5.csv")
datos_30_6 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n30_g6.csv")
datos_40_4 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n40_g4.csv")
datos_40_5 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n40_g5.csv")
datos_40_6 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n40_g6.csv")

# Sacamos el n_grupo.
datos_20_4$X <- NULL
datos_20_4$n_grupo <- 20
datos_30_4$X <- NULL
datos_30_4$n_grupo <- 30
datos_40_4$X <- NULL
datos_40_4$n_grupo <- 40
datos_20_5$X <- NULL
datos_20_5$n_grupo <- 20
datos_30_5$X <- NULL
datos_30_5$n_grupo <- 30
datos_40_5$X <- NULL
datos_40_5$n_grupo <- 40
datos_20_6$X <- NULL
datos_20_6$n_grupo <- 20
datos_30_6$X <- NULL
datos_30_6$n_grupo <- 30
datos_40_6$X <- NULL
datos_40_6$n_grupo <- 40

datos_4 <- bind_rows(datos_20_4, datos_30_4, datos_40_4)
datos_5 <- bind_rows(datos_20_5, datos_30_5, datos_40_5)
datos_6 <- bind_rows(datos_20_6, datos_30_6, datos_40_6)

datos_4$grupo <- as.factor(datos_4$grupo)
datos_4$n_grupo <- as.factor(datos_4$n_grupo)

datos_5$grupo <- as.factor(datos_5$grupo)
datos_5$n_grupo <- as.factor(datos_5$n_grupo)

datos_6$grupo <- as.factor(datos_6$grupo)
datos_6$n_grupo <- as.factor(datos_6$n_grupo)

p <- ggplot(datos_4, aes(x = grupo, y = aciertos, fill = n_grupo)) +
    geom_col(width = 0.75, position = "dodge") +
    labs(
        title = "Aciertos por grupo y n (k = 4)",
        x = "",
        y = "Número de aciertos",
    ) +
    theme_minimal()

print(p)

ggsave(filename = "resultados/resultados_finales/repeticiones/14_cambio_n_4k.png", plot = p, width = 10, height = 6, dpi = 300)

p <- ggplot(datos_5, aes(x = grupo, y = aciertos, fill = n_grupo)) +
    geom_col(width = 0.75, position = "dodge") +
    labs(
        title = "Aciertos por grupo y n (k = 5)",
        x = "",
        y = "Número de aciertos",
    ) +
    theme_minimal()

print(p)

ggsave(filename = "resultados/resultados_finales/repeticiones/14_cambio_n_5k.png", plot = p, width = 10, height = 6, dpi = 300)

p <- ggplot(datos_6, aes(x = grupo, y = aciertos, fill = n_grupo)) +
    geom_col(width = 0.75, position = "dodge") +
    labs(
        title = "Aciertos por grupo y n (k = 6)",
        x = "",
        y = "Número de aciertos",
    ) +
    theme_minimal()

print(p)

ggsave(filename = "resultados/resultados_finales/repeticiones/14_cambio_n_6k.png", plot = p, width = 10, height = 6, dpi = 300)

# Título: 15_analisis_adicionales.R
#
# Autor: Alejandro M.
#
# Descripción: Este 'script' tiene como objetivo realizar pruebas sobre los resultados del proyecto para su inclusión en la memoria con rigor estadístico.

## Librerías y carga:

library(stats)
library(rstatix)
library(tidyverse)
library(car)

# Sacamos los datos.
datos_20_4 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n20_g4.csv")
datos_20_5 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n20_g5.csv")
datos_20_6 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n20_g6.csv")
datos_30_4 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n30_g4.csv")
datos_30_5 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n30_g5.csv")
datos_30_6 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n30_g6.csv")
datos_40_4 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n40_g4.csv")
datos_40_5 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n40_g5.csv")
datos_40_6 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n40_g6.csv")

# Sacamos la tabla.
tabla <- rbind(datos_20_4, datos_30_4, datos_40_4, datos_20_5, datos_30_5, datos_40_5, datos_20_6, datos_30_6, datos_40_6)

tabla$aciertos <- as.numeric(tabla$aciertos)

# Obtenemos las probabilidades.
tabla[1:12,]$aciertos <- tabla[1:12,]$aciertos/3000 # Máximos aciertos para k = 4.
tabla[13:24,]$aciertos <- tabla[13:24,]$aciertos/5000 # Máximos aciertos para k = 5.
tabla[25:36,]$aciertos <- tabla[25:36,]$aciertos/7500 # Máximos aciertos para k = 6.

# Análisis 1: n, k y la naturaleza de los grupos afectan a la probabilidad de acierto: ANOVA de tres vías + 'post hoc'.

# Comprobamos supuestos.
shapiro.test(tabla[tabla$grupo == 1,]$aciertos)
shapiro.test(tabla[tabla$grupo == 2,]$aciertos)
shapiro.test(tabla[tabla$grupo == 3,]$aciertos)
shapiro.test(tabla[tabla$grupo == 4,]$aciertos)

bartlett.test(aciertos ~ grupo, data = tabla)

# Las variables son normales, pero heterocedásticas: corrección HC3.

# Modificamos la tabla.
tabla_1 <- tabla

tabla_1$n[1:12] <- 20
tabla_1$n[13:24] <- 30
tabla_1$n[25:36] <- 40

tabla_1$k <- rep(c(4, 5, 6), times = 3, each = 4)

tabla_1$grupo <- factor(tabla_1$grupo)

modelo <- lm(aciertos ~ n * k * grupo, data = tabla_1)

Anova(modelo, white.adjust = "hc3")

# 'Post hoc' para n:grupo.
tabla_1$ngrupo <- interaction(tabla_1$n, tabla_1$grupo)

print(games_howell_test(tabla_1, aciertos ~ ngrupo), n = 66)

# 'Post hoc' para k:grupo.
tabla_1$kgrupo <- interaction(tabla_1$k, tabla_1$grupo)

print(games_howell_test(tabla_1, aciertos ~ kgrupo), n = 66)

# Guardamos la información.
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")

final <- Sys.time()

tiempo <- final - inicio

tiempo