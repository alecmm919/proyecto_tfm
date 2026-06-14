# Título: 00_funciones.R
#
# Autor: Alejandro M.
#
# Descripción: Este 'script' únicamente contendrá funciones definidas por el usuario para ser usadas en otros 'scripts'. Se cargará este 'script' al inicio de todos y cada uno de los demás para garantizar reproducibilidad independientemente del orden de ejecución.

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

analizar_arboles_shannon <- function(datos_base, muestras_vec, nombres, metodo_clasico, columnas){ # Con esta función, analizamos los datos en función de su entropía de Shannon.
    
    datos <- dplyr::bind_rows(
        lapply(
            split(datos_base, datos_base$grupo),
            function(x) {
                g <- unique(x$grupo)
                n <- muestras_vec[as.integer(g)]
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

sacar_titulo <- function(cadena){ # Saca el título para poder ser utilizado en un gráfico. Si es el grupo de desbalanceados, se queda con ese título.
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

hacer_regresion_binomial <- function(datos_regresion, max_aciertos, titulo_grafico, salida, color_caso = FALSE){ # Saca una regresión binomial para Shannon, la imprime en el visor y la guarda. Nótese que no se normaliza el número de aciertos por ser rigurosos con el objetivo de esta regresión.
    
    modelo <- glm(cbind(n_aciertos, max_aciertos - n_aciertos) ~ H, data = datos_regresion, family = binomial(link = "logit")
    )
    
    print(summary(modelo))
    
    nuevo <- data.frame(
        H = seq(min(datos_regresion$H), max(datos_regresion$H), length.out = 100)
    )
    
    pred <- predict(modelo, newdata = nuevo, type = "link", se.fit = TRUE)
    
    pp_H <- data.frame( # Saca las probabilidades.
        H = nuevo$H,
        probabilidad = plogis(pred$fit),
        probabilidad_li = plogis(pred$fit - 1.96 * pred$se.fit),
        probabilidad_ls = plogis(pred$fit + 1.96 * pred$se.fit)
    ) %>%
        dplyr::mutate(
            aciertos_esperados = probabilidad * max_aciertos,
            aciertos_li = probabilidad_li * max_aciertos,
            aciertos_ls = probabilidad_ls * max_aciertos
        )
    
    if (color_caso){ # Para separar por caso en la global.
        
        grafico <- ggplot2::ggplot(datos_regresion, ggplot2::aes(x = H, y = n_aciertos)) +
            ggplot2::geom_jitter(ggplot2::aes(color = factor(caso)), width = 0, height = 0.12, alpha = 0.35)
        
    } else {
        
        grafico <- ggplot2::ggplot(datos_regresion, ggplot2::aes(x = H, y = n_aciertos)) +
            ggplot2::geom_jitter(width = 0, height = 0.12, alpha = 0.35)
    }
    
    grafico <- grafico +
        ggplot2::geom_ribbon(data = pp_H, ggplot2::aes(x = H, ymin = aciertos_li, ymax = aciertos_ls), inherit.aes = FALSE, alpha = 0.2, fill = "steelblue") +
        ggplot2::geom_line(data = pp_H, ggplot2::aes(x = H, y = aciertos_esperados), inherit.aes = FALSE, color = "steelblue", linewidth = 1.2)
    
    if (color_caso){
        
        grafico <- grafico +
            ggplot2::coord_cartesian(ylim = c(0, max_aciertos)) +
            ggplot2::labs(title = titulo_grafico, x = "H", y = "Número de aciertos", color = "Caso") +
            ggplot2::theme_minimal()
        
    } else {
        
        grafico <- grafico +
            ggplot2::coord_cartesian(ylim = c(0, max_aciertos)) +
            ggplot2::labs(title = titulo_grafico, x = "H", y = "Número de aciertos") +
            ggplot2::theme_minimal()
    }
    
    print(grafico)
    
    ggplot2::ggsave(filename = salida, plot = grafico, width = 8, height = 6, dpi = 300)
}

escribir_resultado <- function(datos_corte, clasica, archivo, homocedastico) { # Compara árboles y estadística clásica pero va guardando los resultados de las iteraciones.
    temporal <- tempfile(fileext = ".csv")
    comparar_clasica_arbol_tamano(datos_corte, clasica, temporal, homocedastico = homocedastico)
    
    resultado <- read_csv(temporal, show_col_types = FALSE)
    
    write_csv(resultado, archivo, append = file.exists(archivo), col_names = !file.exists(archivo))
}