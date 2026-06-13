# Título: 99_correr_todo.R
#
# Autor: Alejandro M.
#
# Descripción: Este 'script' ejecuta, en orden, todo el proyecto. Además, genera el archivo de 'sessionInfo.txt' con toda la información relacionada con los paquetes. Nótese que este archivo solo se actualiza cuando se ejecuta este 'script'.

scripts <- list.files(path = "scripts", pattern = "\\.R$", full.names = TRUE) # Cogemos todos los 'scripts'.

scripts <- scripts[!grepl("/(00_funciones.R|99_correr_todo.R)", scripts)] # Quitamos el 00 y el 99.

for (s in scripts) { # Corre todos los 'scripts' con un bucle.
    message("Ejecutando: ", s)
    source(s)
}

# Guardamos la información.
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")