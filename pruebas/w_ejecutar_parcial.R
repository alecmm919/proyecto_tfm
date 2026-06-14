# Título: w_ejecutar_parcial.R
#
# Autor: Alejandro M.
#
# Descripción: Este 'script' ejecuta, en orden, los 'scripts' que se necesiten para las pruebas.

scripts <- list.files(path = "scripts/", pattern = "^(10|11|12)", full.names = TRUE)

for (s in scripts) {
    message(s)
    source(s)
}