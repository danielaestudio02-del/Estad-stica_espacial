# Ejecuta todo el análisis en orden. Abrir el proyecto (.Rproj) y correr:
#   source("run_all.R")
scripts <- c("R/01_preparar_base.R", "R/02_matrices.R", "R/03_mapas_iniciales.R",
             "R/04_global.R", "R/05_local.R", "R/06_regresiones.R", "R/07_robustez.R")
for (s in scripts) {
  cat("\n=====================", s, "=====================\n")
  source(s, echo = FALSE, encoding = "UTF-8")
}
cat("\nListo. Tablas en resultados/tablas y mapas en resultados/mapas\n")
