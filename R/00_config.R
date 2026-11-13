# =============================================================================
# 00_config.R  ·  Parámetros del proyecto (lo único que deberías editar)
# Parcial 3 · Análisis Espacial · Saber 11 en municipios de Antioquia
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(spdep)
  library(spatialreg)
  library(dplyr)
  library(ggplot2)
  library(classInt)
})

set.seed(2026)                 # reproducibilidad de las permutaciones

# --- Modo de prueba ----------------------------------------------------------
# TRUE  = corre todo con un mapa de ejemplo que trae R (condados de Carolina
#         del Norte) solo para comprobar que el código funciona.
# FALSE = usa tus datos reales (TerriData + MGN del DANE).
MODO_PRUEBA <- FALSE

# --- Territorio --------------------------------------------------------------
COD_DPTO    <- "05"            # 05 = Antioquia (15 = Boyacá, 25 = Cundinamarca)
NOMBRE_TERR <- "Antioquia"
CRS_PROY    <- 9377            # MAGNA-SIRGAS / Origen-Nacional (metros)

# --- Archivos de entrada -----------------------------------------------------
# 1) TerriData (DNP): descarga por dimensión/indicadores para Antioquia
#    (Excel o CSV). Se pueden poner varios archivos.
ARCHIVOS_TERRIDATA <- list.files("datos/crudos", pattern = "(?i)terridata.*\\.(xlsx|csv)$",
                                 full.names = TRUE)
# 2) MGN del DANE: shapefile o gpkg de municipios (MGN_MPIO_POLITICO)
ARCHIVO_MGN <- list.files("datos/crudos", pattern = "(?i)mpio.*\\.(shp|gpkg)$",
                          full.names = TRUE, recursive = TRUE)[1]

# --- Indicadores (nombre EXACTO como aparece en la columna "Indicador") ------
# Corre primero  source("R/01_preparar_base.R"); explorar_indicadores("saber")
# para ver los nombres disponibles y cópialos aquí.
IND_Y  <- "Puntaje promedio Pruebas Saber 11 - Matemáticas"
IND_X1 <- "Índice de Pobreza Multidimensional (IPM)"
IND_X2 <- "Porcentaje de población en zona rural"    # o índice de ruralidad
ANIO_Y  <- 2018                # mismo año que el censo (IPM 2018)
ANIO_X1 <- 2018
ANIO_X2 <- 2018

# Etiquetas para mapas y tablas (con unidades)
ETQ_Y  <- "Puntaje Saber 11 Matemáticas (puntos, 0-100)"
ETQ_X1 <- "IPM (% de personas pobres)"
ETQ_X2 <- "Población rural (%)"

# --- Análisis ----------------------------------------------------------------
ALFA       <- 0.05             # nivel de significancia
NSIM       <- 999              # permutaciones en pruebas locales
K_VECINOS  <- 5                # W2: k vecinos más cercanos
FAMILIA_MAPAS <- "SEM"         # familia espacial para los 4 mapas de residuos

# --- Carpetas de salida ------------------------------------------------------
DIR_TAB  <- "resultados/tablas"
DIR_MAP  <- "resultados/mapas"
dir.create(DIR_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_MAP, recursive = TRUE, showWarnings = FALSE)

guardar_tabla <- function(df, nombre) {
  write.csv(df, file.path(DIR_TAB, paste0(nombre, ".csv")), row.names = FALSE,
            fileEncoding = "UTF-8")
  invisible(df)
}
guardar_mapa <- function(p, nombre, w = 8, h = 7) {
  ggsave(file.path(DIR_MAP, paste0(nombre, ".png")), p, width = w, height = h, dpi = 150)
  invisible(p)
}
FUENTE_MAPA <- if (MODO_PRUEBA) "Fuente: datos de ejemplo (sf::nc) - MODO PRUEBA" else
  "Fuentes: DNP TerriData; DANE MGN. Elaboración propia."
