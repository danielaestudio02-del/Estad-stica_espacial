# =============================================================================
# 01_preparar_base.R  ·  Base analítica: una fila = un municipio
# Entradas : TerriData (DNP) + MGN municipios (DANE)
# Salida   : datos/base_analitica.gpkg  (cod, nombre, y, x1, x2, geometría)
#            resultados/tablas/control_calidad.csv
# =============================================================================
source("R/00_config.R")

# --- Utilidades ---------------------------------------------------------------
limpiar_nombres <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  gsub("^_|_$", "", x)
}
a_numero <- function(x) {
  if (is.numeric(x)) return(x)
  x <- gsub("\\s", "", as.character(x))
  # "1.234,5" -> 1234.5 ; "45,3" -> 45.3 ; "45.3" -> 45.3
  x <- ifelse(grepl(",", x), gsub(",", ".", gsub("\\.", "", x)), x)
  suppressWarnings(as.numeric(x))
}

leer_terridata <- function(archivos) {
  if (length(archivos) == 0)
    stop("No encontré archivos de TerriData en datos/crudos (nombre debe contener 'terridata').")
  td <- lapply(archivos, function(f) {
    d <- if (grepl("\\.xlsx$", f, ignore.case = TRUE)) readxl::read_excel(f) else
      read.csv(f, sep = ifelse(grepl(";", readLines(f, n = 1)), ";", ","),
               encoding = "UTF-8", check.names = FALSE)
    names(d) <- limpiar_nombres(names(d))
    d
  })
  td <- bind_rows(td)
  # columnas esperadas en TerriData (los nombres pueden variar levemente)
  col <- function(patron) {
    hit <- grep(patron, names(td), value = TRUE)[1]
    if (is.na(hit)) stop("No encontré columna con patrón '", patron, "'. Columnas: ",
                         paste(names(td), collapse = ", "))
    hit
  }
  tibble(
    cod       = sprintf("%05d", as.integer(td[[col("^codigo_entidad|^cod.*entidad")]])),
    entidad   = td[[col("^entidad$")]],
    indicador = td[[col("^indicador$")]],
    valor     = a_numero(td[[col("dato_numerico|^valor$")]]),
    anio      = as.integer(td[[col("^ano$|^anio$|^a_o$")]])
  )
}

# Para ver qué indicadores trae tu descarga:  explorar_indicadores("saber")
explorar_indicadores <- function(patron = "") {
  td <- leer_terridata(ARCHIVOS_TERRIDATA)
  td %>% filter(grepl(patron, indicador, ignore.case = TRUE)) %>%
    count(indicador, anio, name = "n_municipios") %>% arrange(indicador, anio) %>%
    print(n = 200)
}

extraer <- function(td, ind, anio, nombre) {
  d <- td %>% filter(indicador == ind, anio == !!anio)
  if (nrow(d) == 0) stop("Indicador/año no encontrado: '", ind, "' ", anio,
                         ". Usa explorar_indicadores() para ver nombres exactos.")
  dup <- sum(duplicated(d$cod))
  if (dup > 0) message("  Aviso: ", dup, " códigos duplicados en '", nombre,
                       "'; se conserva el primero.")
  d %>% distinct(cod, .keep_all = TRUE) %>% select(cod, !!nombre := valor)
}

# --- Construcción --------------------------------------------------------------
if (MODO_PRUEBA) {
  # Datos de ejemplo SOLO para probar el flujo (no son del tema)
  mpios <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
    transmute(cod = as.character(FIPS), nombre = NAME,
              y  = 1000 * SID79 / BIR79,           # "respuesta"
              x1 = 100 * NWBIR79 / BIR79,          # "explicativa 1"
              x2 = log(BIR79)) %>%                 # "explicativa 2"
    st_transform(32119)
  qc <- tibble(control = "Modo prueba", valor = nrow(mpios), decision = "Sin controles")
} else {
  td <- leer_terridata(ARCHIVOS_TERRIDATA)
  vars <- extraer(td, IND_Y,  ANIO_Y,  "y") %>%
    full_join(extraer(td, IND_X1, ANIO_X1, "x1"), by = "cod") %>%
    full_join(extraer(td, IND_X2, ANIO_X2, "x2"), by = "cod")

  mgn <- st_read(ARCHIVO_MGN, quiet = TRUE)
  names(mgn) <- toupper(names(mgn))
  col_cod <- intersect(c("MPIO_CDPMP", "MPIO_CCNCT", "COD_MPIO", "CODIGO"), names(mgn))[1]
  col_nom <- intersect(c("MPIO_CNMBR", "MPIO_NOMBRE", "NOMBRE_MPI", "NOMBRE"), names(mgn))[1]
  mgn <- mgn %>%
    transmute(cod = sprintf("%05d", as.integer(.data[[col_cod]])),
              nombre = .data[[col_nom]]) %>%
    filter(substr(cod, 1, 2) == COD_DPTO) %>%
    st_make_valid() %>% st_transform(CRS_PROY)

  vars_d <- vars %>% filter(substr(cod, 1, 2) == COD_DPTO)
  unido  <- mgn %>% left_join(vars_d, by = "cod")

  # --- Control de calidad ----------------------------------------------------
  sin_datos   <- setdiff(mgn$cod, vars_d$cod)
  sin_poligono <- setdiff(vars_d$cod, mgn$cod)
  faltantes   <- unido %>% st_drop_geometry() %>%
    filter(is.na(y) | is.na(x1) | is.na(x2))
  qc <- tibble(
    control  = c("Municipios en el MGN", "Municipios con datos en TerriData",
                 "MGN sin datos (no unen)", "Datos sin polígono (no unen)",
                 "Duplicados de código en MGN", "Filas con algún faltante en y, x1 o x2",
                 "Municipios en la base final"),
    valor    = c(nrow(mgn), nrow(vars_d), length(sin_datos), length(sin_poligono),
                 sum(duplicated(mgn$cod)), nrow(faltantes), NA),
    detalle  = c("", "", paste(sin_datos, collapse = " "), paste(sin_poligono, collapse = " "),
                 "", paste(faltantes$nombre, collapse = "; "), "")
  )
  mpios <- unido %>% filter(!is.na(y), !is.na(x1), !is.na(x2))
  qc$valor[nrow(qc)] <- nrow(mpios)
  qc$decision <- c("", "", "", "", "",
                   "Se excluyen; las matrices W se construyen DESPUÉS sobre la base final", "")
}

mpios <- mpios %>% arrange(cod)          # orden fijo: el mismo para datos y pesos
print(qc, width = Inf)
guardar_tabla(qc, "control_calidad")

# Descripción de la base
desc <- data.frame(
  variable = c("y", "x1", "x2"),
  etiqueta = c(ETQ_Y, ETQ_X1, ETQ_X2),
  t(sapply(st_drop_geometry(mpios)[, c("y", "x1", "x2")], function(v)
    round(c(n = length(v), min = min(v), media = mean(v), mediana = median(v),
            max = max(v), de = sd(v)), 2))))
print(desc)
guardar_tabla(desc, "descriptivos")
cat("\nObservaciones:", nrow(mpios), "| Geometría:", as.character(st_geometry_type(mpios)[1]),
    "| CRS:", st_crs(mpios)$input, "\n")

st_write(mpios, "datos/base_analitica.gpkg", delete_dsn = TRUE, quiet = TRUE)
