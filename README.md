# Parcial 3 · Análisis Espacial — Saber 11 en los municipios de Antioquia

## Pregunta, hipótesis y objetivos

**Pregunta.** ¿La pobreza multidimensional y la ruralidad explican las diferencias en el
puntaje de Saber 11 entre los municipios de Antioquia, y ese resultado presenta
dependencia espacial?

**Hipótesis.**
- H1: a mayor IPM, menor puntaje promedio en Saber 11 (relación **negativa**).
- H2: a mayor porcentaje de población rural, menor puntaje (relación **negativa**).
- H3: los puntajes no se distribuyen al azar. Municipios vecinos tienen puntajes
  parecidos (autocorrelación positiva), con agrupaciones altas en el Valle de Aburrá y el
  Oriente cercano y agrupaciones bajas en la periferia (Urabá, Bajo Cauca, Nordeste).

**Objetivos.** (1) Describir y mapear el puntaje. (2) Medir la autocorrelación global y local.
(3) Estimar MCO y cinco modelos espaciales con tres matrices W. (4) Evaluar qué
conclusiones se mantienen y cuáles cambian con la W y con el modelo.

| Rol | Variable | Unidad | Fuente |
|---|---|---|---|
| y | Puntaje promedio Saber 11 (Matemáticas o global) | puntos | ICFES vía DNP TerriData |
| x1 (central) | Índice de Pobreza Multidimensional (IPM) | % de personas | DANE CNPV 2018 vía TerriData |
| x2 (central) | Población rural | % | DANE CNPV 2018 vía TerriData |
| Geometría | Polígonos municipales | — | DANE, Marco Geoestadístico Nacional (MGN) |

Una fila de la base = un municipio de Antioquia (125 en total). El sistema de referencia es MAGNA-SIRGAS / Origen-Nacional
(EPSG:9377), con distancias en metros.

## Matrices W

| W | Regla | Qué representa |
|---|---|---|
| **W1 (principal)** | Contigüidad tipo reina | Municipios que comparten límite: intercambio directo (estudiantes, docentes, vías) |
| W2 | k = 5 vecinos más cercanos | El mismo número de vecinos para todos; evita que los municipios grandes (Urabá, Bajo Cauca) tengan muchos vecinos y los pequeños del Valle de Aburrá muy pocos |
| W3 | Distancia inversa con umbral (el mínimo que garantiza al menos un vecino) | La influencia decae con la distancia |

Las tres matrices se estandarizan por filas. La G global de Getis-Ord usa la versión binaria de W1 y W2.

## Cómo conseguir los datos (unos 15 minutos)

1. **TerriData** (terridata.dnp.gov.co), sección de descargas: baja los indicadores
   de Antioquia por municipio. Necesitas Saber 11, IPM y población rural o ruralidad. Guarda el
   archivo en `datos/crudos/` con un nombre que contenga `terridata` (por ejemplo
   `terridata_antioquia.xlsx`).
2. **MGN del DANE** (geoportal.dane.gov.co, descargas, Marco Geoestadístico Nacional):
   baja la capa de municipios (`MGN_MPIO_POLITICO`). Descomprime la carpeta dentro de `datos/crudos/`.
3. Abre `Estadistica_espacial.Rproj` en RStudio y corre:
   ```r
   source("R/01_preparar_base.R")       # la primera vez puede fallar: es normal
   explorar_indicadores("saber")         # muestra los nombres exactos
   explorar_indicadores("pobreza|rural")
   ```
   Copia los nombres exactos y los años en `R/00_config.R` (`IND_Y`, `IND_X1`, `IND_X2`,
   `ANIO_*`). Usa el mismo año o el más cercano a 2018, que es el año del censo.
4. Corre todo: `source("run_all.R")`.

Paquetes: `install.packages(c("sf","spdep","spatialreg","dplyr","ggplot2","classInt","readxl"))`

**Modo prueba:** con `MODO_PRUEBA <- TRUE` en `00_config.R`, todo el flujo corre con un
mapa de ejemplo que trae R. Sirve para comprobar que los paquetes funcionan antes de tener los datos.

## Scripts y qué parte del parcial cubren

| Script | Parcial | Salidas clave |
|---|---|---|
| `01_preparar_base.R` | 1. Base analítica y control de calidad | `control_calidad.csv`, `descriptivos.csv` |
| `02_matrices.R` | 1. Tres matrices | `matrices_resumen.csv`, `matrices_comparacion.csv`, mapa de vecindades |
| `03_mapas_iniciales.R` | 1. Mapas (cuantiles vs. intervalos iguales) | mapas 01–03 |
| `04_global.R` | 2. Moran, Geary, G (12 resultados) | `global_12_resultados.csv` |
| `05_local.R` | 2. LISA: Moran, Geary, Gi con W1 | mapas 05–07, verificación de una unidad |
| `06_regresiones.R` | 3. 16 ajustes, tabla de 18 filas | `diagnostico_18_filas.csv`, `coeficientes_ancho.csv`, `impactos.csv` |
| `07_robustez.R` | 4. Cambio de W y de modelo; 4 mapas de residuos | `robustez_*.csv`, `ranking_AIC_por_W.csv`, mapa 08 |

## Literatura sugerida (verifica cada cita antes de usarla)

- Gaviria, A., & Barrientos, J. H. (2001). *Determinantes de la calidad de la educación en
  Colombia* (Archivos de Economía No. 159). Departamento Nacional de Planeación.
- Iregui, A. M., Melo, L., & Ramos, J. (2007). Análisis de eficiencia de la educación en
  Colombia. *Revista de Economía del Rosario, 10*(1), 21–41.
- Ramos, R., Duque, J. C., & Nieto, S. (2016). Decomposing the rural-urban differential in
  student achievement in Colombia using PISA microdata. *Estudios de Economía Aplicada, 34*(2).
- Anselin, L. (1995). Local indicators of spatial association—LISA. *Geographical Analysis,
  27*(2), 93–115.
