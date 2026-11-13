# =============================================================================
# 03_mapas_iniciales.R  ·  Mapas de y (cuantiles vs. intervalos iguales) y de x1
# =============================================================================
source("R/00_config.R")
load("datos/matrices.RData")

mapa_clases <- function(var, estilo, titulo, etiqueta, n = 5, paleta = "YlGnBu") {
  ci <- classIntervals(mpios[[var]], n = n, style = estilo)
  brks <- unique(round(ci$brks, 2))
  mpios$clase <- cut(mpios[[var]], brks, include.lowest = TRUE, dig.lab = 6)
  tab <- as.data.frame(table(mpios$clase)); names(tab) <- c("clase", "n_municipios")
  p <- ggplot(mpios) + geom_sf(aes(fill = clase), color = "white", linewidth = .15) +
    scale_fill_brewer(palette = paleta, name = etiqueta, drop = FALSE) +
    labs(title = titulo, subtitle = paste(NOMBRE_TERR, "-", ANIO_Y), caption = FUENTE_MAPA) +
    theme_void() + theme(legend.position = "right")
  list(mapa = p, tabla = tab)
}

m_q  <- mapa_clases("y",  "quantile", "y por cuantiles",          ETQ_Y)
m_ei <- mapa_clases("y",  "equal",    "y por intervalos iguales", ETQ_Y)
m_x1 <- mapa_clases("x1", "quantile", "x1 por cuantiles",         ETQ_X1, paleta = "OrRd")
guardar_mapa(m_q$mapa,  "01_y_cuantiles")
guardar_mapa(m_ei$mapa, "02_y_intervalos_iguales")
guardar_mapa(m_x1$mapa, "03_x1_cuantiles")

# Cuántos municipios caen en cada clase: con intervalos iguales suelen
# concentrarse en las clases centrales y los extremos quedan con pocos.
cat("\nCuantiles:\n");          print(m_q$tabla)
cat("\nIntervalos iguales:\n"); print(m_ei$tabla)
guardar_tabla(rbind(cbind(clasificacion = "cuantiles", m_q$tabla),
                    cbind(clasificacion = "intervalos_iguales", m_ei$tabla)),
              "clases_y")

# Lugares concretos para la exposición
extremos <- mpios %>% st_drop_geometry() %>% arrange(desc(y)) %>%
  select(nombre, y, x1, x2)
cat("\nMunicipios con y más alto:\n"); print(head(extremos, 5))
cat("\nMunicipios con y más bajo:\n"); print(tail(extremos, 5))
guardar_tabla(rbind(head(extremos, 5), tail(extremos, 5)), "extremos_y")
