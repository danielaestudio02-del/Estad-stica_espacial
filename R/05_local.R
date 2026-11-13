# =============================================================================
# 05_local.R  ·  LISA sobre y con W1: Moran local, Geary local, Getis-Ord local
# Inferencia por permutación condicional (NSIM permutaciones), alfa = ALFA.
# =============================================================================
source("R/00_config.R")
load("datos/matrices.RData")
y <- mpios$y

# --- 1. Moran local ---------------------------------------------------------
lm_loc <- localmoran_perm(y, W1, nsim = NSIM, zero.policy = TRUE)
p_mor  <- lm_loc[, "Pr(z != E(Ii)) Sim"]
cuad   <- as.character(attr(lm_loc, "quadr")$mean)       # posición de y y su rezago
mpios$moran_Ii   <- lm_loc[, "Ii"]
mpios$moran_p    <- p_mor
mpios$moran_cat  <- ifelse(p_mor < ALFA, cuad, "No significativo")
mpios$moran_cat  <- factor(mpios$moran_cat,
  levels = c("High-High", "Low-Low", "High-Low", "Low-High", "No significativo"),
  labels = c("Alto-Alto", "Bajo-Bajo", "Alto-Bajo", "Bajo-Alto", "No significativo"))

col_lisa <- c("Alto-Alto" = "#d7191c", "Bajo-Bajo" = "#2c7bb6", "Alto-Bajo" = "#fdae61",
              "Bajo-Alto" = "#abd9e9", "No significativo" = "grey90")
p1 <- ggplot(mpios) + geom_sf(aes(fill = moran_cat), color = "white", linewidth = .15) +
  scale_fill_manual(values = col_lisa, drop = FALSE, name = NULL) + theme_void() +
  labs(title = "Moran local de y (W1)",
       subtitle = sprintf("Permutación condicional, %d sim., p < %.2f", NSIM, ALFA),
       caption = FUENTE_MAPA)
guardar_mapa(p1, "05_moran_local_W1")

# --- 2. Geary local ---------------------------------------------------------
gc_loc <- localC_perm(y, W1, nsim = NSIM, zero.policy = TRUE)
mpios$geary_C <- as.numeric(gc_loc)
mpios$geary_p <- attr(gc_loc, "pseudo-p")[, "Pr(z != E(Ci)) Sim"]
p2 <- ggplot(mpios) + geom_sf(aes(fill = geary_C), color = "white", linewidth = .15) +
  scale_fill_viridis_c(option = "magma", direction = -1, trans = "sqrt",
                       name = "C local\n(bajo = semejante,\nalto = distinto)") +
  geom_sf(data = mpios[mpios$geary_p < ALFA, ], fill = NA, color = "black", linewidth = .5) +
  theme_void() +
  labs(title = "Geary local de y (W1)",
       subtitle = sprintf("Borde negro: p < %.2f (permutación)", ALFA), caption = FUENTE_MAPA)
guardar_mapa(p2, "06_geary_local_W1")

# --- 3. Getis-Ord local (Gi) --------------------------------------------------
gi <- localG_perm(y, W1, nsim = NSIM, zero.policy = TRUE)
mpios$gi_z <- as.numeric(gi)
mpios$gi_p <- attr(gi, "internals")[, "Pr(z != E(Gi)) Sim"]
mpios$gi_cat <- factor(ifelse(mpios$gi_p >= ALFA, "No significativo",
                              ifelse(mpios$gi_z > 0, "Concentración alta (hot spot)",
                                     "Concentración baja (cold spot)")),
  levels = c("Concentración alta (hot spot)", "Concentración baja (cold spot)", "No significativo"))
p3 <- ggplot(mpios) + geom_sf(aes(fill = gi_cat), color = "white", linewidth = .15) +
  scale_fill_manual(values = c("#d7191c", "#2c7bb6", "grey90"), drop = FALSE, name = NULL) +
  theme_void() +
  labs(title = "Getis-Ord Gi local de y (W1)",
       subtitle = sprintf("Permutación condicional, %d sim., p < %.2f", NSIM, ALFA),
       caption = FUENTE_MAPA)
guardar_mapa(p3, "07_getis_ord_local_W1")

# --- Resumen y falsos positivos ----------------------------------------------
resumen <- data.frame(
  indicador = c("Moran local", "Geary local", "Getis-Ord Gi"),
  significativos = c(sum(p_mor < ALFA), sum(mpios$geary_p < ALFA), sum(mpios$gi_p < ALFA)),
  # Con n pruebas a la vez, se esperan ~ n*alfa falsos positivos aunque no haya patrón.
  esperados_por_azar = round(nrow(mpios) * ALFA, 1),
  significativos_FDR = c(sum(p.adjust(p_mor, "fdr") < ALFA),
                         sum(p.adjust(mpios$geary_p, "fdr") < ALFA),
                         sum(p.adjust(mpios$gi_p, "fdr") < ALFA)))
print(resumen)
guardar_tabla(resumen, "local_resumen")
print(table(mpios$moran_cat)); print(table(mpios$gi_cat))

tab_local <- mpios %>% st_drop_geometry() %>%
  select(cod, nombre, y, moran_Ii, moran_p, moran_cat, geary_C, geary_p, gi_z, gi_p, gi_cat)
guardar_tabla(tab_local, "local_por_municipio")

# --- Verificar a mano UNA unidad (rúbrica 4A) ---------------------------------
verificar_unidad <- function(nom) {
  i  <- which(mpios$nombre == nom)[1]
  z  <- (y - mean(y)) / sd(y)
  lz <- lag.listw(W1, z)
  cat(sprintf(paste0("\n%s: y = %.2f | z = %.3f | rezago W1 de z = %.3f",
                     "\n  Posición: y %s de la media, vecinos %s de la media -> cuadrante %s",
                     "\n  Ii = %.3f | p (permutación) = %.4f -> categoría: %s\n"),
              nom, y[i], z[i], lz[i], ifelse(z[i] > 0, "ARRIBA", "ABAJO"),
              ifelse(lz[i] > 0, "ARRIBA", "ABAJO"), cuad[i],
              mpios$moran_Ii[i], mpios$moran_p[i], mpios$moran_cat[i]))
  cat("  Vecinos:", paste(mpios$nombre[nb1[[i]]], collapse = ", "), "\n")
}
# Ejemplo: el municipio significativo con mayor |Ii|
sig <- which(p_mor < ALFA)
if (length(sig)) verificar_unidad(mpios$nombre[sig[which.max(abs(mpios$moran_Ii[sig]))]])

save(mpios, file = "datos/local.RData")
