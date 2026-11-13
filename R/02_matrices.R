# =============================================================================
# 02_matrices.R  ·  Tres matrices de conceptualización espacial
#   W1 (principal): contigüidad tipo reina  -> municipios que comparten límite
#   W2            : k = 5 vecinos más cercanos (centroides) -> misma cantidad
#                   de vecinos para todos, sin importar el tamaño del municipio
#   W3            : distancia inversa con umbral -> todos los municipios dentro
#                   de d km, con más peso a los más cercanos
# Todas estandarizadas por filas (estilo "W"). Para la G de Getis-Ord global
# se usan las mismas vecindades en versión binaria (estilo "B").
# =============================================================================
source("R/00_config.R")
mpios <- st_read("datos/base_analitica.gpkg", quiet = TRUE)
coords <- st_coordinates(st_point_on_surface(st_geometry(mpios)))

# --- Vecindades -------------------------------------------------------------
nb1 <- poly2nb(mpios, queen = TRUE)
nb2 <- knn2nb(knearneigh(coords, k = K_VECINOS))
# umbral = mayor distancia al vecino más cercano -> nadie queda sin vecinos
d_min  <- unlist(nbdists(knn2nb(knearneigh(coords, k = 1)), coords))
umbral <- ceiling(max(d_min) / 1000) * 1000          # en metros, redondeado al km
nb3 <- dnearneigh(coords, 0, umbral)

# --- Pesos ------------------------------------------------------------------
W1 <- nb2listw(nb1, style = "W", zero.policy = TRUE)
W2 <- nb2listw(nb2, style = "W", zero.policy = TRUE)
dist3 <- nbdists(nb3, coords)
W3 <- nb2listw(nb3, glist = lapply(dist3, function(d) 1 / (d / 1000)),  # 1/km
               style = "W", zero.policy = TRUE)
W_lista <- list(W1 = W1, W2 = W2, W3 = W3)
# binarias para G global
W1B <- nb2listw(nb1, style = "B", zero.policy = TRUE)
W2B <- nb2listw(nb2, style = "B", zero.policy = TRUE)

# --- Verificaciones ---------------------------------------------------------
resumen_w <- function(nb, lw, nombre, regla) {
  card_nb <- card(nb)
  sumas <- sapply(lw$weights, sum)
  data.frame(matriz = nombre, regla = regla, n = length(nb),
             enlaces = sum(card_nb), vecinos_prom = round(mean(card_nb), 2),
             vecinos_min = min(card_nb), vecinos_max = max(card_nb),
             sin_vecinos = sum(card_nb == 0),
             simetrica = is.symmetric.nb(nb, force = TRUE, verbose = FALSE),
             filas_suman_1 = all(abs(sumas[card_nb > 0] - 1) < 1e-10))
}
tab_w <- rbind(
  resumen_w(nb1, W1, "W1", "Contigüidad reina"),
  resumen_w(nb2, W2, "W2", paste0("k = ", K_VECINOS, " vecinos más cercanos")),
  resumen_w(nb3, W3, "W3", paste0("Distancia inversa, umbral ", umbral / 1000, " km")))
print(tab_w)
guardar_tabla(tab_w, "matrices_resumen")

# ¿Son distintas? proporción de enlaces compartidos (índice de Jaccard)
enlaces <- function(nb) unlist(lapply(seq_along(nb), function(i)
  if (nb[[i]][1] == 0) character(0) else paste(i, nb[[i]], sep = "-")))
jacc <- function(a, b) { a <- enlaces(a); b <- enlaces(b)
  round(length(intersect(a, b)) / length(union(a, b)), 3) }
tab_jac <- data.frame(par = c("W1-W2", "W1-W3", "W2-W3"),
                      jaccard_enlaces = c(jacc(nb1, nb2), jacc(nb1, nb3), jacc(nb2, nb3)),
                      identicas = c(isTRUE(all.equal(nb1, nb2, check.attributes = FALSE)),
                                    isTRUE(all.equal(nb1, nb3, check.attributes = FALSE)),
                                    isTRUE(all.equal(nb2, nb3, check.attributes = FALSE))))
print(tab_jac)
guardar_tabla(tab_jac, "matrices_comparacion")

# Mismo orden de unidades en datos y pesos
stopifnot(all(sapply(W_lista, function(w) length(w$neighbours)) == nrow(mpios)))
if (any(card(nb1) == 0))
  message("W1 tiene unidades sin vecinos: ", paste(mpios$nombre[card(nb1) == 0], collapse = ", "))
cat("Orden verificado: datos y pesos tienen", nrow(mpios), "unidades en el mismo orden.\n")

# Mapa de las tres vecindades
lineas <- function(nb, nombre) {
  sf_l <- nb2lines(nb, coords = coords, as_sf = TRUE)
  st_crs(sf_l) <- st_crs(mpios); sf_l$matriz <- nombre; sf_l[, "matriz"] }
p_w <- ggplot() + geom_sf(data = mpios, fill = "grey95", color = "grey70", linewidth = .2) +
  geom_sf(data = rbind(lineas(nb1, "W1 reina"), lineas(nb2, paste0("W2 k=", K_VECINOS)),
                       lineas(nb3, paste0("W3 dist. inversa <= ", umbral / 1000, " km"))),
          color = "#1f5f8b", linewidth = .25) +
  facet_wrap(~matriz) + theme_void() +
  labs(title = paste("Matrices de vecindad -", NOMBRE_TERR), caption = FUENTE_MAPA)
guardar_mapa(p_w, "00_matrices_vecindad", w = 12, h = 5)

save(mpios, coords, nb1, nb2, nb3, W1, W2, W3, W_lista, W1B, W2B, umbral,
     file = "datos/matrices.RData")
