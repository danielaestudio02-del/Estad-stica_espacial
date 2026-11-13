# =============================================================================
# 07_robustez.R  ·  ¿Qué cambia con la matriz y con el modelo?
# =============================================================================
source("R/00_config.R")
load("datos/matrices.RData")
modelos  <- readRDS("datos/modelos.rds")
tab_diag <- read.csv(file.path(DIR_TAB, "diagnostico_18_filas.csv"))
tab_coef <- read.csv(file.path(DIR_TAB, "coeficientes_largo.csv"))

# --- 1. Cambiar la matriz, mantener la familia --------------------------------
cmp_w <- tab_coef %>% filter(termino %in% c("x1", "x2"), W != "-") %>%
  mutate(signo = ifelse(coef > 0, "+", "-"), sig = p_valor < ALFA) %>%
  group_by(familia, termino) %>%
  summarise(coef_W1 = coef[W == "W1"], coef_W2 = coef[W == "W2"], coef_W3 = coef[W == "W3"],
            p_W1 = p_valor[W == "W1"], p_W2 = p_valor[W == "W2"], p_W3 = p_valor[W == "W3"],
            mismo_signo = n_distinct(signo) == 1, misma_signif = n_distinct(sig) == 1,
            rango_relativo = round((max(coef) - min(coef)) / abs(mean(coef)), 3),
            .groups = "drop") %>%
  mutate(clasificacion = ifelse(mismo_signo & misma_signif & rango_relativo < 0.25,
                                "ESTABLE", "SENSIBLE"))
mco <- tab_coef %>% filter(familia == "MCO", termino %in% c("x1", "x2"))
cat("\nMCO de referencia:\n"); print(mco[, c("termino", "coef", "p_valor")])
cat("\nCambiar W manteniendo la familia:\n"); print(as.data.frame(cmp_w))
guardar_tabla(cmp_w, "robustez_cambiar_W")

# --- 2. Cambiar el modelo, mantener la matriz (W1) -----------------------------
cmp_m <- tab_coef %>% filter(W %in% c("W1", "-")) %>%
  select(familia, termino, coef, p_valor) %>% arrange(termino, familia)
cat("\nCambiar el modelo con W1 fija:\n"); print(cmp_m)
guardar_tabla(cmp_m, "robustez_cambiar_modelo_W1")

# --- 3. Orden por AIC bajo cada W ---------------------------------------------
rank_aic <- tab_diag %>% group_by(W) %>% arrange(AIC, .by_group = TRUE) %>%
  mutate(puesto = row_number(), resid_autocorr = ifelse(moran_p < ALFA, "sí", "no")) %>%
  select(W, puesto, modelo, AIC, pseudo_R2, moran_resid, moran_p, resid_autocorr)
print(as.data.frame(rank_aic))
guardar_tabla(rank_aic, "ranking_AIC_por_W")
cat("\nMenor AIC por W:\n")
print(rank_aic %>% filter(puesto == 1))

# --- 4. Cuatro mapas de residuos con la MISMA escala --------------------------
res <- data.frame(
  MCO = residuals(modelos$MCO),
  setNames(lapply(names(W_lista), function(w) residuals(modelos[[paste0(FAMILIA_MAPAS, "_", w)]])),
           paste0(FAMILIA_MAPAS, " ", names(W_lista))), check.names = FALSE)
res_sf <- do.call(rbind, lapply(names(res), function(n) {
  d <- mpios[, "nombre"]; d$residuo <- res[[n]]; d$ajuste <- n; d }))
res_sf$ajuste <- factor(res_sf$ajuste, levels = names(res))
cortes <- unique(round(quantile(unlist(res), probs = seq(0, 1, .2)), 2))  # cortes comunes
res_sf$clase <- cut(res_sf$residuo, cortes, include.lowest = TRUE)
p_res <- ggplot(res_sf) + geom_sf(aes(fill = clase), color = "white", linewidth = .1) +
  scale_fill_brewer(palette = "RdBu", direction = -1, name = "Residuo\n(quintiles comunes)") +
  facet_wrap(~ajuste, ncol = 2) + theme_void() +
  labs(title = paste("Residuos: MCO y", FAMILIA_MAPAS, "con W1, W2, W3"),
       subtitle = "Mismos cortes en los cuatro mapas", caption = FUENTE_MAPA)
guardar_mapa(p_res, "08_residuos_4_mapas", w = 10, h = 9)

# Municipios que mantienen residuos grandes en los cuatro ajustes
persist <- data.frame(nombre = mpios$nombre, res) %>%
  mutate(siempre_sobreestima = rowSums(res < 0) == 4 & abs(MCO) > sd(MCO),
         siempre_subestima   = rowSums(res > 0) == 4 & abs(MCO) > sd(MCO))
cat("\nSubestimados en los 4 ajustes (y observado > predicho):\n")
print(persist$nombre[persist$siempre_subestima])
cat("Sobreestimados en los 4 ajustes:\n"); print(persist$nombre[persist$siempre_sobreestima])
guardar_tabla(persist, "residuos_por_municipio")
