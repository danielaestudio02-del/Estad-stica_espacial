# =============================================================================
# 04_global.R  ·  Autocorrelación global: Moran, Geary y Getis-Ord G
# 2 variables (y, x1) x 2 matrices (W1, W2) x 3 índices = 12 resultados
# Inferencia: aproximación normal bajo aleatorización; alfa = ALFA.
# =============================================================================
source("R/00_config.R")
load("datos/matrices.RData")

prueba_global <- function(var, nombre_w, lw, lwB) {
  x <- mpios[[var]]
  mo <- moran.test(x, lw, randomisation = TRUE, alternative = "greater", zero.policy = TRUE)
  ge <- geary.test(x, lw, randomisation = TRUE, alternative = "greater", zero.policy = TRUE)
  gg <- globalG.test(x, lwB, alternative = "two.sided", zero.policy = TRUE)
  fila <- function(indice, est, esp, z, p, h1, concl_sig, concl_no) data.frame(
    variable = var, matriz = nombre_w, indice = indice,
    estadistico = signif(est, 4), esperado_H0 = signif(esp, 4), z = round(z, 3),
    p_valor = signif(p, 4), H1 = h1,
    conclusion = ifelse(p < ALFA, concl_sig, concl_no))
  rbind(
    fila("I de Moran", mo$estimate[1], mo$estimate[2], mo$statistic, mo$p.value,
         "I > E[I]: autocorrelación positiva",
         "Valores similares tienden a ser vecinos", "Sin evidencia de autocorrelación"),
    fila("C de Geary", ge$estimate[1], ge$estimate[2], ge$statistic, ge$p.value,
         "C < 1: vecinos más parecidos que al azar",
         "Vecinos semejantes (autocorrelación positiva)", "Sin evidencia de autocorrelación"),
    fila("G de Getis-Ord", gg$estimate[1], gg$estimate[2], gg$statistic, gg$p.value,
         "G != E[G]: concentración de valores altos o bajos",
         ifelse(gg$estimate[1] > gg$estimate[2], "Concentración de valores ALTOS",
                "Concentración de valores BAJOS"), "Sin concentración significativa"))
}

tab_global <- rbind(
  prueba_global("y",  "W1", W1, W1B), prueba_global("y",  "W2", W2, W2B),
  prueba_global("x1", "W1", W1, W1B), prueba_global("x1", "W2", W2, W2B))
rownames(tab_global) <- NULL
print(tab_global, right = FALSE)
guardar_tabla(tab_global, "global_12_resultados")

# Comparación W1 vs W2 para y (para la exposición)
cmp <- reshape(tab_global[tab_global$variable == "y", c("indice", "matriz", "estadistico", "p_valor")],
               idvar = "indice", timevar = "matriz", direction = "wide")
print(cmp)

# Diagrama de dispersión de Moran (y, W1)
z  <- as.numeric(scale(mpios$y)); lz <- lag.listw(W1, z)
p_mor <- ggplot(data.frame(z, lz), aes(z, lz)) +
  geom_hline(yintercept = 0, color = "grey60") + geom_vline(xintercept = 0, color = "grey60") +
  geom_point(alpha = .7) + geom_smooth(method = "lm", se = FALSE, color = "#c0392b") +
  labs(x = "y estandarizada", y = "Rezago espacial de y (W1)",
       title = sprintf("Diagrama de Moran - I = %.3f", tab_global$estadistico[1])) +
  theme_minimal()
guardar_mapa(p_mor, "04_dispersion_moran_y_W1", w = 6, h = 5)
