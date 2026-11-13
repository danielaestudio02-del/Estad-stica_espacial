# =============================================================================
# 06_regresiones.R  ·  16 ajustes (1 MCO + 5 familias x 3 W) y tabla de 18 filas
#   MCO    y = Xb + e
#   SLM    y = rho W y + Xb + e
#   SEM    y = Xb + u,  u = lambda W u + e
#   SLX    y = Xb + WX theta + e
#   SDM    y = rho W y + Xb + WX theta + e
#   SARAR  y = rho W y + Xb + u,  u = lambda W u + e   (misma W en ambos)
# =============================================================================
source("R/00_config.R")
load("datos/matrices.RData")

form <- y ~ x1 + x2            # misma respuesta, muestra y explicativas en todo
datos <- st_drop_geometry(mpios)

# --- Estimación --------------------------------------------------------------
modelos <- list(MCO = lm(form, data = datos))
for (w in names(W_lista)) {
  lw <- W_lista[[w]]
  modelos[[paste0("SLM_",   w)]] <- lagsarlm(form, data = datos, listw = lw, zero.policy = TRUE)
  modelos[[paste0("SEM_",   w)]] <- errorsarlm(form, data = datos, listw = lw, zero.policy = TRUE)
  modelos[[paste0("SLX_",   w)]] <- lmSLX(form, data = datos, listw = lw, zero.policy = TRUE)
  modelos[[paste0("SDM_",   w)]] <- lagsarlm(form, data = datos, listw = lw, Durbin = TRUE,
                                             zero.policy = TRUE)
  modelos[[paste0("SARAR_", w)]] <- sacsarlm(form, data = datos, listw = lw, listw2 = lw,
                                             zero.policy = TRUE)
}
cat("Ajustes estimados:", length(modelos), "(deben ser 16)\n")

# --- Tabla común de diagnóstico (18 filas) ------------------------------------
diag_fila <- function(m, familia, w) {
  e <- residuals(m)
  mi <- moran.test(e, W_lista[[w]], randomisation = TRUE, alternative = "greater",
                   zero.policy = TRUE)
  data.frame(modelo = familia, W = w,
             pseudo_R2 = round(1 - var(e) / var(datos$y), 4),
             var_residual = signif(var(e), 5),
             moran_resid = round(unname(mi$estimate[1]), 4),
             moran_p = signif(mi$p.value, 4),
             AIC = round(AIC(m), 2))
}
familias <- c("MCO", "SLM", "SEM", "SLX", "SDM", "SARAR")
tab_diag <- do.call(rbind, lapply(familias, function(f) do.call(rbind, lapply(names(W_lista),
  function(w) diag_fila(if (f == "MCO") modelos$MCO else modelos[[paste0(f, "_", w)]], f, w)))))
tab_diag$nota <- ifelse(tab_diag$modelo == "MCO",
                        "Misma estimación; W solo para Moran residual", "W de estimación = W de Moran")
print(tab_diag, row.names = FALSE)
guardar_tabla(tab_diag, "diagnostico_18_filas")

# --- Coeficientes y valores p (x1, x2, rezagos y parámetros espaciales) --------
extraer_coef <- function(m, nombre) {
  ct <- if (inherits(m, "Sarlm")) summary(m)$Coef else summary(m)$coefficients
  ct <- ct[rownames(ct) != "(Intercept)", , drop = FALSE]
  out <- data.frame(ajuste = nombre, termino = rownames(ct),
                    coef = signif(ct[, 1], 4), p_valor = signif(ct[, 4], 4))
  if (inherits(m, "Sarlm")) {
    par <- function(nm, est, se) if (!is.null(est) && !is.null(se) && length(se))
      data.frame(ajuste = nombre, termino = nm, coef = signif(est, 4),
                 p_valor = signif(2 * pnorm(-abs(est / se)), 4))
    out <- rbind(out, par("rho", m$rho, m$rho.se), par("lambda", m$lambda, m$lambda.se))
  }
  out$termino <- sub("^lag\\.", "W_", out$termino)
  rownames(out) <- NULL; out
}
tab_coef <- do.call(rbind, Map(extraer_coef, modelos, names(modelos)))
tab_coef$familia <- sub("_W[123]$", "", tab_coef$ajuste)
tab_coef$W <- ifelse(grepl("_W[123]$", tab_coef$ajuste), sub(".*_", "", tab_coef$ajuste), "-")
guardar_tabla(tab_coef, "coeficientes_largo")

# Formato ancho: una fila por ajuste, columnas coef (p)
tab_coef$celda <- sprintf("%s (%s)", format(tab_coef$coef, digits = 4),
                          format.pval(tab_coef$p_valor, digits = 2, eps = 1e-4))
ancho <- reshape(tab_coef[, c("ajuste", "familia", "W", "termino", "celda")],
                 idvar = c("ajuste", "familia", "W"), timevar = "termino", direction = "wide")
names(ancho) <- sub("^celda\\.", "", names(ancho))
orden <- intersect(c("ajuste", "familia", "W", "x1", "x2", "W_x1", "W_x2", "rho", "lambda"),
                   names(ancho))
ancho <- ancho[, orden]; ancho[is.na(ancho)] <- ""
print(ancho, row.names = FALSE)
guardar_tabla(ancho, "coeficientes_ancho")

# --- Efectos (impactos) directos/indirectos para modelos con rezago de y ------
# En SLM, SDM y SARAR el coeficiente beta NO es el efecto marginal total.
impactos_tab <- function(nombre) {
  m <- modelos[[nombre]]; w <- sub(".*_", "", nombre)
  im <- summary(impacts(m, listw = W_lista[[w]], R = 999), zstats = TRUE, short = TRUE)
  data.frame(ajuste = nombre, variable = rownames(im$pzmat),
             directo = signif(im$res$direct, 4), indirecto = signif(im$res$indirect, 4),
             total = signif(im$res$total, 4),
             p_total = signif(im$pzmat[, "Total"], 4))
}
tab_imp <- do.call(rbind, lapply(grep("^(SLM|SDM|SARAR)_", names(modelos), value = TRUE),
                                 impactos_tab))
print(tab_imp, row.names = FALSE)
guardar_tabla(tab_imp, "impactos")

saveRDS(modelos, "datos/modelos.rds")
