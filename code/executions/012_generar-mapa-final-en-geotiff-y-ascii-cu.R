# ═══════════════════════════════════════════════════════════
# Ressearch AI — Execution Record
# ═══════════════════════════════════════════════════════════
# Run ID:      fast-chat-run-1782439168655-ahocd4
# Sequence:    #12
# Title:       Generar entregables finales del modelo MaxEnt
# Purpose:     Generar mapa final en GeoTIFF y ASCII, curvas de respuesta y extraer métricas del mejor modelo de nicho
# Objective:   o3_outputs
# Language:    r
# Runtime:     modal · sdm-r
# Status:      failed
# Duration:    7833ms
# Started:     2026-06-26T01:59:52.85+00:00
# ═══════════════════════════════════════════════════════════
# Ressearch AI: emit_metric harvest no-op para ejecución offline
emit_metric <- function(name, value, ...) invisible(value)

library(terra)
library(ENMeval)
library(maxnet)

message("Cargando resultados de modelamiento previo...")
e <- readRDS("o2_enmeval_object.rds")
envs <- rast("o1_envs_selected.tif")
occs <- read.csv("o1_occs_clean.csv")

# Extraer el mejor modelo basado en delta.AICc
res <- eval.results(e)
best_tune_idx <- which.min(res$delta.AICc)
best_tune <- as.character(res$tune.args[best_tune_idx])
best_auc <- res$auc.val.avg[best_tune_idx]

message(sprintf("Mejor modelo seleccionado: %s (AICc=%.1f, deltaAICc=0.0, AUC val=%.3f)",
                best_tune, res$AICc[best_tune_idx], best_auc))

emit_metric("AUC_val", best_auc, label="AUC (Validación Cruzada)", kind="scalar")
emit_metric("N_occs", nrow(occs), label="Ocurrencias", kind="count")

# Predicción del mejor modelo
preds <- eval.predictions(e)
best_pred <- preds[[best_tune]]

# Exportar resultados
message("Exportando GeoTIFF y ASCII...")
writeRaster(best_pred, "o3_Sobralia_turkeliae_suitability.tif", overwrite=TRUE)
writeRaster(best_pred, "o3_Sobralia_turkeliae_suitability.asc", overwrite=TRUE)

# Generar Mapa PNG de Idoneidad
message("Generando mapa de idoneidad...")
png("o3_suitability_map.png", width=3000, height=2000, res=300)
par(mar=c(4,4,3,1), bg="white", family="sans")
pal <- colorRampPalette(c("#003296", "#2e65dc", "#b0e16a", "#f6c23e", "#ec0f01"))(100)
plot(best_pred, main=paste("Idoneidad de Hábitat - Sobralia turkeliae\n(MaxEnt:", best_tune, ")"),
     col=pal, axes=TRUE, plg=list(title="Idoneidad\n(cloglog)"))
points(occs$lon, occs$lat, pch=21, bg="#ffffff", col="#2a3340", cex=1.2, lwd=1)
dev.off()

# Curvas de respuesta
message("Generando curvas de respuesta...")
best_mod <- eval.models(e)[[best_tune]]
png("o3_response_curves.png", width=3000, height=2000, res=300)
plot(best_mod, type="cloglog")
dev.off()

# Imprimir variables involucradas en el modelo
cat("\nVariables retenidas por VIF y evaluadas:\n")
print(names(envs))

cat("\nCoeficientes activos en el mejor modelo de maxnet:\n")
betas <- best_mod$betas
active_betas <- betas[betas != 0]

raw_vars <- names(active_betas)
var_list <- c()
for (v in names(envs)) {
  if (any(grepl(v, raw_vars))) {
    var_list <- c(var_list, v)
  }
}
cat("Variables predictoras que influyen la distribución final:\n")
print(unique(var_list))