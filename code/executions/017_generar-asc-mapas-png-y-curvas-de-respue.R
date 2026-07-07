# ═══════════════════════════════════════════════════════════
# Ressearch AI — Execution Record
# ═══════════════════════════════════════════════════════════
# Run ID:      fast-chat-run-1782439654151-q4snns
# Sequence:    #17
# Title:       Generar mapas y curvas de respuesta
# Purpose:     Generar ASC, mapas PNG y curvas de respuesta que quedaron pendientes por el fallo de GDAL
# Objective:   o3_outputs
# Language:    r
# Runtime:     modal · sdm-r
# Status:      succeeded
# Duration:    10240ms
# Started:     2026-06-26T02:08:10.841+00:00
# ═══════════════════════════════════════════════════════════
# Ressearch AI: emit_metric harvest no-op para ejecución offline
emit_metric <- function(name, value, ...) invisible(value)

library(terra)
library(ENMeval)
library(maxnet)

e <- readRDS("o2_enmeval_object.rds")
envs <- rast("o1_envs_selected.tif")
occs <- read.csv("o1_occs_clean.csv")

# Extraer el mejor modelo
res <- eval.results(e)
best_tune <- as.character(res$tune.args[which.min(res$delta.AICc)])
best_mod <- eval.models(e)[[best_tune]]

# Re-predicción para evitar el error de puntero nulo (SpatRaster guardado en RDS sin wrap)
message("Generando predicción espacial...")
best_pred <- terra::predict(envs, best_mod, type="cloglog", na.rm=TRUE)
names(best_pred) <- "suitability"

# Generar el archivo ASCII correctamente
message("Exportando a formato ASCII...")
writeRaster(best_pred, "o3_Sobralia_turkeliae_suitability.asc", NAflag=-9999, overwrite=TRUE)

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
png("o3_response_curves.png", width=3000, height=2000, res=300)
plot(best_mod, type="cloglog")
dev.off()

# Variables predictoras que influyen la distribución
cat("\n--- Coeficientes activos en el mejor modelo (maxnet) ---\n")
betas <- best_mod$betas
active_betas <- betas[betas != 0]

raw_vars <- names(active_betas)
var_list <- c()
for (v in names(envs)) {
  if (any(grepl(v, raw_vars))) {
    var_list <- c(var_list, v)
  }
}
cat("\nVariables retenidas e influyentes en la distribución final:\n")
print(unique(var_list))