# ═══════════════════════════════════════════════════════════
# Ressearch AI — Execution Record
# ═══════════════════════════════════════════════════════════
# Run ID:      fast-chat-run-1782966023038-fz7ho6
# Sequence:    #20
# Title:       Generar mapa con output logístico
# Purpose:     Generar mapa de idoneidad usando la escala logística y preservar el estilo gráfico exacto
# Objective:   o4_logistic_map
# Language:    r
# Runtime:     modal · sdm-r
# Status:      succeeded
# Duration:    11119ms
# Started:     2026-07-02T04:21:18.382+00:00
# ═══════════════════════════════════════════════════════════
# Ressearch AI: emit_metric harvest no-op para ejecución offline
emit_metric <- function(name, value, ...) invisible(value)

library(terra)
library(ENMeval)
library(maxnet)

message("Cargando modelo y datos ambientales...")
e <- readRDS("o2_enmeval_object.rds")
envs <- rast("o1_envs_selected.tif")
occs <- read.csv("o1_occs_clean.csv")

# Extraer el mejor modelo de maxnet
res <- eval.results(e)
best_tune <- as.character(res$tune.args[which.min(res$delta.AICc)])
best_mod <- eval.models(e)[[best_tune]]

message("Generando predicción en escala logística...")
# La proyección por defecto de ENMeval fue cloglog, así que re-proyectamos manualmente
pred_logistic <- terra::predict(envs, best_mod, type="logistic", na.rm=TRUE)

# Guardamos también los archivos raster en escala logística por si los necesitas en tu SIG
writeRaster(pred_logistic, "o4_Sobralia_turkeliae_suitability_logistic.tif", overwrite=TRUE)
writeRaster(pred_logistic, "o4_Sobralia_turkeliae_suitability_logistic.asc", filetype="AAIGrid", NAflag=-9999, overwrite=TRUE)

message("Generando mapa de idoneidad (logistic)...")
png("o4_suitability_map_logistic.png", width=3000, height=2000, res=300)
par(mar=c(4,4,3,1), bg="white", family="sans")
pal <- colorRampPalette(c("#003296", "#2e65dc", "#b0e16a", "#f6c23e", "#ec0f01"))(100)

# Exactamente el mismo código de ploteo, solo cambia el texto de la leyenda
plot(pred_logistic, main=paste("Idoneidad de Hábitat - Sobralia turkeliae\n(MaxEnt:", best_tune, ")"),
     col=pal, axes=TRUE, plg=list(title="Idoneidad\n(logistic)"))
points(occs$lon, occs$lat, pch=21, bg="#ffffff", col="#2a3340", cex=1.2, lwd=1)
dev.off()

message("Mapa logístico generado correctamente.")