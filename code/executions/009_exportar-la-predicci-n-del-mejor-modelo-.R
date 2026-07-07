# ═══════════════════════════════════════════════════════════
# Ressearch AI — Execution Record
# ═══════════════════════════════════════════════════════════
# Run ID:      fast-chat-run-1782423738560-0oli21
# Sequence:    #9
# Title:       Generación de Mapas y Curvas de Respuesta
# Purpose:     Exportar la predicción del mejor modelo a ASCII y GeoTIFF, graficar el mapa de idoneidad final y extraer las curvas de respuesta para interpretación de nicho.
# Objective:   o3_enmeval-export
# Language:    r
# Runtime:     modal · sdm-r
# Status:      failed
# Duration:    8104ms
# Started:     2026-06-25T21:42:32.86+00:00
# ═══════════════════════════════════════════════════════════
# Ressearch AI: emit_metric harvest no-op para ejecución offline
emit_metric <- function(name, value, ...) invisible(value)

library(terra)
library(sf)
library(ENMeval)
library(maxnet)

# 1. Recuperar el modelo ganador de ENMeval
e <- readRDS("o2_enmeval_object.rds")
res <- eval.results(e)
best_idx <- which.min(res$AICc)
# Corregir el tipo de dato: res$tune.args es un factor, pero para subseleccionar
# el SpatRaster de predicciones se necesita un character.
best_tune <- as.character(res$tune.args[best_idx])
best_fc <- as.character(res$fc[best_idx])
best_rm <- res$rm[best_idx]

message(sprintf("Parámetros óptimos -> FC: %s, RM: %s", best_fc, best_rm))

# 2. Extraer predicción ya calculada por ENMeval
# Ahora best_tune es un character y puede usarse para seleccionar la capa por su nombre.
best_pred <- eval.predictions(e)[[best_tune]]

# 3. Guardar formatos exportables (ASCII y GeoTIFF)
writeRaster(best_pred, "o3_Sobralia_turkeliae_suitability.asc", filetype="AAIGrid", overwrite=TRUE)
writeRaster(best_pred, "o3_Sobralia_turkeliae_suitability.tif", filetype="GTiff", overwrite=TRUE)
message("Archivos ASCII y GeoTIFF guardados exitosamente.")

# 4. Crear Mapa de Idoneidad publicable
png("o3_mapa_idoneidad.png", width=2000, height=2000, res=300)
# Paleta divergente/secuencial (azul oscuro a rojo/verde)
pal <- colorRampPalette(c("#003296", "#2e65dc", "#61bff9", "#b0e16a", "#ec0f01"))
plot(best_pred, col=pal(100), axes=FALSE,
     main=sprintf("Idoneidad de Hábitat: Sobralia turkeliae\n(MaxEnt FC: %s, RM: %s)", best_fc, best_rm))
# Agregar puntos de presencia
occs <- read.csv("o1_occs_clean.csv")
points(occs$lon, occs$lat, pch=21, bg="#ffffff", col="#1a2233", cex=0.8, lwd=0.6)
dev.off()

# 5. Generar Curvas de Respuesta Ambiental
best_mod <- e@models[[best_tune]]
png("o3_curvas_respuesta.png", width=2400, height=1200, res=300)
par(mfrow=c(2, 4), mar=c(4,4,2,1))
plot(best_mod, type="cloglog")
dev.off()

# 6. Analizar variables más importantes (coeficientes distintos de 0)
betas <- best_mod$betas
# Extraer nombres base de las variables, eliminando transformaciones de maxnet
vars_importantes <- unique(gsub("\\^2|\\:.*|\\(.*\\)", "", names(betas[betas != 0])))
message("\nVariables bioclimáticas que están impulsando el modelo (con coeficientes activos):")
print(vars_importantes)

# 7. Registrar para métricas
emit_metric("best_aic", res$AICc[best_idx], label="AICc del mejor modelo", kind="scalar")