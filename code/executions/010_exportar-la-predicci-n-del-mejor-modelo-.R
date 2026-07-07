# ═══════════════════════════════════════════════════════════
# Ressearch AI — Execution Record
# ═══════════════════════════════════════════════════════════
# Run ID:      fast-chat-run-1782423823987-lxln6a
# Sequence:    #10
# Title:       Generación de Mapas y Curvas de Respuesta
# Purpose:     Exportar la predicción del mejor modelo a ASCII y GeoTIFF, graficar el mapa de idoneidad final y extraer las curvas de respuesta para interpretación de nicho.
# Objective:   o3_enmeval-export
# Language:    r
# Runtime:     modal · sdm-r
# Status:      failed
# Duration:    11179ms
# Started:     2026-06-25T21:44:01.142+00:00
#
# Path rewrites applied (sandbox → relative):
#   - /data/worldclim (×1)
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
best_tune <- as.character(res$tune.args[best_idx])
best_fc <- as.character(res$fc[best_idx])
best_rm <- res$rm[best_idx]

message(sprintf("Parámetros óptimos -> FC: %s, RM: %s", best_fc, best_rm))

# 2. Re-predecir el modelo (el SpatRaster guardado en RDS pierde su puntero C++)
best_mod <- e@models[[best_tune]]

# Cargar variables climáticas desde disco (pre-cargadas)
f <- list.files("./data/worldclim", pattern="_bio_?[0-9]+\\.tif$", recursive=TRUE, full.names=TRUE)
f <- f[order(as.integer(sub(".*_bio_?([0-9]+).*", "\\1", basename(f))))]
bio <- terra::rast(f)
names(bio) <- paste0("bio", seq_len(terra::nlyr(bio)))

# Seleccionar solo las variables usadas en el modelo
mod_vars <- names(best_mod$samplemeans)
env <- bio[[mod_vars]]

# Determinar la extensión a partir de las ocurrencias y puntos de fondo
occs <- read.csv("o1_occs_clean.csv")
bg <- e@bg.pts

if (!is.null(bg)) {
    lon_col <- intersect(colnames(bg), c("lon", "x", "longitude"))[1]
    lat_col <- intersect(colnames(bg), c("lat", "y", "latitude"))[1]
    min_x <- min(c(occs$lon, bg[[lon_col]]), na.rm=TRUE)
    max_x <- max(c(occs$lon, bg[[lon_col]]), na.rm=TRUE)
    min_y <- min(c(occs$lat, bg[[lat_col]]), na.rm=TRUE)
    max_y <- max(c(occs$lat, bg[[lat_col]]), na.rm=TRUE)
} else {
    min_x <- min(occs$lon, na.rm=TRUE)
    max_x <- max(occs$lon, na.rm=TRUE)
    min_y <- min(occs$lat, na.rm=TRUE)
    max_y <- max(occs$lat, na.rm=TRUE)
}

# Expandir ligeramente la extensión para evitar recortes justos
ext_all <- terra::ext(min_x - 0.5, max_x + 0.5, min_y - 0.5, max_y + 0.5)
env <- terra::crop(env, ext_all)

# Generar la predicción espacial
best_pred <- terra::predict(env, best_mod, type="cloglog", na.rm=TRUE)

# 3. Guardar formatos exportables (ASCII y GeoTIFF)
writeRaster(best_pred, "o3_Sobralia_turkeliae_suitability.asc", filetype="AAIGrid", overwrite=TRUE)
writeRaster(best_pred, "o3_Sobralia_turkeliae_suitability.tif", filetype="GTiff", overwrite=TRUE)
message("Archivos ASCII y GeoTIFF guardados exitosamente.")

# 4. Crear Mapa de Idoneidad publicable
png("o3_mapa_idoneidad.png", width=2000, height=2000, res=300)
pal <- colorRampPalette(c("#003296", "#2e65dc", "#61bff9", "#b0e16a", "#ec0f01"))
plot(best_pred, col=pal(100), axes=FALSE,
     main=sprintf("Idoneidad de Hábitat: Sobralia turkeliae\n(MaxEnt FC: %s, RM: %s)", best_fc, best_rm))
points(occs$lon, occs$lat, pch=21, bg="#ffffff", col="#1a2233", cex=0.8, lwd=0.6)
dev.off()

# 5. Generar Curvas de Respuesta Ambiental
png("o3_curvas_respuesta.png", width=2400, height=1200, res=300)
par(mfrow=c(2, ceiling(length(mod_vars)/2)), mar=c(4,4,2,1))
plot(best_mod, type="cloglog")
dev.off()

# 6. Analizar variables más importantes (coeficientes distintos de 0)
betas <- best_mod$betas
vars_importantes <- unique(gsub("\\^2|\\:.*|\\(.*\\)", "", names(betas[betas != 0])))
message("\nVariables bioclimáticas que están impulsando el modelo (con coeficientes activos):")
print(vars_importantes)

# 7. Registrar para métricas
if (exists("emit_metric")) {
  emit_metric("best_aic", res$AICc[best_idx], label="AICc del mejor modelo", kind="scalar")
}