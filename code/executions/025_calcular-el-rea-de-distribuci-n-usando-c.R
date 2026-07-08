# ═══════════════════════════════════════════════════════════
# Ressearch AI — Execution Record
# ═══════════════════════════════════════════════════════════
# Run ID:      fast-chat-run-1783483031803-3xs78e
# Sequence:    #25
# Title:       Cálculo de Área de Distribución
# Purpose:     Calcular el área de distribución usando Convex Hull y umbralización del SDM logístico al 10th percentile
# Objective:   o5_biogeography_analysis
# Language:    r
# Runtime:     modal · sdm-r
# Status:      succeeded
# Duration:    7782ms
# Started:     2026-07-08T03:58:21.542+00:00
# ═══════════════════════════════════════════════════════════
# Ressearch AI: emit_metric harvest no-op para ejecución offline
emit_metric <- function(name, value, ...) invisible(value)

library(terra)
library(sf)
library(readr)
library(dplyr)

cat("--- OBTENIENDO COORDENADAS ---\n")
occs <- read_csv("o1_occs_clean.csv", show_col_types = FALSE)
lon_col <- intersect(names(occs), c("Longitude", "lon", "x"))[1]
lat_col <- intersect(names(occs), c("Latitude", "lat", "y"))[1]

pts <- st_as_sf(occs, coords = c(lon_col, lat_col), crs = 4326)

# 1. Minimum Convex Hull
cat("\n--- METODO 1: MINIMUM CONVEX HULL ---\n")
hull <- st_convex_hull(st_union(pts))
area_hull_km2 <- as.numeric(st_area(hull)) / 1e6
cat(sprintf("Área del Convex Hull: %.2f km2\n", area_hull_km2))

st_write(hull, "o5_convex_hull.geojson", delete_dsn=TRUE, quiet=TRUE)

# 2. Binarización del SDM
cat("\n--- METODO 2: SDM THRESHOLD ---\n")
r_log <- rast("o4_Sobralia_turkeliae_suitability_logistic.tif")
pts_spat <- vect(occs, geom=c(lon_col, lat_col), crs="EPSG:4326")

# Extraer los valores de idoneidad en los puntos de presencia
pred_vals <- extract(r_log, pts_spat)[[2]]

# Definir umbrales
thresh_10 <- quantile(pred_vals, probs = 0.1, na.rm = TRUE)
thresh_mtp <- min(pred_vals, na.rm = TRUE)

cat(sprintf("Umbral 10th Percentile: %.4f\n", thresh_10))
cat(sprintf("Umbral Minimum Training Presence (MTP): %.4f\n", thresh_mtp))

# Aplicar el umbral del 10%
r_bin <- r_log >= thresh_10
names(r_bin) <- "Presence"

# Calcular el área con expanse() de terra, que ajusta por la curvatura terrestre
areas <- expanse(r_bin, byValue=TRUE, unit="km")
area_sdm_km2 <- areas$area[areas$value %in% c(1, TRUE)]

cat(sprintf("Área del SDM Binario (Umbral 10p): %.2f km2\n", area_sdm_km2))

writeRaster(r_bin, "o5_sdm_binary_10th.tif", overwrite=TRUE)

# Generar un gráfico comparativo de las áreas
png("o5_areas_comparacion.png", width=2000, height=1000, res=200)
par(mfrow=c(1,2), mar=c(2,2,4,2), bg="white", family="sans")

plot(hull, col="#e6f2ff", border="#2e65dc", lwd=1.5, 
     main=sprintf("Minimum Convex Hull\nÁrea: %s km²", formatC(round(area_hull_km2, 1), format="f", big.mark=",")),
     col.main="#0b1a3a", cex.main=1.2)
plot(st_geometry(pts), add=TRUE, pch=21, col="white", bg="#ec0f01", cex=1.2, lwd=0.8)

plot(r_bin, col=c("#ffffff", "#01781b"), legend=FALSE, axes=FALSE,
     main=sprintf("SDM Binarizado (Umbral 10%%)\nÁrea: %s km²", formatC(round(area_sdm_km2, 1), format="f", big.mark=",")),
     col.main="#0b1a3a", cex.main=1.2)
plot(st_geometry(pts), add=TRUE, pch=21, col="white", bg="#ec0f01", cex=0.8, lwd=0.5)

dev.off()

# Capturar métricas para el sistema
cat(sprintf("\n{\"metric_ch\": %.2f, \"metric_sdm\": %.2f, \"threshold\": %.4f}\n", area_hull_km2, area_sdm_km2, thresh_10))