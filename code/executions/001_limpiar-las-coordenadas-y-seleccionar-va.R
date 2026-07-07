# ═══════════════════════════════════════════════════════════
# Ressearch AI — Execution Record
# ═══════════════════════════════════════════════════════════
# Run ID:      fast-chat-run-1782400208024-m1ocmo
# Sequence:    #1
# Title:       Preparación SDM y VIF
# Purpose:     Limpiar las coordenadas y seleccionar variables climáticas independientes para el modelo MaxEnt. Selecciono mediante VIF umbral 10 para reducir multicolinealidad. Descarto un buffer de 5 grados como extensión de background.
# Objective:   o1_data-prep
# Language:    r
# Runtime:     modal · sdm-r
# Status:      failed
# Duration:    9338ms
# Started:     2026-06-25T15:10:38.731+00:00
#
# Path rewrites applied (sandbox → relative):
#   - /data/worldclim (×1)
# ═══════════════════════════════════════════════════════════
# Ressearch AI: emit_metric harvest no-op para ejecución offline
emit_metric <- function(name, value, ...) invisible(value)

library(readxl)
library(terra)
library(sf)
library(usdm)

# 1. Cargar y limpiar ocurrencias
df <- read_excel("Coordenadas_Sobralia turkeliae_nov.xlsx")
occs <- data.frame(lon = as.numeric(df$lon), lat = as.numeric(df$lat))
occs <- occs[!is.na(occs$lon) & !is.na(occs$lat), ]
message(sprintf("Registros originales: %d", nrow(occs)))

# 2. Cargar datos climáticos locales (WorldClim 2.5 min)
f <- list.files("./data/worldclim", pattern="_bio_?[0-9]+\\.tif$", recursive=TRUE, full.names=TRUE)
f <- f[order(as.integer(sub(".*_bio_?([0-9]+).*", "\\1", basename(f))))]
bio <- terra::rast(f)
names(bio) <- paste0("bio", seq_len(terra::nlyr(bio)))

# 3. Definir área de estudio (Bounding Box + Buffer de 5 grados ~ 500 km)
pts <- vect(occs, geom=c("lon", "lat"), crs="EPSG:4326")
ext_pts <- ext(pts)
bbox <- ext(ext_pts[1]-5, ext_pts[2]+5, ext_pts[3]-5, ext_pts[4]+5)
bio_crop <- crop(bio, bbox)
message("Área de estudio (bbox): ", paste(round(as.numeric(bbox), 2), collapse=", "))

# 4. Limpieza espacial (1 registro por celda)
cells <- extract(bio_crop[[1]], pts, cells=TRUE)$cell
occs_clean <- occs[!duplicated(cells), ]
message(sprintf("Registros tras filtro espacial (1 por celda 2.5'): %d", nrow(occs_clean)))
write.csv(occs_clean, "o1_occs_clean.csv", row.names=FALSE)

# 5. Selección de variables mediante VIF sobre el paisaje (background)
set.seed(42)
bg_sample <- spatSample(bio_crop, size=10000, method="random", na.rm=TRUE, as.df=TRUE)
vif_res <- vifstep(bg_sample, th=10) # Umbral estricto para maxent
vars_selected <- vif_res@results$Variables

message("\nVariables no colineales seleccionadas:")
print(vars_selected)

bio_sel <- bio_crop[[vars_selected]]
writeRaster(bio_sel, "o1_envs_selected.tif", overwrite=TRUE)

# Registrar métricas al sistema
emit_metric("n_occs", nrow(occs_clean), label="Ocurrencias limpias", kind="count")
emit_metric("n_vars", length(vars_selected), label="Variables bioclimáticas", kind="count")
