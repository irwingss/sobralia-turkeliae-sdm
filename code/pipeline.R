# OK Modelado de nicho ecológico de Sobralia turkeliae — Unified R Analysis Pipeline
# Generado con asistencia de Ressearch AI (https://ressearchai.app).
# AVISO: este pipeline contiene código generado o asistido por IA.
# Debe ser validado por una persona responsable antes de su uso
# publicación, clínico, regulatorio o académico.
# Marco normativo: Ley N.° 31814 (Perú) y DS 115-2025-PCM.
# Para reproducir: 'make reproduce-r'.

PACKAGE_ROOT <- normalizePath(dirname(dirname(sys.frame(1)$ofile %||% '.')))
if (file.exists(file.path(PACKAGE_ROOT, 'MANIFEST.json'))) setwd(PACKAGE_ROOT)

check_hash <- function(path, expected) {
  if (!file.exists(path)) { message('⚠ ', path, ' not found'); return(FALSE) }
  actual <- digest::digest(file = path, algo = 'sha256')
  if (actual != expected) { message('❌ ', path, ' hash mismatch'); return(FALSE) }
  TRUE
}

# Random seed determinístico (derivado del project_id)
# Garantiza reproducibilidad bit-exacta entre re-ejecuciones
set.seed(95718229)
Sys.setenv(R_SEED = "95718229")

# Ressearch AI: emit_metric harvest no-op para ejecución offline
emit_metric <- function(name, value, ...) invisible(value)

# Ressearch AI: hook de error de reproducibilidad (añade contexto, no lo silencia)
local({
  .ressearch_prev_error <- getOption("error")
  options(error = function() {
    msg <- geterrmessage()
    if (grepl("object '.*' not found", msg)) {
      message("Este paso referencia estado de una sesión interactiva previa que no se recrea aquí. Ver README.md §Reproducibilidad.")
    }
    if (!is.null(.ressearch_prev_error)) {
      if (is.function(.ressearch_prev_error)) .ressearch_prev_error()
    }
  })
})

# ─── Step 1: Limpiar las coordenadas y seleccionar variables climáticas independientes para el modelo MaxEnt. Selecciono mediante VIF umbral 10 para reducir multicolinealidad. Descarto un buffer de 5 grados como extensión de background. ───────────────────
# Run ID: fast-chat-run-1782400208024-m1ocmo
# Objective: o1_data-prep

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


# ─── Step 2: Limpiar las coordenadas y seleccionar variables climáticas independientes para el modelo MaxEnt. Selecciono mediante VIF umbral 10 para reducir multicolinealidad. Descarto un buffer de 5 grados como extensión de background. ───────────────────
# Run ID: fast-chat-run-1782400312343-3ur9i4
# Objective: o1_data-prep

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
message("Área de estudio (bbox): ", paste(round(as.vector(bbox), 2), collapse=", "))

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

# ─── Step 5: Optimización de hiperparámetros de MaxEnt utilizando ENMeval con Feature Classes de baja/media complejidad y validación cruzada por bloques espaciales (Spatial Block CV) para seleccionar el modelo con el mejor delta AICc. ───────────────────
# Run ID: fast-chat-run-1782404740111-u0r7pc
# Objective: o2_enmeval-tuning

library(terra)
library(sf)
library(ENMeval)
library(maxnet)
library(dplyr)

# 1. Cargar datos preparados (occs y rasters limpios)
occs_clean <- read.csv("o1_occs_clean.csv")
envs <- terra::rast("o1_envs_selected.tif")

# 2. Extraer puntos de background dentro del extent
set.seed(42)
bg <- spatSample(envs, size = 10000, method = "random", na.rm = TRUE, xy = TRUE)
bg_coords <- as.data.frame(bg[, c("x", "y")])
colnames(bg_coords) <- c("lon", "lat")

# 3. Tuning de MaxEnt (ENMeval)
tune_grid <- list(
  fc = c("L", "LQ", "H", "LQH"),
  rm = seq(0.5, 3, by = 0.5)
)

message("Ajustando ", length(tune_grid$fc) * length(tune_grid$rm), " modelos en ENMeval (k-fold espacial por bloques)...")

# Validación espacial (block) muy útil para probar transferencia geográfica
e <- ENMevaluate(
  occs = occs_clean, 
  envs = envs, 
  bg = bg_coords, 
  algorithm = "maxnet", 
  partitions = "block", # Spatial block CV
  tune.args = tune_grid,
  parallel = FALSE # Previene OOM en el sandbox
)

# 4. Resultados de evaluación y modelo óptimo
res <- eval.results(e)
write.csv(res, "o2_enmeval_results.csv", row.names = FALSE)

# Seleccionar el mejor modelo según AICc
best_model_idx <- which.min(res$AICc)
best_settings <- res[best_model_idx, ]
message(sprintf("Mejor modelo: FC=%s, RM=%.1f (AUC_train=%.3f, AICc=%.2f, delta.AICc=0)", 
                best_settings$fc, best_settings$rm, best_settings$auc.train, best_settings$AICc))

# Gráfico de desempeño (tune args)
png("o2_enmeval_tuning.png", width=800, height=600, res=100)
evalplot.stats(e, stats = c("auc.val.avg", "AICc"), x.var = "rm", color.var = "fc")
dev.off()

# 5. Generar métricas formales
emit_metric("best_fc", best_settings$fc, label="Mejor Feature Class", kind="scalar")
emit_metric("best_rm", best_settings$rm, label="Mejor Reg. Multiplier", kind="scalar")
emit_metric("auc_val_avg", best_settings$auc.val.avg, label="AUC Validación (Media)", kind="scalar")

# Guardar el objeto ENMevaluation completo
saveRDS(e, "o2_enmeval_object.rds")


# ─── Step 6: Optimización de hiperparámetros de MaxEnt utilizando ENMeval con Feature Classes de baja/media complejidad y validación cruzada por bloques espaciales (Spatial Block CV) para seleccionar el modelo con el mejor delta AICc. ───────────────────
# Run ID: fast-chat-run-1782404907536-jbl0om
# Objective: o2_enmeval-tuning

library(terra)
library(sf)
library(ENMeval)
library(maxnet)
library(dplyr)

# 1. Cargar datos preparados (occs y rasters limpios)
occs_clean <- read.csv("o1_occs_clean.csv")
envs <- terra::rast("o1_envs_selected.tif")

# 2. Extraer puntos de background dentro del extent
set.seed(42)
bg <- spatSample(envs, size = 10000, method = "random", na.rm = TRUE, xy = TRUE)
bg_coords <- as.data.frame(bg[, c("x", "y")])
colnames(bg_coords) <- c("lon", "lat")

# 3. Tuning de MaxEnt (ENMeval)
tune_grid <- list(
  fc = c("L", "LQ", "H", "LQH"),
  rm = seq(0.5, 3, by = 0.5)
)

message("Ajustando ", length(tune_grid$fc) * length(tune_grid$rm), " modelos en ENMeval (k-fold espacial por bloques)...")

# Validación espacial (block) muy útil para probar transferencia geográfica
e <- ENMevaluate(
  occs = occs_clean, 
  envs = envs, 
  bg = bg_coords, 
  algorithm = "maxnet", 
  partitions = "block", # Spatial block CV
  tune.args = tune_grid,
  parallel = FALSE # Previene OOM en el sandbox
)

# 4. Resultados de evaluación y modelo óptimo
res <- eval.results(e)
write.csv(res, "o2_enmeval_results.csv", row.names = FALSE)

# Seleccionar el mejor modelo según AICc
best_model_idx <- which.min(res$AICc)
best_settings <- res[best_model_idx, ]
message(sprintf("Mejor modelo: FC=%s, RM=%.1f (AUC_train=%.3f, AICc=%.2f, delta.AICc=0)", 
                best_settings$fc, best_settings$rm, best_settings$auc.train, best_settings$AICc))

# Gráfico de desempeño (tune args)
png("o2_enmeval_tuning.png", width=800, height=600, res=100)
evalplot.stats(e, stats = c("auc.val.avg", "AICc"), x.var = "rm", color.var = "fc")
dev.off()

# 5. Generar métricas formales
emit_metric("best_fc", best_settings$fc, label="Mejor Feature Class", kind="scalar")
emit_metric("best_rm", best_settings$rm, label="Mejor Reg. Multiplier", kind="scalar")
emit_metric("auc_val_avg", best_settings$auc.val.avg, label="AUC Validación (Media)", kind="scalar")

# Guardar el objeto ENMevaluation completo
saveRDS(e, "o2_enmeval_object.rds")


# ─── Step 7: Recuperar los resultados del tuning asíncrono para verificar si el objeto ENMeval se guardó correctamente y extraer las métricas de evaluación del modelo seleccionado antes de avanzar a las predicciones finales. ───────────────────
# Run ID: fast-chat-run-1782423507209-hpgyni
# Objective: o2_enmeval-tuning-check

library(terra)
library(sf)
library(ENMeval)
library(maxnet)

# 1. Cargar el objeto guardado o los resultados si falló la re-carga de la sesión
if(file.exists("o2_enmeval_object.rds")) {
  e <- readRDS("o2_enmeval_object.rds")
  res <- eval.results(e)
  
  best_model_idx <- which.min(res$AICc)
  best_settings <- res[best_model_idx, ]
  
  message(sprintf("Mejor modelo recuperado: FC=%s, RM=%.1f (AUC_train=%.3f, AICc=%.2f, delta.AICc=0)", 
                  best_settings$fc, best_settings$rm, best_settings$auc.train, best_settings$AICc))
  
  # Registrar métricas al vuelo si no se emitieron antes
  emit_metric("best_fc", best_settings$fc, label="Mejor Feature Class", kind="scalar")
  emit_metric("best_rm", best_settings$rm, label="Mejor Reg. Multiplier", kind="scalar")
  emit_metric("auc_val_avg", best_settings$auc.val.avg, label="AUC Validación (Media)", kind="scalar")
  
  # También mostrar la matriz de confusión (Omission Rates) del modelo ganador
  message(sprintf("Tasa de omisión al 10%% (OR.10p): %.3f", best_settings$or.10p.avg))
  
} else {
  message("El objeto ENMeval no existe, el run anterior falló en guardar el output.")
}


# ─── Step 8: Exportar la predicción del mejor modelo a ASCII y GeoTIFF, graficar el mapa de idoneidad final y extraer las curvas de respuesta para interpretación de nicho. ───────────────────
# Run ID: fast-chat-run-1782423677634-8cgsw5
# Objective: o3_enmeval-export

library(terra)
library(sf)
library(ENMeval)
library(maxnet)

# 1. Recuperar el modelo ganador de ENMeval
e <- readRDS("o2_enmeval_object.rds")
res <- eval.results(e)
best_idx <- which.min(res$AICc)
best_tune <- res$tune.args[best_idx]
best_fc <- as.character(res$fc[best_idx])
best_rm <- res$rm[best_idx]

message(sprintf("Parámetros óptimos -> FC: %s, RM: %s", best_fc, best_rm))

# 2. Extraer predicción ya calculada por ENMeval
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
vars_importantes <- unique(gsub("\\^2|\\:.*|\\(.*\\)", "", names(betas)))
message("\nVariables bioclimáticas que están impulsando el modelo (con coeficientes activos):")
print(vars_importantes)

# 7. Registrar para métricas
emit_metric("best_aic", res$AICc[best_idx], label="AICc del mejor modelo", kind="scalar")


# ─── Step 9: Exportar la predicción del mejor modelo a ASCII y GeoTIFF, graficar el mapa de idoneidad final y extraer las curvas de respuesta para interpretación de nicho. ───────────────────
# Run ID: fast-chat-run-1782423738560-0oli21
# Objective: o3_enmeval-export

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

# ─── Step 10: Exportar la predicción del mejor modelo a ASCII y GeoTIFF, graficar el mapa de idoneidad final y extraer las curvas de respuesta para interpretación de nicho. ───────────────────
# Run ID: fast-chat-run-1782423823987-lxln6a
# Objective: o3_enmeval-export

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

# ─── Step 11: Exportar la predicción del mejor modelo a ASCII y GeoTIFF, graficar el mapa de idoneidad final y extraer las curvas de respuesta para interpretación de nicho. ───────────────────
# Run ID: fast-chat-run-1782423869896-lxsgja
# Objective: o3_enmeval-export

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
bg <- e@bg

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

# ─── Step 12: Generar mapa final en GeoTIFF y ASCII, curvas de respuesta y extraer métricas del mejor modelo de nicho ───────────────────
# Run ID: fast-chat-run-1782439168655-ahocd4
# Objective: o3_outputs

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

# ─── Step 13: Generar mapa final en GeoTIFF y ASCII, curvas de respuesta y extraer métricas del mejor modelo de nicho ───────────────────
# Run ID: fast-chat-run-1782439289599-1la9gf
# Objective: o3_outputs

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
# NOTA: No usamos eval.predictions(e) porque los SpatRaster pierden sus punteros C++ al guardarse en RDS.
# En su lugar, extraemos el modelo y re-predicemos sobre los predictores ambientales.
best_mod <- eval.models(e)[[best_tune]]
best_pred <- terra::predict(envs, best_mod, type="cloglog")

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

# ─── Step 14: Generar mapa final en GeoTIFF y ASCII, curvas de respuesta y extraer métricas del mejor modelo de nicho ───────────────────
# Run ID: fast-chat-run-1782439329881-n72hv3
# Objective: o3_outputs

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
# NOTA: No usamos eval.predictions(e) porque los SpatRaster pierden sus punteros C++ al guardarse en RDS.
# En su lugar, extraemos el modelo y re-predicemos sobre los predictores ambientales.
best_mod <- eval.models(e)[[best_tune]]
best_pred <- terra::predict(envs, best_mod, type="cloglog", na.rm=TRUE)

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

# ─── Step 15: Generar ASC, mapas PNG y curvas de respuesta que quedaron pendientes por el fallo de GDAL ───────────────────
# Run ID: fast-chat-run-1782439426948-5o981v
# Objective: o3_outputs

library(terra)
library(ENMeval)
library(maxnet)

e <- readRDS("o2_enmeval_object.rds")
envs <- rast("o1_envs_selected.tif")
occs <- read.csv("o1_occs_clean.csv")

# Extraer el mejor modelo
res <- eval.results(e)
best_tune <- as.character(res$tune.args[which.min(res$delta.AICc)])
best_pred <- eval.predictions(e)[[best_tune]]
best_mod <- eval.models(e)[[best_tune]]

# Generar el archivo ASCII correctamente
message("Exportando a formato ASCII...")
writeRaster(best_pred, "o3_Sobralia_turkeliae_suitability.asc", filetype="AAIGrid", NAflag=-9999, overwrite=TRUE)

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

# ─── Step 16: Generar ASC, mapas PNG y curvas de respuesta que quedaron pendientes por el fallo de GDAL ───────────────────
# Run ID: fast-chat-run-1782439590556-dx9y7a
# Objective: o3_outputs

library(terra)
library(ENMeval)
library(maxnet)

e <- readRDS("o2_enmeval_object.rds")
envs <- rast("o1_envs_selected.tif")
occs <- read.csv("o1_occs_clean.csv")

# Extraer el mejor modelo
res <- eval.results(e)
best_tune <- as.character(res$tune.args[which.min(res$delta.AICc)])
best_pred <- eval.predictions(e)[[best_tune]]
best_mod <- eval.models(e)[[best_tune]]

# Generar el archivo ASCII correctamente
message("Exportando a formato ASCII...")
writeRaster(best_pred, "o3_Sobralia_turkeliae_suitability.asc", filetype="AAIGrid", NAflag=-9999, overwrite=TRUE)

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

# ─── Step 17: Generar ASC, mapas PNG y curvas de respuesta que quedaron pendientes por el fallo de GDAL ───────────────────
# Run ID: fast-chat-run-1782439654151-q4snns
# Objective: o3_outputs

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

# ─── Step 18: Generar ASC, mapas PNG y curvas de respuesta que quedaron pendientes por el fallo de GDAL ───────────────────
# Run ID: fast-chat-run-1782439705761-pis2m8
# Objective: o3_outputs

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

# Re-predecir el modelo porque los objetos SpatRaster guardados con saveRDS() 
# pierden sus punteros C++ (nil pointer) al ser cargados en una nueva sesión.
message("Generando predicción espacial del mejor modelo...")
best_pred <- terra::predict(envs, best_mod, type="cloglog", na.rm=TRUE)

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

# ─── Step 20: Generar mapa de idoneidad usando la escala logística y preservar el estilo gráfico exacto ───────────────────
# Run ID: fast-chat-run-1782966023038-fz7ho6
# Objective: o4_logistic_map

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

message('\n✅ R pipeline complete.')