# Modelado de nicho ecológico de Sobralia turkeliae — Unified R Analysis Pipeline
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

# ─── Step 21: Cargar el objeto de ENMeval guardado para inspeccionar su estructura y configuraciones de hiperparámetros óptimos. ───────────────────
# Run ID: fast-chat-run-1783441817940-cvcwqb
# Objective: o5_response-curves-ggplot

# Cargar librerías necesarias
library(ENMeval)
library(terra)
library(ggplot2)

# Inspeccionar el estado del kernel vivo o cargar el objeto guardado
if (file.exists("o2_enmeval_object.rds")) {
  message("Cargando objeto ENMeval desde disco...")
  e <- readRDS("o2_enmeval_object.rds")
  print(e)
} else {
  stop("No se encontró el objeto o2_enmeval_object.rds en el workspace.")
}


# ─── Step 22: Generar los datos de las curvas de respuesta del mejor modelo MaxEnt (AICc) y visualizar en un arreglo tipo multi-panel con ggplot2, aplicando directrices estéticas de alta calidad de publicación. ───────────────────
# Run ID: fast-chat-run-1783442048461-8c8879
# Objective: o5_response-curves-ggplot

library(ENMeval)
library(terra)
library(ggplot2)
library(dplyr)
library(maxnet) # Necesario para predecir con el objeto maxnet de ENMeval

# 1. Cargar el objeto ENMeval y los predictores
message("Cargando modelo óptimo y variables...")
e <- readRDS("o2_enmeval_object.rds")
envs_selected <- terra::rast("o1_envs_selected.tif")

# 2. Extraer el mejor modelo basado en AICc
results <- eval.results(e)
best_tune <- as.character(results$tune.args[which.min(results$delta.AICc)])
mod_opt <- eval.models(e)[[best_tune]]

message(sprintf("Modelo óptimo seleccionado: %s", best_tune))

# 3. Calcular las medias de cada predictor para usarlas como baseline
predictor_vars <- names(envs_selected)
means_df <- global(envs_selected, "mean", na.rm = TRUE)

# 4. Generar datos predictivos por variable
message("Calculando curvas de respuesta individuales...")
response_list <- list()

for (var in predictor_vars) {
  # Definir el rango empírico de la variable
  rng <- minmax(envs_selected[[var]])
  var_seq <- seq(rng[1], rng[2], length.out = 100)
  
  # Crear un dataframe baseline donde todas las variables están en su promedio
  pred_df <- data.frame(matrix(ncol = length(predictor_vars), nrow = 100))
  colnames(pred_df) <- predictor_vars
  
  for (p in predictor_vars) {
    if (p == var) {
      pred_df[[p]] <- var_seq
    } else {
      pred_df[[p]] <- means_df[p, "mean"]
    }
  }
  
  # Predecir usando el modelo maxnet
  suit <- as.numeric(predict(mod_opt, newdata = pred_df, type = "cloglog"))
  
  response_list[[var]] <- data.frame(variable = var, value = var_seq, suitability = suit)
}

# Consolidar y guardar
response_curves_df <- bind_rows(response_list)
readr::write_csv(response_curves_df, "o5_response_curves_data.csv")

# 5. Generar la figura con ggplot2 bajo estética Ressearch AI
message("Generando gráfico multi-panel...")
theme_ressearch <- theme(
  plot.background = element_rect(fill = "#ffffff", color = NA),
  panel.background = element_rect(fill = "#ffffff", color = NA),
  panel.grid.major.y = element_line(color = "#d9deea", linewidth = 0.5, linetype = "solid"),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  axis.line.x = element_line(color = "#2a3340", linewidth = 0.9),
  axis.line.y = element_line(color = "#2a3340", linewidth = 0.9),
  plot.title = element_text(color = "#0b1a3a", size = 11, face = "bold", hjust = 0),
  plot.subtitle = element_text(color = "#4a5568", size = 9),
  axis.title = element_text(color = "#1a2233", size = 9.5),
  axis.text = element_text(color = "#1a2233", size = 8.5),
  strip.background = element_rect(fill = "transparent", color = NA),
  strip.text = element_text(color = "#0b1a3a", size = 10, face = "bold"),
  legend.position = "none"
)

p <- ggplot(response_curves_df, aes(x = value, y = suitability)) +
  geom_line(color = "#2e65dc", linewidth = 1.2, lineend = "round", linejoin = "round") +
  facet_wrap(~variable, scales = "free_x") +
  labs(
    title = "Curvas de Respuesta de Idoneidad Ambiental para Sobralia turkeliae",
    subtitle = sprintf("Modelo óptimo (%s, cloglog). Los demás predictores se mantienen en su valor promedio local.", best_tune),
    x = "Valor de la Variable Bioclimática",
    y = "Idoneidad Predicha"
  ) +
  theme_ressearch

ggsave("o5_response_curves.png", p, width = 10, height = 5.625, dpi = 300, bg = "#ffffff")
message("Figura exportada con éxito: o5_response_curves.png")

# ─── Step 23: Extraer métricas del mejor modelo y contribución de variables para redactar la sección de resultados. ───────────────────
# Run ID: fast-chat-run-1783445275382-6b5rgp
# Objective: o5_results_synthesis

library(ENMeval)
library(dplyr)
library(terra)

# 1. Extraer N de ocurrencias y variables
occs <- read.csv("o1_occs_clean.csv")
envs <- terra::rast("o1_envs_selected.tif")

cat("--- OCURRENCIAS Y VARIABLES ---\n")
cat("N ocurrencias limpias:", nrow(occs), "\n")
cat("Variables seleccionadas:", paste(names(envs), collapse=", "), "\n\n")

# 2. Resultados del modelo óptimo
res <- read.csv("o2_enmeval_results.csv")
best_res <- res %>% arrange(delta.AICc) %>% slice(1)
cat("--- MÉTRICAS DEL MODELO ÓPTIMO ---\n")
print(best_res)
cat("\n")

# 3. Importancia de variables
e <- readRDS("o2_enmeval_object.rds")
best_model_name <- best_res$tune.args
cat("--- IMPORTANCIA DE VARIABLES (", best_model_name, ") ---\n")
var_imp <- e@variable.importance[[best_model_name]]
if (!is.null(var_imp)) {
  print(var_imp)
} else {
  cat("La importancia de variables no está directamente en el slot de ENMeval. Evaluando si es maxnet...\n")
  # Si es maxnet sin var.imp calculado, mostramos los coeficientes no nulos
  mod <- e@models[[best_model_name]]
  if(inherits(mod, "maxnet")) {
     cat("Modelo maxnet. Variables en el modelo:\n")
     print(names(mod$samplemeans))
  }
}


# ─── Step 24: Calcular la importancia por permutación de las variables del modelo óptimo y generar un gráfico de barras. ───────────────────
# Run ID: fast-chat-run-1783445357145-zejufs
# Objective: o5_results_synthesis

library(ENMeval)
library(dplyr)
library(terra)
library(ggplot2)

# Cargar resultados previos
e <- readRDS("o2_enmeval_object.rds")
res <- read.csv("o2_enmeval_results.csv")
best_model_name <- res %>% arrange(delta.AICc) %>% slice(1) %>% pull(tune.args)

# Extraer el mejor modelo
mod <- e@models[[best_model_name]]

# Extraer datos de evaluación del objeto ENMeval
occs_test <- e@occ.test
bg_test <- e@bg.coords

# Crear un data frame para la evaluación
eval_data <- rbind(occs_test, bg_test)
presence <- c(rep(1, nrow(occs_test)), rep(0, nrow(bg_test)))

# Calcular la importancia por permutación
var_imp <- dismo::evaluate(p = occs_test, a = bg_test, model = mod, x = e@env)@cor

var_imp_df <- data.frame(
  variable = names(e@env),
  importance = var_imp
) %>% arrange(desc(importance))

# Guardar la tabla de importancia
write.csv(var_imp_df, "o5_variable_importance.csv", row.names = FALSE)

# Visualizar y guardar
gg_imp <- ggplot(var_imp_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity", fill = "#2e65dc") +
  coord_flip() +
  labs(
    title = "Importancia de las Variables (Permutación)",
    subtitle = "Para el modelo óptimo de S. turkeliae (fc.LQ_rm.0.5)",
    x = "Variable Bioclimática",
    y = "Importancia por Permutación"
  ) +
  theme_minimal(base_size = 12) +
  theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size=10)
  )

ggsave("o5_variable_importance.png", plot = gg_imp, width = 10, height = 6, dpi = 300)

# Imprimir la tabla en consola
cat("\n--- IMPORTANCIA DE VARIABLES (PERMUTACIÓN) ---\n")
print(var_imp_df)


# ─── Step 25: Calcular el área de distribución usando Convex Hull y umbralización del SDM logístico al 10th percentile ───────────────────
# Run ID: fast-chat-run-1783483031803-3xs78e
# Objective: o5_biogeography_analysis

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

# ─── Step 26: Regenerar mapa comparativo de áreas incluyendo el mapa base de Sudamérica para dar contexto cartográfico de fondo. ───────────────────
# Run ID: fast-chat-run-1783483452879-ulymgr
# Objective: o5_biogeography_analysis

library(terra)
library(sf)
library(readr)
library(dplyr)
library(maps)

# 1. Cargar presencia y calcular Convex Hull
occs <- read_csv("o1_occs_clean.csv", show_col_types = FALSE)
lon_col <- intersect(names(occs), c("Longitude", "lon", "x"))[1]
lat_col <- intersect(names(occs), c("Latitude", "lat", "y"))[1]
pts <- st_as_sf(occs, coords = c(lon_col, lat_col), crs = 4326)

hull <- st_convex_hull(st_union(pts))
area_hull_km2 <- as.numeric(st_area(hull)) / 1e6

# 2. Cargar SDM y binarizar
r_log <- rast("o4_Sobralia_turkeliae_suitability_logistic.tif")
pts_spat <- vect(occs, geom=c(lon_col, lat_col), crs="EPSG:4326")
pred_vals <- extract(r_log, pts_spat)[[2]]
thresh_10 <- quantile(pred_vals, probs = 0.1, na.rm = TRUE)

r_bin <- r_log >= thresh_10
names(r_bin) <- "Presence"
areas <- expanse(r_bin, byValue=TRUE, unit="km")
area_sdm_km2 <- areas$area[areas$value %in% c(1, TRUE)]

# 3. Preparar mapa base de Sudamérica usando el paquete preinstalado 'maps'
world_map <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))
sa_countries <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", 
                  "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela", 
                  "French Guiana", "Falkland Islands")
south_america <- world_map %>% filter(ID %in% sa_countries)

# Definir la extensión geográfica de interés enfocada en la distribución de la especie y alrededores
bbox <- st_bbox(hull)
# Ampliar el bbox un poco para dar contexto regional, pero manteniendo el foco
xlim <- c(bbox["xmin"] - 4, bbox["xmax"] + 4)
ylim <- c(bbox["ymin"] - 4, bbox["ymax"] + 4)

# 4. Generar el gráfico comparativo con mapa base
png("o5_areas_comparacion.png", width=2000, height=1000, res=200)
par(mfrow=c(1,2), mar=c(3,3,4,3), bg="white", family="sans")

# Panel 1: Convex Hull
plot(st_geometry(south_america), col="#f2f2f2", border="#cccccc", lwd=0.5, 
     xlim=xlim, ylim=ylim, 
     main=sprintf("Minimum Convex Hull\nÁrea: %s km²", formatC(round(area_hull_km2, 1), format="f", big.mark=",")),
     col.main="#0b1a3a", cex.main=1.1, axes=TRUE)
plot(hull, col=adjustcolor("#2e65dc", alpha.f=0.25), border="#2e65dc", lwd=1.5, add=TRUE)
plot(st_geometry(pts), add=TRUE, pch=21, col="white", bg="#ec0f01", cex=1.1, lwd=0.8)

# Panel 2: SDM Binario
# terra::plot dibuja rásters. Para que use el bbox y respete el fondo:
# Graficamos primero el mapa base de Sudamérica para establecer los límites
plot(st_geometry(south_america), col="#f2f2f2", border="#cccccc", lwd=0.5, 
     xlim=xlim, ylim=ylim, 
     main=sprintf("SDM Binarizado (Umbral 10%%)\nÁrea: %s km²", formatC(round(area_sdm_km2, 1), format="f", big.mark=",")),
     col.main="#0b1a3a", cex.main=1.1, axes=TRUE)

# Convertir el raster a dataframe de celdas "Presencia" para pintarlas como polígonos o usar el raster directamente
# Para pintar el raster transparente encima del mapa base de fondo:
# Generamos una paleta donde 0 es transparente y 1 es verde
r_bin_colors <- c(adjustcolor("#ffffff", alpha.f=0.0), adjustcolor("#01781b", alpha.f=0.7))
plot(r_bin, col=r_bin_colors, legend=FALSE, add=TRUE)
plot(st_geometry(pts), add=TRUE, pch=21, col="white", bg="#ec0f01", cex=0.8, lwd=0.5)

dev.off()
cat("Mapas regenerados con éxito incluyendo Sudamérica de fondo.\n")

message('\n✅ R pipeline complete.')