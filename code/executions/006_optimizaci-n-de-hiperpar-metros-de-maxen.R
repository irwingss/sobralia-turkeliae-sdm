# ═══════════════════════════════════════════════════════════
# Ressearch AI — Execution Record
# ═══════════════════════════════════════════════════════════
# Run ID:      fast-chat-run-1782404907536-jbl0om
# Sequence:    #6
# Title:       Tuning de hiperparámetros de MaxEnt
# Purpose:     Optimización de hiperparámetros de MaxEnt utilizando ENMeval con Feature Classes de baja/media complejidad y validación cruzada por bloques espaciales (Spatial Block CV) para seleccionar el modelo con el mejor delta AICc.
# Objective:   o2_enmeval-tuning
# Language:    r
# Runtime:     modal · sdm-r
# Status:      failed
# Duration:    31485ms
# Started:     2026-06-25T16:29:58.889+00:00
# ═══════════════════════════════════════════════════════════
# Ressearch AI: emit_metric harvest no-op para ejecución offline
emit_metric <- function(name, value, ...) invisible(value)

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
