# ═══════════════════════════════════════════════════════════
# Ressearch AI — Execution Record
# ═══════════════════════════════════════════════════════════
# Run ID:      fast-chat-run-1782423507209-hpgyni
# Sequence:    #7
# Title:       Verificar resultados del Tuning de MaxEnt
# Purpose:     Recuperar los resultados del tuning asíncrono para verificar si el objeto ENMeval se guardó correctamente y extraer las métricas de evaluación del modelo seleccionado antes de avanzar a las predicciones finales.
# Objective:   o2_enmeval-tuning-check
# Language:    r
# Runtime:     modal · sdm-r
# Status:      succeeded
# Duration:    10924ms
# Started:     2026-06-25T21:39:09.775+00:00
# ═══════════════════════════════════════════════════════════
# Ressearch AI: emit_metric harvest no-op para ejecución offline
emit_metric <- function(name, value, ...) invisible(value)

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
