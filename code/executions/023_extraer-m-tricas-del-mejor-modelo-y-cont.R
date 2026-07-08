# ═══════════════════════════════════════════════════════════
# Ressearch AI — Execution Record
# ═══════════════════════════════════════════════════════════
# Run ID:      fast-chat-run-1783445275382-6b5rgp
# Sequence:    #23
# Title:       Extraer métricas del modelo óptimo
# Purpose:     Extraer métricas del mejor modelo y contribución de variables para redactar la sección de resultados.
# Objective:   o5_results_synthesis
# Language:    r
# Runtime:     modal · sdm-r
# Status:      succeeded
# Duration:    8958ms
# Started:     2026-07-07T17:28:32.758+00:00
# ═══════════════════════════════════════════════════════════
# Ressearch AI: emit_metric harvest no-op para ejecución offline
emit_metric <- function(name, value, ...) invisible(value)

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
