# ═══════════════════════════════════════════════════════════
# Ressearch AI — Execution Record
# ═══════════════════════════════════════════════════════════
# Run ID:      fast-chat-run-1783441817940-cvcwqb
# Sequence:    #21
# Title:       Cargar objeto de ENMeval
# Purpose:     Cargar el objeto de ENMeval guardado para inspeccionar su estructura y configuraciones de hiperparámetros óptimos.
# Objective:   o5_response-curves-ggplot
# Language:    r
# Runtime:     modal · sdm-r
# Status:      succeeded
# Duration:    8720ms
# Started:     2026-07-07T16:30:54.059+00:00
# ═══════════════════════════════════════════════════════════
# Ressearch AI: emit_metric harvest no-op para ejecución offline
emit_metric <- function(name, value, ...) invisible(value)

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
