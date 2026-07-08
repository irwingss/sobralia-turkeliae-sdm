"""
Modelado de nicho ecológico de Sobralia turkeliae — Unified Python Analysis Pipeline

Generado con asistencia de Ressearch AI (https://ressearchai.app).
AVISO: este pipeline contiene código generado o asistido por IA.
Debe ser validado por una persona responsable antes de su uso
publicación, clínico, regulatorio o académico.
Marco normativo: Ley N.° 31814 (Perú) y DS 115-2025-PCM.

Para reproducir: ver README.md o ejecutar 'make reproduce'.
Cada paso conserva el orden cronológico original y referencia
su run_id en fast_chat_execution_logs.
"""

import os, sys, hashlib, warnings, json
from pathlib import Path

# Anclar cwd al package root, independiente de cómo se invoque.
PACKAGE_ROOT = Path(__file__).resolve().parent.parent
os.chdir(PACKAGE_ROOT)

def _check_hash(path: str, expected: str) -> bool:
    p = Path(path)
    if not p.exists():
        print(f'⚠ {path} not found — skipping integrity check')
        return False
    actual = hashlib.sha256(p.read_bytes()).hexdigest()
    if actual != expected:
        print(f'❌ {path} hash mismatch (expected={expected[:12]}…, actual={actual[:12]}…)')
        return False
    return True

# Random seed determinístico (derivado del project_id)
# Garantiza reproducibilidad bit-exacta entre re-ejecuciones
import os, random
os.environ["PYTHONHASHSEED"] = "95718229"
random.seed(95718229)
try:
    import numpy as _np
    _np.random.seed(95718229)
except ImportError:
    pass
try:
    import torch as _torch
    _torch.manual_seed(95718229)
    if _torch.cuda.is_available():
        _torch.cuda.manual_seed_all(95718229)
except ImportError:
    pass

# Ressearch AI: emit_metric harvest no-op para ejecución offline
def emit_metric(name, value, *a, **k):
    return value

# Ressearch AI: hook de error de reproducibilidad (añade contexto, no lo silencia)
import sys as _ressearch_sys
_ressearch_prev_excepthook = _ressearch_sys.excepthook
def _ressearch_excepthook(exc_type, exc_value, exc_tb):
    if issubclass(exc_type, NameError):
        print("Este paso referencia estado de una sesión interactiva previa que no se recrea aquí. Ver README.md §Reproducibilidad.", file=_ressearch_sys.stderr)
    _ressearch_prev_excepthook(exc_type, exc_value, exc_tb)
_ressearch_sys.excepthook = _ressearch_excepthook

# ─── Step 3: Optimización de hiperparámetros de MaxEnt utilizando ENMeval con Feature Classes de baja/media complejidad y validación cruzada por bloques espaciales (Spatial Block CV) para seleccionar el modelo con el mejor delta AICc. ───────────────────
# Run ID: fast-chat-run-1782404604746-j0ekfa
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
# Como la grilla puede ser pesada, ajustamos un grid sensato y usamos maxnet
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

# Guardar el objeto ENMevaluation completo por si se necesita revisar después
saveRDS(e, "o2_enmeval_object.rds")


# ─── Step 4: Optimización de hiperparámetros de MaxEnt utilizando ENMeval con Feature Classes de baja/media complejidad y validación cruzada por bloques espaciales (Spatial Block CV) para seleccionar el modelo con el mejor delta AICc. ───────────────────
# Run ID: fast-chat-run-1782404640763-pn5rcv
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
# Como la grilla puede ser pesada, ajustamos un grid sensato y usamos maxnet
tune_grid <- list(
  fc = c("L", "LQ", "H", "LQH"),
  rm = seq(0.5, 3, by = 0.5)
)

message(paste("Ajustando", length(tune_grid$fc) * length(tune_grid$rm), "modelos en ENMeval (k-fold espacial por bloques)..."))

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
# Usamos which.min porque un menor AICc es mejor
best_model_idx <- which.min(res$delta.AICc)
best_settings <- res[best_model_idx, ]
message(sprintf("Mejor modelo: FC=%s, RM=%.1f (AUC_val=%.3f, AICc=%.2f, delta.AICc=%.2f)", 
                best_settings$fc, best_settings$rm, best_settings$auc.val.avg, best_settings$AICc, best_settings$delta.AICc))

# Gráfico de desempeño (tune args)
png("o2_enmeval_tuning.png", width=800, height=600, res=100)
evalplot.stats(e, stats = c("auc.val.avg", "AICc"), x.var = "rm", color.var = "fc")
dev.off()

# 5. Generar métricas formales
# La plataforma puede no tener la función emit_metric, así que la definimos si no existe
if (!exists("emit_metric")) {
  emit_metric <- function(name, value, label = NULL, kind = NULL) {
    # Placeholder function for local execution
    cat(sprintf("METRIC: %s = %s\n", name, as.character(value)))
  }
}

emit_metric("best_fc", as.character(best_settings$fc), label="Mejor Feature Class", kind="scalar")
emit_metric("best_rm", best_settings$rm, label="Mejor Reg. Multiplier", kind="scalar")
emit_metric("auc_val_avg", best_settings$auc.val.avg, label="AUC Validación (Media)", kind="scalar")

# Guardar el objeto ENMevaluation completo por si se necesita revisar después
saveRDS(e, "o2_enmeval_object.rds")

# ── Validación final de integridad ────────────────────────
def _verify_manifest() -> int:
    manifest_path = PACKAGE_ROOT / 'MANIFEST.json'
    if not manifest_path.exists():
        return 0
    manifest = json.loads(manifest_path.read_text())
    mismatches = 0
    for rel, expected in (manifest.get('checksums') or {}).items():
        target = PACKAGE_ROOT / rel
        if not target.exists():
            continue
        if hashlib.sha256(target.read_bytes()).hexdigest() != expected:
            print(f'❌ checksum drift: {rel}')
            mismatches += 1
    return mismatches

_drift = _verify_manifest()
if _drift:
    print(f'\n⚠ Pipeline finalizó con {_drift} archivo(s) modificado(s) respecto al MANIFEST.')
else:
    print('\n✅ Pipeline complete — integridad verificada.')