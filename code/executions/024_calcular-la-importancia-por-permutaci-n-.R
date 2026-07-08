# ═══════════════════════════════════════════════════════════
# Ressearch AI — Execution Record
# ═══════════════════════════════════════════════════════════
# Run ID:      fast-chat-run-1783445357145-zejufs
# Sequence:    #24
# Title:       Calcular Importancia de Variables por Permutación
# Purpose:     Calcular la importancia por permutación de las variables del modelo óptimo y generar un gráfico de barras.
# Objective:   o5_results_synthesis
# Language:    r
# Runtime:     modal · sdm-r
# Status:      succeeded
# Duration:    7733ms
# Started:     2026-07-07T17:29:56.698+00:00
# ═══════════════════════════════════════════════════════════
# Ressearch AI: emit_metric harvest no-op para ejecución offline
emit_metric <- function(name, value, ...) invisible(value)

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
