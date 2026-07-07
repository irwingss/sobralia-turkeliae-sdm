# ═══════════════════════════════════════════════════════════
# Ressearch AI — Execution Record
# ═══════════════════════════════════════════════════════════
# Run ID:      fast-chat-run-1783442048461-8c8879
# Sequence:    #22
# Title:       Curvas de Respuesta Multi-Panel (ggplot2)
# Purpose:     Generar los datos de las curvas de respuesta del mejor modelo MaxEnt (AICc) y visualizar en un arreglo tipo multi-panel con ggplot2, aplicando directrices estéticas de alta calidad de publicación.
# Objective:   o5_response-curves-ggplot
# Language:    r
# Runtime:     modal · sdm-r
# Status:      succeeded
# Duration:    9330ms
# Started:     2026-07-07T16:35:16.411+00:00
# ═══════════════════════════════════════════════════════════
# Ressearch AI: emit_metric harvest no-op para ejecución offline
emit_metric <- function(name, value, ...) invisible(value)

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