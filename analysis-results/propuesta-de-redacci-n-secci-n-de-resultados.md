## Sección de Resultados Propuesta

### Optimización y Rendimiento del Modelo

Se evaluó un total de 24 modelos de MaxEnt utilizando validación cruzada espacial con el paquete `ENMeval`. El modelo con el rendimiento óptimo, seleccionado por el menor valor del Criterio de Información de Akaike corregido (AICc), fue aquel que empleó características de respuesta lineales y cuadráticas (LQ) y un multiplicador de regularización de 0.5 (AICc = 459.96, w.AIC = 0.996).

El modelo final demostró un alto poder predictivo, alcanzando un valor promedio del Área Bajo la Curva (AUC) de **0.929** en los datos de validación y una tasa de omisión promedio de **0.083**, calculada sobre el umbral del 10% del percentil de presencia de los datos de entrenamiento.

### Contribución de las Variables Ambientales

El análisis de la importancia de las variables, medido por permutación (Tabla 1, Figura 1), reveló que no todas las variables contribuyeron de igual manera al modelo de distribución de *Sobralia turkeliae*. La isotermalidad (bio3) fue la variable más influyente, con una importancia por permutación de 0.50, seguida por el rango diurno de la temperatura (bio2) con 0.18 y la temperatura media del trimestre más húmedo (bio8) con 0.14. Las variables restantes, precipitación del trimestre más seco (bio19), precipitación del trimestre más cálido (bio18), precipitación del mes más seco (bio14) y estacionalidad de la temperatura (bio15), tuvieron una contribución considerablemente menor al modelo (importancia < 0.11).

---
**Tabla 1: Resultados de importancia de variables por permutación.**
(Corresponde al archivo `o5_variable_importance.csv`)

| variable | importance |
|:---|---:|
| bio3 | 0.4988365 |
| bio2 | 0.1843377 |
| bio8 | 0.1378396 |
| bio19 | 0.1093150 |
| bio18 | 0.0381622 |
| bio14 | 0.0210292 |
| bio15 | 0.0104798 |

**Figura 1: Gráfico de importancia de variables por permutación.**
(Corresponde al archivo `o5_variable_importance.png`)
