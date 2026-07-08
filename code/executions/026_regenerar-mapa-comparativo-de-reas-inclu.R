# ═══════════════════════════════════════════════════════════
# Ressearch AI — Execution Record
# ═══════════════════════════════════════════════════════════
# Run ID:      fast-chat-run-1783483452879-ulymgr
# Sequence:    #26
# Title:       Regenerar mapa comparativo con fondo de Sudamérica
# Purpose:     Regenerar mapa comparativo de áreas incluyendo el mapa base de Sudamérica para dar contexto cartográfico de fondo.
# Objective:   o5_biogeography_analysis
# Language:    r
# Runtime:     modal · sdm-r
# Status:      succeeded
# Duration:    6574ms
# Started:     2026-07-08T04:05:22.14+00:00
# ═══════════════════════════════════════════════════════════
# Ressearch AI: emit_metric harvest no-op para ejecución offline
emit_metric <- function(name, value, ...) invisible(value)

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