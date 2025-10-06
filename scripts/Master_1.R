##########################################################
# Taller 1 - Económia Urbana
# Ejercicio 2
# author: Eimmy Nicoll Tovar Escobar y Juan Sebastian Tellez Melo
##########################################################

# Clean the workspace -----------------------------------------------------

rm(list=ls())

# Definir directorios -----------------------------------------------------

users <- tolower(Sys.info()[["user"]])  

rutas <- list(
  usuario = "C:/Users/Usuario/OneDrive - RADDAR/Documentos/Documents/Sebastian Tellez/MAESTRIA/ECONOMIA URBANA/TALLER/TALLER 1/Taller_1_EU_Punto_2/",
  mora  = "C:/Users/mora/Path/To/TALLER/TALLER 1/"
)

root <- rutas[[users]]

setwd(root)

stores <- file.path(root, "stores")
scripts <- file.path(root, "scripts")
views <- file.path(root, "views")


# Load Packages -----------------------------------------------------------

#install.packages("pacman")
require("pacman")

p_load(rio, 
       dplyr, 
       ggplot2, 
       viridis, 
       forcats,
       leaflet,
       sf,
       here,
       scales,
       gt,
       tidyr,
       osmdata,
       fixest
       )

# Cargar datos -----------------------------------------------------------

data_input <- import("stores/dataTaller01_Amenidades.rds") %>% as_tibble()

# Limpieza y transformación -----------------------------------------------------------

# Filtramos datos sin NA en price y superficie, y sin ceros o negativos

housing_data <- data_input %>% 
  filter(!is.na(price), !is.na(surface_total)) %>% 
  filter(price > 0, surface_total > 0)

# Identificación de outliers

iqr_surface <- IQR(housing_data$surface_total, na.rm = TRUE)

housing_data <- housing_data %>%
  mutate(outlier_surface = ifelse(surface_total > 4 * iqr_surface, 1, 0))

table(housing_data$outlier_surface)

# Categorización por cuantiles

q_surface <- quantile(housing_data$surface_total, na.rm = TRUE)

housing_data <- housing_data %>% 
  mutate(surface_q = case_when(
    surface_total < q_surface[2] ~ "Q1",
    surface_total < q_surface[3] ~ "Q2",
    surface_total < q_surface[4] ~ "Q3",
    TRUE ~ "Q4"
  ))

# Clasificación tipo de edifició (casa, apartamento, comercial) - Para ver detalle del codigo consultar script en carpeta "scripts"

source(file.path(scripts, "extraccion_tipo.R"))

table(housing_data$tipo)

# Creamos variable logarítmica

housing_data <- housing_data %>% 
  mutate(log_price = log(price),
         log_surface = log(surface_total))

# Visualización descriptivan -----------------------------------------------------------

# Relación log–log entre superficie y precio

ggplot(housing_data, aes(x = log_surface, y = log_price)) +
  # nube de puntos ligera
  geom_point(alpha = 0.15, size = 0.6) +
  # capas de densidad para ver concentraciones
  stat_density_2d(aes(fill = after_stat(level)),
                  geom = "polygon", alpha = 0.18, contour_var = "ndensity",
                  show.legend = FALSE) +
  
  scale_fill_viridis_c(option = "C", guide = "none") +
  # recta de tendencia por panel
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9, color = "black") +
  # paneles por tipo de operación (mismos límites para comparar)
  facet_wrap(~ operation, ncol = 2, scales = "fixed") +
  labs(
    title = "Relación log–log entre superficie y precio",
    subtitle = "Paneles por operación (Venta vs. Arriendo)",
    x = "Log(Superficie total)",
    y = "Log(Precio)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# Boxplot por cuartiles de superficie

ggplot(housing_data, aes(x = surface_q, y = log_price, fill = surface_q)) +
  geom_violin(trim = FALSE, width = 0.9, alpha = 0.15, color = NA) +
  geom_boxplot(width = 0.55, notch = TRUE, outlier.alpha = 0.12) +
  stat_summary(fun = mean, geom = "point",
               shape = 21, size = 2, stroke = 0.3,
               fill = "white", color = "black") +
  scale_x_discrete(limits = c("Q1","Q2","Q3","Q4")) +
  scale_fill_viridis(discrete = TRUE, option = "C", guide = "none") +
  facet_wrap(~ operation, ncol = 2, scales = "fixed") +
  labs(
    title = "Distribución de precios por cuartiles de superficie",
    subtitle = "Paneles por operación (Venta vs. Alquiler)",
    x = "Cuartil de superficie",
    y = "Log(Precio)"
  ) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

# Boxplot por tipo de propiedad

ggplot(
  housing_data %>%
    filter(is.finite(price)) %>%
    mutate(
      operation = factor(operation, levels = c("Venta","Alquiler"))
    ),
  aes(
    x = log(price),
    y = fct_reorder(tipo, price, .fun = median, .desc = FALSE)
  )
) +
  # forma de la distribución
  geom_violin(trim = FALSE, alpha = 0.15, color = NA, fill = "#6C8EBF") +
  # caja (dispersión) con outliers atenuados
  geom_boxplot(width = 0.55, notch = TRUE, outlier.alpha = 0.12, color = "grey20", fill = "grey90") +
  # punto de la media
  stat_summary(fun = mean, geom = "point",
               shape = 21, size = 2.2, stroke = 0.3,
               fill = "white", color = "black") +
  facet_wrap(~ operation, ncol = 2, scales = "fixed") +
  labs(
    title = "Distribución de precios por tipo de propiedad",
    subtitle = "Paneles por operación (Venta vs. Alquiler)\nViolin = forma | Boxplot = dispersión | Punto = media",
    x = "Log(Precio)",
    y = "Tipo de propiedad (ordenado por mediana)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


# Estadísticas descriptivas por operation × tipo

source(file.path(scripts, "Tabla_est_descriptivas.R"))

gt_tbl_propiedades
gt::gtsave(gt_tbl_propiedades, filename = file.path(views, "descriptivas_tipo_operacion.png"))


# Shapefile base precios  -----------------------------------------------------------

#Pasar la base de precios a sf

housing_data_sf <- st_as_sf(housing_data, coords = c("lon", "lat"), crs = 4326)


# Censo Nacional de Población 2018 ---------------------------------------
#solo para cargar los archivos, la base resultante se adjunta.
#para descargar archivos del censo: https://microdatos.dane.gov.co/index.php/catalog/643/related-materials

# BBox de Bogotá (para leer solo esa zona y acelerar)

#bog_bbox_4326 <- st_as_text(st_as_sfc(
#st_bbox(c(xmin = -74.35, ymin = 4.45, xmax = -73.95, ymax = 4.95), crs = 4326)
#))

# Leer shapefile recortando al BBox, poner en 4326 y sanear

#manzanas_bog <- st_read(
#dsn = here("stores", "MGN_NivelManzana_Integrado_CNPV", "MGN_ANM_MANZANA.shp"),
#wkt_filter = bog_bbox_4326, quiet = TRUE
#) |>
#st_transform(4326) |>
#st_make_valid()

# Filtrar Bogotá (MPIO_CDPMP es character)

#manzanas_bog <- manzanas_bog |> filter(MPIO_CDPMP == "11001")

#guardar base del censo para Bogotá nivel manzana

#saveRDS(
#manzanas_bog,
#file = file.path(stores, "manzanas_bogota_cnpv2018.rds"),
#compress = "xz"
#)

#cargar base censo para Bogotá

manzanas_bog <- readRDS(file.path(stores, "manzanas_bogota_cnpv2018.rds"))


# Esadisticas descriptivas de densidad por manzanas 

s_m2 <- manzanas_bog |>
  st_drop_geometry() |>
  summarize(
    n      = n(),
    min    = min(DENSIDAD, na.rm = TRUE),
    q1     = quantile(DENSIDAD, 0.25, na.rm = TRUE),
    median = median(DENSIDAD, na.rm = TRUE),
    mean   = mean(DENSIDAD, na.rm = TRUE),
    q3     = quantile(DENSIDAD, 0.75, na.rm = TRUE),
    p90    = quantile(DENSIDAD, 0.90, na.rm = TRUE),
    p95    = quantile(DENSIDAD, 0.95, na.rm = TRUE),
    max    = max(DENSIDAD, na.rm = TRUE),
    sd     = sd(DENSIDAD, na.rm = TRUE)
  )

# Misma tabla en hab/ha

to_ha <- function(x) ifelse(is.numeric(x), x * 1e4, x)
s_ha <- s_m2 |> mutate(across(everything(), to_ha))

# unir tablas 

stat_labels <- c(
  n = "N (manzanas)",
  min = "Mínimo",
  q1  = "Q1 (25%)",
  median = "Mediana",
  mean   = "Media",
  q3  = "Q3 (75%)",
  p90 = "P90",
  p95 = "P95",
  max = "Máximo",
  sd  = "Desv. estándar"
)

tabla <- tibble(
  Estadístico = unname(stat_labels[names(s_m2)]),
  `hab/m²` = unlist(s_m2[1, ]),
  `hab/ha` = unlist(s_ha[1, ])
)


gt_tbl_densidad <- tabla |>
  gt() |>
  fmt_number(columns = `hab/m²`, decimals = 4) |>
  fmt_number(columns = `hab/ha`, decimals = 1, sep_mark = ".", dec_mark = ",") |>
  tab_header(
    title = "Densidad poblacional — Resumen descriptivo",
    subtitle = "Censo 2018, Bogotá D.C. (manzana)"
  ) |>
  cols_label(
    `hab/m²` = html("Densidad (hab/m²)"),
    `hab/ha` = html("Densidad (hab/ha)")
  ) |>
  tab_source_note(md("**Nota:** 1 ha = 10.000 m². La densidad en hab/ha es DENSIDAD·10.000."))

#Exportar

gt_tbl_densidad
gt::gtsave(gt_tbl_densidad, filename = file.path(views, "resumen_densidad_bogota.png")) 

# Gráfica de distribución (histograma + densidad)

ggplot(st_drop_geometry(manzanas_bog), aes(x = DENSIDAD)) +
  geom_histogram(bins = 40, alpha = 0.7) +
  geom_density(linewidth = 0.9) +
  scale_x_continuous(labels = label_number(accuracy = 0.0001)) +
  labs(
    title = "Distribución de la densidad poblacional (hab/m²)",
    x = "Densidad (hab/m²)",
    y = "Frecuencia"
  ) +
  theme_minimal()

#Densidad poblacional (manzana)

lims <- quantile(manzanas_bog$DENSIDAD, probs = c(0.02, 0.98), na.rm = TRUE)

ggplot() +
  geom_sf(data = manzanas_bog, aes(fill = DENSIDAD), color = NA) +
  scale_fill_viridis_c(
    option = "C",
    name   = "hab/m²",
    limits = lims,            # escala acotada P2–P98
    oob    = scales::squish   # valores fuera de rango se “aplastan” al borde
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Censo 2018 — Densidad poblacional por manzana (Bogotá)",
    subtitle = "Habitantes por m² (escala acotada P2–P98)",
    x = NULL, y = NULL
  )

# UPZ Shapefile ------------------

upz_shp  <- st_read(file.path(stores,"UPZ_Bogota", "UPZ_Bogota.shp"),  quiet = TRUE)

upz_shp  <- st_make_valid(upz_shp)
upz_shp  <- st_transform(upz_shp, crs = 4326)

ggplot(upz_shp) +
  geom_sf(fill = "grey95", color = "grey30", linewidth = 0.2) +
  theme_void() +
  labs(title = "UPZ — Bogotá")

# Graficas de densidad poblacional y proporción Arriendo/venta por UPZ-------

source(file.path(scripts, "Densidad_propiedades_UPZ.R"))

p_upz_dens

p_prop


# gradientes de densidad y precios por metro cuadrado 


# 1) Centro Internacional (OSM) y distancia (km) -------------------------------

bb_ci  <- getbb("Bogotá, Colombia")
ci_osm <- opq(bbox = bb_ci) %>%
  add_osm_feature(key = "name",
                  value = c("Centro Internacional", "Centro Internacional de Bogotá"),
                  value_exact = FALSE, match_case = FALSE) %>%
  osmdata_sf()

centro_int <- dplyr::bind_rows(ci_osm$osm_points, ci_osm$osm_polygons, ci_osm$osm_multipolygons) %>%
  sf::st_make_valid() %>%
  sf::st_point_on_surface() %>%
  dplyr::slice(1)

# CRS proyectado (metros) para distancias

crs_proj <- 3116
housing_proj <- sf::st_transform(housing_data_sf, crs_proj)
centro_proj  <- sf::st_transform(centro_int,      crs_proj)

housing_data_sf <- housing_data_sf %>%
  dplyr::mutate(
    dist_km   = as.numeric(sf::st_distance(housing_proj, centro_proj)) / 1000,
    log_p_m2  = log(price) - log(surface_total) # ln(precio/m²)
  )

# 2) Pegar UPZ a la base central y definir FE -----------------------------------

housing_data_sf <- sf::st_join(
  housing_data_sf,
  upz_shp[, "UPLCODIGO"],
  join = sf::st_within,
  left = FALSE
)

# 3) Estimar gradiente por operación con FE(tipo) y FE(UPZ) ---------------------

ajusta_gradiente_fe <- function(op) {
  df <- sf::st_drop_geometry(housing_data_sf) %>% dplyr::filter(operation == op)
  feols(
    log_p_m2 ~ dist_km + bedrooms + bathrooms | tipo + UPLCODIGO,
    data = df,
    cluster = ~ UPLCODIGO
  )
}

m_venta    <- ajusta_gradiente_fe("Venta")

summary(m_venta)

m_alquiler <- ajusta_gradiente_fe("Alquiler")

summary(m_alquiler)

# Crear objeto de la gráfica

p_disp <- ggplot(
  housing_data_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      log_price_m2 = log(price / surface_total),
      operation    = factor(operation, levels = c("Venta","Alquiler"))
    ),
  aes(x = dist_km, y = log_price_m2, color = operation)
) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
  facet_wrap(~ operation, ncol = 2, scales = "free_x") +
  scale_color_viridis_d(end = 0.7, guide = "none") +
  labs(
    title = "Distancia vs. log(Precio por m²)",
    subtitle = "Dispersión y recta OLS por tipo de operación",
    x = "Distancia al Centro Internacional (km)",
    y = "log(Precio por m²)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# Guardar en el path 'views'
ggsave(filename = file.path(views, "dispersion_log_pm2_distancia.png"),
       plot = p_disp, width = 10, height = 6, dpi = 300)


# 4) Curvas predichas con IC (delta), fijando controles en su media -----------

mk_curve_abs <- function(modelo, op) {
  df_op <- housing_data_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(operation == op)
  
  # Secuencia de distancias
  dseq <- data.frame(dist_km = seq(min(df_op$dist_km), max(df_op$dist_km), length.out = 200))
  
  # Coefs que usaremos
  coef_names <- names(stats::coef(modelo))
  keep <- intersect(c("(Intercept)", "dist_km", "bedrooms", "bathrooms"), coef_names)
  b    <- stats::coef(modelo)[keep]
  V    <- stats::vcov(modelo)[keep, keep, drop = FALSE]
  
  # Medias de controles (solo los presentes en el modelo)
  ctrl_means <- c(
    bedrooms  = mean(df_op$bedrooms,  na.rm = TRUE),
    bathrooms = mean(df_op$bathrooms, na.rm = TRUE)
  )
  ctrl_means <- ctrl_means[names(ctrl_means) %in% keep]
  
  # Matriz de diseño con columnas por NOMBRE (evita desalineaciones)
  X <- matrix(0, nrow = nrow(dseq), ncol = length(keep))
  colnames(X) <- keep
  if ("(Intercept)" %in% keep) X[, "(Intercept)"] <- 1
  if ("dist_km"     %in% keep) X[, "dist_km"]     <- dseq$dist_km
  for (nm in names(ctrl_means)) X[, nm] <- ctrl_means[[nm]]
  
  # Offset de FE (combos modales)
  fe_list  <- fixef(modelo)
  ref_tipo <- df_op$tipo[ which.max(tabulate(match(df_op$tipo, levels(df_op$tipo)))) ]
  ref_upz  <- df_op$UPLCODIGO[ which.max(tabulate(match(df_op$UPLCODIGO, levels(df_op$UPLCODIGO)))) ]
  fe_tipo  <- if ("tipo"      %in% names(fe_list)) unname(fe_list[["tipo"]][as.character(ref_tipo)]) else 0
  fe_upz   <- if ("UPLCODIGO" %in% names(fe_list)) unname(fe_list[["UPLCODIGO"]][as.character(ref_upz)]) else 0
  fe_off   <- sum(c(fe_tipo, fe_upz), na.rm = TRUE)
  
  # Predicción e IC (delta)
  fit_log <- as.vector(X %*% b) + fe_off
  se_log  <- sqrt(pmax(0, rowSums((X %*% V) * X)))
  lwr_log <- fit_log - qnorm(0.975) * se_log
  upr_log <- fit_log + qnorm(0.975) * se_log
  
  tibble::tibble(
    dist_km   = dseq$dist_km,
    fit_log   = fit_log,
    lwr_log   = lwr_log,
    upr_log   = upr_log,
    fit_lvl   = exp(fit_log),
    lwr_lvl   = exp(lwr_log),
    upr_lvl   = exp(upr_log),
    operation = op
  )
}

# Curvas para Venta y Alquiler
curvas_abs <- dplyr::bind_rows(
  mk_curve_abs(m_venta, "Venta"),
  mk_curve_abs(m_alquiler, "Alquiler")
)

# 5) Escalas robustas por panel (P2–P98) y gráficos ----------------------------

trim_panel <- function(df, yvar, xvar = "dist_km", p = c(0.02, 0.98)) {
  rng <- df %>%
    dplyr::group_by(operation) %>%
    dplyr::summarise(
      x_min = quantile(.data[[xvar]], p[1], na.rm = TRUE),
      x_max = quantile(.data[[xvar]], p[2], na.rm = TRUE),
      y_min = quantile(.data[[yvar]], p[1], na.rm = TRUE),
      y_max = quantile(.data[[yvar]], p[2], na.rm = TRUE),
      .groups = "drop"
    )
  df %>%
    dplyr::left_join(rng, by = "operation") %>%
    dplyr::filter(
      .data[[xvar]] >= x_min, .data[[xvar]] <= x_max,
      .data[[yvar]] >= y_min, .data[[yvar]] <= y_max
    ) %>%
    dplyr::select(-x_min, -x_max, -y_min, -y_max)
}

curvas_abs_log_trim <- trim_panel(curvas_abs, yvar = "fit_log")
curvas_abs_lvl_trim <- trim_panel(curvas_abs, yvar = "fit_lvl")

# (a) log(Precio/m²)
p_log_grad <- ggplot(curvas_abs_log_trim, aes(x = dist_km, y = fit_log)) +
  geom_ribbon(aes(ymin = lwr_log, ymax = upr_log), alpha = 0.20) +
  geom_line(linewidth = 1) +
  facet_wrap(~ operation, ncol = 2, scales = "free") +
  labs(
    title = "Gradiente: log(Precio/m²) vs. distancia al Centro Internacional",
    subtitle = "FE de tipo y UPZ; controles (bedrooms, bathrooms) a su media; IC95% (delta).",
    x = "Distancia (km)", y = "log(Precio/m²)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# (b) Precio/m² (nivel)
p_lvl_grad <- ggplot(curvas_abs_lvl_trim, aes(x = dist_km, y = fit_lvl)) +
  geom_ribbon(aes(ymin = lwr_lvl, ymax = upr_lvl), alpha = 0.20) +
  geom_line(linewidth = 1) +
  facet_wrap(~ operation, ncol = 2, scales = "free") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Gradiente: Precio/m² vs. distancia al Centro Internacional",
    subtitle = "FE de tipo y UPZ; controles (bedrooms, bathrooms) a su media; IC95% (delta).",
    x = "Distancia (km)", y = "Precio por m²"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# Guardar
ggsave(file.path(views, "gradiente_log_pm2_simple.png"),
       p_log_grad, width = 10, height = 6, dpi = 300)
ggsave(file.path(views, "gradiente_pm2_simple.png"),
       p_lvl_grad, width = 10, height = 6, dpi = 300)
