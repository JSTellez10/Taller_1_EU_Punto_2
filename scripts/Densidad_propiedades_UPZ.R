# =====================  Densidad poblacional por UPZ (vía unión espacial) =====================

# --- 0) Configura el nombre del código (y nombre) de UPZ en upz_shp --------

upz_code_col <- "UPLCODIGO"   # <--- AJUSTA AL NOMBRE REAL (p. ej. "SETU_CCDGO", "UPZ", etc.)
upz_name_col <- "UPLNOMBRE"       # <--- (opcional) nombre de la UPZ si existe

# --- 1) Alinear CRS y sanear geometrías --------------

upz_shp  <- st_make_valid(upz_shp)
if (!st_crs(upz_shp) == st_crs(manzanas_bog)) {
  upz_shp <- st_transform(upz_shp, st_crs(manzanas_bog))
}

# --- 2) Preparar atributos de manzana (asegurar tipos) --------

manz_pre <- manzanas_bog |>
  mutate(
    perso_mz = suppressWarnings(as.numeric(TP27_PERSO)),
    area_m2  = suppressWarnings(as.numeric(AREA))
  ) |>
  filter(!is.na(perso_mz), !is.na(area_m2), area_m2 > 0)

# --- 3) Punto representativo de cada manzana y unión espacial con UPZ ---------

#point_on_surface evita problemas de centroides fuera por polígonos irregulares

manz_pts <- st_point_on_surface(manz_pre)

keep_cols <- c(upz_code_col, intersect(upz_name_col, names(upz_shp)))
upz_key   <- upz_shp[, keep_cols] |>
  mutate(upz_key = str_trim(as.character(.data[[upz_code_col]])))

# Unión espacial: asignar UPZ a cada manzana

manz_with_upz <- st_join(
  manz_pts,
  upz_key,
  join = st_within,
  left = FALSE # solo manzanas que caen dentro de alguna UPZ
) |>
  st_drop_geometry() |>
  mutate(upz_key = str_trim(as.character(upz_key)))

# --- 4) Agregar a nivel UPZ y calcular densidad (hab/m²) -------------
agg_upz <- manz_with_upz |>
  group_by(upz_key) |>
  summarize(
    pob_upz  = sum(perso_mz, na.rm = TRUE),
    area_upz = sum(area_m2,  na.rm = TRUE),
    .groups  = "drop"
  ) |>
  mutate(dens_hab_m2 = if_else(area_upz > 0, pob_upz / area_upz, NA_real_))

# traer nombre de UPZ si lo tienes
if (upz_name_col %in% names(upz_shp)) {
  upz_names <- upz_shp |>
    st_drop_geometry() |>
    transmute(
      upz_key = str_trim(as.character(.data[[upz_code_col]])),
      upz_nom = .data[[upz_name_col]]
    ) |>
    distinct(upz_key, upz_nom)
  agg_upz <- left_join(agg_upz, upz_names, by = "upz_key")
}

# --- 5) Unir agregados a la geometría de UPZ para mapear ----------
upz_join <- upz_shp |>
  mutate(upz_key = str_trim(as.character(.data[[upz_code_col]]))) |>
  left_join(agg_upz, by = "upz_key")

# --- 6) Escala acotada (P2–P98) para evitar “aplastamiento” por outliers ---------
lims_upz <- quantile(upz_join$dens_hab_m2, probs = c(0.02, 0.98), na.rm = TRUE)

bbox_lim <- c(xmin = -74.35, ymin = 4.45, xmax = -73.95, ymax = 4.95)
bbox_sf <- st_as_sfc(st_bbox(bbox_lim, crs = st_crs(4326)))
upz_join_crop <- st_intersection(st_make_valid(upz_join), bbox_sf)

p_upz_crop <- ggplot() +
  geom_sf(data = upz_join_crop, aes(fill = dens_hab_m2), color = "white", linewidth = 0.15) +
  scale_fill_viridis_c(
    option = "C",
    name   = "hab/m²",
    limits = lims_upz,
    oob    = squish,
    labels = label_number(accuracy = 0.0001),
    na.value = "grey90"
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Censo 2018 — Densidad poblacional por UPZ (Bogotá)",
    subtitle = "Recortado a bbox especificado",
    x = NULL, y = NULL
  )

p_upz_crop

# --- 7) Exportar a 'views' -------------

ggsave(
  filename = file.path(views, "mapa_densidad_UPZ.png"),
  plot = p_upz,
  width = 10, height = 8, dpi = 300
)



# =====================  porporción arriendo/venta por UPZ (vía unión espacial) =====================

# --- 1) Asignar UPZ a cada propiedad y agregar proporciones por UPZ --------

prop_upz <- housing_data_sf %>%
  mutate(operation = str_trim(as.character(operation))) %>%
  filter(operation %in% c("Venta","Alquiler")) %>%
  st_join(upz_shp[, c(upz_code_col, upz_name_col)[!is.na(c(upz_code_col, upz_name_col))]],
          join = st_within, left = FALSE) %>%
  st_drop_geometry() %>%
  transmute(
    upz_key = str_trim(as.character(.data[[upz_code_col]])),
    upz_nom = if (!is.na(upz_name_col)) .data[[upz_name_col]] else NA_character_,
    operation
  ) %>%
  group_by(upz_key, upz_nom) %>%
  summarize(
    n = n(),
    prop_venta    = mean(operation == "Venta", na.rm = TRUE),
    prop_alquiler = mean(operation == "Alquiler", na.rm = TRUE),
    .groups = "drop"
  )

# --- 2) Unir proporciones a geometría de UPZ y recortar al bbox solicitado ---

bbox_lim <- c(xmin = -74.35, ymin = 4.45, xmax = -73.95, ymax = 4.95)
bbox_sf  <- st_as_sfc(st_bbox(bbox_lim, crs = st_crs(upz_shp)))

upz_join_crop <- upz_shp %>%
  mutate(upz_key = str_trim(as.character(.data[[upz_code_col]]))) %>%
  left_join(prop_upz, by = "upz_key") %>%
  st_make_valid() %>%
  st_intersection(bbox_sf)

upz_join_crop <- upz_join_crop |>
  dplyr::mutate(upz_key = stringr::str_trim(as.character(upz_key))) |>
  dplyr::filter(is.na(upz_key) | upz_key != "UPR3")

# --- 3) Centro histórico de Bogotá: Plaza de Bolívar (OSM) -------------


bb_osm  <- getbb("Bogotá, Colombia")
q_plaza <- opq(bbox = bb_osm) |> add_osm_feature(key = "name", value = "Plaza de Bolívar")
plaza   <- osmdata_sf(q_plaza)

centro_bogota <- dplyr::bind_rows(plaza$osm_points, plaza$osm_polygons, plaza$osm_multipolygons) |>
  sf::st_make_valid() |>
  sf::st_point_on_surface() |>
  dplyr::slice(1) |>
  sf::st_transform(sf::st_crs(upz_join_crop))

# --- 4) Preparar datos largos para facetas y escala (0–1) ------

map_dat <- upz_join_crop %>%
  select(upz_key, upz_nom = all_of(upz_name_col), prop_venta, prop_alquiler, geometry) %>%
  pivot_longer(c(prop_venta, prop_alquiler), names_to = "metric", values_to = "prop") %>%
  mutate(metric_lab = recode(metric, prop_venta = "Proporción: Venta", prop_alquiler = "Proporción: Alquiler"))


# --- 5) Mapa y exportación -------

p_prop <- ggplot() +
  geom_sf(data = map_dat, aes(fill = prop), color = "white", linewidth = 0.12) +
  scale_fill_viridis_c(
    option = "C",
    labels = percent_format(accuracy = 1),
    name   = "Proporción",
    limits = c(0, 1),
    oob    = squish,
    na.value = "grey90"
  ) +
  geom_sf(data = centro_bogota, shape = 8, size = 3, color = "red", stroke = 1) +
  facet_wrap(~ metric_lab, ncol = 2) +
  coord_sf(
    xlim = c(bbox_lim["xmin"], bbox_lim["xmax"]),
    ylim = c(bbox_lim["ymin"], bbox_lim["ymax"]),
    expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = "Proporción de propiedades en Venta/Alquiler por UPZ — Bogotá",
    subtitle = "Centro histórico: Plaza de Bolívar (OSM)",
    x = NULL, y = NULL
  )

p_prop

ggsave(filename = file.path(views, "mapa_proporcion_venta_alquiler_UPZ.png"),
       plot = p_prop, width = 12, height = 7, dpi = 300)
