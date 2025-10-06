# =====================  porporción arriendo/venta por UPZ (vía unión espacial) =====================

# --- 0) Configura el nombre del código (y nombre) de UPZ en upz_shp --------

upz_code_col <- "UPLCODIGO"   
upz_name_col <- "UPLNOMBRE"     

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


# =====================  Densidad poblacional por UPZ (vía unión espacial) =====================

# --- 1) Preparar atributos de manzana (tipos y filtro) --------

manz_pre <- manzanas_bog %>%
  dplyr::mutate(
    perso_mz = as.numeric(TP27_PERSO),
    area_m2  = as.numeric(AREA)
  ) %>%
  dplyr::filter(area_m2 > 0)

# --- 2) Punto representativo + unión espacial con UPZ ---------

manz_pts <- sf::st_point_on_surface(manz_pre)

manz_with_upz <- sf::st_join(
  manz_pts,
  upz_shp[, c(upz_code_col, upz_name_col)],
  join = sf::st_within,
  left = FALSE
) %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(
    upz_key = as.character(.data[[upz_code_col]]),
    perso_mz, area_m2
  )

# --- 3) Agregar a UPZ y calcular densidad (hab/m²) -------------

agg_upz <- manz_with_upz %>%
  dplyr::group_by(upz_key) %>%
  dplyr::summarize(
    pob_upz  = sum(perso_mz, na.rm = TRUE),
    area_upz = sum(area_m2,  na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  dplyr::mutate(dens_hab_m2 = pob_upz / area_upz)

# --- 4) Añadir densidad a `upz_join_crop` y mapear -------------

upz_join_crop <- upz_join_crop %>%
  dplyr::mutate(upz_key = as.character(upz_key)) %>%
  dplyr::left_join(agg_upz, by = "upz_key")

lims_upz <- stats::quantile(upz_join_crop$dens_hab_m2, c(0.02, 0.98), na.rm = TRUE)

p_upz_dens <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = upz_join_crop, ggplot2::aes(fill = dens_hab_m2),
                   color = "white", linewidth = 0.15) +
  ggplot2::scale_fill_viridis_c(
    option = "C", name = "hab/m²",
    limits = lims_upz, oob = scales::squish,
    labels = scales::label_number(accuracy = 0.0001),
    na.value = "grey90"
  ) +
  ggplot2::coord_sf(
    xlim = c(bbox_lim["xmin"], bbox_lim["xmax"]),
    ylim = c(bbox_lim["ymin"], bbox_lim["ymax"]),
    expand = FALSE
  ) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Censo 2018 — Densidad poblacional por UPZ (Bogotá)",
    subtitle = "Habitantes por m² (agregado desde manzana; escala acotada P2–P98)",
    x = NULL, y = NULL
  )

p_upz_dens

ggplot2::ggsave(
  filename = file.path(views, "mapa_densidad_UPZ.png"),
  plot = p_upz_dens, width = 10, height = 8, dpi = 300
)
