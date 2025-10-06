# ========= Estadísticas descriptivas por operation × tipo =========

# Variables numéricas a resumir

vars_num <- c("price", "rooms", "bedrooms", "bathrooms", "surface_total")

# (1) Resumen por variable, tipo y operación (mediana[IQR], media(DE))

df_sum <- housing_data %>%
  filter(!if_any(all_of(vars_num), ~ !is.finite(.))) %>%
  mutate(
    operation = factor(operation, levels = c("Venta","Alquiler")),
    tipo      = factor(tipo, levels = c("Casa","Apartamento/Apartaestudio","Comercial"))
  ) %>%
  select(operation, tipo, all_of(vars_num)) %>%
  pivot_longer(cols = all_of(vars_num), names_to = "variable", values_to = "value") %>%
  group_by(tipo, operation, variable) %>%
  summarize(
    n      = n(),
    med    = median(value, na.rm = TRUE),
    p25    = quantile(value, 0.25, na.rm = TRUE),
    p75    = quantile(value, 0.75, na.rm = TRUE),
    mean   = mean(value, na.rm = TRUE),
    sd     = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

fmt_num <- function(x, var) {
  if (var == "price") {
    dollar(x, prefix = "$", big.mark = ".", decimal.mark = ",", accuracy = 1)
  } else if (var == "surface_total") {
    number(x, accuracy = 1)
  } else {
    number(x, accuracy = 1)
  }
}

df_fmt <- df_sum %>%
  rowwise() %>%
  mutate(
    med_iqr = paste0(
      fmt_num(med, variable), " [", fmt_num(p25, variable), "–", fmt_num(p75, variable), "]"
    ),
    mean_sd = paste0(
      fmt_num(mean, variable), " (", fmt_num(sd, variable), ")"
    )
  ) %>%
  ungroup()

# (2) Conteos N por tipo × operación (una sola vez por grupo)

counts <- housing_data %>%
  mutate(
    operation = factor(operation, levels = c("Venta","Alquiler")),
    tipo      = factor(tipo, levels = c("Casa","Apartamento/Apartaestudio","Comercial"))
  ) %>%
  filter(!is.na(tipo), !is.na(operation)) %>%
  count(tipo, operation, name = "N") %>%
  pivot_wider(names_from = operation, values_from = N,
              names_glue = "N | {operation}")   # columnas: "N | Venta", "N | Alquiler"

# (3) Tabla ancha (mediana/mean) + unir conteos y ordenar columnas

wide_tbl <- df_fmt %>%
  select(tipo, variable, operation, med_iqr, mean_sd) %>%
  pivot_wider(
    names_from  = operation,
    values_from = c(med_iqr, mean_sd),
    names_sep   = " | "
  ) %>%
  left_join(counts, by = "tipo") %>%
  relocate(`N | Venta`, .before = `med_iqr | Venta`) %>%
  relocate(`N | Alquiler`, .before = `med_iqr | Alquiler`)

# Etiquetas de variables

var_labels <- c(
  price = "Precio",
  rooms = "Habitaciones",
  bedrooms = "Dormitorios",
  bathrooms = "Baños",
  surface_total = "Superficie total (m²)"
)

wide_tbl <- wide_tbl %>%
  mutate(variable = factor(variable, levels = names(var_labels), labels = unname(var_labels))) %>%
  arrange(tipo, variable)

# (4) Tabla por operación + N por operación

gt_tbl_propiedades <- wide_tbl %>%
  gt(rowname_col = "variable", groupname_col = "tipo") %>%
  cols_label(
    `N | Venta`         = "N",
    `med_iqr | Venta`   = "Mediana [P25–P75]",
    `mean_sd | Venta`   = "Media (DE)",
    `N | Alquiler`      = "N",
    `med_iqr | Alquiler`= "Mediana [P25–P75]",
    `mean_sd | Alquiler`= "Media (DE)"
  ) %>%
  tab_spanner(label = "Venta",
              columns = c(`N | Venta`, `med_iqr | Venta`, `mean_sd | Venta`)) %>%
  tab_spanner(label = "Alquiler",
              columns = c(`N | Alquiler`, `med_iqr | Alquiler`, `mean_sd | Alquiler`)) %>%
  tab_header(
    title = "Estadísticas descriptivas por tipo y operación",
    subtitle = "Mediana [P25–P75], Media (DE) y conteos N"
  ) %>%
  tab_stubhead(label = "Variable") %>%
  opt_row_striping() %>%
  cols_align("center", columns = -c(variable)) %>%
  tab_source_note(md("Notas: precios en moneda local; superficie en m². Mediana/IQR son robustas a outliers."))



