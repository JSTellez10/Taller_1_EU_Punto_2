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
       scales
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





