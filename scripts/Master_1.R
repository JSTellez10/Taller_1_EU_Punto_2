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




