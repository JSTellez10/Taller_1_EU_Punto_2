##########################################################
# Taller 1 - Económia Urbana
# Ejercicio 1
# author: Eimmy Nicoll Tovar Escobar y Juan Sebastian Tellez Melo
##########################################################

# Clean the workspace -----------------------------------------------------

rm(list=ls())

# Definir directorios -----------------------------------------------------

usuario <- tolower(Sys.info()[["user"]])  # o: tolower(Sys.getenv("USERNAME", Sys.getenv("USER")))

rutas <- list(
  sebas = "C:/Users/sebas/OneDrive - RADDAR/Documentos/Documents/Sebastian Tellez/MAESTRIA/ECONOMIA URBANA/TALLER/TALLER 1/Taller_1_EU/",
  mora  = "C:/Users/mora/Path/To/TALLER/TALLER 1/"
)

root <- rutas[[usuario]]

stores <- file.path(root, "stores")
scripts <- file.path(root, "scripts")
views <- file.path(root, "views")

# Load Packages -----------------------------------------------------------

# Cargar datos -----------------------------------------------------------








