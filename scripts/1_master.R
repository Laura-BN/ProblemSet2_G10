#-----------------------------------------------------------------------------//
#  Master
# Problem Set 1 G10 - BDML 202501
# Fecha: 
#-----------------------------------------------------------------------------//

rm(list = ls())

#-----------------------------------------------------------------------------//
# 1. Usuarios ----
#-----------------------------------------------------------------------------//

paths = c(
  #"H:/My Drive/1. General/3. Académico/3. Uniandes/Machine Learning + BD/Repos_GitHub",
  #"/Users/camilaortiz/Dropbox/PEG/BigData",
  "G:/Mi unidad/Academia/Maestría MEcA/Big data y machine learning/Taller 1"
)

# Iterar sobre las rutas y seleccionar la primera que exista
path_user = NULL
for (path in paths) {
  if (dir.exists(path)) {
    path_user <- path
    break 
  }
}

# Ruta seleccionada
if (!is.null(path_user)) {
  print(paste("Ruta seleccionada:", path_user))
} else {
  print("Ninguno de las rutas es accesible.")
}

#-----------------------------------------------------------------------------//
# 2. Ruta de los archivos ----
#-----------------------------------------------------------------------------//

path_main = "ProblemSet1_G10"
path_gen = file.path(path_user, path_main)

document_path = file.path(path_gen, "document") 
scripts_path  = file.path(path_gen, "scripts") 
stores_path   = file.path(path_gen, "stores")
view_path     = file.path(path_gen, "views")

#-----------------------------------------------------------------------------//
# 3. Paquetes ----
#-----------------------------------------------------------------------------//

# Instalar paqueta pacman.
#install.packages("pacman")

# Llamar librerías
require(pacman)
p_load(tidyverse, 
       rvest,
       dplyr,
       stargazer, 
       foreign, 
       skimr, # summary data
       visdat, # visualizing missing data
       corrplot, 
       scales, 
       broom, 
       xtable, 
       gridExtra, 
       survey,
       VIM, 
       fastDummies, 
       xtable, 
       broom, 
       caret,
       boot) 

#Instalar paquete DescTools
# install.packages("DescTools")
# library(DescTools)
