#-----------------------------------------------------------------------------//
# Master
# Problem Set 2 G10 - BDML 202501
#-----------------------------------------------------------------------------//

rm(list = ls())

#-----------------------------------------------------------------------------//
# 1. Configurar rutas relativas con {here} ----
#-----------------------------------------------------------------------------//

# Instalar paquete 'here' si no está instalado
if (!require(here)) install.packages("here", dependencies = TRUE)
library(here)

# Definir la ruta principal como la raíz del proyecto
path_main <- here() 

# Definir subcarpetas dentro del proyecto
document_path <- file.path(path_main, "document")
raw_path <- file.path(path_main, "raw") 
scripts_path  <- file.path(path_main, "scripts") 
stores_path   <- file.path(path_main, "stores")
view_path     <- file.path(path_main, "views")

#-----------------------------------------------------------------------------//
# 2. Cargar paquetes ----
#-----------------------------------------------------------------------------//

# Instalar paquetería {pacman} si no está instalada
if (!require(pacman)) install.packages("pacman", dependencies = TRUE)
library(pacman)

# Cargar paquetes necesarios
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
       caret,
       boot,
       DescTools) 


#----------------------------------------------
# Ejecutar scripts

