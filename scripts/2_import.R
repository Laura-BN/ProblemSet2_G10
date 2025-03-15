#-----------------------------------------------------------------------------//
# Importar datos
# Problem Set 2 G10 - BDML 202501
# Fecha: 14 de marzo de 2025
#-----------------------------------------------------------------------------//

# Instalamos paquetes si no están instalados
if (!require(pacman)) install.packages("pacman", dependencies = TRUE)
pacman::p_load(dplyr, readr, zip)

if (!require(crayon)) install.packages("crayon", dependencies = TRUE)
library(crayon)

# Ruta con las bases de datos en ZIP
zip_path <- file.path(raw_path, "uniandes-bdml-202510-ps-2.zip")

# Listar los archivos dentro del ZIP para verificar la estructura
zip_files <- zip::zip_list(zip_path)
print(zip_files$file)  # Para ver cómo están organizados dentro del ZIP

# Definir los nombres correctos de los archivos dentro del ZIP
test_personas <- read_csv(unz(zip_path, "test_personas.csv"))
test_hogares  <- read_csv(unz(zip_path, "test_hogares.csv"))
train_personas <- read_csv(unz(zip_path, "train_personas.csv"))
train_hogares  <- read_csv(unz(zip_path, "train_hogares.csv"))

# Merge por la izquierda
test_merged  <- left_join(test_personas, test_hogares, by = "id")
train_merged <- left_join(train_personas, train_hogares, by = "id")

# Guardar los archivos en formato .rds en la carpeta stores
saveRDS(test_merged, file.path(stores_path, "test_merged.rds"))
saveRDS(train_merged, file.path(stores_path, "train_merged.rds"))

# Mensaje de proceso realizado
message(green("✅ Bases guardadas en "), green(stores_path))


 