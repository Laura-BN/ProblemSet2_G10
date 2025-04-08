#-----------------------------------------------------------------------------//
# Importar datos
# Problem Set 2 G10 - BDML 202501
# Fecha: 14 de marzo de 2025
#-----------------------------------------------------------------------------//

# 1. INSTALAR PAQUETES ---------------------------------------------------------

if (!require(pacman)) install.packages("pacman", dependencies = TRUE)
pacman::p_load(dplyr, readr, zip)

if (!require(crayon)) install.packages("crayon", dependencies = TRUE)
library(crayon)


# 2. IMPORTAR DATOS ------------------------------------------------------------

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

intersect(colnames(test_personas), colnames(train_personas))
intersect(colnames(test_hogares), colnames(train_hogares))

   # train_personas <- train_personas %>% slice(1:100)


# 3. CREAR NUEVAS VARIABLES EN LA BASE PERSONAS --------------------------------

# Variables para la base train personas

train_personas_vars <- train_personas %>% 
                  mutate(
                      pt = 1L,
                      mujer = ifelse(P6020==2,1,0), # Mujer
                      jefe_hogar = ifelse(P6050== 1, 1, 0), # Jefe(a) de hogar
                      jefe_mujer = ifelse(P6050 == 1 & P6020 == 2, 1, 0), # Mujer es la jefa de hogar
                      jefe_salud_sub = ifelse(P6050 == 1 & (P6090 == 2 | P6090 == 9 | P6100 == 3 | P6100 == 9), 1, 0), # Jefe de hogar sin afiliacion a salud o subsidiado
                      jefe_pension = if_else(P6050 == 1 & P6920==2, 1L, 0L, missing = 1L), # Jefe de hogar cotiza a pensión
                      jefe_edad = ifelse(P6050 == 1, P6040, NA_real_), # Edad jefe(a) de hogar
                      jefe_edad2 = ifelse(P6050 == 1, (P6040^2), NA_real_), # Edad jefe de hogar al cuadrado
                      jefe_ocu = if_else(Oc == 1, 1L, 0L, missing = 0L), # Jefe(a) de hogar ocupado
                      menor = ifelse(P6040<=6,1,0), # Persona menor en el hogar
                      mayor_dependiente = ifelse(P6040>=60 & (Ina==1 | Des==1), 1 ,0), # Persona mayor dependiente en el hogar
                      nivel_educ = ifelse(P6210==9,0,P6210), # Nivel educativo, reemplazar con 0 el nivel no sabe, no informa
                      ocupado = ifelse(is.na(Oc),0,1), # Variable ocupado
                      desocupado = ifelse(is.na(Des),0,1), # Variable desocupado
                      inactivo = ifelse(is.na(Ina),0,1), # Variable inactivo
                      jefe_nivel_educ = ifelse(P6050 == 1, nivel_educ, NA),  # Jefe de hogar no cotiza a pensión
                      
                      # nuevas variables 
                      antiguedad_empleo = (P6426/12),  # tiempo empresa (ocupados) en meses
                      ocupacion_ocu = P6430,  # ocupaciones ocupados
                      ocupacion_des = P7350,  # ocupaciones anteriores desocupados
                      tamano_emp = P6870, # tamaño empresa (ocupados)
                      subsidios = P7510s3 # ¿recibió c. ayudas en dinero de instituciones del país?) 1 sí 2 no 9 no sabe, noinforma

                      ) %>% 
                      select(id, Orden, pt, Pet, mujer, jefe_hogar, jefe_mujer, jefe_salud_sub,
                             jefe_pension, jefe_edad, jefe_edad2, menor, mayor_dependiente,
                             nivel_educ, ocupado, desocupado, inactivo, jefe_nivel_educ, jefe_ocu, 
                             
                             antiguedad_empleo, ocupacion_ocu, ocupacion_des, subsidios)

# Variables para la base test personas
test_personas_vars <- test_personas %>% 
                      mutate(
                        pt = 1L,
                        mujer = ifelse(P6020==2,1,0), # Mujer
                        jefe_hogar = ifelse(P6050== 1, 1, 0), # Jefe(a) de hogar
                        jefe_mujer = ifelse(P6050 == 1 & P6020 == 2, 1, 0), # Mujer es la jefa de hogar
                        jefe_salud_sub = ifelse(P6050 == 1 & (P6090 == 2 | P6090 == 9 | P6100 == 3 | P6100 == 9), 1, 0), # Jefe de hogar sin afiliacion a salud o subsidiado
                        jefe_pension = if_else(P6050 == 1 & P6920==2, 1L, 0L, missing = 1L), # Jefe de hogar cotiza a pensión
                        jefe_edad = ifelse(P6050 == 1, P6040, NA_real_), # Edad jefe(a) de hogar
                        jefe_edad2 = ifelse(P6050 == 1, (P6040^2), NA_real_), # Edad jefe de hogar al cuadrado
                        jefe_ocu = if_else(Oc == 1, 1L, 0L, missing = 0L), # Jefe(a) de hogar ocupado
                        menor = ifelse(P6040<=6,1,0), # Persona menor en el hogar
                        mayor_dependiente = ifelse(P6040>=60 & (Ina==1 | Des==1), 1 ,0), # Persona mayor dependiente en el hogar
                        nivel_educ = ifelse(P6210==9,0,P6210), # Nivel educativo, reemplazar con 0 el nivel no sabe, no informa
                        ocupado = ifelse(is.na(Oc),0,1), # Variable ocupado
                        desocupado = ifelse(is.na(Des),0,1), # Variable desocupado
                        inactivo = ifelse(is.na(Ina),0,1), # Variable inactivo
                        jefe_nivel_educ = ifelse(P6050 == 1, nivel_educ, NA), # Jefe de hogar no cotiza a pensión
                        
                        # nuevas variables 
                        antiguedad_empleo = (P6426/12),  # tiempo empresa (ocupados) en meses
                        ocupacion_ocu = P6430,  # ocupaciones ocupados
                        ocupacion_des = P7350,  # ocupaciones anteriores desocupados
                        tamano_emp = P6870, # tamaño empresa (ocupados)
                        subsidios = P7510s3 # ¿recibió c. ayudas en dinero de instituciones del país?) 1 sí 2 no 9 no sabe, noinforma
                        
                      ) %>% 
                      select(id, Orden, pt, Pet, mujer, jefe_hogar, jefe_mujer, jefe_salud_sub,
                             jefe_pension, jefe_edad, jefe_edad2, menor, mayor_dependiente,
                             nivel_educ, ocupado, desocupado, inactivo, jefe_nivel_educ, jefe_ocu, 
                             
                             antiguedad_empleo, ocupacion_ocu, ocupacion_des, subsidios)


# 4. CREAR VARIABLES A NIVEL DE HOGAR ------------------------------------------

# Variables hogar con base en la base de personas

train_personas_hogar_B <- train_personas_vars  %>% 
                        group_by(id) %>%
                        summarize(N_personas = sum(pt, na.rm=TRUE), # Personas en el hogar
                                  N_desocupados = sum(desocupado, na.rm=TRUE), # Inactivos en el hogar
                                  N_inactivos = sum(inactivo, na.rm=TRUE), # Inactivos en el hogar
                                  N_ocupados = sum(ocupado, na.rm=TRUE), # Ocupados por hogar
                                  N_pet = sum(Pet, na.rm = TRUE), # Personas en la PET por hogar
                                  N_menores = sum(menor, na.rm = TRUE), # Personas menores en el hogar
                                  N_mayor_dependiente = sum(mayor_dependiente, na.rm = TRUE), # Adultos mayores dependientes en el hogar
                                  N_mujer = sum(mujer, na.ram=TRUE), # Mujeres en el hogar
                                  max_nivel_educ = max(nivel_educ, na.rm=TRUE) # Maximo nivel educativo en el hogar
                                  ) %>%
                        mutate(prop_ina_pet = N_inactivos/N_personas, # Proporcion inactivos / pt
                               prop_ocu_pet = N_ocupados/N_personas # Proporcion ocupados / pt
                               ) %>% 
                        ungroup()


test_personas_hogar_B <- test_personas_vars  %>% 
                        group_by(id) %>%
                        summarize(N_personas = sum(pt, na.rm=TRUE), # Personas en el hogar
                                  N_desocupados = sum(desocupado, na.rm=TRUE), # Inactivos en el hogar
                                  N_inactivos = sum(inactivo, na.rm=TRUE), # Inactivos en el hogar
                                  N_ocupados = sum(ocupado, na.rm=TRUE), # Ocupados por hogar
                                  N_pet = sum(Pet, na.rm = TRUE), # Personas en la PET por hogar
                                  N_menores = sum(menor, na.rm = TRUE), # Personas menores en el hogar
                                  N_mayor_dependiente = sum(mayor_dependiente, na.rm = TRUE), # Adultos mayores dependientes en el hogar
                                  N_mujer = sum(mujer, na.ram=TRUE), # Mujeres en el hogar
                                  max_nivel_educ = max(nivel_educ, na.rm=TRUE) # Maximo nivel educativo en el hogar
                                  ) %>%
                        mutate(prop_ina_pet = N_inactivos/N_personas, # Proporcion inactivos / pt
                               prop_ocu_pet = N_ocupados/N_personas # Proporcion ocupados / pt
                              ) %>% 
                        ungroup()


# Variables jefe de hogar con base en la base de personas

train_personas_hogar <- train_personas_vars  %>% 
                        filter(jefe_hogar==1) %>%
                        select(id, jefe_mujer, jefe_salud_sub, jefe_pension, 
                               jefe_edad, jefe_edad2, jefe_nivel_educ, 
                               
                               jefe_ocu, 
                               
                               antiguedad_empleo, ocupacion_ocu, ocupacion_des, subsidios
                               ) %>%
                        left_join(train_personas_hogar_B, by = "id")

test_personas_hogar <- test_personas_vars  %>% 
                        filter(jefe_hogar==1) %>%
                        select(id, jefe_mujer, jefe_salud_sub, jefe_pension, 
                               jefe_edad, jefe_edad2, jefe_nivel_educ, 
                               
                               jefe_ocu, 
                               
                               antiguedad_empleo, ocupacion_ocu, ocupacion_des, subsidios
                        ) %>%
                        left_join(test_personas_hogar_B, by = "id")


# Variables a nivel de hogar con la base de hogares

train_hogares_vars <- train_hogares %>% 
                      mutate(hacinamiento = Nper/P5000, # Personas por cuarto en el hogar
                             viv_noPropia = ifelse(P5090 == 1 | P5090 == 2, 0L, 1L)) %>%
                      select(id, Clase, Dominio, hacinamiento, Nper, Pobre, viv_noPropia) # Seleccionar variables de interes

test_hogares_vars <- test_hogares %>% 
                      mutate(hacinamiento = Nper/P5000, # Personas por cuarto en el hogar
                             viv_noPropia = ifelse(P5090 == 1 | P5090 == 2, 0L, 1L)) %>%
                      select(id, Clase, Dominio, hacinamiento, Nper, viv_noPropia) # Seleccionar variables de interes


# 5. CREAR VARIABLES A NIVEL DE HOGAR ------------------------------------------
                        
# Unir las bases de datos 

pre_train <- train_hogares_vars %>% 
            left_join(train_personas_hogar, by = "id") %>%
            select(-id, -Nper) # No se necesitará más la variable id

pre_test <- test_hogares_vars %>% 
            left_join(test_personas_hogar, by = "id") %>%
            select(-Nper)

# Eliminar NAs
pre_train$jefe_salud_sub[is.na(pre_train$jefe_salud_sub)] <- 1
pre_test$jefe_salud_sub[is.na(pre_test$jefe_salud_sub)] <- 1


# Convertir las variables categoricas y obtener la base final

train <- pre_train %>%
        mutate(Pobre = factor(Pobre, levels=c(1,0), labels=c("Pobre","No_pobre")),
               jefe_mujer = factor(jefe_mujer, levels = c(1, 0), labels = c("Jefe_mujer", "Jefe_hombre")),
               jefe_salud_sub = factor(jefe_salud_sub, levels = c(1, 0), labels = c("Jefe_salud_subsidiado", "Jefe_salud_contributivo")),
               jefe_pension = factor(jefe_pension, levels = c(1, 0), labels = c("Jefe_af_pension", "Jefe_no_af_pension"),),
               viv_noPropia = factor(viv_noPropia, levels = c(1, 0), labels = c("sin_vivienda", "con_vivienda"),),
               
               Dominio = factor(Dominio),
               jefe_nivel_educ = factor(jefe_nivel_educ, levels=c(0:6), labels=c('Ns','Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
               max_nivel_educ = factor(max_nivel_educ,levels=c(0:6), labels=c('Ns','Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
               Clase = factor(Clase,levels=c(1:2), labels=c('Cabecera','Resto')), 
               ocupacion_ocu_f = factor(case_when(
                                 ocupacion_ocu %in% c(1, 2) ~ "Asal_formal",
                                 ocupacion_ocu %in% c(4, 5) ~ "Cuenta_propia_empleador",
                                 ocupacion_ocu %in% c(3, 8, 9) ~ "Informal_precario",
                                 ocupacion_ocu %in% c(6, 7) ~ "Sin_remuneracion",
                                 TRUE ~ NA_character_)), 
               
               ocupacion_des_f = factor(case_when(
                                 ocupacion_des %in% c(1, 2) ~ "Asal_formal",
                                 ocupacion_des %in% c(4, 5) ~ "Cuenta_propia_empleador",
                                 ocupacion_des %in% c(3, 8, 9) ~ "Informal_precario",
                                 ocupacion_des %in% c(6, 7) ~ "Sin_remuneracion",
                                 TRUE ~ NA_character_))
              )

test <- pre_test %>%
        mutate(jefe_mujer = factor(jefe_mujer, levels = c(1, 0), labels = c("Jefe_mujer", "Jefe_hombre")),
               jefe_salud_sub = factor(jefe_salud_sub, levels = c(1, 0), labels = c("Jefe_salud_subsidiado", "Jefe_salud_contributivo")),
               jefe_pension = factor(jefe_pension, levels = c(1, 0), labels = c("Jefe_af_pension", "Jefe_no_af_pension")),
               Dominio = factor(Dominio),
               jefe_nivel_educ = factor(jefe_nivel_educ, levels=c(0:6), labels=c('Ns','Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
               max_nivel_educ = factor(max_nivel_educ,levels=c(0:6), labels=c('Ns','Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
               viv_noPropia = factor(viv_noPropia, levels = c(1, 0), labels = c("sin_vivienda", "con_vivienda"),),
               Clase = factor(Clase,levels=c(1:2), labels=c('Cabecera','Resto')), 
                            
               ocupacion_ocu_f = factor(case_when(
                                 ocupacion_ocu %in% c(1, 2) ~ "Asal_formal",
                                 ocupacion_ocu %in% c(4, 5) ~ "Cuenta_propia_empleador",
                                 ocupacion_ocu %in% c(3, 8, 9) ~ "Informal_precario",
                                 ocupacion_ocu %in% c(6, 7) ~ "Sin_remuneracion",
                                 TRUE ~ NA_character_)), 
               
               ocupacion_des_f = factor(case_when(
                                 ocupacion_des %in% c(1, 2) ~ "Asal_formal",
                                 ocupacion_des %in% c(4, 5) ~ "Cuenta_propia_empleador",
                                 ocupacion_des %in% c(3, 8, 9) ~ "Informal_precario",
                                 ocupacion_des %in% c(6, 7) ~ "Sin_remuneracion",
                                 TRUE ~ NA_character_)))


# Normalizar variables numericas
  train <- train %>%
    mutate(across(where(is.numeric), ~ scale(.)[, 1], .names = "{.col}_z"))
  
  test <-test%>%
    mutate(across(where(is.numeric), ~ scale(.)[, 1], .names = "{.col}_z"))

# Up sampling para manejar clase imbalanceada
# Proporcion de la clase minoritaria = 20% -> Desbalance moderado
  set.seed(1103)
  upSampledTrain  <- upSample(x = train %>% select(-Pobre),
                             y = train$Pobre,
                             yname = "Pobre")
  dim(train)
  dim(upSampledTrain)
  table(upSampledTrain$Pobre)

# 5. GUARDAR BASES DE DATOS -train_personas_vars$jefe_salud_sub[is.na(train_personas_vars$jefe_salud_sub)] <- 1---------------------------------------------------

# Guardar los archivos en formato .rds en la carpeta stores
saveRDS(train, file.path(stores_path, "train_data.rds"))
saveRDS(test, file.path(stores_path, "test_data.rds"))
saveRDS(upSampledTrain, file.path(stores_path, "upsampled_train_data.rds"))

# Mensaje de proceso realizado
message(green("✅ Bases guardadas en "), green(stores_path))


