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

# Crear lista de variables relacionadas con ingresos
vars_ingresos <- c("P6510", "P6545", "P6580", "P6585s1", "P6585s2", "P6585s3", "P6585s4",
                   "P6590", "P6600", "P6620", "P6630s1", "P6630s2", "P6630s3", "P6630s4", "P6630s6",
                   "P7472", "P7495", "P7500s2", "P7500s3", "P7505", "P7422")


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
                      menor12 = ifelse(P6040<=12,1,0), # Persona menores 10 anios en el hogar
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
                mutate(anios_educ = case_when(
                    is.na(P6210) ~ 0,  # Si no hay info de nivel educativo
                    is.na(P6210s1) & P6210 == 1 ~ 0,
                    is.na(P6210s1) & P6210 == 2 ~ 0,
                    is.na(P6210s1) & P6210 == 3 ~ 0,
                    is.na(P6210s1) & P6210 == 4 ~ 5,
                    is.na(P6210s1) & P6210 == 5 ~ 9,
                    is.na(P6210s1) & P6210 == 6 ~ 11,
                  
                    P6210 == 9 ~ 0,
                    P6210 == 1 ~ 0,  # Ninguno
                    P6210 == 2 ~ ifelse(P6210s1 == 1, 1, 0),  # Preescolar: 1 si aprobó, 0 si no
                    P6210 == 3 ~ P6210s1,  # Primaria: años 1 a 5
                    P6210 == 4 ~ P6210s1,  # Secundaria: años 6 a 9
                    P6210 == 5 ~ case_when(
                      P6210s1 == 10 ~ 10,
                      P6210s1 == 11 ~ 11,
                      P6210s1 %in% c(12, 13) ~ 11,  # Normalistas
                      TRUE ~ 0  # Si no cumple ninguna de las anteriores
                    ),
                    P6210 == 6 ~ 11 + P6210s1,  # Educación superior
                    TRUE ~ 0  # Cualquier otro caso no contemplado
                  ))


# Incluir el indicador de ingreso aproximado en train_personas_vars

train_personas_vars <- train_personas_vars %>%
                      mutate(across(all_of(vars_ingresos), ~ ifelse(. == 1, 1, 0)))

train_personas_vars$ind_ingresos_aprox <- rowSums(train_personas_vars[ , vars_ingresos], na.rm = TRUE)

train_personas_vars <- train_personas_vars %>% 
                      select(id, Orden, pt, Pet, mujer, jefe_hogar, jefe_mujer, jefe_salud_sub,
                             jefe_pension, jefe_edad, jefe_edad2, menor, mayor_dependiente,
                             nivel_educ, ocupado, desocupado, inactivo, jefe_nivel_educ, jefe_ocu,
                             antiguedad_empleo, ocupacion_ocu, ocupacion_des, subsidios, 
                             ind_ingresos_aprox, anios_educ, P6040, menor12)


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
                        menor12 = ifelse(P6040<=12,1,0), # Persona menores 10 anios en el hogar
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
                    mutate(anios_educ = case_when(
                            is.na(P6210) ~ 0,  # Si no hay info de nivel educativo
                            is.na(P6210s1) & P6210 == 1 ~ 0,
                            is.na(P6210s1) & P6210 == 2 ~ 0,
                            is.na(P6210s1) & P6210 == 3 ~ 0,
                            is.na(P6210s1) & P6210 == 4 ~ 5,
                            is.na(P6210s1) & P6210 == 5 ~ 9,
                            is.na(P6210s1) & P6210 == 6 ~ 11,
                            
                            P6210 == 9 ~ 0,
                            P6210 == 1 ~ 0,  # Ninguno
                            P6210 == 2 ~ ifelse(P6210s1 == 1, 1, 0),  # Preescolar: 1 si aprobó, 0 si no
                            P6210 == 3 ~ P6210s1,  # Primaria: años 1 a 5
                            P6210 == 4 ~ P6210s1,  # Secundaria: años 6 a 9
                            P6210 == 5 ~ case_when(
                              P6210s1 == 10 ~ 10,
                              P6210s1 == 11 ~ 11,
                              P6210s1 %in% c(12, 13) ~ 11,  # Normalistas
                              TRUE ~ 0  # Si no cumple ninguna de las anteriores
                            ),
                            P6210 == 6 ~ 11 + P6210s1,  # Educación superior
                            TRUE ~ 0  # Cualquier otro caso no contemplado
                          ))


# Incluir el indicador de ingreso aproximado en test_personas_vars

test_personas_vars <- test_personas_vars %>%
                      mutate(across(all_of(vars_ingresos), ~ ifelse(. == 1, 1, 0)))

test_personas_vars$ind_ingresos_aprox <- rowSums(test_personas_vars[ , vars_ingresos], na.rm = TRUE)

test_personas_vars <- test_personas_vars %>% 
                      select(id, Orden, pt, Pet, mujer, jefe_hogar, jefe_mujer, jefe_salud_sub,
                             jefe_pension, jefe_edad, jefe_edad2, menor, mayor_dependiente,
                             nivel_educ, ocupado, desocupado, inactivo, jefe_nivel_educ, jefe_ocu,
                             antiguedad_empleo, ocupacion_ocu, ocupacion_des, subsidios, 
                             ind_ingresos_aprox, anios_educ, P6040, menor12)



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
                                  N_menores12 = sum(menor12, na.rm = TRUE), # Personas menores 10 anios en el hogar
                                  N_mayor_dependiente = sum(mayor_dependiente, na.rm = TRUE), # Adultos mayores dependientes en el hogar
                                  N_mujer = sum(mujer, na.ram=TRUE), # Mujeres en el hogar
                                  max_nivel_educ = max(nivel_educ, na.rm=TRUE), # Maximo nivel educativo en el hogar
                                  total_ind_ingresos = sum(ind_ingresos_aprox, na.rm = TRUE), # total fuentes de ingreso por hogar
                                  # Promedio de educación para personas de 15 años o más
                                  promedio_anios_educ = ifelse(
                                                        sum(P6040 >= 15 & !is.na(anios_educ)) > 0,
                                                        mean(anios_educ[P6040 >= 15], na.rm = TRUE),
                                                        0
                                                        ),
                                  anios_educ_hogar = sum(anios_educ, na.ram=TRUE), # Anios educacion hogar
                                  ) %>%
                        mutate(prop_des_pet = N_desocupados/N_personas, # Proporcion desocupados / pt
                               prop_ina_pet = N_inactivos/N_personas, # Proporcion inactivos / pt
                               prop_ocu_pet = N_ocupados/N_personas, # Proporcion ocupados / pt
                               prop_fuentes_ing = total_ind_ingresos/N_personas, # Proporcion fuentes de ingreso por persona
                               prop_menores_pob = N_menores/N_personas, # Proporcion menores hogar
                               prop_menores12_pob = N_menores12/N_personas, # Proporcion menores 10 anios hogar
                               prop_mayores_pob = N_mayor_dependiente/N_personas, # Proporcion personas mayores hogar
                               prop_anios_educ = anios_educ_hogar/N_personas # Anios promedio educacion hogar
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
                                  N_menores12 = sum(menor12, na.rm = TRUE), # Personas menores 10 anios en el hogar
                                  N_mayor_dependiente = sum(mayor_dependiente, na.rm = TRUE), # Adultos mayores dependientes en el hogar
                                  N_mujer = sum(mujer, na.ram=TRUE), # Mujeres en el hogar
                                  max_nivel_educ = max(nivel_educ, na.rm=TRUE), # Maximo nivel educativo en el hogar
                                  total_ind_ingresos = sum(ind_ingresos_aprox, na.rm = TRUE), # total fuentes de ingreso por hogar
                                  # Promedio de educación para personas de 15 años o más
                                  promedio_anios_educ = ifelse(
                                    sum(P6040 >= 15 & !is.na(anios_educ)) > 0,
                                    mean(anios_educ[P6040 >= 15], na.rm = TRUE),
                                    0
                                    ),
                                  anios_educ_hogar = sum(anios_educ, na.ram=TRUE), # Anios educacion hogar
                                  ) %>%
                        mutate(prop_des_pet = N_desocupados/N_personas, # Proporcion desocupados / pt
                               prop_ina_pet = N_inactivos/N_personas, # Proporcion inactivos / pt
                               prop_ocu_pet = N_ocupados/N_personas, # Proporcion ocupados / pt
                               prop_fuentes_ing = total_ind_ingresos/N_personas, # Proporcion fuentes de ingreso por persona
                               prop_menores_pob = N_menores/N_personas, # Proporcion menores hogar
                               prop_menores12_pob = N_menores12/N_personas, # Proporcion menores 10 anios hogar
                               prop_mayores_pob = N_mayor_dependiente/N_personas, # Proporcion personas mayores hogar
                               prop_anios_educ = anios_educ_hogar/N_personas # Anios promedio educacion hogar
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
                             viv_noPropia = ifelse(P5090 == 1 | P5090 == 2, 0L, 1L),
                             pago_arriendo = case_when(
                                               is.na(P5140) ~ "no_arriendo",
                                               P5140 >= 0 & P5140 <= 500000 ~ "arriendo_bajo",
                                               P5140 > 500000 & P5140 <= 1500000 ~ "arriendo_medio",
                                               P5140 > 1500000 ~ "arriendo_alto"
                                             ),
                             pago_arriendo = factor(
                                         pago_arriendo,
                                         levels = c("no_arriendo", "arriendo_bajo", "arriendo_medio", "arriendo_alto")
                                       )
                             ) %>%
                      select(id, Clase, Dominio, hacinamiento, Nper, Pobre, viv_noPropia, Lp, pago_arriendo) # Seleccionar variables de interes

test_hogares_vars <- test_hogares %>% 
                      mutate(hacinamiento = Nper/P5000, # Personas por cuarto en el hogar
                             viv_noPropia = ifelse(P5090 == 1 | P5090 == 2, 0L, 1L),
                             pago_arriendo = case_when(
                                               is.na(P5140) ~ "no_arriendo",
                                               P5140 >= 0 & P5140 <= 500000 ~ "arriendo_bajo",
                                               P5140 > 500000 & P5140 <= 1500000 ~ "arriendo_medio",
                                               P5140 > 1500000 ~ "arriendo_alto"
                                             ),
                             pago_arriendo = factor(
                                              pago_arriendo,
                                             levels = c("no_arriendo", "arriendo_bajo", "arriendo_medio", "arriendo_alto")
                                            )
                      ) %>%
                      select(id, Clase, Dominio, hacinamiento, Nper, viv_noPropia, Lp, pago_arriendo) # Seleccionar variables de interes


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
               jefe_ocu = factor(jefe_ocu, levels = c(1, 0), labels = c("Jefe_ocu", "Jefe_no_ocu")),
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
               
                tipo_trabajo = factor(case_when(
                   ocupacion_ocu %in% c(1, 2, 3, 8) ~ "ocu_asalariado",
                   ocupacion_ocu %in% c(4) ~ "ocu_propia",
                   ocupacion_ocu %in% c(5) ~ "ocu_patron",
                   ocupacion_ocu %in% c(6, 7, 9) ~ "ocu_otro",
                   TRUE ~ "no_ocu")), 
               
               ocupacion_des_f = factor(case_when(
                                 ocupacion_des %in% c(1, 2) ~ "Asal_formal",
                                 ocupacion_des %in% c(4, 5) ~ "Cuenta_propia_empleador",
                                 ocupacion_des %in% c(3, 8, 9) ~ "Informal_precario",
                                 ocupacion_des %in% c(6, 7) ~ "Sin_remuneracion",
                                 TRUE ~ NA_character_))
              )

test <- pre_test %>%
        mutate(jefe_mujer = factor(jefe_mujer, levels = c(1, 0), labels = c("Jefe_mujer", "Jefe_hombre")),
               jefe_ocu = factor(jefe_ocu, levels = c(1, 0), labels = c("Jefe_ocu", "Jefe_no_ocu")),
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
               
               tipo_trabajo = factor(case_when(
                             ocupacion_ocu %in% c(1, 2, 3, 8) ~ "ocu_asalariado",
                             ocupacion_ocu %in% c(4) ~ "ocu_propia",
                             ocupacion_ocu %in% c(5) ~ "ocu_patron",
                             ocupacion_ocu %in% c(6, 7, 9) ~ "ocu_otro",
                             TRUE ~ "no_ocu")),
               
               
               ocupacion_des_f = factor(case_when(
                                 ocupacion_des %in% c(1, 2) ~ "Asal_formal",
                                 ocupacion_des %in% c(4, 5) ~ "Cuenta_propia_empleador",
                                 ocupacion_des %in% c(3, 8, 9) ~ "Informal_precario",
                                 ocupacion_des %in% c(6, 7) ~ "Sin_remuneracion",
                                 TRUE ~ NA_character_))
               )


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


