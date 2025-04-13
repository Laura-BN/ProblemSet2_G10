#-----------------------------------------------------------------------------//
# Modelo Random Forest
# Problem Set 2 G10 - BDML 202501
# Fecha actualización: 03 de abril de 2025
#-----------------------------------------------------------------------------//


# 1. IMPORTAR DATOS ------------------------------------------------------------

up_train_raw <- readRDS(file.path(stores_path, "upsampled_train_data.rds"))
train_raw <- readRDS(file.path(stores_path, "train_data.rds"))
test_raw  <- readRDS(file.path(stores_path, "test_data.rds"))

# Eliminar algunas variables que no entran en el modelo
train_raw <- train_raw %>% select(-ends_with("_z"))
test_raw <- test_raw %>% select(-ends_with("_z"))

# Configuracion inicial: utilizar como referencia "Pobre" para la variable Pobre
train_raw <- train_raw  %>% mutate(Pobre = relevel(Pobre, ref="Pobre"))


# 3. DIVISION DE LA MUESTRA ----------------------------------------------------

# Establecer semillar
set.seed(91519) 

inTrain <- createDataPartition(
          y = train_raw$Pobre, ## La variable dependiente u objetivo 
          p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
          list = FALSE)


train <- train_raw[ inTrain,]
test  <- train_raw[-inTrain,]

# Verificar la distribucion 
table(train$Pobre)
table(test$Pobre)


# 3. CONSTRUIR EL ARBOL --------------------------------------------------------

# Crear el arbol lo más complejo posible

complex_tree <- rpart(Pobre ~ jefe_edad + jefe_edad2 + jefe_mujer + 
                        N_personas + hacinamiento + N_ocupados + N_inactivos + N_desocupados +
                        N_menores + N_mayor_dependiente + max_nivel_educ + Clase + Dominio, 
                      data = train,
                      method = "class",
                      cp = 0,  # complexity parameter, nuestro alpha
                      minbucket = 15 # Numero minimo de obs en hojas
                      )


# Utilizamos la función prp del paquete rpart.plot para graficar el árbol de decisión
rpart.plot::prp(
            complex_tree,      
            under = TRUE,      # Mostrar la información debajo de cada nodo
            branch.lty = 2,    # Tipo de línea para las ramas (2 = línea punteada)
            yesno = 2,         # Mostrar indicadores de "sí"/"no"
            faclen = 0,        # Longitud de la abreviación para niveles de factores (0 = sin abreviación)
            varlen = 10,       # Longitud máxima para abreviar los nombres de variables
            box.palette = "-RdYlGn"  # Paleta de colores para las hojas
            )

# 4. PODAR EL ARBOL ------------------------------------------------------------

# Establecer los parametros del proceso de validación cruzada

  fiveStats <- function(...) {
    c(
      twoClassSummary(...),
      defaultSummary(...)
    )
  }
  ## Para usar ROC) (u otras más) para tuning
  
  ctrl<- trainControl(method = "cv",
                      number = 5,
                      summaryFunction = fiveStats, # nuestra función 
                      classProbs = TRUE, 
                      verbose=FALSE,
                      savePredictions = T)
  
  # especificamos la grilla de los alphas
  grid <- expand.grid(cp = seq(0, 0.03, 0.001))

  cv_tree <- train(Pobre ~ jefe_edad + jefe_edad2 + jefe_mujer + 
                     N_personas + hacinamiento + N_ocupados + N_inactivos + N_desocupados +
                     N_menores + N_mayor_dependiente + max_nivel_educ + Clase + Dominio,
                   data = train,
                   method = "rpart", 
                   trControl = ctrl, 
                   tuneGrid = grid, 
                   metric= "ROC"
                  )
  cv_tree
  
  # Ver el valor del alfa que maximiza el AUC
  cv_tree$bestTune$cp

# Graficar el arbol final
  rpart.plot::prp(
            cv_tree$finalModel,      
            under = TRUE,      # Mostrar la información debajo de cada nodo
            branch.lty = 2,    # Tipo de línea para las ramas (2 = línea punteada)
            yesno = 2,         # Mostrar indicadores de "sí"/"no"
            faclen = 0,        # Longitud de la abreviación para niveles de factores (0 = sin abreviación)
            varlen = 10,       # Longitud máxima para abreviar los nombres de variables
            box.palette = "-RdYlGn"  # Paleta de colores para las hojas
            )
  
# Calcular el AUC sobre los datos de prueba
  
  pobre <- ifelse(test$Pobre=="Si", 1, 0) #Volder default en test  numérico
  
  pred_prob <- predict(cv_tree, newdata = test, type = "prob")   

  aucval_cvtree <- Metrics::auc(actual = pobre, predicted = pred_prob[,2])
  aucval_cvtree
  
  length(pobre)  
  
  length(pred_prob[,2])
  
  

# -----------

# Obtener alfa que minimiza el error

  ## Ver tabla de costos y mejor cp
  printcp(complex_tree)  

  ## Graficar el error vs cp
  plotcp(complex_tree)

  ## Elegir el cp Óptimo
  best_cp <- complex_tree$cptable[which.min(complex_tree$cptable[, "xerror"]), "CP"]
  best_cp

# Podar el arbol con el mejor cp

# Graficar el arbol con alfa que minimiza el error (podar el arbol)

arbol <- rpart(Pobre ~ jefe_edad + jefe_edad2 + jefe_mujer + 
                        N_personas + hacinamiento + N_ocupados + N_inactivos + N_desocupados +
                        N_menores + N_mayor_dependiente + max_nivel_educ + Clase, 
                      data = train,
                      method = "class",
                      cp = ,  # complexity parameter, nuestro alpha
                      minbucket = 15 # Numero minimo de obs en hojas
              )


# Graficar el arbol luego de podarlo

rpart.plot::prp(
            arbol,      
            under = TRUE,      # Mostrar la información debajo de cada nodo
            branch.lty = 2,    # Tipo de línea para las ramas (2 = línea punteada)
            yesno = 2,         # Mostrar indicadores de "sí"/"no"
            faclen = 0,        # Longitud de la abreviación para niveles de factores (0 = sin abreviación)
            varlen = 10,       # Longitud máxima para abreviar los nombres de variables
            box.palette = "-RdYlGn"  # Paleta de colores para las hojas
          )


# Calcular el AUC del arbol

  ## Volver Pobre en test numerico
  Pobre <- ifelse(test$Pobre=="Pobre", 1, 0)
  
  ## Predecri la probabilidad (en lugar de la clase)
  pred_prob <- predict(arbol, newdata = test, type = "prob")
  
  ## Calcular el AUC
  aucval_arbol <- Metrics::auc(actual = Pobre, predicted = pred_prob[,2]) 

  aucval_arbol



# *****************************************************************************

# 2. CREAR NUEVAS VARIABLES EN LA BASE PERSONAS --------------------------------

# Variable para la base train personas

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
                      jefe_nivel_educ = ifelse(P6050 == 1, nivel_educ, NA) # Jefe de hogar no cotiza a pensión
                      ) %>% 
                      select(id, Orden, pt, Pet, mujer, jefe_hogar, jefe_mujer, jefe_salud_sub,
                             jefe_pension, jefe_edad, jefe_edad2, menor, mayor_dependiente,
                             nivel_educ, ocupado, desocupado, inactivo, jefe_nivel_educ, jefe_ocu)


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
                        jefe_nivel_educ = ifelse(P6050 == 1, nivel_educ, NA) # Jefe de hogar no cotiza a pensión
                      ) %>% 
                      select(id, Orden, pt, Pet, mujer, jefe_hogar, jefe_mujer, jefe_salud_sub,
                             jefe_pension, jefe_edad, jefe_edad2, menor, mayor_dependiente,
                             nivel_educ, ocupado, desocupado, inactivo, jefe_nivel_educ, jefe_ocu)


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
                               jefe_edad, jefe_edad2, jefe_nivel_educ) %>%
                        left_join(train_personas_hogar_B, by = "id")

test_personas_hogar <- test_personas_vars  %>% 
                        filter(jefe_hogar==1) %>%
                        select(id, jefe_mujer, jefe_salud_sub, jefe_pension, 
                               jefe_edad, jefe_edad2, jefe_nivel_educ) %>%
                        left_join(test_personas_hogar_B, by = "id")


# Variables a nivel de hogar con la base de hogares

train_hogares_vars <- train_hogares %>% 
                      mutate(hacinamiento = Nper/P5000, # Personas por cuarto en el hogar
                             viv_noPropia = ifelse(P5090 == 1 | P5090 == 2, 0L, 1L)) %>%
                      select(id, Clase, Dominio, hacinamiento, Nper, Pobre) # Seleccionar variables de interes

test_hogares_vars <- test_hogares %>% 
                      mutate(hacinamiento = Nper/P5000, # Personas por cuarto en el hogar
                             viv_noPropia = ifelse(P5090 == 1 | P5090 == 2, 0L, 1L)) %>%
                      select(id, Clase, Dominio, hacinamiento, Nper) # Seleccionar variables de interes


# 5. CREAR VARIABLES A NIVEL DE HOGAR ------------------------------------------
                        
# Unir las bases de datos 

pre_train <- train_hogares_vars %>% 
            left_join(train_personas_hogar, by = "id") %>%
            select(-id) # No se necesitará más la variable id

pre_test <- test_hogares_vars %>% 
            left_join(test_personas_hogar, by = "id") 

# Convertir las variables categoricas y obtener la base final

train <- pre_train %>%
        mutate(Pobre = factor(Pobre, levels=c(1,0), labels=c("Pobre","No_pobre")),
               jefe_mujer = factor(jefe_mujer, levels = c(1, 0), labels = c("Jefe_mujer", "Jefe_hombre")),
               jefe_salud_sub = factor(jefe_salud_sub, levels = c(1, 0), labels = c("Jefe_salud_subsidiado", "Jefe_salud_contributivo")),
               jefe_pension = factor(jefe_pension, levels = c(1, 0), labels = c("Jefe_af_pension", "Jefe_no_af_pension")),
               
               Dominio = factor(Dominio),
               jefe_nivel_educ = factor(jefe_nivel_educ, levels=c(0:6), labels=c('Ns','Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
               max_nivel_educ = factor(max_nivel_educ,levels=c(0:6), labels=c('Ns','Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
               Clase = factor(Clase,levels=c(1:2), labels=c('Cabecera','Resto'))
              )

test <- pre_test %>%
        mutate(jefe_mujer = factor(jefe_mujer, levels = c(1, 0), labels = c("Jefe_mujer", "Jefe_hombre")),
               jefe_salud_sub = factor(jefe_salud_sub, levels = c(1, 0), labels = c("Jefe_salud_subsidiado", "Jefe_salud_contributivo")),
               jefe_pension = factor(jefe_pension, levels = c(1, 0), labels = c("Jefe_af_pension", "Jefe_no_af_pension")),
               Dominio = factor(Dominio),
               jefe_nivel_educ = factor(jefe_nivel_educ, levels=c(0:6), labels=c('Ns','Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
               max_nivel_educ = factor(max_nivel_educ,levels=c(0:6), labels=c('Ns','Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
               Clase = factor(Clase,levels=c(1:2), labels=c('Cabecera','Resto'))
              )

# Normalizar variables numericas
  train <- train %>%
    mutate(across(where(is.numeric), ~ scale(.)[, 1], .names = "{.col}_z"))
  
  test <-test%>%
    mutate(across(where(is.numeric), ~ scale(.)[, 1], .names = "{.col}_z"))

# Up sampling para manejar clase imbalanceada
# Proporcion de la clase minoritaria = 20% -> Desbalance moderado
  set.seed(1103)
  upSampledTrain  <- upSample(x = train,
                             y = train$Pobre,
                             yname = "Pobre")
  dim(train)
  dim(upSampledTrain)
  table(upSampledTrain$Pobre)

# 5. GUARDAR BASES DE DATOS ----------------------------------------------------

# Guardar los archivos en formato .rds en la carpeta stores
saveRDS(train, file.path(stores_path, "train_data.rds"))
saveRDS(test, file.path(stores_path, "test_data.rds"))
saveRDS(upSampledTrain, file.path(stores_path, "upsampled_train_data.rds"))


# Mensaje de proceso realizado
message(green("✅ Bases guardadas en "), green(stores_path))


 