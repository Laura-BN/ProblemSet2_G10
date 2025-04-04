#-----------------------------------------------------------------------------//
# Modelo CART
# Problem Set 2 G10 - BDML 202501
# Fecha actualización: 04 de abril de 2025
#-----------------------------------------------------------------------------//


# 1. IMPORTAR DATOS ------------------------------------------------------------

up_train_raw <- readRDS(file.path(stores_path, "upsampled_train_data.rds"))
train_raw <- readRDS(file.path(stores_path, "train_data.rds"))
test_raw  <- readRDS(file.path(stores_path, "test_data.rds"))


# 2. PREPROCESAMIENTO ----------------------------------------------------------
colnames(up_train_raw)
# Eliminar algunas variables que no entran en el modelo
train_raw <- up_train_raw %>% select(-ends_with("_z"))
test_raw <- test_raw %>% select(-ends_with("_z"))

# Asegurar que Pobre es un factor con niveles correctos
train_raw$Pobre <- factor(train_raw$Pobre, levels = c("Pobre", "No_pobre"))

# Configuracion inicial: utilizar como referencia "Pobre" para la variable Pobre
train_raw <- train_raw  %>% mutate(Pobre = relevel(Pobre, ref="Pobre"))


# 3. DIVISION DE LA MUESTRA ----------------------------------------------------

# Establecer semillar
set.seed(91519) 

# Dividir datos en entrenamiento y validación

inTrain <- createDataPartition(
          y = train_raw$Pobre, ## La variable dependiente u objetivo 
          p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
          list = FALSE)

train <- train_raw[inTrain, ]
validation <- train_raw[-inTrain, ]

# Verificar la distribucion 
table(train$Pobre)
table(validation$Pobre)


# 4. CONSTRUIR EL ARBOL DE DECISION COMPLEJO -----------------------------------

# Crear el arbol complejo

complex_tree <- rpart(Pobre ~ jefe_edad + jefe_mujer + jefe_edad2 + jefe_salud_sub +
                        N_personas + hacinamiento + N_ocupados + N_inactivos +
                        N_menores + N_mayor_dependiente + max_nivel_educ + Clase, 
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

# 5. PODAR EL ARBOL USANDO MEJOR CP --------------------------------------------

# Ver tabla de costos y mejor cp
printcp(complex_tree)  

# Graficar el error vs cp
plotcp(complex_tree)

# Elegir el cp Óptimo
best_cp <- complex_tree$cptable[which.min(complex_tree$cptable[, "xerror"]), "CP"]
best_cp


# Crear el arbol complejo

cp_tree <- rpart(Pobre ~ jefe_edad + jefe_mujer + jefe_edad2 + jefe_salud_sub +
                    N_personas + hacinamiento + N_ocupados + N_inactivos +
                    N_menores + N_mayor_dependiente + max_nivel_educ + Clase, 
                  data = train,
                  method = "class",
                  cp = best_cp,  # complexity parameter, nuestro alpha
                  minbucket = 15 # Numero minimo de obs en hojas
                  )

# Utilizamos la función prp del paquete rpart.plot para graficar el arbol con cp
rpart.plot::prp(
            cp_tree,      
            under = TRUE,      # Mostrar la información debajo de cada nodo
            branch.lty = 2,    # Tipo de línea para las ramas (2 = línea punteada)
            yesno = 2,         # Mostrar indicadores de "sí"/"no"
            faclen = 0,        # Longitud de la abreviación para niveles de factores (0 = sin abreviación)
            varlen = 10,       # Longitud máxima para abreviar los nombres de variables
            box.palette = "-RdYlGn"  # Paleta de colores para las hojas
          )


# 6. EVALUACIÓN EN VALIDACIÓN --------------------------------------------------
  
  # Predecir en el conjunto de validación
  pred_prob_val <- predict(cp_tree, newdata = validation, type = "prob")
  
  # Clasificación basada en un umbral de 0.5
  validation$Pobre_Predicho <- ifelse(pred_prob_val[,1] >= 0.5, "Pobre", "No_pobre")
  
  # Calcular el AUC
  auc_val <- roc(validation$Pobre, pred_prob_val[,1])$auc
  print(paste("AUC en validación:", auc_val))
  
  

# 7. REDICCIÓN FINAL PARA KAGGLE ------------------------------------------
  
  # Predecir en test_raw
  pred_prob_test <- predict(cp_tree, newdata = test_raw, type = "prob")
  
  # Asignar predicciones
  test_raw$pobre <- ifelse(pred_prob_test[,1] >= 0.5, "Pobre", "No_pobre")
  
  # Verificar la distribucion 
  table(test_raw$Pobre)
  
  # Ajustar la base para enviar
  predictSample <- test_raw %>% 
                select(id, pobre) %>%
                mutate(pobre=ifelse(pobre=="Pobre",1,0))
  
  # Guardar la base de datos
  name <- paste0("CART_alfa_", best_cp, ".csv") 
  write.csv(predictSample, file.path(stores_path, name), row.names = FALSE)
  
  # Verificar la distribucion 
  table(predictSample$pobre)
  
  
  
  #######################################################
  
  predictSample <- test_subset %>% 
    mutate(pobre_pred = predict(model2, newdata = test_subset, type = "raw")) %>% 
    select(id, pobre_pred) # alpha= 0.6 and lambda = 0.01097
  
  predictSample <- predictSample %>% 
    mutate(pobre=ifelse(pobre_pred=="Si",1,0)) %>% 
    select(id,pobre)
  
  table(predictSample$pobre) #24404 
  
  
  
  #######################################################
  
# Calcular el AUC sobre los datos de prueba
  
  pobre <- ifelse(test$Pobre=="Si", 1, 0) #Volder default en test  numérico
  
  pred_prob <- predict(cv_tree, newdata = test, type = "prob")   

  aucval_cvtree <- Metrics::auc(actual = pobre, predicted = pred_prob[,1])
  aucval_cvtree
  
  roc_obj <- roc(pobre, pred_prob[,1])
  auc(roc_obj)
  
  colnames(pred_prob)  # Verifica los nombres de las columnas
  
  summary(pred_prob[,2])  # Revisa el rango de valores de las predicciones
  hist(pred_prob[,2])  # Verifica si todas las probabilidades son altas
  
  
  
  aucval_cvtree <- Metrics::auc(actual = pobre, predicted = pred_prob[,2])
  print(aucval_cvtree)
  
  
  length(pobre)  
  length(pred_prob[,2])
  
  table(test$Pobre)  # Verifica cuántos "Si" y "No" hay en test
  class(pobre)  # Debe ser "numeric"
  sum(is.na(pobre))  # Si hay valores NA, eso puede causar problemas

  head(pred_prob)  # Muestra las primeras filas
  dim(pred_prob)  # Verifica las dimensiones
  sum(is.na(pred_prob))  # Revisa si hay valores NA
  
  
  table(pobre) 
  
  summary(pred_prob[,2])  # Muestra mínimo, máximo, media, etc.
  length(unique(pred_prob[,2]))  # Cantidad de valores únicos
  
  
  summary(pred_prob[,2])  # Revisa el rango de valores de las probabilidades
  hist(pred_prob[,2])  # Gráfica de las predicciones
  
  
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

arbol <- rpart(Pobre ~ jefe_edad + jefe_mujer + jefe_edad2 +
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




 