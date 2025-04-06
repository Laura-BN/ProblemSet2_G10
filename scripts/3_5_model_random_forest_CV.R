#-----------------------------------------------------------------------------//
# Modelo Random Forest with Cross validation
# Problem Set 2 G10 - BDML 202501
# Fecha actualización: 04 de abril de 2025
#-----------------------------------------------------------------------------//


# 1. IMPORTAR DATOS ------------------------------------------------------------

up_train_raw <- readRDS(file.path(stores_path, "upsampled_train_data.rds"))
train_raw <- readRDS(file.path(stores_path, "train_data.rds"))
test_raw  <- readRDS(file.path(stores_path, "test_data.rds"))


# 2. PREPROCESAMIENTO ----------------------------------------------------------

# Eliminar algunas variables que no entran en el modelo
train_raw <- up_train_raw %>% select(-ends_with("_z"))
test_raw <- test_raw %>% select(-ends_with("_z"))

# Crear algunas interacciones
train_raw <- train_raw %>% 
  mutate(inter_haci_tam = N_personas * hacinamiento)

test_raw <- test_raw %>% 
  mutate(inter_haci_tam = N_personas * hacinamiento)

# Asegurar que Pobre es un factor con niveles correctos
train_raw$Pobre <- factor(train_raw$Pobre, levels = c("Pobre", "No_pobre"))

# Configuracion inicial: utilizar como referencia "Pobre" para la variable Pobre
train_raw <- train_raw  %>% mutate(Pobre = relevel(Pobre, ref="Pobre"))

# Crear variable numerica en train_raw
train_raw$pobre_num <- ifelse(train_raw$Pobre == "Pobre", 1, 0)


# 3. DIVISION DE LA MUESTRA ----------------------------------------------------

# Establecer semilla
set.seed(91519) 

# Dividir datos en entrenamiento y validación

inTrain <- createDataPartition(
          y = train_raw$Pobre, ## La variable dependiente u objetivo 
          p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
          list = FALSE)

train <- train_raw[inTrain, ]
validation <- train_raw[-inTrain, ]
train <- train_raw

# Verificar la distribucion 
table(train$Pobre)
table(validation$Pobre)


# 4. CONSTRUIR EL MODELO RANDOM FOREST ______-----------------------------------

# Crear el arbol complejo
rf<- ranger::ranger(
      Pobre ~ jefe_edad + jefe_mujer + jefe_edad2 + jefe_salud_sub +
        N_personas + hacinamiento + N_ocupados + N_inactivos +
        N_menores + N_mayor_dependiente + max_nivel_educ + Clase +
        inter_haci_tam, 
      data = train,
      num.trees= 1000, ## Numero de bootstrap samples y arboles a estimar. Default 500  
      mtry= 4,   # N. var aleatoriamente seleccionadas en cada partición
      min.node.size  = 1, ## Numero minimo de observaciones en un nodo
      importance="impurity") 

rf

# 5. PREDICCIONES EN VALIDACION (usando votos de los arboles) ------------------

# Predecimos con todos los arboles individualmente
pred_raw <-predict(rf, data = validation, predict.all = TRUE)$predictions


# Convertimos a data frame para operar con votos
pred.rf <- as.data.frame(pred_raw)

# Contamos votos por clase "Pobre" (asumimos codificada como 1)
ntrees <- ncol(pred.rf)
phat_rf_val <- rowSums(pred.rf == 1) / ntrees


# 6. CALCULAR AUC --------------------------------------------------------------

# Crear vector numérico para la variable dependiente real
actual_val <- ifelse(validation$Pobre == "Pobre", 1, 0)

# AUC
aucval_rf <- Metrics::auc(
                          actual = actual_val, 
                          predicted = phat_rf_val
                          )

print(paste("AUC en validación (votos RF):", round(aucval_rf, 5)))


# 7. TUNING AUTOMÁTICO CON CARET ----------------------------------------------

set.seed(91519)

# Definir control para cross-validation
ctrl <- trainControl(
  method = "cv",         # Validación cruzada
  number = 5,            # 5-fold CV
  classProbs = TRUE,     # Necesario para AUC
  summaryFunction = twoClassSummary, # Usamos AUC como métrica
  verboseIter = TRUE     # Para seguir el proceso
)

# Reetiquetar niveles para que caret los tome bien (Pobre = "yes")
train$Pobre <- relevel(train$Pobre, ref = "Pobre")

# Definir grid de búsqueda
tuneGrid <- expand.grid(
  mtry = c(2, 4, 6, 8),
  splitrule = "gini",
  min.node.size = c(1, 5, 10)
)

# Entrenar modelo con tuning
rf_tuned <- train(
            Pobre ~ jefe_edad + jefe_mujer + jefe_edad2 + jefe_salud_sub +
              N_personas + hacinamiento + N_ocupados + N_inactivos +
              N_menores + N_mayor_dependiente + max_nivel_educ + Clase + 
              inter_haci_tam,
            data = train,
            method = "ranger",
            trControl = ctrl,
            tuneGrid = tuneGrid,
            metric = "ROC",
            num.trees = 1000,
            importance = "impurity"
            )


# Mejor modelo
print(rf_tuned)
plot(rf_tuned)


# 9. VALIDACIÓN ---------------------------------------------------------------

# Predecir la probabilidad de ser "Pobre"
pred_prob <- predict(rf_tuned, newdata = train, type = "prob")[, "Pobre"]

# Calcular el AUC
roc_obj <- roc(response = train$Pobre, predictor = pred_prob)
auc(roc_obj)

# Graficar la curva ROC
plot(roc_obj, col = "#2c3e50", lwd = 2, main = "Curva ROC - Random Forest")


# Predecir la probabilidad de ser "Pobre"
pred_prob_val <- predict(rf_tuned, newdata = validation, type = "prob")[, "Pobre"]

# Calcular el AUC
roc_val <- roc(response = validation$Pobre, predictor = pred_prob_val)
auc_val <- auc(roc_val)
print(auc_val)


# 8. PREDECIR CON MODELO TUNED ----------------------------------------------
  
# Predecimos en test
pred_prob_test <- predict(rf_tuned, newdata = test_raw, type = "prob")[, "Pobre"]

# Crear predicción final
submission_tuned <- test_raw %>%
                    select(id) %>%
                    mutate(pobre = pred_prob_test)

# --------- NOMBRE DEL ARCHIVO SEGÚN PARÁMETROS TUNEADOS ------------

# Extraer mejores hiperparámetros del modelo entrenado con caret
best_mtry <- rf_tuned$bestTune$mtry
best_min_node <- rf_tuned$bestTune$min.node.size
best_splitrule <- rf_tuned$bestTune$splitrule

# Crear nombre del archivo con los mejores parámetros
name_tuned <- paste0("RFcv_ntrees_", rf_tuned$finalModel$num.trees,
                     "_mtry_", best_mtry,
                     "_minNode_", best_min_node,
                     "_split_", best_splitrule,
                     ".csv")

# Guardar archivo
write.csv(submission_tuned, file = file.path(stores_path, name_tuned), row.names = FALSE)

# Confirmación
print(paste("Archivo guardado:", name_tuned))



 