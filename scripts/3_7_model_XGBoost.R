#-----------------------------------------------------------------------------//
# Modelo XGBoost
# Problem Set 2 G10 - BDML 202501
# Fecha actualización: 05 de abril de 2025
#-----------------------------------------------------------------------------//

# 1. IMPORTAR DATOS ------------------------------------------------------------

#up_train_raw <- readRDS(file.path(stores_path, "upsampled_train_data.rds"))
train_raw <- readRDS(file.path(stores_path, "train_data.rds"))
test_raw  <- readRDS(file.path(stores_path, "test_data.rds"))


# 2. PREPROCESAMIENTO ----------------------------------------------------------

# Eliminar algunas variables que no entran en el modelo
train_raw <- train_raw %>% select(-ends_with("_z"))
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

# Verificar la distribucion 
table(train$Pobre)
table(validation$Pobre)


# 4. CONSTRUIR EL XGBOOST -----------------------------------------------------

#   Definir la grilla

grid_xbgoost <- expand.grid(nrounds = c(250,500),
                            max_depth = c(1, 2),
                            eta = c(0.01, 0.1), 
                            gamma = c(0, 1), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.4, 0.7), 
                            subsample = c(0.7))
grid_xbgoost


# Definir el cross validation

fiveStats <- function(...) {
                c(
                  caret::twoClassSummary(...), # Returns ROC, Sensitivity, and Specificity
                  caret::defaultSummary(...)  # Returns RMSE and R-squared (for regression) or Accuracy and Kappa (for classification)
                )
              }

fitControl <- trainControl(method = "cv",
                          number = 5,
                          summaryFunction = fiveStats,
                          classProbs = TRUE,
                          savePredictions = T,
                          verboseIter = TRUE)


# Definir el modelo

set.seed(91519) # semilla

Xgboost_tree <- train(Pobre ~ jefe_edad + jefe_mujer + jefe_edad2 + jefe_salud_sub +
                        N_personas + hacinamiento + N_ocupados + N_inactivos +
                        N_menores + N_mayor_dependiente + max_nivel_educ + Clase +
                        viv_noPropia,
                      data = train, 
                      method = "xgbTree", 
                      trControl = fitControl,
                      tuneGrid=grid_xbgoost,
                      metric = "ROC",
                      verbosity = 0
                      )

Xgboost_tree



# 5. PREDICCIONES EN VALIDACION (XGBoost) --------------------------------------


# Probabilidades estimadas de ser "Pobre"
phat_xgb_val <- predict(Xgboost_tree,
                        newdata = validation,
                        type = "prob")[, "Pobre"]

# Clasificación con umbral 0.5
pred_class_val <- ifelse(phat_xgb_val >= 0.3, 1, 0)

# Vector real binario
actual_val <- ifelse(validation$Pobre == "Pobre", 1, 0)

# Verificar que todos tienen misma longitud
stopifnot(length(actual_val) == length(phat_xgb_val))
stopifnot(length(actual_val) == length(pred_class_val))

# AUC
aucval_xgb <- Metrics::auc(actual = actual_val, predicted = phat_xgb_val)

# F1 Score
f1_val_xgb <- MLmetrics::F1_Score(y_true = actual_val, y_pred = pred_class_val, positive = "1")

# Matriz de confusión
cm_xgb <- caret::confusionMatrix(as.factor(pred_class_val), as.factor(actual_val), positive = "1")

# Imprimir métricas
cat("AUC en validación (XGBoost):", round(aucval_xgb, 5), "\n")
cat("F1 Score en validación (umbral 0.3):", round(f1_val_xgb, 4), "\n")
print(cm_xgb)


# 6. PREDICCIONES EN TEST PARA KAGGLE (XGBoost) --------------------------------

# 1. Estimar probabilidad de ser "Pobre"
phat_xgb_test <- predict(Xgboost_tree, newdata = test_raw, type = "prob")[, "Pobre"]

# 2. Clasificar con umbral 0.5
test_raw$pobre <- ifelse(phat_xgb_test >= 0.3, "Pobre", "No_pobre")

# 3. Crear base de predicción para Kaggle
predictSample <- test_raw %>%
  select(id, pobre) %>%
  mutate(pobre = ifelse(pobre == "Pobre", 1, 0))

# 4. Crear nombre del archivo dinámicamente con mejores parámetros
best_params <- Xgboost_tree$bestTune

# (Opcional: redondear algunos para nombre más corto)
name <- sprintf("XGB_cv_%dfolds_n%d_d%d_eta%.2f_g%.1f_cs%.2f_mc%d_ss%.2f.csv",
                fitControl$number,
                best_params$nrounds,
                best_params$max_depth,
                best_params$eta,
                best_params$gamma,
                best_params$colsample_bytree,
                best_params$min_child_weight,
                best_params$subsample)

# 5. Guardar CSV para envío a Kaggle
write.csv(predictSample, file.path(stores_path, name), row.names = FALSE)

# 6. Mostrar resumen y confirmar guardado
cat("Predicciones guardadas correctamente\n")
print(table(predictSample$pobre))
cat("Archivo de predicciones:", name, "\n")

# 7. Guardar parámetros en un archivo de texto como bitácora
param_log_name <- gsub(".csv", "_params.txt", name)

sink(file.path(stores_path, param_log_name))
cat("Mejores parámetros del modelo XGBoost\n\n")
print(best_params)
sink()

cat("Bitacora de parámetros guardada en:", param_log_name, "\n")





# 7. SELECCIONAR UMBRAL ÓPTIMO (XGBoost) --------------------------------

# Vector de probabilidades (ya deberías tenerlo)
phat_xgb_val <- predict(Xgboost_tree, newdata = validation, type = "prob")[, "Pobre"]
actual_val <- ifelse(validation$Pobre == "Pobre", 1, 0)

# Buscar el mejor umbral
thresholds <- seq(0.1, 0.9, by = 0.01)
f1_scores <- sapply(thresholds, function(thresh) {
  pred_class <- ifelse(phat_xgb_val >= thresh, 1, 0)
  F1_Score(y_true = actual_val, y_pred = pred_class, positive = "1")
})

best_thresh <- thresholds[which.max(f1_scores)]
cat("Mejor umbral para F1 Score:", round(best_thresh, 2), "\n")
cat("Mejor F1 Score obtenido:", round(max(f1_scores), 4), "\n")

# Graficar F1 Score vs Umbral
plot(thresholds, f1_scores, type = "l", col = "blue", lwd = 2,
     xlab = "Umbral", ylab = "F1 Score", main = "Optimización del umbral")
abline(v = best_thresh, col = "red", lty = 2)
