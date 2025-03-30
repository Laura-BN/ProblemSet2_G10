
# IMPORTAR DATOS ------------------------------------------------------------
train <- readRDS(file.path(stores_path, "upsampled_train_data.rds"))
test  <- readRDS(file.path(stores_path, "test_data.rds"))

summary(train[, c("hacinamiento", "Clase", "jefe_mujer", "jefe_pension", 
                  "jefe_edad", "jefe_nivel_educ", "N_ocupados", "N_mayor_dependiente")])

summary(test)

# Seleccionar solo las variables deseadas más la variable dependiente
columnas <- c("hacinamiento", "Clase", "jefe_mujer", "jefe_pension", 
              "jefe_edad", "jefe_edad2", "jefe_nivel_educ", "N_ocupados",
              "N_mayor_dependiente", "Pobre")

# Filtrar el dataset
train_subset <- train[, columnas]

# Definir función de F1
f1_function <- function(data, lev = NULL, model = NULL) {
   precision <- posPredValue(data$pred, data$obs, positive = lev[1])
                recall  <- sensitivity(data$pred, data$obs, positive = lev[1])
                f1_val <- (2 * precision * recall) / (precision + recall)
                c(F1 = f1_val)
 }
 
# Configurar `trainControl` para maximizar F1
 ctrl <- trainControl(
   method = "cv",  
   number = 5,  
   summaryFunction = f1_function, 
   classProbs = T,  
   verbose = F,  
   savePredictions = T  
 )
 
 # 5 Definición de la grilla de hiperparámetros 
 lambda <- 10^seq(2, -6, length = 100)  
 alpha <- seq(0, 1, by = 0.1) 
 grid <- expand.grid("alpha" = alpha, "lambda" = lambda) 
 
# 6. Entrenar el modelo usando `glmnet`
set.seed(1234)
model1 <- train(
   Pobre ~ .,  
   data = train_subset,  
   method = "glmnet",
   family = "binomial",
   trControl = ctrl,  
   preProcess = c("center", "scale"),
   metric = "F1",  
   tuneGrid = grid
 )
model1

 #7. Calcular predicciones para test
 predictSample <- test %>% 
   mutate(pobre_pred = predict(model1, newdata = test, type = "raw")) %>% 
   select(id, pobre_pred)
 
 predictSample <- predictSample %>% 
   mutate(pobre=ifelse(pobre_pred=="Yes",1,0)) %>% 
   select(id,pobre)
 
 # Replace '.' with '_' in the numeric values converted to strings
 lambda_str <- gsub(
   "\.", "_", 
   as.character(round(model1$bestTune$lambda, 4)))
 alpha_str <- gsub("\.", "_", as.character(model1$bestTune$alpha))
 
 name<- paste0(
   "EN_lambda_", lambda_str,
   "_alpha_" , alpha_str, 
   ".csv") 
 
 write.csv(predictSample,name, row.names = FALSE)

 