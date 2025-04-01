
# IMPORTAR DATOS ------------------------------------------------------------
train <- readRDS(file.path(stores_path, "upsampled_train_data.rds"))
test  <- readRDS(file.path(stores_path, "test_data.rds"))

# Variables explicativas
columnas_train <- c("hacinamiento", "Clase", "jefe_mujer", "prop_ocu_pet",
              "jefe_edad", "jefe_edad2", "jefe_nivel_educ",
              "N_mayor_dependiente", "Pobre")

columnas_test <- c("id", "hacinamiento", "Clase", "jefe_mujer", "prop_ocu_pet", 
              "jefe_edad", "jefe_edad2", "jefe_nivel_educ", 
              "N_mayor_dependiente")

# Filtrar el dataset
train_subset <- train[, columnas_train]
test_subset <- test[, columnas_test]

#Normalizar variables numericas
train_subset <- train_subset %>%
  dplyr::mutate_if(is.numeric, scale)

test_subset <- test_subset %>%
  dplyr::mutate_if(is.numeric, scale)

#Definir función de evaluación personalizada 
multiStats <- function(...) c(twoClassSummary(...), defaultSummary(...), prSummary(...))
 
#Hiperparámetros usando cross validación 
 ctrl <- trainControl(
   method = "cv",  
   number = 5,  
   summaryFunction = multiStats, 
   classProbs = T,  
   verbose = F,  
   savePredictions = T  
 )
 
#Definición de la grilla de hiperparámetros 
 lambda <- 10^seq(2, -6, length = 100)  
 alpha <- seq(0, 1, by = 0.2) 
 grid <- expand.grid("alpha" = alpha, "lambda" = lambda) 

#Entrenar el modelo usando `glmnet`
 set.seed(1234)
 model1 <- train(Pobre~.,
   data = train_subset,  
   method = "glmnet",
   family = "binomial",
   trControl = ctrl,  
   metric = "F",  
   tuneGrid = grid
 )
 model1
 
#Calcular las predicciones para la base de datos test
 predictSample <- test_subset %>% 
   mutate(pobre_pred = predict(model1, newdata = test_subset, type = "raw")) %>% 
   select(id, pobre_pred) # alpha= 0.2 and lambda = 0.0191791.
 
 predictSample <- predictSample %>% 
   mutate(pobre=ifelse(pobre_pred=="Si",1,0)) %>% 
   select(id,pobre)
 
 table(predictSample$pobre) #31194
 
 #save
 lambda_str <- gsub("[.]", "_", as.character(round(model1$bestTune$lambda, 4)))
 alpha_str <- gsub("[.]", "_", as.character(model1$bestTune$alpha))
 
 name <- paste0("EN_lambda_", lambda_str, "_alpha_", alpha_str, ".csv") 
 
 write.csv(predictSample, file.path(stores_path, name), row.names = FALSE)

 