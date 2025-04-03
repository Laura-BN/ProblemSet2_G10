
# IMPORTAR DATOS ------------------------------------------------------------
train <- readRDS(file.path(stores_path, "upsampled_train_data.rds"))
test  <- readRDS(file.path(stores_path, "test_data.rds"))

# Variables explicativas
#Modelo 1
columnas_train <- c("hacinamiento_z", "Clase", "jefe_mujer", "prop_ocu_pet_z",
                      "jefe_edad_z", "jefe_edad2_z", "jefe_nivel_educ",
                      "N_mayor_dependiente_z", "Pobre")

columnas_test <- c("id","hacinamiento_z", "Clase", "jefe_mujer", "prop_ocu_pet_z",
                   "jefe_edad_z", "jefe_edad2_z", "jefe_nivel_educ",
                    "N_mayor_dependiente_z")

# Filtrar el dataset
train_subset <- train[, columnas_train]
test_subset <- test[, columnas_test]

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

 set.seed(1234)
 model1  <- train(Pobre~.,
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

 
#Modelo 2
 columnas_train <- c("hacinamiento_z", "Clase", "jefe_mujer", "prop_ocu_pet_z",
                     "jefe_edad_z", "jefe_edad2_z", "jefe_nivel_educ",
                     "N_mayor_dependiente_z", "N_menores_z", "Pobre")
 
 columnas_test <- c("id","hacinamiento_z", "Clase", "jefe_mujer", "prop_ocu_pet_z",
                    "jefe_edad_z", "jefe_edad2_z", "jefe_nivel_educ",
                    "N_mayor_dependiente_z", "N_menores_z")

#Forma de variables explicativas
X <- c("Clase", "prop_ocu_pet_z", "jefe_mujer", "hacinamiento_z", 
       "N_mayor_dependiente_z", "jefe_nivel_educ", "N_menores_z", 
       "jefe_edad_z", "jefe_edad2_z", 
       "hacinamiento_z:N_mayor_dependiente_z", 
       "jefe_mujer:jefe_nivel_educ", 
       "N_menores_z:jefe_mujer")

# Filtrar el dataset
train_subset <- train[, columnas_train]
test_subset <- test[, columnas_test]

#Entrenar el modelo usando `glmnet`
 set.seed(1234)
 model2  <- train(
   formula(paste0("Pobre ~", paste0(X, collapse = " + "))),
   data = train_subset,  
   method = "glmnet",
   family = "binomial",
   trControl = ctrl,  
   metric = "F",  
   tuneGrid = grid
 )
 model2
 
 #Calcular las predicciones para la base de datos test
 predictSample <- test_subset %>% 
   mutate(pobre_pred = predict(model2, newdata = test_subset, type = "raw")) %>% 
   select(id, pobre_pred) # alpha= 0.6 and lambda = 0.01097
 
 predictSample <- predictSample %>% 
   mutate(pobre=ifelse(pobre_pred=="Si",1,0)) %>% 
   select(id,pobre)
 
 table(predictSample$pobre) #24404 
 
 #save
 lambda_str <- gsub("[.]", "_", as.character(round(model2$bestTune$lambda, 4)))
 alpha_str <- gsub("[.]", "_", as.character(model2$bestTune$alpha))
 
 name <- paste0("EN_lambda_", lambda_str, "_alpha_", alpha_str, ".csv") 
 
 write.csv(predictSample, file.path(stores_path, name), row.names = FALSE)
 
 
 #Modelo 3
 columnas_train <- c("hacinamiento_z", "Clase", "jefe_mujer", "prop_ocu_pet_z",
                     "jefe_edad_z", "jefe_edad2_z", "jefe_nivel_educ",
                     "N_mayor_dependiente_z", "N_menores_z", "Pobre",
                     
                     "prop_ocu_pet", "jefe_pension", "jefe_salud_sub", "N_mujer_z")
 
 columnas_test <- c("hacinamiento_z", "Clase", "jefe_mujer", "prop_ocu_pet_z",
                    "jefe_edad_z", "jefe_edad2_z", "jefe_nivel_educ",
                    "N_mayor_dependiente_z", "N_menores_z", 
                    
                    "prop_ocu_pet", "jefe_pension", "jefe_salud_sub", "N_mujer_z")
 
 
 #Forma de variables explicativas
 X = c("poly(hacinamiento_z, 2, raw=TRUE)", 
         "Clase",
         "prop_ocu_pet", 
         "jefe_edad_z",
         "jefe_edad2_z", 
         "poly(jefe_nivel_educ, 2, raw=TRUE):jefe_mujer",
         "N_mayor_dependiente_z:jefe_pension", 
         "jefe_salud_sub",
         "prop_ocu_pet_z:N_mujer_z",
         "poly(N_menores_z, 2, raw = TRUE):jefe_mujer")
 
 # Filtrar el dataset
 train_subset <- train[, columnas_train]
 test_subset <- test[, columnas_test]
 
 # toca poner reemplazos en NAs de la variable de salud 
 # train_subset$jefe_salud_sub[which(is.na(train_subset$jefe_salud_sub))] <- "Jefe_salud_subsidiado"
 # test_subset$jefe_salud_sub[which(is.na(test_subset$jefe_salud_sub))] <- "Jefe_salud_subsidiado"
 
 #Definición de la grilla de hiperparámetros 
 lambda <- 10^seq(2, -6, length = 50)  
 alpha <- seq(0, 1, by = 0.2) 
 grid <- expand.grid("alpha" = alpha, "lambda" = lambda) 
 
 
 #Entrenar el modelo usando `glmnet`
 set.seed(1234)
 model3  <- train(
   formula(paste0("Pobre ~", paste0(X, collapse = " + "))),
   data = train_subset,  
   method = "glmnet",
   family = "binomial",
   trControl = ctrl,  
   metric = "F",  
   tuneGrid = grid
 )
 model3
 
 #Calcular las predicciones para la base de datos test
 predictSample <- test_subset %>% 
   mutate(pobre_pred = predict(model3, newdata = test_subset, type = "raw")) %>% 
   select(id, pobre_pred) # alpha= 0.6 and lambda = 0.01097
 
 predictSample <- predictSample %>% 
   mutate(pobre=ifelse(pobre_pred=="Si",1,0)) %>% 
   select(id,pobre)
 
 table(predictSample$pobre) #24404 
 
 #save
 lambda_str <- gsub("[.]", "_", as.character(round(model2$bestTune$lambda, 4)))
 alpha_str <- gsub("[.]", "_", as.character(model3$bestTune$alpha))
 
 name <- paste0("EN_lambda_", lambda_str, "_alpha_", alpha_str, ".csv") 
 
 write.csv(predictSample, file.path(stores_path, name), row.names = FALSE)
 