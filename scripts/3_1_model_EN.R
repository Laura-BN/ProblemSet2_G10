
#------------------------------------------------------------------------------#
# Modelos: Elastic NET 
#------------------------------------------------------------------------------#

train <- readRDS(file.path(stores_path, "upsampled_train_data.rds"))
test  <- readRDS(file.path(stores_path, "test_data.rds"))

#------------------------------------------------------------------------------#
# 1. Modelos ----
#------------------------------------------------------------------------------#

#-----------------------
# Variables explicativas
#-----------------------

X_1 <- c("hacinamiento_z", "Clase", "jefe_mujer", "prop_ocu_pet_z",
       "jefe_edad_z", "jefe_edad2_z", "jefe_nivel_educ",
       "N_mayor_dependiente_z")

X_2 <- c("Clase", "prop_ocu_pet_z", "jefe_mujer", "hacinamiento_z", 
       "N_mayor_dependiente_z", "jefe_nivel_educ", "N_menores_z", 
       "jefe_edad_z", "jefe_edad2_z", 
       "hacinamiento_z:N_mayor_dependiente_z", 
       "jefe_mujer:jefe_nivel_educ", 
       "N_menores_z:jefe_mujer")

X_3 <- c("Clase", "jefe_mujer", "hacinamiento_z", 
         "N_mayor_dependiente_z", "jefe_nivel_educ", "N_menores_z", 
         "jefe_edad_z", "jefe_edad2_z", "jefe_pension",
         "poly(prop_ocu_pet_z, 2, raw = TRUE)",
         "hacinamiento_z:N_mayor_dependiente_z", 
         "jefe_mujer:jefe_nivel_educ", 
         "N_menores_z:jefe_mujer")

X_4 = c("poly(hacinamiento_z, 2, raw=TRUE)", 
        "Clase",
        "jefe_mujer", 
        "prop_ocu_pet", 
        "jefe_edad_z",
        "jefe_edad2_z", 
        "poly(jefe_nivel_educ, 2 ,raw=TRUE)",
        "N_mayor_dependiente_z", 
        "prop_ocu_pet_z",
        "jefe_pension", 
        "poly(N_menores_z, 2, raw = TRUE):jefe_mujer")

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
alpha <- seq(0, 1, by = 0.15) 
grid <- expand.grid("alpha" = alpha, "lambda" = lambda) 

#-----------------------
# Modelos
#-----------------------

set.seed(1234)
model1  <- train(
  formula(paste0("Pobre ~", paste0(X_1, collapse = " + "))),
  data = train,  
  method = "glmnet",
  family = "binomial",
  trControl = ctrl,  
  metric = "F",  
  tuneGrid = grid
)

set.seed(1234)
model2  <- train(
  formula(paste0("Pobre ~", paste0(X_2, collapse = " + "))),
  data = train,  
  method = "glmnet",
  family = "binomial",
  trControl = ctrl,  
  metric = "F",  
  tuneGrid = grid
)

set.seed(1234)
model3  <- train(
  formula(paste0("Pobre ~", paste0(X_3, collapse = " + "))),
  data = train,  
  method = "glmnet",
  family = "binomial",
  trControl = ctrl,  
  metric = "F",  
  tuneGrid = grid
)

set.seed(1234)
model4  <- train(
  formula(paste0("Pobre ~", paste0(X_4, collapse = " + "))),
  data = train,  
  method = "glmnet",
  family = "binomial",
  trControl = ctrl,  
  metric = "F",  
  tuneGrid = grid
)


model1
model2 
model3
model4

model4$results %>%
  dplyr::filter(alpha == model4$bestTune$alpha,
                lambda == model4$bestTune$lambda)

# table(train$jefe_pension)
# table(train$jefe_salud_sub)


#------------------------------------------------------------------------------#
# Resultados para Kaggle
#------------------------------------------------------------------------------#

#Calcular las predicciones para la base de datos test
predictSample <- test %>% 
  mutate(pobre_pred = predict(model4, newdata = test, type = "raw")) %>% 
  select(id, pobre_pred) 

predictSample <- predictSample %>% 
  mutate(pobre=ifelse(pobre_pred=="Pobre",1,0)) %>% 
  select(id,pobre)

table(predictSample$pobre) 

#Guardar CSV
lambda_str <- gsub("[.]", "_", as.character(round(model4$bestTune$lambda, 4)))
alpha_str <- gsub("[.]", "_", as.character(model4$bestTune$alpha))

name <- paste0("EN_lambda_", lambda_str, "_alpha_", alpha_str, ".csv") 
write.csv(predictSample, file.path(stores_path, name), row.names = FALSE)

