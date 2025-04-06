#------------------------------------------------------------------------------#
# Modelos: Naive Bayes
#------------------------------------------------------------------------------#

train <- readRDS(file.path(stores_path, "upsampled_train_data.rds"))
test  <- readRDS(file.path(stores_path, "test_data.rds"))

#------------------------------------------------------------------------------#
# 1. Modelos ----
#------------------------------------------------------------------------------#

#-----------------------
# Variables explicativas
#-----------------------

X_1 <- c("Dominio", "Clase", "jefe_mujer", "hacinamiento_z", "jefe_pension",
         "N_mayor_dependiente_z", "jefe_nivel_educ",
         "jefe_edad_z", "jefe_edad2_z", 
         "hacinamiento_z:N_mujer_z", 
         "jefe_mujer:jefe_nivel_educ", 
         "N_menores_z:max_nivel_educ",
         "Dominio:Clase")
         

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

#-----------------------
# Modelos
#-----------------------

set.seed(1234)
model1  <- train(
  formula(paste0("Pobre ~", paste0(X_1, collapse = " + "))),
  data = train,  
  method = "naive_bayes",
  trControl = ctrl,  
  metric = "F",  
)
model1


#------------------------------------------------------------------------------#
# Resultados para Kaggle
#------------------------------------------------------------------------------#

#Calcular las predicciones para la base de datos test
predictSample <- test %>% 
  mutate(pobre_pred = predict(model1, newdata = test, type = "raw")) %>% 
  select(id, pobre_pred) 

predictSample <- predictSample %>% 
  mutate(pobre=ifelse(pobre_pred=="Pobre",1,0)) %>% 
  select(id,pobre)

table(predictSample$pobre) 

#Guardar CSV
write.csv(predictSample, file.path(stores_path, "NB_1.csv"), row.names = FALSE)



