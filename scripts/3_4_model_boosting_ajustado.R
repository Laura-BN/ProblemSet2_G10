#------------------------------------------------------------------------------#
# Modelos: Regresión lineal y logit ----
#------------------------------------------------------------------------------#

train = readRDS(file.path(stores_path, "upsampled_train_data.rds"))
# train = readRDS(file.path(stores_path, "train_data.rds"))
test  = readRDS(file.path(stores_path, "test_data.rds"))

intersect(colnames(train), colnames(test))

#---------------------------
# Ajuste de variables y cols
#---------------------------

train = train[, !duplicated(colnames(train))]
train = train %>% dplyr::mutate(Pobre_d = ifelse(Pobre == "Pobre", 1, 0))

Pobre_num = train$Pobre_d # para calcular los indicadores de rendimiento

#---------------------------
# Ajuste de variables factor
#---------------------------
X_1 = c("hacinamiento", 
        "Clase",
        "jefe_mujer", 
        "prop_ocu_pet", 
        "jefe_edad",
        "jefe_nivel_educ",
        "N_mayor_dependiente", 
        "prop_ina_pet",
        "jefe_pension", 
        "N_menores")

summary(train$hacinamiento)
summary(train$jefe_edad)
summary(train$N_menores)
table(train$jefe_mujer)
summary(train$prop_ocu_pet)
summary(train$N_mayor_dependiente)
table(train$jefe_nivel_educ)
table(train$jefe_pension)
table(train$jefe_salud_sub)
table(train$Pobre)

train = train %>% mutate(
        Pobre_d        = factor(Pobre_d, levels=c(0,1),labels=c("No","Si")),
        hacinamiento_f = factor(ifelse(hacinamiento > 3, 1, 0), labels = c("Si", "No")), # porque es el a lo que se aproxima el 3 cuantil
        jefe_mayor_f   = factor(ifelse(jefe_edad > 49, 1, 0), labels = c("Si", "No")), # puede ser indicador de que la persona que sostiene el hogar tenga más o menos dinámica laboral
        N_menores_f    = factor(ifelse(N_menores > 1.6, 1, 0), labels = c("Si", "No")), # número promedio hijas/os por mujer en Colombia (podrían ser más grandes pero por practicidad)
        jefe_mujer_f   = factor(ifelse(jefe_mujer == "Jefe_mujer", 1, 0), labels = c("Si", "No")), 
        prop_ocu_pet_f = factor(ifelse(prop_ocu_pet > 0.6, 1, 0), labels = c("Si", "No")), # hogares usualmente 3,3 personas, que trabaje al menos el 60 % (cuantil 3) 
        Mayor_dependiente_f = factor(ifelse(N_mayor_dependiente >= 1, 1, 0), labels = c("Si", "No")),
        jefe_cot_pens  = factor(ifelse(jefe_pension == "Jefe_af_pension", 1, 0), labels = c("Si", "No")),
        jefe_cont_salud  = factor(ifelse(jefe_salud_sub == "Jefe_salud_contributivo", 1, 0), labels = c("Si", "No")), 
        N_desocupados_f = factor(ifelse(N_desocupados >= 1, 1, 0), labels = c("Si", "No"))
        
        )

test = test %>% mutate(
        hacinamiento_f = factor(ifelse(hacinamiento > 3, 1, 0), labels = c("Si", "No")), # porque es el a lo que se aproxima el 3 cuantil
        jefe_mayor_f   = factor(ifelse(jefe_edad > 49, 1, 0), labels = c("Si", "No")), # puede ser indicador de que la persona que sostiene el hogar tenga más o menos dinámica laboral
        N_menores_f    = factor(ifelse(N_menores > 1.6, 1, 0), labels = c("Si", "No")), # número promedio hijas/os por mujer en Colombia (podrían ser más grandes pero por practicidad)
        jefe_mujer_f   = factor(ifelse(jefe_mujer == "Jefe_mujer", 1, 0), labels = c("Si", "No")), 
        prop_ocu_pet_f = factor(ifelse(prop_ocu_pet > 0.6, 1, 0), labels = c("Si", "No")), # hogares usualmente 3,3 personas, que trabaje al menos el 60 % (cuantil 3) 
        Mayor_dependiente_f = factor(ifelse(N_mayor_dependiente >= 1, 1, 0), labels = c("Si", "No")),
        jefe_cot_pens  = factor(ifelse(jefe_pension == "Jefe_af_pension", 1, 0), labels = c("Si", "No")),
        jefe_cont_salud  = factor(ifelse(jefe_salud_sub == "Jefe_salud_contributivo", 1, 0), labels = c("Si", "No")), 
        N_desocupados_f = factor(ifelse(N_desocupados >= 1, 1, 0), labels = c("Si", "No"))
) 

sapply(train, class)

table(train$jefe_salud_sub)
table(test$jefe_salud_sub)


X_2 = c("jefe_edad", 
        "jefe_mujer",
        "jefe_edad2",
        "jefe_salud_sub", 
        "N_personas",
        "hacinamiento",
        "max_nivel_educ", 
        "Clase",
        "viv_noPropia", 
        "Lp", 
        "prop_fuentes_ing", 
        "prop_ina_pet", 
        "Dominio",
        "prop_ocu_pet",
        "jefe_pension",
        "prop_menores_pob",
        "prop_mayores_pob",
        "promedio_anios_educ",
        "tipo_trabajo")

table(train$N_desocupados)

#------------------------------------------------------------------------------#
# 1. Modelos ----
#------------------------------------------------------------------------------#

#-----------------------
# 1.1. Bagging 
#-----------------------

bagged_tree = ranger::ranger(
              formula(paste0("Pobre_d ~", paste0(X_2, collapse = " + "))),
              data = train,
              num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
              # mtry = 9,
              mtry = sqrt(length(X_2)),
              min.node.size  = 1, ## Numero minimo de observaciones en un nodo para intentar 
            ) 
bagged_tree

table(train$Pobre)
table(train$Pobre_d)

# Clacular predicciones

bagged_pred = predict(
              bagged_tree,
              data = train, # toca en train porque en test no existe "Pobre"
              predict.all = TRUE # para obtener la predicción de cada arbol. 
            )

# Guardamos la predicción de cada árbol dataframe
pred.bag_ranger = as.data.frame( bagged_pred$predictions )

# Visualizemoslo
head(tibble(pred.bag_ranger))

# Calcular las probabilidades de Default (promedio todos los árboles)
ntrees = ncol( pred.bag_ranger )
phat.bag = rowSums(pred.bag_ranger == "Si") / ntrees


length(Pobre_num)
length(phat.bag)

# Calcular y guardar AUC de bagging
aucval_bag = Metrics::auc(
             actual = Pobre_num,
             predicted = phat.bag)
aucval_bag

# Calcular el F1
yhat.bag = ifelse(phat.bag >= 0.5, 1, 0) 
F1_Score(y_pred = yhat.bag, y_true = Pobre_num, positive = "1")


#------------------------------------------------------------------------------#
# Resultados para Kaggle
#------------------------------------------------------------------------------#

preds_test <- predict(bagged_tree, data = test)$predictions
predictSample <- test %>%
  mutate(pobre_lab = preds_test) %>%
  select(id, pobre_lab)


head(predictSample)

predictSample = predictSample %>% 
  mutate(pobre = ifelse(pobre_lab == "Si", 1, 0)) %>% 
  select(id, pobre)

predictSample = predictSample %>% arrange(id)

head(predictSample)
table(predictSample$pobre)

zip_path = file.path(raw_path, "uniandes-bdml-202510-ps-2.zip")
sample_submission = read_csv(unz(zip_path, "sample_submission.csv"))
head(sample_submission)

table(sample_submission$pobre)


# Replace '.' with '_' in the numeric values converted to strings
# lambda_str <- gsub( "\\.", "_", as.character(round(logit_4$bestTune$lambda, 4)))
# alpha_str <- gsub("\\.", "_", as.character(logit_4$bestTune$alpha))

name = paste0(
  "Bagging_2",
  ".csv") 

write.csv(predictSample, file.path(stores_path, name), row.names = FALSE)


#-----------------------
# 1.2. Boosting 
#-----------------------

fiveStats <- function(...) {
  c(
    caret::twoClassSummary(...), # Returns ROC, Sensitivity, and Specificity
    caret::defaultSummary(...)  # Returns RMSE and R-squared (for regression) or Accuracy and Kappa (for classification)
  )
}


ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE, 
                    verboseIter = TRUE,   # muestra el progreso
                    savePredictions = T)

adagrid = expand.grid(
          mfinal = c( 50, 300 ,500),
          maxdepth = c(1,2,5),
          coeflearn = c('Breiman','Freund'))


set.seed(91519) # important set seed. 

adaboost_tree <- train(
                       formula(paste0("Pobre_d ~", paste0(X_2, collapse = " + "))),
                       data = train, 
                       method = "AdaBoost.M1",  # para implementar el algoritmo antes descrito
                       trControl = ctrl,
                       metric = "ROC",
                       tuneGrid = adagrid
)

adaboost_tree

table(train$Pobre_num)

pred_prob <- predict(adaboost_tree,
                     newdata = train, 
                     type = "prob")   

F1_Score(y_pred = pred_prob, y_true = Pobre_num, positive = "1")


#------------------------------------------------------------------------------#
# Resultados para Kaggle
#------------------------------------------------------------------------------#

predictSample = test   %>% 
  mutate(pobre_lab = predict(adaboost_tree, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id, pobre_lab)

head(predictSample)
table(predictSample$pobre_lab)


predictSample = predictSample %>% 
  mutate(pobre = ifelse(pobre_lab == "1", 1, 0)) %>% 
  select(id, pobre)

head(predictSample)
table(predictSample$pobre)


zip_path = file.path(raw_path, "uniandes-bdml-202510-ps-2.zip")
sample_submission = read_csv(unz(zip_path, "sample_submission.csv"))
head(sample_submission)

table(sample_submission$pobre)

# Replace '.' with '_' in the numeric values converted to strings
# lambda_str <- gsub( "\\.", "_", as.character(round(logit_4$bestTune$lambda, 4)))
# alpha_str <- gsub("\\.", "_", as.character(logit_4$bestTune$alpha))

name = paste0(
  "Boosting_2",
  ".csv") 

write.csv(predictSample, file.path(stores_path, name), row.names = FALSE)

