#------------------------------------------------------------------------------#
# Modelos: Regresión lineal y logit ----
#------------------------------------------------------------------------------#

train = readRDS(file.path(stores_path, "upsampled_train_data.rds"))
test  = readRDS(file.path(stores_path, "test_data.rds"))

colnames(train)

"hacinamiento_z"
#------------------------------------------------------------------------------#
# 1. Modelos ----
#------------------------------------------------------------------------------#

#-----------------------
# Variables explicativas
#-----------------------

X_1 = c("poly(hacinamiento_z, 2, raw=TRUE)", 
        "Clase",
        "jefe_mujer", 
        "prop_ocu_pet", 
        "jefe_edad_z",
        "jefe_edad2_z", 
        "poly(jefe_nivel_educ, 2 ,raw=TRUE)",
        "N_mayor_dependiente_z", 
        "prop_ina_pet_z",
        "jefe_pension", 
        "poly(N_menores_z, 2 ,raw=TRUE)")

X_2 = c("poly(hacinamiento_z, 2, raw=TRUE)", 
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

X_3 = c("poly(hacinamiento_z, 2, raw=TRUE)", 
          "Clase",
          "prop_ocu_pet", 
          "jefe_edad_z",
          "jefe_edad2_z", 
          "poly(jefe_nivel_educ, 2, raw=TRUE):jefe_mujer",
          "N_mayor_dependiente_z", 
          "prop_ocu_pet_z:N_mujer_z",
          "jefe_pension", 
          "poly(N_menores_z, 2, raw = TRUE):jefe_mujer")

X_4 = c("poly(hacinamiento_z, 2, raw=TRUE)", 
        "Clase",
        "prop_ocu_pet", 
        "jefe_edad_z",
        "jefe_edad2_z", 
        "poly(jefe_nivel_educ, 2, raw=TRUE):jefe_mujer",
        "N_mayor_dependiente_z:jefe_pension", 
        "jefe_salud_sub",
        "prop_ocu_pet_z:N_mujer_z",
        "poly(N_menores_z, 2, raw = TRUE):jefe_mujer")

X_5 = c("poly(hacinamiento_z, 2, raw=TRUE)", 
        "Clase",
        "prop_ocu_pet",                            # maaaaal debe ser z, no? 
        "jefe_edad_z",
        "jefe_edad2_z", 
        "poly(jefe_nivel_educ, 2, raw=TRUE):jefe_mujer",
        "N_mayor_dependiente_z:jefe_pension", 
        "jefe_salud_sub",
        "prop_ina_pet_z:N_mujer_z",
        "poly(N_menores_z, 2, raw = TRUE):jefe_mujer")


X_6 = c("poly(hacinamiento_z, 2, raw=TRUE):jefe_salud_sub", 
        "Clase",
        "prop_ocu_pet", 
        "jefe_edad_z",
        "jefe_edad2_z", 
        "poly(jefe_edad_z, 2, raw=TRUE):jefe_mujer",
        "poly(jefe_nivel_educ, 1, raw=TRUE):jefe_mujer",
        "poly(jefe_edad_z, 2, raw=TRUE):jefe_nivel_educ",
        
        "N_mayor_dependiente_z:jefe_pension", 
        "jefe_salud_sub",
        "prop_ocu_pet_z:N_mujer_z",
        "poly(N_menores_z, 2, raw = TRUE):jefe_mujer")




table(test$jefe_salud_sub)

ctrl = trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    savePredictions = TRUE,
                    verbose = T)

#-----------------------
# Modelo
#-----------------------

# train = train %>% mutate(jefe_salud_sub = ifelse(is.na(jefe_salud_sub) & jefe_pension == "Si", "Si", jefe_salud_sub ))
# train_subset$jefe_salud_sub[which(is.na(train_subset$jefe_salud_sub))] <- "Jefe_salud_subsidiado"
# test_subset$jefe_salud_sub[which(is.na(test_subset$jefe_salud_sub))] <- "Jefe_salud_subsidiado"




# table(train$jefe_pension)
# table(train$jefe_salud_sub)

set.seed(9873)
logit_1 =  train(
                formula(paste0("Pobre ~", paste0(X_1, collapse = " + "))),
                       data = train, 
                       method = "glm",
                       trControl = ctrl,
                       family = "binomial")


 logit_acc_1 = logit_1$results$Accuracy; logit_acc_1

set.seed(9873)
logit_2 =  train(
  formula(paste0("Pobre ~", paste0(X_2, collapse = " + "))),
  data = train, 
  method = "glm",
  trControl = ctrl,
  family = "binomial")


 logit_acc_2 = logit_2$results$Accuracy; logit_acc_2

set.seed(9873)
logit_3 =  train(
  formula(paste0("Pobre ~", paste0(X_3, collapse = " + "))),
  data = train, 
  method = "glm",
  trControl = ctrl,
  family = "binomial")


 logit_acc_3 = logit_3$results$Accuracy; logit_acc_3

set.seed(9873)
logit_4 =  train(
  formula(paste0("Pobre ~", paste0(X_4, collapse = " + "))),
  data = train, 
  method = "glm",
  trControl = ctrl,
  family = "binomial")

 logit_acc_4 = logit_4$results$Accuracy; logit_acc_4

set.seed(9873)
logit_5 =  train(
  formula(paste0("Pobre ~", paste0(X_5, collapse = " + "))),
  data = train, 
  method = "glm",
  trControl = ctrl,
  family = "binomial")

 logit_acc_5 = logit_5$results$Accuracy; logit_acc_5

set.seed(9873)
logit_6 =  train(
   formula(paste0("Pobre ~", paste0(X_6, collapse = " + "))),
   data = train, 
   method = "glm",
   trControl = ctrl,
   family = "binomial")
 
logit_acc_6 = logit_6$results$Accuracy; logit_acc_6
 
 
logit_acc_1
logit_acc_2
logit_acc_3
logit_acc_4
logit_acc_5
logit_acc_6

#------------------------------------------------------------------------------#
# Resultados para Kaggle
#------------------------------------------------------------------------------#

predictSample = test   %>% 
                mutate(pobre_lab = predict(logit_6, newdata = test, type = "raw")    ## predicted class labels
                )  %>% select(id, pobre_lab)

head(predictSample)

predictSample = predictSample %>% 
                mutate(pobre = ifelse(pobre_lab == "Si", 1, 0)) %>% 
                select(id, pobre)

head(predictSample)

template = read.csv(file.path(raw_path, "sample_submission.csv"))             
head(template)
                
table(predictSample$pobre)


# Replace '.' with '_' in the numeric values converted to strings
# lambda_str <- gsub( "\\.", "_", as.character(round(logit_4$bestTune$lambda, 4)))
# alpha_str <- gsub("\\.", "_", as.character(logit_4$bestTune$alpha))

name = paste0(
  "Logit_3",
  ".csv") 

write.csv(predictSample, file.path(stores_path, name), row.names = FALSE)


