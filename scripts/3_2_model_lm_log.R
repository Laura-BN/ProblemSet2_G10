#------------------------------------------------------------------------------#
# Modelos: Regresión lineal y logit ----
#------------------------------------------------------------------------------#

# train = readRDS(file.path(stores_path, "upsampled_train_data.rds"))
train = readRDS(file.path(stores_path, "train_data.rds"))
test  = readRDS(file.path(stores_path, "test_data.rds"))

colnames(train)
Pobre_num = train$Pobre_d # para calcular los indicadores de rendimiento

train = train %>% dplyr::mutate(Pobre_d = ifelse(Pobre == "Pobre", 1, 0))

table(train$Pobre_d)
"hacinamiento_z"
#------------------------------------------------------------------------------#
# 1. Modelos ----
#------------------------------------------------------------------------------#

#-----------------------
# Variables explicativas
#-----------------------

X_1 = c("poly(hacinamiento, 2, raw=TRUE)", 
        "Clase",
        "jefe_mujer", 
        "prop_ocu_pet", 
        "jefe_edad",
        "jefe_edad2", 
        "poly(jefe_nivel_educ, 2 ,raw=TRUE)",
        "N_mayor_dependiente", 
        "prop_ina_pet",
        "jefe_pension", 
        "poly(N_menores, 2 ,raw=TRUE)", 
        "poly(viv_noPropia, 2 ,raw=TRUE):hacinamiento")

X_2 = c("poly(hacinamiento, 2, raw=TRUE)", 
        "Clase",
        "jefe_mujer", 
        "prop_ocu_pet", 
        "jefe_edad",
        "jefe_edad2", 
        "poly(jefe_nivel_educ, 2 ,raw=TRUE)",
        "N_mayor_dependiente", 
        "prop_ocu_pet",
        "jefe_pension", 
        "poly(N_menores, 2, raw = TRUE):jefe_mujer", 
        "poly(viv_noPropia, 2 ,raw=TRUE):hacinamiento")

X_3 = c("poly(hacinamiento, 2, raw=TRUE)", 
          "Clase",
          "prop_ocu_pet", 
          "jefe_edad",
          "jefe_edad2", 
          "poly(jefe_nivel_educ, 2, raw=TRUE):jefe_mujer",
          "N_mayor_dependiente", 
          "prop_ocu_pet:N_mujer",
          "jefe_pension", 
          "poly(N_menores, 2, raw = TRUE):jefe_mujer", 
         "poly(viv_noPropia, 2 ,raw=TRUE):hacinamiento")

X_4 = c("poly(hacinamiento, 2, raw=TRUE)", 
        "Clase",
        "prop_ocu_pet", 
        "jefe_edad",
        "jefe_edad2", 
        "poly(jefe_nivel_educ, 2, raw=TRUE):jefe_mujer",
        "N_mayor_dependiente:jefe_pension", 
        "jefe_salud_sub",
        "prop_ocu_pet:N_mujer",
        "poly(N_menores, 2, raw = TRUE):jefe_mujer", 
        "poly(viv_noPropia, 2 ,raw=TRUE):hacinamiento")

X_5 = c("poly(hacinamiento, 2, raw=TRUE)", 
        "Clase",
        "prop_ocu_pet",                            # maaaaal debe ser z, no? 
        "jefe_edad",
        "jefe_edad2", 
        "poly(jefe_nivel_educ, 2, raw=TRUE):jefe_mujer",
        "N_mayor_dependiente:jefe_pension", 
        "jefe_salud_sub",
        "prop_ina_pet:N_mujer",
        "poly(N_menores, 2, raw = TRUE):jefe_mujer", 
        "poly(viv_noPropia, 2 ,raw=TRUE):hacinamiento")


X_6 = c("poly(hacinamiento, 2, raw=TRUE):jefe_salud_sub", 
        "Clase",
        "prop_ocu_pet", 
        "jefe_edad",
        "jefe_edad2", 
        "poly(jefe_edad, 2, raw=TRUE):jefe_mujer",
        "poly(jefe_nivel_educ, 1, raw=TRUE):jefe_mujer",
        "poly(jefe_edad, 2, raw=TRUE):jefe_nivel_educ",
        
        "N_mayor_dependiente:jefe_pension", 
        "jefe_salud_sub",
        "prop_ocu_pet:N_mujer",
        "poly(N_menores, 2, raw = TRUE):jefe_mujer", 
        "poly(viv_noPropia, 2 ,raw=TRUE):hacinamiento")


ctrl = trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    savePredictions = TRUE,
                    verbose = T)

#-----------------------
# Modelo
#-----------------------

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

Pobre_num = train$Pobre # para calcular los indicadores de rendimiento

F1_Score(y_pred = predict(logit_1, newdata = train, type = "raw"), y_true = Pobre_num, positive = "Pobre")
F1_Score(y_pred = predict(logit_2, newdata = train, type = "raw"), y_true = Pobre_num, positive = "Pobre")
F1_Score(y_pred = predict(logit_3, newdata = train, type = "raw"), y_true = Pobre_num, positive = "Pobre")
F1_Score(y_pred = predict(logit_4, newdata = train, type = "raw"), y_true = Pobre_num, positive = "Pobre")
F1_Score(y_pred = predict(logit_5, newdata = train, type = "raw"), y_true = Pobre_num, positive = "Pobre")
F1_Score(y_pred = predict(logit_6, newdata = train, type = "raw"), y_true = Pobre_num, positive = "Pobre")


#------------------------------------------------------------------------------#
# Resultados para Kaggle
#------------------------------------------------------------------------------#

predictSample = test   %>% 
                mutate(pobre_lab = predict(logit_6, newdata = test, type = "raw")    ## predicted class labels
                )  %>% select(id, pobre_lab)

head(predictSample)
table(predictSample$pobre_lab)


predictSample = predictSample %>% 
                mutate(pobre = ifelse(pobre_lab == "Pobre", 1, 0)) %>% 
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
  "Logit_4",
  ".csv") 

write.csv(predictSample, file.path(stores_path, name), row.names = FALSE)
