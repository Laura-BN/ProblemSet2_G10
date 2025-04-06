#------------------------------------------------------------------------------#
# Modelos: Regresión lineal y logit ----
#------------------------------------------------------------------------------#

train = readRDS(file.path(stores_path, "upsampled_train_data.rds"))
test  = readRDS(file.path(stores_path, "test_data.rds"))

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


train = train %>% mutate(
        Pobre_d        = factor(Pobre_d, levels=c(0,1),labels=c("No","Si")),
        hacinamiento_f = factor(ifelse(hacinamiento > 2, 1, 0), labels = c("Si", "No")), # porque es el a lo que se aproxima el 3 cuantil
        jefe_mayor_f   = factor(ifelse(jefe_edad > 49, 1, 0), labels = c("Si", "No")), # puede ser indicador de que la persona que sostiene el hogar tenga más o menos dinámica laboral
        N_menores_f    = factor(ifelse(N_menores > 1.6, 1, 0), labels = c("Si", "No")), # número promedio hijas/os por mujer en Colombia (podrían ser más grandes pero por practicidad)
        jefe_mujer_f   = factor(ifelse(jefe_mujer == "Jefe_mujer", 1, 0), labels = c("Si", "No")), 
        prop_ocu_pet_f = factor(ifelse(prop_ocu_pet > 0.6, 1, 0), labels = c("Si", "No")), # hogares usualmente 3,3 personas, que trabaje al menos el 60 % (cuantil 3) 
        Mayor_dependiente_f = factor(ifelse(N_mayor_dependiente >= 1, 1, 0), labels = c("Si", "No")),
        jefe_cot_pens  = factor(ifelse(jefe_pension == "Jefe_af_pension", 1, 0), labels = c("Si", "No"))
        )

test = test %>% mutate(
        hacinamiento_f = factor(ifelse(hacinamiento > 2, 1, 0), labels = c("Si", "No")), # porque es el a lo que se aproxima el 3 cuantil
        jefe_mayor_f   = factor(ifelse(jefe_edad > 49, 1, 0), labels = c("Si", "No")), # puede ser indicador de que la persona que sostiene el hogar tenga más o menos dinámica laboral
        N_menores_f    = factor(ifelse(N_menores > 1.6, 1, 0), labels = c("Si", "No")), # número promedio hijas/os por mujer en Colombia (podrían ser más grandes pero por practicidad)
        jefe_mujer_f   = factor(ifelse(jefe_mujer == "Jefe_mujer", 1, 0), labels = c("Si", "No")), 
        prop_ocu_pet_f = factor(ifelse(prop_ocu_pet > 0.6, 1, 0), labels = c("Si", "No")), # hogares usualmente 3,3 personas, que trabaje al menos el 60 % (cuantil 3) 
        Mayor_dependiente_f = factor(ifelse(N_mayor_dependiente >= 1, 1, 0), labels = c("Si", "No")),
        jefe_cot_pens  = factor(ifelse(jefe_pension == "Jefe_af_pension", 1, 0), labels = c("Si", "No"))
) 

sapply(train, class)

table(train$jefe_salud_sub)

X_2 = c("hacinamiento_f", 
        "Clase",
        "jefe_mujer_f", 
        "N_ocupados", 
        "jefe_mayor_f",
        "jefe_nivel_educ",
        "Mayor_dependiente_f", 
        "prop_ocu_pet_f",
        "jefe_cot_pens", 
        "N_menores_f")

#------------------------------------------------------------------------------#
# 1. Modelos ----
#------------------------------------------------------------------------------#

#-----------------------
# 1.1. Bagging 
#-----------------------

bagged_tree = ranger::ranger(
              formula(paste0("Pobre ~", paste0(X_2, collapse = " + "))),
              data = train,
              num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
              mtry= 8,   # N. var aleatoriamente seleccionadas en cada partición. Baggin usa todas las vars.
              min.node.size  = 1, ## Numero minimo de observaciones en un nodo para intentar 
            ) 
bagged_tree

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
phat.bag = rowSums(pred.bag_ranger == 2) / ntrees


length(Pobre_num)
length(phat.bag)

# Calcular y guardar AUC de bagging
aucval_bag = Metrics::auc(
             actual = Pobre_num,
             predicted = phat.bag)
aucval_bag


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
                    verbose=FALSE,
                    savePredictions = T)

adagrid = expand.grid(
          mfinal = c( 50, 300 ,500),
          maxdepth = c(1,2,5),
          coeflearn = c('Breiman','Freund'))


set.seed(91519) # important set seed. 

adaboost_tree <- train(
                       formula(paste0("Pobre ~", paste0(X_2, collapse = " + "))),
                       data = train, 
                       method = "AdaBoost.M1",  # para implementar el algoritmo antes descrito
                       trControl = ctrl,
                       metric = "ROC",
                       tuneGrid = adagrid
)

adaboost_tree



