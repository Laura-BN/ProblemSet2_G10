
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

#Gráfica validacion cruzada de cada modelo

# Extraer mejor combinación
best <- model1$bestTune
best_f <- model1$results[model1$results$alpha == best$alpha & model1$results$lambda == best$lambda, ]

# Filtrar solo lambda <= 0.005
filtered_results <- subset(model1$results, lambda <= 0.005)

model1_plot <- ggplot(filtered_results, aes(x = lambda, y = F, color = factor(alpha))) +
  geom_line() +
  geom_point(data = best_f[best_f$lambda <= 0.005, ], 
             aes(x = lambda, y = F), 
             size = 3, shape = 21, fill = "black", show.legend = FALSE) +
  geom_text(data = best_f[best_f$lambda <= 0.005, ], 
            aes(x = lambda, y = F, label = paste0("α=", alpha, ", λ=", signif(lambda, 2))), 
            vjust = -1, hjust = 0.5, size = 3.5, color = "black", show.legend = FALSE) +
  labs(title = "", 
       x = "Lambda", 
       y = "F1 (Cross-validation)",
       color = "Alpha") +
  theme_minimal()
ggsave(file.path(view_path, "model1_plot.png"), plot = model1, width = 8, height = 6, dpi = 300)

# Model 2
best <- model2$bestTune
best_f <- model2$results[model2$results$alpha == best$alpha & model2$results$lambda == best$lambda, ]

# Filtrar solo lambda <= 0.05
filtered_results <- subset(model2$results, lambda <= 0.05)

model2_plot <- ggplot(filtered_results, aes(x = lambda, y = F, color = factor(alpha))) +
  geom_line() +
  geom_point(data = best_f[best_f$lambda <= 0.05, ], 
             aes(x = lambda, y = F), 
             size = 3, shape = 21, fill = "black", show.legend = FALSE) +
  geom_text(data = best_f[best_f$lambda <= 0.05, ], 
            aes(x = lambda, y = F, label = paste0("α=", alpha, ", λ=", signif(lambda, 2))), 
            vjust = -1, hjust = 0.5, size = 3.5, color = "black", show.legend = FALSE) +
  labs(title = "", 
       x = "Lambda", 
       y = "F1 (Cross-validation)",
       color = "Alpha") +
  theme_minimal()
model2_plot

ggsave(file.path(view_path, "model2.png"), plot = model2_plot, width = 8, height = 6, dpi = 300)


# Model 3
best <- model3$bestTune
best_f <- model3$results[model3$results$alpha == best$alpha & model3$results$lambda == best$lambda, ]

# Filtrar solo lambda <= 0.05
filtered_results <- subset(model3$results, lambda <= 0.05)

model3_plot <- ggplot(filtered_results, aes(x = lambda, y = F, color = factor(alpha))) +
  geom_line() +
  geom_point(data = best_f[best_f$lambda <= 0.05, ], 
             aes(x = lambda, y = F), 
             size = 3, shape = 21, fill = "black", show.legend = FALSE) +
  geom_text(data = best_f[best_f$lambda <= 0.05, ], 
            aes(x = lambda, y = F, label = paste0("α=", alpha, ", λ=", signif(lambda, 2))), 
            vjust = -1, hjust = 0.5, size = 3.5, color = "black", show.legend = FALSE) +
  labs(title = "", 
       x = "Lambda", 
       y = "F1 (Cross-validation)",
       color = "Alpha") +
  theme_minimal()
ggsave(file.path(view_path, "model3.png"), plot = model3_plot, width = 8, height = 6, dpi = 300)

# Model 4
best <- model4$bestTune
best_f <- model4$results[model4$results$alpha == best$alpha & model4$results$lambda == best$lambda, ]

# Filtrar solo lambda <= 0.1
filtered_results <- subset(model4$results, lambda <= 0.1)

model4_plot <- ggplot(filtered_results, aes(x = lambda, y = F, color = factor(alpha))) +
  geom_line() +
  geom_point(data = best_f[best_f$lambda <= 0.1, ], 
             aes(x = lambda, y = F), 
             size = 3, shape = 21, fill = "black", show.legend = FALSE) +
  geom_text(data = best_f[best_f$lambda <= 0.1, ], 
            aes(x = lambda, y = F, label = paste0("α=", alpha, ", λ=", signif(lambda, 2))), 
            vjust = -1, hjust = 0.5, size = 3.5, color = "black", show.legend = FALSE) +
  labs(title = "", 
       x = "Lambda", 
       y = "F1 (Cross-validation)",
       color = "Alpha") +
  theme_minimal()
model4_plot

ggsave(file.path(view_path, "model4.png"), plot = model4_plot, width = 8, height = 6, dpi = 300)



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

