#-----------------------------------------------------------------------------//
# Estadísticas descriptivas
# Problem Set 2 G10 - BDML 202501
# Fecha: 14 de marzo de 2025
#-----------------------------------------------------------------------------//

#-----------------------------------------------------------------------------//
# 1. Importar bases ----
#-----------------------------------------------------------------------------//

train_up = readRDS(file.path(stores_path, "upsampled_train_data.rds"))
train = readRDS(file.path(stores_path, "train_data.rds"))
test  = readRDS(file.path(stores_path, "test_data.rds"))

colnames(train)
colnames(train_up)
intersect(colnames(train), colnames(train_up))
colnames(test)

variables_modelos = c(
  "Pobre", "Clase", "Dominio", "hacinamiento", "Lp", "viv_noPropia",
  "jefe_salud_sub", "jefe_cot_pens", "jefe_edad", "jefe_edad2",
  "jefe_nivel_educ", "jefe_ocu", "N_personas", "N_ocupados", "N_inactivos",
  "N_desocupados", "N_pet", "N_menores", "N_mayor_dependiente", "N_mujer",
  "max_nivel_educ", "total_ind_ingresos", "prop_ina_pet", "prop_ocu_pet",
  "prop_fuentes_ing", "prop_menores_pob", "prop_mayores_pob",
  "promedio_anios_educ", "tipo_trabajo", "promedio_anios_educ", "anios_educ_hogar",
  
  # variables factores
  
  "jefe_mujer_f", "jefe_mayor_f"
  )

train_up = train_up %>% select(all_of(variables_modelos))
train = train %>% select(all_of(variables_modelos))
test = test %>% select(any_of(variables_modelos))

colnames(train)
colnames(train_up)
intersect(colnames(train), colnames(train_up))
colnames(test)

#-----------------------------------------------------------------------------//
# 2. Gráfica de desbalance de variable predicha: Pobre ----
#-----------------------------------------------------------------------------//

# Gráfica pobre train 

df_plot_train = train %>%
                count(Pobre) %>%
                mutate(prop = n / sum(n),
                       Pobre = as.factor(Pobre))

pobre_train = ggplot(df_plot_train, aes(x = Pobre, y = prop)) +
              geom_bar(stat = "identity", fill = "#EDEDED") +
              geom_text(
                aes(label = percent(prop, accuracy = 0.1)),
                vjust = -0.5,
                fontface = "plain",       
                family = "sans"          
              ) +
              scale_y_continuous(labels = percent_format(accuracy = 1)) +
              labs(
                title = "Train",
                x = "",
                y = "Porcentaje"
              ) +
              theme_classic()


# Gráfica pobre train upsampling 

df_plot_train_up = train_up %>%
                   count(Pobre) %>%
                   mutate(prop = n / sum(n),
                          Pobre = as.factor(Pobre))

pobre_train_up = ggplot(df_plot_train_up, aes(x = Pobre, y = prop)) +
                 geom_bar(stat = "identity", fill = "#EDEDED") +
                 geom_text(
                    aes(label = percent(prop, accuracy = 0.1)),
                    vjust = -0.5,
                    fontface = "plain",       
                    family = "sans"           
                  ) +
                 scale_y_continuous(labels = percent_format(accuracy = 1)) +
                 labs(
                    title = "Train upsampling",
                    x = "",
                    y = ""
                  ) +
                 theme_classic()

plots_pobre = grid.arrange(pobre_train, pobre_train_up, ncol = 2, 
              top = textGrob("", # Distribución porcentual de la variable ``Pobre'' 
              gp = gpar(fontsize = 14))); plots_pobre

# Guardar gráficos 

ggsave(file.path(paste0(view_path, "/plots_pobre.png")), 
       plot = plots_pobre, width = 10, height = 6, dpi = 300)

#-----------------------------------------------------------------------------//
# 3. Tabla descriptivas variables train ----
#-----------------------------------------------------------------------------//

skim(train)
skim(train_up)

table(train$Pobre)
table(train_up$Pobre)

#-----------------------------------------------------------------------------//
# 3.1 Train variables factor ----
#-----------------------------------------------------------------------------//

variables_factor = train %>%
                   select(where(is.factor)) %>% 
                   select(-c(Dominio, max_nivel_educ))

summary(variables_factor)
# skim(variables_factor)

vars_factor = colnames(variables_factor)

vars_factores_train = c("Pobre",               
                        "Clase",               
                        # "Dominio",            
                        "Propiedad vivienda",        
                        "Régimen salud jefe hogar",
                        "Cotiza pensión jefe hogar", 
                        "Nivel educación jefe hogar",     
                        "Estado ocupación jefe hogar",           
                        # "Máximo nivel educación hogar",      
                        "Tipo ocupación",        
                        "Jefe hogar mujer",       
                        "Jefe hogar mayor")

names(variables_factor) = vars_factores_train

# Por condición de pobreza

train_factores = variables_factor %>%
                select(Pobre, where(is.factor)) %>%
                pivot_longer(cols = -Pobre, names_to = "variable", values_to = "valor") %>%
                group_by(Pobre, variable, valor) %>%
                summarise(n = n(), .groups = "drop") %>%
                group_by(Pobre, variable) %>%
                mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
                ungroup() %>%
                select(-n) %>%
                pivot_wider(names_from = Pobre, values_from = porcentaje, values_fill = 0)


# Total

train_factores_total = variables_factor %>%
                        select(where(is.factor)) %>%
                        select(-Pobre) %>%
                        pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>%
                        group_by(variable, valor) %>%
                        summarise(n = n(), .groups = "drop") %>%
                        group_by(variable) %>%
                        mutate(Total_train = round(n / sum(n) * 100, 1)) %>%
                        ungroup() %>%
                        select(-n) 

train_factores_unido = merge(train_factores, train_factores_total, by = c("variable", "valor"))

train_factores_unido = train_factores_unido %>%
                        mutate(variable = factor(variable, levels = vars_factores_train)) %>%
                        arrange(variable)



#-----------------------------------------------------------------------------//
# 3.2 Train variables numericas  ----
#-----------------------------------------------------------------------------//

variables_numericas = train %>%
                      select(where(is.numeric), Pobre, -jefe_edad2)

summary(variables_numericas)

colnames(variables_numericas)
# skim(variables_numericas)

vars_numeric = colnames(variables_numericas)

vars_numeric_train = c("Hacinamiento",
                       "Línea pobreza",                  
                       "Edad jefe hogar",
                       "Personas hogar",          
                       "Ocupados",          
                       "Inactivos",         
                       "Desocupados hogar",      
                       "Pet",              
                       "Menores",           
                       "Mayores dependientes hogar", 
                       "Mujeres",             
                       "Fuentes ingresos hogar",  
                       "Prop inactivos/pet", 
                       "Prop ocupados/pet",
                       "Prop fuentes ingresos / persona",    
                       "Proporcion menores hogar",    
                       "Proporcion mayores hogar",    
                       "Promedio años educ > 15 años",  
                       "Años promedio edu hogar", 
                       "Pobre")    

names(variables_numericas) = vars_numeric_train

options(scipen = 999)  # Evita notación científica en todo


train_numericas = variables_numericas %>%
                  pivot_longer(cols = -Pobre, names_to = "variable", values_to = "valor") %>%
                  dplyr::group_by(Pobre, variable) %>%
                  dplyr::summarise(promedio = round(mean(valor, na.rm = TRUE), 2)) %>%
                  pivot_wider(names_from = Pobre, values_from = promedio, values_fill = 0)

train_numericas = train_numericas %>%
                  mutate(variable = factor(variable, levels = vars_numeric_train)) %>%
                  arrange(variable)

train_numericas_total = variables_numericas %>%
                        select(-Pobre) %>%
                        pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>%
                        group_by(variable) %>%
                        summarise(Total_train = round(mean(valor, na.rm = TRUE), 2)) %>%
                        arrange(match(variable, vars_numeric_train))  # si quieres conservar el orden

train_numericas_total = train_numericas_total %>%
                        mutate(variable = factor(variable, levels = vars_numeric_train)) %>%
                        arrange(variable)

str(train_numericas_total)
str(train_numericas)

train_numericas_unido = merge(train_numericas, train_numericas_total, by = c("variable"))


#-----------------------------------------------------------------------------//
# 4. Tabla descriptivas variables test ----
#-----------------------------------------------------------------------------//

#-----------------------------------------------------------------------------//
# 4.1 Test variables factor ----
#-----------------------------------------------------------------------------//

variables_factor_test = test %>%
                        select(where(is.factor)) %>% 
                        select(-c(Dominio, max_nivel_educ))

summary(variables_factor_test)
vars_factor_test = colnames(variables_factor_test)

# skim(variables_factor_test)

vars_factores_test = c("Clase",               
                        # "Dominio",            
                        "Propiedad vivienda",        
                        "Régimen salud jefe hogar",     
                        "Cotiza pensión jefe hogar", 
                        "Nivel educación jefe hogar",     
                        "Estado ocupación jefe hogar",           
                        # "Máximo nivel educación hogar",      
                        "Tipo ocupación",        
                        "Jefe hogar mujer",       
                        "Jefe hogar mayor")


names(variables_factor_test) = vars_factores_test

test_factores_total = variables_factor_test %>%
                        select(where(is.factor)) %>%
                        pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>%
                        group_by(variable, valor) %>%
                        summarise(n = n(), .groups = "drop") %>%
                        group_by(variable) %>%
                        mutate(Total_test = round(n / sum(n) * 100, 1)) %>%
                        ungroup() %>%
                        select(-n) 

test_factores_total = test_factores_total %>%
                        mutate(variable = factor(variable, levels = vars_factores_test)) %>%
                        arrange(variable)

#-----------------------------------------------------------------------------//
# 4.2 Test variables numéricas ----
#-----------------------------------------------------------------------------//

variables_numericas_test = test %>%
                          select(where(is.numeric), -jefe_edad2)

summary(variables_numericas_test)

colnames(variables_numericas_test)
# skim(variables_numericas_test)

vars_numeric_test_ = colnames(variables_numericas_test)

vars_numeric_test = c("Hacinamiento",
                       "Línea pobreza",                  
                       "Edad jefe hogar",
                       "Personas hogar",          
                       "Ocupados",          
                       "Inactivos",         
                       "Desocupados hogar",      
                       "Pet",              
                       "Menores",           
                       "Mayores dependientes hogar", 
                       "Mujeres",             
                       "Fuentes ingresos hogar",  
                       "Prop inactivos/pet", 
                       "Prop ocupados/pet",
                       "Prop fuentes ingresos / persona",    
                       "Proporcion menores hogar",    
                       "Proporcion mayores hogar",    
                       "Promedio años educ > 15 años",  
                       "Años promedio edu hogar")    

names(variables_numericas_test) = vars_numeric_test

options(scipen = 999)  # Evita notación científica en todo

test_numericas_total = variables_numericas_test %>%
                        pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>%
                        group_by(variable) %>%
                        summarise(Total_test = round(mean(valor, na.rm = TRUE), 2)) %>%
                        arrange(match(variable, variables_numericas_test))  # si quieres conservar el orden

test_numericas_total = test_numericas_total %>%
                        mutate(variable = factor(variable, levels = vars_numeric_test)) %>%
                        arrange(variable)

#-----------------------------------------------------------------------------//
# 5. Tablas finales  ----
#-----------------------------------------------------------------------------//

# Factores

# train_factores_unido
# test_factores_total
factores_unido = merge(train_factores_unido, test_factores_total, by = c("variable", "valor"))


sink(file.path(paste0(view_path, "/descriptivas_facotres.txt")))

xtable(factores_unido, caption = "Distribución porcentual por factor, condición de pobreza y total - Bases train y test") %>%
  print(type = "latex", include.rownames = FALSE)

sink()

# Numericas

# train_numericas_unido
# test_numericas_total
numericas_unido = merge(train_numericas_unido, test_numericas_total, by = c("variable"))


sink(file.path(paste0(view_path, "/descriptivas_numericas.txt")))

xtable(numericas_unido, caption = "Promedio variables según condición de pobreza y total - Bases train y test") %>%
  print(type = "latex", include.rownames = FALSE)

sink()

while (sink.number() > 0) sink()

#-----------------------------------------------------------------------------//
# 6. Tablas dominios ----
#-----------------------------------------------------------------------------//

#-----------------------------------------------------------------------------//
# 6.1 Train dominios ----
#-----------------------------------------------------------------------------//

variables_factor_dom = train %>%
                        select(where(is.factor)) %>% 
                        select(Dominio, Pobre)

vars_factor_dom = colnames(variables_factor_dom)

# Por condición de pobreza

train_factores_dom = variables_factor_dom %>%
                select(Pobre, where(is.factor)) %>%
                pivot_longer(cols = -Pobre, names_to = "variable", values_to = "valor") %>%
                group_by(Pobre, variable, valor) %>%
                summarise(n = n(), .groups = "drop") %>%
                group_by(Pobre, variable) %>%
                mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
                ungroup() %>%
                select(-n) %>%
                pivot_wider(names_from = Pobre, values_from = porcentaje, values_fill = 0)
              

# Total

train_factores_total_dom = variables_factor_dom %>%
                        select(where(is.factor)) %>%
                        select(-Pobre) %>%
                        pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>%
                        group_by(variable, valor) %>%
                        summarise(n = n(), .groups = "drop") %>%
                        group_by(variable) %>%
                        mutate(Total_train = round(n / sum(n) * 100, 1)) %>%
                        ungroup() %>%
                        select(-n) 

train_factores_unido_dom = merge(train_factores_dom, train_factores_total_dom, by = c("variable", "valor"))

#-----------------------------------------------------------------------------//
# 6.2 Test dominios ----
#-----------------------------------------------------------------------------//

variables_factor_test_dom = test %>%
  select(where(is.factor)) %>% 
  select(Dominio)

test_factores_total_dom  = variables_factor_test_dom %>%
                           select(where(is.factor)) %>%
                           pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>%
                           group_by(variable, valor) %>%
                           summarise(n = n(), .groups = "drop") %>%
                           group_by(variable) %>%
                           mutate(Total_test = round(n / sum(n) * 100, 1)) %>%
                           ungroup() %>%
                           select(-n) 
#-----------------------------------------------------------------------------//
# 6.3 Unión train y test dominios distribuciones ----
#-----------------------------------------------------------------------------//

dominios_unido = merge(train_factores_unido_dom, test_factores_total_dom, by = c("variable", "valor"))

dominios_unido = dominios_unido %>% select(-variable)

sink(file.path(paste0(view_path, "/tabla_dominios.txt")))

xtable(dominios_unido, caption = "Distribución porcentual por dominio según condición de pobreza y total - Bases train y test") %>%
  print(type = "latex", include.rownames = FALSE)

sink()

while (sink.number() > 0) sink()

