########### Modelos de prueba de con Random Forest ###############

#- 1 | Carga de librerias y base de datos ----------------------------------------------------
require("pacman")
p_load("tidyverse",
       "glmnet",
       "rio", # read datasets
       "tm", # para Text Mining
       "tidytext",#Para tokenización
       "caret",
       "smotefamily",
       "dplyr",
       "stopwords", # consultar stopwords
       "tidymodels",
       "sf",
       "spatialsample",
       "dummy",
       "rpart", # Recursive Partition and Regression Trees (To run Trees)
       "rpart.plot", ## for trees graphs
       "Metrics", # Evaluation Metrics for ML
       "MLeval",#*MLeval: Machine Learning Model Evaluation
       "ipred", # For Bagging 
       "pROC",
       "DiagrammeR",
       "xgboost",
       "ROSE",#remuestreo ROSE
       "ranger") #random forest 
library(dplyr)
wd <- ("/Users/aleja/Documents/Maestría Uniandes/Clases/Big Data y Machine Learning/Repositorios Git Hub/Problem_set_3")
setwd(paste0(wd,"/stores"))
load("data_final.RData")

#- 2 | Modelo 1: Random forest con variables relevantes ---------------------

## guardar las descripciones en un vector source
descriptions_train <- train$description
des_train_scource <- VectorSource(descriptions_train)
descriptions_test <- test$description
des_test_scource <- VectorSource(descriptions_test)

# Make a volatile corpus: coffee_corpus
des_corpus_train <- VCorpus(des_train_scource, readerControl = list( language = "es"))
des_corpus_test <- VCorpus(des_test_scource, readerControl = list( language = "es"))

# Función para reemplazar números por palabras
reemplazar_numeros <- function(texto) {
  palabras <- c("cero", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")
  # Reemplazar números del 0 al 10 por palabras
  for (i in 0:10) {
    texto <- gsub(sprintf("\\b%d\\b", i), palabras[i + 1], texto)}
  return(texto)}

# Convertir texto a formato ASCII eliminando tildes y caracteres especiales
eliminar_tildes <- function(texto) {
  texto_sin_tildes <- iconv(texto, "UTF-8", "ASCII", sub = "")
  return(texto_sin_tildes)}

reemplazar_car_especiales <- function(texto) {
  texto_sin_espe <-str_replace_all(texto, "[^[:alnum:]]", " ")
  return(texto_sin_espe)}

## volver a las palabras a sus raíces
stem_espanol<-  function(texto) {
  texto_stem <- stemDocument(texto, language="spanish")
  return(texto_stem)}

# Descargamos la lista de las stopwords en español de dos fuentes diferentes y las combinamos
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)
lista_palabras<- union(lista_palabras,  c("vendo", "venta", "vende", "etc", "carrera", "calle", "casa", "apto", "apartamento",
                                          "ubicado","ubicada") )

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace) ## remover espacios en blanco
  corpus <- tm_map(corpus, removePunctuation)  ## remover puntuacióm
  corpus <- tm_map(corpus, content_transformer(tolower)) # todo minuscula 
  corpus <- tm_map(corpus, removeWords, c(lista_palabras)) # remover stopwords y otras que se quieran aádir
  corpus<-  tm_map(corpus, content_transformer(reemplazar_numeros))  ## incluir funciones que nosotros creamos 
  corpus<-  tm_map(corpus, content_transformer(eliminar_tildes))  ## incluir funciones que nosotros creamos
  corpus<-  tm_map(corpus, content_transformer(reemplazar_car_especiales))  ## incluir funciones que nosotros creamos
  corpus<-  tm_map(corpus, content_transformer(stem_espanol))
  corpus<-  tm_map(corpus, removeNumbers)  # remover numeros restantes
  return(corpus)}

# apliquemos nuestra función de limpieza:
clean_des_train <- clean_corpus(des_corpus_train)
clean_des_test <- clean_corpus(des_corpus_test)

# crear la document - term matrix
description_dtm_train <- DocumentTermMatrix(clean_des_train)
description_dtm_test <- DocumentTermMatrix(clean_des_test)

#dejar en train solo variables que comparta con test
des_train <- as.data.frame(as.matrix(removeSparseTerms(description_dtm_train, 0.9), sparse=TRUE))

des_test <- as.data.frame(as.matrix(removeSparseTerms(description_dtm_test, 0.9), sparse=TRUE))

var_compartidas <- intersect(names(des_train), names(des_test))
des_train <- des_train[,var_compartidas]
des_test <- des_test[,var_compartidas]

# componentes principales eliminando las q tienen 90% de entradas nulas
pcdescriptions_train <- prcomp(as.matrix(des_train), scale=TRUE)
pcdescriptions_test <- prcomp(as.matrix(des_test), scale=TRUE)

# guardar componentes 
zdes_train <- as.data.frame(predict(pcdescriptions_train)) %>%
  mutate(property_id = train$property_id)

zdes_test <- as.data.frame(predict(pcdescriptions_test)) %>%
  mutate(property_id = test$property_id)

des_train <- des_train %>%
  mutate(property_id = train$property_id)

des_test <- des_test %>%
  mutate(property_id = test$property_id)

# unir bases de datos de texto y de componentes principales
train_full<-  train %>% 
  full_join(des_train, join_by(property_id)) %>%
  full_join(zdes_train, join_by(property_id))

test_full<-  test %>% 
  full_join(des_test, join_by(property_id)) %>%
  full_join(zdes_test, join_by(property_id))

#modelo 1 y 2 - Elastic Net - variables importantes, texto como regresores y componentes principales de texto --------------------------
bow_vars_train <- des_train %>%
  dplyr::select(-property_id) %>%
  colnames()

pc_vars_train <- zdes_train %>%
  dplyr::select(-property_id) %>%
  colnames()

#precio normal
colnames(train_full)
full_formula<- as.formula(
  paste("price ~ property_type + rooms3 + bathrooms3 + surface_total3 +
          surface_covered3 + n_pisos_numerico + zona_t_g ",
        paste(pc_vars_train, collapse = "+"), 
        paste(bow_vars_train, collapse = "+"),  sep = "+"))

#precio con logaritmo
full_formula1<- as.formula(
  paste(" Log_Precio ~ property_type + rooms3 + bathrooms3 + surface_total3 +
          surface_covered3 + n_pisos_numerico + zona_t_g ",
        paste(pc_vars_train, collapse = "+"), 
        paste(bow_vars_train, collapse = "+"),  sep = "+"))

#precio normal con mas variables
full_formula2<- as.formula(
  paste("price ~ property_type + rooms3 + bathrooms3 + surface_total3 +
          surface_covered3 + n_pisos_numerico + zona_t_g + estrato +
          Dist_pol + dist_parque",
        paste(pc_vars_train, collapse = "+"), 
        paste(bow_vars_train, collapse = "+"),  sep = "+"))


# Random Forest
RF<- ranger(Pobre~Dominio + Depto + N_cuartos_hog + Nper + nmenores_5 + nmenores_6_11 + 
              nmenores_12_17 + nocupados + nincapacitados + ntrabajo_menores + Head_Mujer + Head_Afiliado_SS + 
              Head_exper_ult_trab + Head_Rec_alimento + Head_Rec_subsidio + Head_Rec_vivienda + Head_Ocupacion + 
              Head_Segundo_trabajo + DormitorXpersona + Ln_Cuota + Ln_Pago_arrien + nmujeres + Ocup_vivienda + 
              Head_Cot_pension, 
            data = train_hogares1,
            num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
            mtry= 4,   # N. var aleatoriamente seleccionadas en cada partición. Baggin usa todas las vars.
            min.node.size  = 1, ## Numero minimo de observaciones en un nodo para intentar 
            importance="impurity") 
RF

elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")
install.packages("tidymodels")
# Actualizar rlang a la versión más reciente
install.packages("rlang")
install.packages("yardstick")

# Cargar las librerías necesarias
library(tidymodels)
library(tidyverse)
library(sf)
library(parsnip)
library(spatialsample)
library(yardstick) # Asegurarse de cargar yardstick para las métricas

# Especificación del Modelo de Random Forest con el modo establecido
rf_spec <- rand_forest(mtry = tune(), trees = 1000, min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# Creación de la grilla de valores para los hiperparámetros
grid_values <- grid_regular(mtry(range = c(1, 10)), min_n(range = c(2, 20)), levels = 5)

# Preparación de las recetas
rec_full <- recipe(full_formula, data = train_full) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

rec_full1 <- recipe(full_formula1, data = train_full) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

rec_full2 <- recipe(full_formula2, data = train_full) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# Creación de flujos de trabajo
workflow_full <- workflow() %>%
  add_recipe(rec_full) %>%
  add_model(rf_spec)

workflow_full1 <- workflow() %>%
  add_recipe(rec_full1) %>%
  add_model(rf_spec)

workflow_full2 <- workflow() %>%
  add_recipe(rec_full2) %>%
  add_model(rf_spec)

# Conversión de datos espaciales y validación cruzada espacial
train_sf <- st_as_sf(
  train_full, 
  coords = c("lon", "lat"),
  crs = 4326
)

set.seed(86936)
block_folds <- spatial_block_cv(train_sf, v = 5)
autoplot(block_folds)

# Ajuste del modelo con validación cruzada y selección del mejor modelo
set.seed(86936)
tune_full <- tune_grid(
  workflow_full,
  resamples = block_folds,
  grid = grid_values,
  metrics = metric_set(mae)
)

tune_full1 <- tune_grid(
  workflow_full1,
  resamples = block_folds,
  grid = grid_values,
  metrics = metric_set(mae)
)
###AQUI
tune_full2 <- tune_grid(
  workflow_full2,
  resamples = block_folds,
  grid = grid_values,
  metrics = metric_set(mae)
)

best_tune_full <- select_best(tune_full, metric = "mae")
best_tune_full1 <- select_best(tune_full1, metric = "mae")
best_tune_full2 <- select_best(tune_full2, metric = "mae")

# Finalizar el flujo de trabajo con el mejor valor de parámetros
full_final <- finalize_workflow(workflow_full, best_tune_full)
full_final_fit <- fit(full_final, data = train_full)

full_final1 <- finalize_workflow(workflow_full1, best_tune_full1)
full_final_fit1 <- fit(full_final1, data = train_full)

full_final2 <- finalize_workflow(workflow_full2, best_tune_full2)
full_final_fit2 <- fit(full_final2, data = train_full)

augment(full_final_fit, new_data = train_full) %>%
  mae(truth = price, estimate = .pred) #mae = 52343581

## Predicción en el conjunto de prueba y exportación de resultados modelo 1
predicted <- augment(full_final_fit, new_data = test_full)
model1_rf_predictions <- predicted[,c("property_id",".pred")]
colnames(model1_rf_predictions) <- c("property_id","price")
write.csv(model1_rf_predictions, "model1_rf_predictions_ale.csv", row.names = F)
#Kaggle 272878005.99285

## Predicción y evaluación del modelo 2 en el conjunto de entrenamiento
pred_train1 <- augment(full_final_fit1, new_data = train_full)
pred_train1$price1 <- round(exp(pred_train1$.pred), -6)
mae(data = pred_train1, truth = price, estimate = price1) # mae = 53247997

## Predicción en el conjunto de prueba (modelo 2)
predicted1 <- augment(full_final_fit1, new_data = test_full)
predicted1$price1 <- round(exp(predicted1$.pred), -6)
model2_rf_predictions_logprice <- predicted1[,c("property_id","price1")]
colnames(model2_rf_predictions_logprice) <- c("property_id","price")
write.csv(model2_rf_predictions_logprice, "model2_rf_predictions_logprice_ale.csv", row.names = F)

# Predicción y evaluación del modelo 3 en el conjunto de entrenamiento
augment(full_final_fit2, new_data = train_full) %>%
  mae(truth = price, estimate = .pred) # mae = 654534655

# Predicción en el conjunto de prueba (modelo 3)
predicted2 <- augment(full_final_fit2, new_data = test_full)
model3_rf_predictions <- predicted2[,c("property_id",".pred")]
colnames(model3_rf_predictions) <- c("property_id","price")
write.csv(model3_rf_predictions, "model3_rf_predictions_ale.csv", row.names = F)