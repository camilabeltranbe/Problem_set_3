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
       "blockCV",
       "terra",
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
library(caret)
library(dplyr)
wd <- ("/Users/aleja/Documents/Maestría Uniandes/Clases/Big Data y Machine Learning/Repositorios Git Hub/Problem_set_3")
setwd(paste0(wd,"/stores"))
load("data_final.RData")

#- 2 | Arreglo data, regresores de texto, PCA ---------------------

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

#-3 | Random Forest: Modelos 1, 2 y 3 - variables importantes, texto como regresores y componentes principales de texto --------------------------
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


## Random Forest

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

#Modelo 1
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
#Kaggle = 302944224.41176

# Predicción y evaluación del modelo 3 en el conjunto de entrenamiento
augment(full_final_fit2, new_data = train_full) %>%
  mae(truth = price, estimate = .pred) # mae = 48923390

# Predicción en el conjunto de prueba (modelo 3)
predicted2 <- augment(full_final_fit2, new_data = test_full)
model3_rf_predictions <- predicted2[,c("property_id",".pred")]
colnames(model3_rf_predictions) <- c("property_id","price")
write.csv(model3_rf_predictions, "model3_rf_predictions_ale.csv", row.names = F)
#Kaggle = 264966609.86125




#- 4 | Boosting: Modelo ---------------------------------------------
## Primero creamos diferentes subsets para aplicar a boosting ##
# predictores sin stopwords y componentes principales
train1 <- train %>% 
  select(property_id,price,surface_total3,property_type,lat,lon,
         rooms3,bathrooms3,estrato,n_pisos_numerico,zona_t_g,localidad,
         Dist_pol,dist_parque)

test1 <- test %>% 
  select(property_id,surface_total3,property_type,lat,lon,
         rooms3,bathrooms3,estrato,n_pisos_numerico,zona_t_g,localidad,
         Dist_pol,dist_parque)

#crear dummys train
dummys <- dummy(subset(train1, select = c(property_type, localidad)))
dummys <- as.data.frame(apply(dummys,2,function(x){as.numeric(x)}))
train1 <- cbind(subset(train1, select = -c(property_type, localidad)),dummys)
#crear dummys test
dummys <- dummy(subset(test1, select = c(property_type, localidad)))
dummys <- as.data.frame(apply(dummys,2,function(x){as.numeric(x)}))
test1 <- cbind(subset(test1, select = -c(property_type, localidad)),dummys)
#dejar variables que comparten test y train depsues de crear dummys
train1 <- train1[c(colnames(test1),"price")]

#componentes principales
pc_train <- train1 %>% 
  select(-property_id,-price)
pc_test <- test1 %>% 
  select(-property_id)
pc_train <- prcomp(as.matrix(pc_train), scale=TRUE)
pc_test <- prcomp(as.matrix(pc_test), scale=TRUE)

#guardar componentes 
pc_train <- as.data.frame(predict(pc_train)) %>%
  mutate(property_id = train1$property_id)
pc_test <- as.data.frame(predict(pc_test)) %>%
  mutate(property_id = test1$property_id)

#unir base de datos 
train2 <- merge(train1,pc_train,by = "property_id")
test2 <- merge(test1,pc_test,by = "property_id")
#base con solo 10 componentes
train3 <- cbind(train1$price,pc_train)[c(1:11,ncol(pc_train)+1)]
colnames(train3)[1] <- "price"
test3 <- pc_test[,c(1:10,ncol(pc_test))]

## Creamos una data full con las dummys nuevas ##
#crear dummys train
dummys <- dummy(subset(train_full, select = c(property_type, localidad)))
dummys <- as.data.frame(apply(dummys,2,function(x){as.numeric(x)}))
train_full_dummys <- cbind(subset(train_full, select = -c(property_type, localidad)),dummys)
#crear dummys test
dummys <- dummy(subset(test_full, select = c(property_type, localidad)))
dummys <- as.data.frame(apply(dummys,2,function(x){as.numeric(x)}))
test_full_dummys <- cbind(subset(test_full, select = -c(property_type, localidad)),dummys)
#dejar variables que comparten test y train despues de crear dummys
train_full_dummys <- train_full_dummys[c(colnames(test_full_dummys),"price")]
#Quitamos el segundo price de la train full dummys
train_full_dummys$price.1=NULL



#- 4.1 | Modelo Boosting 1 saturado ----------------------------------------
fitControl <- trainControl(method ="cv",number=5)
#Cargamos los parámetros del boosting
grid_xbgoost <- expand.grid(nrounds = c(500),
                            max_depth = c(4),
                            eta = c(0.01), 
                            gamma = c(0), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.33,0.66), 
                            subsample = c(0.4))


#xgboost con variables y componentes principales
set.seed(63928)
XGBoost_model1 <- train(price ~ estrato + surface_covered3 + n_pisos_numerico + Dist_pol + dist_parque + rooms3 +
                  bathrooms3 + lat + lon + localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                  localidad_ENGATIVA + localidad_LOS.MARTIRES + localidad_SAN.CRISTOBAL + localidad_SANTA.FE + 
                  localidad_SUBA + localidad_TEUSAQUILLO + localidad_USAQUEN + zona_t_g + property_type_Apartamento +
                  property_type_Casa + PC1 + PC5 + PC7 + PC8 + PC12 + PC15 + PC16 + PC17 + PC18 + PC23 + PC28 +
                  PC32 + PC35 + PC37 + PC41 + PC42 + PC45 + PC47 + PC49 + PC50 + PC51 +
                  PC55 + PC59 + PC60 + PC61 + PC62 + PC63 + PC68 + PC71,
                  data=train_full_dummys[-1], #excluye variable de property_id
                  method = "xgbTree",
                  trControl = fitControl,
                  tuneGrid=grid_xbgoost)        

train_XGBoost_model1 <- train_full_dummys %>% 
  mutate(price_pred = predict(XGBoost_model1, newdata = train_full_dummys))  
yardstick::mae(train_XGBoost_model1, truth = price, estimate = price_pred) #predicción en train: mae = 149304612


Predic_XGBoost_model1 <- test_full_dummys %>%
  mutate(price = predict(XGBoost_model1, newdata = test_full_dummys)) %>% 
  select(property_id,price) 

write.csv(Predic_XGBoost_model1,"XGBoost_model1_ale.csv",row.names = F) 
#Puntaje Kaggle: 

#- 4.2 | Modelo Boosting 2 sin tantas variables ----------------------------------------
fitControl <- trainControl(method ="cv",number=5)
#Cargamos los parámetros del boosting
grid_xbgoost <- expand.grid(nrounds = c(500),
                            max_depth = c(4),
                            eta = c(0.01), 
                            gamma = c(0), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.33,0.66), 
                            subsample = c(0.4))


#xgboost sin tantas variables y componentes principales
set.seed(63928)
XGBoost_model2 <- train(price ~ estrato + surface_covered3 + n_pisos_numerico + rooms3 +
                          bathrooms3 + lat + lon + localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                          localidad_ENGATIVA + localidad_LOS.MARTIRES + localidad_SAN.CRISTOBAL + localidad_SANTA.FE + 
                          localidad_SUBA + localidad_TEUSAQUILLO + localidad_USAQUEN + zona_t_g + property_type_Apartamento +
                          property_type_Casa + PC7 + PC8 + PC12 + PC15 + PC18 + PC23 + PC28 +
                          PC32 + PC35 + PC37 + PC42 + PC47 + PC49 + PC51 +
                          PC55 + PC59 + PC63 + PC68,
                        data=train_full_dummys[-1], #excluye variable de property_id
                        method = "xgbTree",
                        trControl = fitControl,
                        tuneGrid=grid_xbgoost)        

train_XGBoost_model2 <- train_full_dummys %>% 
  mutate(price_pred = predict(XGBoost_model2, newdata = train_full_dummys))  
yardstick::mae(train_XGBoost_model2, truth = price, estimate = price_pred) #predicción en train: mae = 150564863


Predic_XGBoost_model2 <- test_full_dummys %>%
  mutate(price = predict(XGBoost_model2, newdata = test_full_dummys)) %>% 
  select(property_id,price) 

write.csv(Predic_XGBoost_model2,"XGBoost_model2_ale.csv",row.names = F) 
#Puntaje Kaggle: 

#- 4.3 | Modelo Boosting 3 sin tantas variables ----------------------------------------
fitControl <- trainControl(method ="cv",number=5)
#Cargamos los parámetros del boosting
grid_xbgoost <- expand.grid(nrounds = c(500),
                            max_depth = c(4), 
                            eta = c(0.25,0.5), 
                            gamma = c(0), 
                            min_child_weight = c(50),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4))


#xgboost sin tantas variables y componentes principales
set.seed(63928)
XGBoost_model3 <- train(price ~ estrato + surface_covered3 + n_pisos_numerico + rooms3 + surface_covered3^2 + rooms3^2 +
                          bathrooms3 + lat + lon + localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                          localidad_ENGATIVA + localidad_LOS.MARTIRES + localidad_SAN.CRISTOBAL + localidad_SANTA.FE + 
                          localidad_SUBA + localidad_TEUSAQUILLO + localidad_USAQUEN + zona_t_g + property_type_Apartamento +
                          property_type_Casa + PC7 + PC8 + PC12 + PC15 + PC18 + PC23 + PC28 +
                          PC32 + PC35 + PC37 + PC42 + PC47 + PC49 + PC51 +
                          PC55 + PC59 + PC63 + PC68,
                        data=train_full_dummys[-1], #excluye variable de property_id
                        method = "xgbTree",
                        trControl = fitControl,
                        tuneGrid=grid_xbgoost)        

train_XGBoost_model3 <- train_full_dummys %>% 
  mutate(price_pred = predict(XGBoost_model3, newdata = train_full_dummys))  
yardstick::mae(train_XGBoost_model3, truth = price, estimate = price_pred) #predicción en train: mae = 113750326


Predic_XGBoost_model3 <- test_full_dummys %>%
  mutate(price = predict(XGBoost_model3, newdata = test_full_dummys)) %>% 
  select(property_id,price) 

write.csv(Predic_XGBoost_model3,"XGBoost_model3_ale.csv",row.names = F) 
#Puntaje Kaggle: 

#- 4.4 | Modelo Boosting 4 sin tantas variables ----------------------------------------
fitControl <- trainControl(method ="cv",number=5)
#Cargamos los parámetros del boosting
grid_xbgoost <- expand.grid(nrounds = c(500),
                            max_depth = c(4), 
                            eta = c(0.25,0.5), 
                            gamma = c(0), 
                            min_child_weight = c(50),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4))


#xgboost sin tantas variables y componentes principales
set.seed(63928)
XGBoost_model4 <- train(price ~ estrato + surface_covered3 + n_pisos_numerico + rooms3 + surface_covered3^2 + rooms3^2 +
                          bathrooms3 + lat + lon + localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                          localidad_ENGATIVA + localidad_LOS.MARTIRES + localidad_SAN.CRISTOBAL + localidad_SANTA.FE + 
                          localidad_SUBA + localidad_TEUSAQUILLO + localidad_USAQUEN + zona_t_g + property_type_Apartamento +
                          property_type_Casa + PC7 + PC8 + PC12 + PC15 + PC18 + PC23 + PC28 +
                          PC32 + PC35 + PC37 + PC42 + PC47 + PC49 + PC51 +
                          PC55 + PC59 + PC63 + PC68 + surface_total3 + surface_total3^2 + chimene +
                          bbq + deposit + excelent + vist,
                        data=train_full_dummys[-1], #excluye variable de property_id
                        method = "xgbTree",
                        trControl = fitControl,
                        tuneGrid=grid_xbgoost)        

train_XGBoost_model4 <- train_full_dummys %>% 
  mutate(price_pred = predict(XGBoost_model4, newdata = train_full_dummys))  
yardstick::mae(train_XGBoost_model4, truth = price, estimate = price_pred) #predicción en train: mae = 112020789


Predic_XGBoost_model4 <- test_full_dummys %>%
  mutate(price = predict(XGBoost_model4, newdata = test_full_dummys)) %>% 
  select(property_id,price) 

write.csv(Predic_XGBoost_model4,"XGBoost_model4_ale.csv",row.names = F) 
#Puntaje Kaggle: 

#- 4.5 | Modelo Boosting 5 sin tantas variables ----------------------------------------
fitControl <- trainControl(method ="cv",number=5)
#Cargamos los parámetros del boosting
grid_xbgoost <- expand.grid(nrounds = c(500),
                            max_depth = c(4), 
                            eta = c(0.25,0.5), 
                            gamma = c(0), 
                            min_child_weight = c(50),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4))


#xgboost sin tantas variables y componentes principales
set.seed(63928)
XGBoost_model5 <- train(price ~ estrato + surface_covered3 + n_pisos_numerico + rooms3 + surface_covered3^2 + rooms3^2 +
                          bathrooms3 + lat + lon + localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                          localidad_ENGATIVA + localidad_LOS.MARTIRES + localidad_SAN.CRISTOBAL + localidad_SANTA.FE + 
                          localidad_SUBA + localidad_TEUSAQUILLO + localidad_USAQUEN + zona_t_g + property_type_Apartamento +
                          property_type_Casa + PC7 + PC8 + PC12 + PC15 + PC18 + PC23 + PC28 +
                          PC32 + PC35 + PC37 + surface_total3^2 + chimene + garaj + localidad_CHAPINERO:rooms3 +
                          bbq + deposit + excelent + vist,
                        data=train_full_dummys[-1], #excluye variable de property_id
                        method = "xgbTree",
                        trControl = fitControl,
                        tuneGrid=grid_xbgoost)        

train_XGBoost_model5 <- train_full_dummys %>% 
  mutate(price_pred = predict(XGBoost_model5, newdata = train_full_dummys))  
yardstick::mae(train_XGBoost_model5, truth = price, estimate = price_pred) #predicción en train: mae = 115129437


Predic_XGBoost_model5 <- test_full_dummys %>%
  mutate(price = predict(XGBoost_model5, newdata = test_full_dummys)) %>% 
  select(property_id,price) 

write.csv(Predic_XGBoost_model5,"XGBoost_model5_ale.csv",row.names = F) 
#Puntaje Kaggle: 


#- 4.6 | Modelo Boosting 6 sin tantas variables ----------------------------------------
fitControl <- trainControl(method ="cv",number=5)
#Cargamos los parámetros del boosting
grid_xbgoost <- expand.grid(nrounds = c(500),
                            max_depth = c(4), 
                            eta = c(0.25,0.5), 
                            gamma = c(0), 
                            min_child_weight = c(50),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4))


#xgboost sin tantas variables y componentes principales
set.seed(63928)
XGBoost_model6 <- train(price ~ estrato + surface_covered3 + n_pisos_numerico + rooms3 + surface_covered3^2 + rooms3^2 +
                          bathrooms3 + lat + lon + zona_t_g + property_type_Apartamento +
                          property_type_Casa + PC7 + PC8 + PC12 + PC15 + PC18 + PC23 + PC28 +
                          PC32 + PC35 + PC37 + surface_total3^2 + chimene + garaj +
                          bbq + deposit + excelent + vist,
                        data=train_full_dummys[-1], #excluye variable de property_id
                        method = "xgbTree",
                        trControl = fitControl,
                        tuneGrid=grid_xbgoost)        

train_XGBoost_model6 <- train_full_dummys %>% 
  mutate(price_pred = predict(XGBoost_model6, newdata = train_full_dummys))  
yardstick::mae(train_XGBoost_model6, truth = price, estimate = price_pred) #predicción en train: mae = 115797313


Predic_XGBoost_model6 <- test_full_dummys %>%
  mutate(price = predict(XGBoost_model6, newdata = test_full_dummys)) %>% 
  select(property_id,price) 

write.csv(Predic_XGBoost_model6,"XGBoost_model6_ale.csv",row.names = F) 
#Puntaje Kaggle: 

#- 4.7 | Modelo Boosting 7 con las mejores variables de RF ----------------------------------------
fitControl <- trainControl(method ="cv",number=5)
#Cargamos los parámetros del boosting
grid_xbgoost <- expand.grid(nrounds = c(500),
                            max_depth = c(4), 
                            eta = c(0.25,0.5), 
                            gamma = c(0), 
                            min_child_weight = c(50),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4))


#xgboost sin tantas variables y componentes principales
set.seed(63928)
XGBoost_model7 <- train(full_formula2,
                        data=train_full[-1], #excluye variable de property_id
                        method = "xgbTree",
                        trControl = fitControl,
                        tuneGrid=grid_xbgoost)        

train_XGBoost_model7 <- train_full_dummys %>% 
  mutate(price_pred = predict(XGBoost_model7, newdata = train_full))  
yardstick::mae(train_XGBoost_model7, truth = price, estimate = price_pred) #predicción en train: mae = 100766720


Predic_XGBoost_model7 <- test_full_dummys %>%
  mutate(price = predict(XGBoost_model7, newdata = test_full)) %>% 
  select(property_id,price) 

write.csv(Predic_XGBoost_model7,"XGBoost_model7_ale.csv",row.names = F) 
#Puntaje Kaggle: 

#- 4.8 | Modelo Boosting 8 sin tantas variables ----------------------------------------
fitControl <- trainControl(method ="cv",number=5)
#Cargamos los parámetros del boosting
grid_xbgoost <- expand.grid(nrounds = c(500),
                            max_depth = c(4), 
                            eta = c(0.25,0.5), 
                            gamma = c(0), 
                            min_child_weight = c(50),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4))


#xgboost sin tantas variables y componentes principales
set.seed(63928)
XGBoost_model8 <- train(price ~ rooms3 + bathrooms3 + surface_total3 + 
                          surface_covered3 + n_pisos_numerico + zona_t_g + estrato + 
                          PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                          PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + 
                          PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + 
                          PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + PC33 + 
                          PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + 
                          PC43 + PC44 + PC45 + PC46 + PC47 + PC48 + PC49 + PC50 + PC51 + 
                          PC52 + PC53 + PC54 + PC55 + PC56 + PC57 + PC58 + PC59 + PC60 + 
                          PC61 + PC62 + PC63 + PC64 + PC65 + PC66 + PC67 + PC68 + PC69 + 
                          PC70 + PC71 + abiert + acab + acces + alcob + ampli + are + 
                          ascensor + balcon + ban + bao + baos + bbq + bogot + buen + 
                          centr + cerc + cerr + chimene + closet + cocin + comedor + 
                          comercial + comunal + cuart + cuatr + cubiert + cuent + deposit + 
                          dos + edifici + espaci + estudi + excelent + exterior + garaj + 
                          gas + gimnasi + habit + habitacion + hermos + ilumin + independient + 
                          integral + interior + lavanderi + lind + mader + mts + natural + 
                          parqu + parqueader + pis + principal + priv + remodel + rop + 
                          sal + salon + sector + segur + servici + social + terraz + 
                          tres + ubicacion + uno + vias + vigil + visit + vist + zon +
                          localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                          localidad_ENGATIVA + localidad_LOS.MARTIRES + localidad_SAN.CRISTOBAL + localidad_SANTA.FE + 
                          localidad_SUBA + localidad_TEUSAQUILLO + localidad_USAQUEN + surface_covered3^2 + rooms3^2 +
                          surface_total3^2 + property_type_Apartamento + property_type_Casa,
                        data=train_full_dummys[-1], #excluye variable de property_id
                        method = "xgbTree",
                        trControl = fitControl,
                        tuneGrid=grid_xbgoost)        


# Obtener los mejores hiperparámetros
best_hyperparameters <- XGBoost_model8$bestTune
print(best_hyperparameters)

# Resumen del modelo
summary(XGBoost_model8)

train_XGBoost_model8 <- train_full_dummys %>% 
  mutate(price_pred = predict(XGBoost_model8, newdata = train_full_dummys))  
yardstick::mae(train_XGBoost_model8, truth = price, estimate = price_pred) #predicción en train: mae = 100712716


Predic_XGBoost_model8 <- test_full_dummys %>%
  mutate(price = predict(XGBoost_model8, newdata = test_full_dummys)) %>% 
  select(property_id,price) 

write.csv(Predic_XGBoost_model8,"XGBoost_model8_ale.csv",row.names = F) 
#Puntaje Kaggle: 248837015.95089

## Graficas relevantes ##

# Extraer el modelo xgboost entrenado
xgb_model <- XGBoost_model8$finalModel

# Calcular las predicciones en el conjunto de entrenamiento
train_full_dummys <- train_full_dummys %>% 
  mutate(price_pred = predict(XGBoost_model8, newdata = train_full_dummys))

# Gráfica de Dispersión de Predicciones vs Valores Reales
ggplot(train_full_dummys, aes(x = price, y = price_pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Predicciones vs Valores Reales") +
  xlab("Valores Reales") +
  ylab("Predicciones") +
  theme_minimal()

# Obtener los resultados de la validación cruzada
cv_results <- XGBoost_model8$resample

# Graficar el rendimiento en validación cruzada (MAE para cada fold) con líneas de color azul claro
ggplot(cv_results, aes(x = Resample, y = MAE)) +
  geom_boxplot(color = "cadetblue3", fill = "cadetblue3", alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  ggtitle("MAE en cada fold de validación cruzada") +
  xlab("Fold de Validación") +
  ylab("MAE") +
  theme_minimal()


# Convertir los datos a un objeto sf (simple features)
train_sf <- st_as_sf(train_full_dummys, coords = c("lon", "lat"), crs = 4326)

set.seed(86936)
block_folds <- spatial_block_cv(train_sf, v = 5)
autoplot(block_folds)



#- 5 | Modelos de regresión lineal ---------------------------------------------------
reg_lin_model1 <- lm(price ~ rooms3 + bathrooms3 + surface_total3 + 
                       surface_covered3 + n_pisos_numerico + zona_t_g + estrato + 
                       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                       PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + 
                       PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + 
                       PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + PC33 + 
                       PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + 
                       PC43 + PC44 + PC45 + PC46 + PC47 + PC48 + PC49 + PC50 + PC51 + 
                       PC52 + PC53 + PC54 + PC55 + PC56 + PC57 + PC58 + PC59 + PC60 + 
                       PC61 + PC62 + PC63 + PC64 + PC65 + PC66 + PC67 + PC68 + PC69 + 
                       PC70 + PC71 + abiert + acab + acces + alcob + ampli + are + 
                       ascensor + balcon + ban + bao + baos + bbq + bogot + buen + 
                       centr + cerc + cerr + chimene + closet + cocin + comedor + 
                       comercial + comunal + cuart + cuatr + cubiert + cuent + deposit + 
                       dos + edifici + espaci + estudi + excelent + exterior + garaj + 
                       gas + gimnasi + habit + habitacion + hermos + ilumin + independient + 
                       integral + interior + lavanderi + lind + mader + mts + natural + 
                       parqu + parqueader + pis + principal + priv + remodel + rop + 
                       sal + salon + sector + segur + servici + social + terraz + 
                       tres + ubicacion + uno + vias + vigil + visit + vist + zon +
                       localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                       localidad_ENGATIVA + localidad_LOS.MARTIRES + localidad_SAN.CRISTOBAL + localidad_SANTA.FE + 
                       localidad_SUBA + localidad_TEUSAQUILLO + localidad_USAQUEN + surface_covered3^2 + rooms3^2 +
                       surface_total3^2 + property_type_Apartamento + property_type_Casa,
                     data=train_full_dummys)

train_full_dummys$price_pred <- predict(reg_lin_model1,newdata = train_full_dummys)
test_full_dummys$price <- predict(reg_lin_model1,newdata = test_full_dummys)
yardstick::mae(data = train_full_dummys,truth = price, estimate = price_pred) #predicción en train: mae = 174322593

Predic_lin_reg_model1 <- test_full_dummys[,c("property_id","price")]
colnames(Predic_lin_reg_model1) <- c("property_id","price")
write.csv(Predic_lin_reg_model1,"Linear_reg_model1_ale.csv",row.names = F)


# - 6 | Random Forest saturado --------------------------------------

#Creamos las variables al cuadrado
test_full_dummys$surface_covered3_cuadrado <- test_full_dummys$surface_covered3^2
train_full_dummys$surface_covered3_cuadrado <- train_full_dummys$surface_covered3^2

test_full_dummys$rooms3_cuadrado <- test_full_dummys$rooms3^2
train_full_dummys$rooms3_cuadrado <- train_full_dummys$rooms3^2

test_full_dummys$surface_total3_cuadrado <- test_full_dummys$surface_total3^2
train_full_dummys$surface_total3_cuadrado <- train_full_dummys$surface_total3^2




#precio normal con mas variables
full_formula3<- as.formula(price ~ rooms3 + bathrooms3 + surface_total3 + 
                             surface_covered3 + n_pisos_numerico + zona_t_g + estrato + 
                             PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                             PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + 
                             PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + 
                             PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + PC33 + 
                             PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + 
                             PC43 + PC44 + PC45 + PC46 + PC47 + PC48 + PC49 + PC50 + PC51 + 
                             PC52 + PC53 + PC54 + PC55 + PC56 + PC57 + PC58 + PC59 + PC60 + 
                             PC61 + PC62 + PC63 + PC64 + PC65 + PC66 + PC67 + PC68 + PC69 + 
                             PC70 + PC71 + abiert + acab + acces + alcob + ampli + are + 
                             ascensor + balcon + ban + bao + baos + bbq + bogot + buen + 
                             centr + cerc + cerr + chimene + closet + cocin + comedor + 
                             comercial + comunal + cuart + cuatr + cubiert + cuent + deposit + 
                             dos + edifici + espaci + estudi + excelent + exterior + garaj + 
                             gas + gimnasi + habit + habitacion + hermos + ilumin + independient + 
                             integral + interior + lavanderi + lind + mader + mts + natural + 
                             parqu + parqueader + pis + principal + priv + remodel + rop + 
                             sal + salon + sector + segur + servici + social + terraz + 
                             tres + ubicacion + uno + vias + vigil + visit + vist + zon +
                             localidad_BARRIOS.UNIDOS + localidad_CANDELARIA + localidad_CHAPINERO +
                             localidad_ENGATIVA + localidad_LOS.MARTIRES + localidad_SAN.CRISTOBAL + localidad_SANTA.FE + 
                             localidad_SUBA + localidad_TEUSAQUILLO + localidad_USAQUEN + surface_covered3_cuadrado + rooms3_cuadrado +
                             surface_total3_cuadrado + property_type_Apartamento + property_type_Casa)


# Especificación del Modelo de Random Forest con el modo establecido
rf_spec <- rand_forest(mtry = tune(), trees = 1000, min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# Creación de la grilla de valores para los hiperparámetros
grid_values <- grid_regular(mtry(range = c(1, 10)), min_n(range = c(2, 20)), levels = 5)

# Preparación de las recetas
rec_full3 <- recipe(full_formula3, data = train_full_dummys) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

workflow_full3 <- workflow() %>%
  add_recipe(rec_full3) %>%
  add_model(rf_spec)

# Conversión de datos espaciales y validación cruzada espacial
train_sf <- st_as_sf(
  train_full_dummys, 
  coords = c("lon", "lat"),
  crs = 4326
)

set.seed(86936)
block_folds <- spatial_block_cv(train_sf, v = 5)
autoplot(block_folds)

# Ajuste del modelo con validación cruzada y selección del mejor modelo
detach("package:Metrics", unload=TRUE)
set.seed(86936)
tune_full3 <- tune_grid(
  workflow_full3,
  resamples = block_folds,
  grid = grid_values,
  metrics = metric_set(mae)
)

best_tune_full3 <- select_best(tune_full3, metric = "mae")

# Finalizar el flujo de trabajo con el mejor valor de parámetros
full_final3 <- finalize_workflow(workflow_full3, best_tune_full3)
full_final_fit3 <- fit(full_final3, data = train_full_dummys)

# Predicción y evaluación del modelo 4 en el conjunto de entrenamiento
augment(full_final_fit3, new_data = train_full_dummys) %>%
  mae(truth = price, estimate = .pred) # mae = 47857225

# Predicción en el conjunto de prueba (modelo 4)
predicted3 <- augment(full_final_fit3, new_data = test_full_dummys)
model4_rf_predictions <- predicted3[,c("property_id",".pred")]
colnames(model4_rf_predictions) <- c("property_id","price")
write.csv(model4_rf_predictions, "model4_rf_predictions_ale.csv", row.names = F)
#Kaggle = 255325981.63193


