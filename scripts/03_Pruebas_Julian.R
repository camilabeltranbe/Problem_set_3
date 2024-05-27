#05_Pruebas_Julian
#Neural Networks

setwd(paste0(wd,"/stores"))
load("data_final.RData")

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

#----------------------------------------------------------------------
# -------------------- Neural Network ---------------------------------
#----------------------------------------------------------------------

#Formula para predicción
formula <- as.formula(paste("price~surface_total3+surface_covered3+rooms3+bathrooms3+estrato+n_pisos_numerico+zona_t_g+Dist_pol+dist_parque+lat+lon+localidad <- "))

#Receta asociada - Investigar
recipe_nnet <- recipe( formula  , data = train_full) %>%
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%    # elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores. 

#Definamos la estructura de red 
nnet_base <- 
  mlp(hidden_units = 7, epochs = 10000) %>% 
  set_mode("regression") %>% 
  set_engine("nnet")
nnet_base

#flujo de trabajo
workflow_base <- workflow() %>% 
  add_recipe(recipe_nnet) %>%
  add_model(nnet_base) 

#Entrenamiento
base_final_fit <- fit(workflow_base, data = train_full)
#desempeño
augment(base_final_fit, new_data = train_full) %>% mae(truth = price, estimate = .pred)


#predicción en test (NN-modelo1)
predicted <- augment(base_final_fit, new_data = test_full)
NN_model3 <- predicted[,c("property_id",".pred")]
colnames(NN_model3) <- c("property_id","price")
write.csv(NN_model3,"NN_model3.csv",row.names = F)


#### definir validación cruzada espacial 
train_sf <- st_as_sf(
  train_full, 
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326,
  remove = FALSE
)

set.seed(86936)
block_folds <- spatial_block_cv(train_sf, v = 5)

#Ahora estamos listos para definir nuestra grilla y hacer el ejercicio de validación cruzada para nuestro nuevo flujo de trabajo:

nnet_tune <- 
  mlp(hidden_units =tune(), epochs = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("nnet", trace = 0) #trace 0 previene la verbosidad del entrenamiento

#Definamos la grilla de parámetros que vamos a usar en el ejercicio de validación cruzada espacial:

grid_values <- crossing( #`crossing` nos permite generar una grilla rectangular con la combinación de todos los hiperparámetros. 
  hidden_units = seq(from= 5, to=60, by = 5),
  epochs =  seq(from= 300, to=1000, by = 100)
)

#Especificamos un nuevo flujo de trabajo

workflow_tune <- workflow() %>% 
  add_recipe(recipe_nnet) %>%
  add_model(nnet_tune) 
# y estamos en condiciones de realizar la validación cruzada espacial

set.seed(86936)

tune_nnet <- tune_grid(
  workflow_tune,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  grid = grid_values,        # Grilla de valores 
  metrics = metric_set(mae)  # métrica
)

#Después seleccionamos las mejores métricas utilizando select_best(), esta función requiere que especifiques una metric en la que debe seleccionar.
best_tune_nnet <- select_best(tune_nnet, metric = "mae")
best_tune_nnet

#Por último calculemos el desempeño fuera de muestra usando la base de datos que contiene las propiedades en el Valle de Lili:
  
  # Finalizar el flujo de trabajo 'workflow' con el mejor valor de parámetros
  nnet_tuned_final <- finalize_workflow(workflow_tune, best_tune_nnet)

nnet_tuned_final_fit <- fit(nnet_tuned_final, data = train_full)
 
# y evaluemos su desempeño

## predicciones finales 
predicted1 <- augment(nnet_tuned_final_fit,new_data = test_full)
NN_model2 <- predicted1[,c("property_id",".pred")]
colnames(NN_model2) <- c("property_id","price")


#----------------------------------------------------------------------
# -------------------- Neural Network  3 capas ------------------------
#----------------------------------------------------------------------
# Cargar y preparar los datos
# Dividir los datos en entrenamiento y prueba
# Cargar las librerías necesarias
# Suponiendo que ya tienes train_full y test_full definidos
# y el data frame seleccionado con las columnas necesarias se llama selected_df

# Crear la receta de preprocesamiento
recipe_nnet <- recipe(price ~ surface_total3 + surface_covered3 + rooms3 + bathrooms3 + estrato + n_pisos_numerico + zona_t_g + Dist_pol + dist_parque + lat + lon + localidad, data = train_full) %>%
  step_novel(all_nominal_predictors()) %>%   # Para las clases no antes vistas en el train
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%  # Crear dummies para las variables categóricas
  step_zv(all_predictors()) %>%    # Elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # Normaliza los predictores

# Preparar la receta con los datos de entrenamiento
recipe_prep <- prep(recipe_nnet, training = train_full)

# Aplicar la receta a los datos de entrenamiento y prueba
train_prepped <- bake(recipe_prep, new_data = train_full)
test_prepped <- bake(recipe_prep, new_data = test_full)

# Convertir a matrices numéricas
train_NN <- as.matrix(select(train_prepped, -price))
target_NN <- as.matrix(train_prepped$price)

test_NN <- as.matrix(select(test_prepped, -price))
test_target_NN <- as.matrix(test_prepped$price)

# Revisar las formas de los datos
print(dim(train_NN))
print(dim(target_NN))

# Definir el modelo
model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = 'relu', input_shape = ncol(train_NN)) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 1)


# Compilar el modelo con MAE como métrica
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(),
  metrics = c('mean_absolute_error')
)

# Resumen del modelo
summary(model)

# Entrenar el modelo con Early Stopping y reducción de la tasa de aprendizaje
history <- model %>% fit(
  train_NN, target_NN,
  epochs = 100,
  batch_size = 32,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(
    callback_early_stopping(monitor = "val_mean_absolute_error", patience = 10),
    callback_reduce_lr_on_plateau(monitor = "val_mean_absolute_error", factor = 0.1, patience = 5)
  )
)

# Evaluar el modelo
model %>% evaluate(train_NN, target_NN)

# Realizar predicciones en los datos de Tests
predictions_tests <- model %>% predict(test_NN)

test$y_pred <- predictions_tests
test$y_pred <- predict(model,newdata = test_NN)
model4_NN_3capas <-as.data.frame(test[,c("property_id","y_pred")])
colnames(model4_NN_3capas) <- c("property_id","price")

write.csv(model4_NN_3capas,"model4_NN_3capas.csv",row.names = F)

#----------------------------------------------------------------------
# -------------------- 2 Neural Network  4 capas ------------------------
#----------------------------------------------------------------------
# Cargar y preparar los datos
# Dividir los datos en entrenamiento y prueba
# Cargar las librerías necesarias
# Suponiendo que ya tienes train_full y test_full definidos
# y el data frame seleccionado con las columnas necesarias se llama selected_df

# Crear la receta de preprocesamiento
recipe_nnet <- recipe(price ~ year+ surface_covered3 + rooms3 + bathrooms3 + estrato + n_pisos_numerico + zona_t_g + lat + lon, data = train_full) %>%
  step_novel(all_nominal_predictors()) %>%   # Para las clases no antes vistas en el train
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%  # Crear dummies para las variables categóricas
  step_zv(all_predictors()) %>%    # Elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # Normaliza los predictores

# Preparar la receta con los datos de entrenamiento
recipe_prep <- prep(recipe_nnet, training = train_full)

# Aplicar la receta a los datos de entrenamiento y prueba
train_prepped <- bake(recipe_prep, new_data = train_full)
test_prepped <- bake(recipe_prep, new_data = test_full)

# Convertir a matrices numéricas
train_NN <- as.matrix(select(train_prepped, -price))
target_NN <- as.matrix(train_prepped$price)

test_NN <- as.matrix(select(test_prepped, -price))
test_target_NN <- as.matrix(test_prepped$price)

# Revisar las formas de los datos
print(dim(train_NN))
print(dim(target_NN))

# Definir el modelo
model <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = 'relu', input_shape = ncol(train_NN)) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 1)


# Compilar el modelo con MAE como métrica
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(),
  metrics = c('mean_absolute_error')
)

# Resumen del modelo
summary(model)

# Entrenar el modelo con Early Stopping y reducción de la tasa de aprendizaje
history <- model %>% fit(
  train_NN, target_NN,
  epochs = 100,
  batch_size = 32,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(
    callback_early_stopping(monitor = "val_mean_absolute_error", patience = 10),
    callback_reduce_lr_on_plateau(monitor = "val_mean_absolute_error", factor = 0.1, patience = 5)
  )
)

# Evaluar el modelo
model %>% evaluate(train_NN, target_NN)
model %>% evaluate(test_NN, test_target_NN)

# Realizar predicciones en los datos de Tests
predictions_tests <- model %>% predict(test_NN)

test$y_pred <- predictions_tests
model5_NN_4capas <-as.data.frame(test[,c("property_id","y_pred")])
colnames(model5_NN_4capas) <- c("property_id","price")

write.csv(model5_NN_4capas,"model5_NN_4capas.csv",row.names = F)


ite.csv(model4_NN_3capas,"model4_NN_3capas.csv",row.names = F)

#----------------------------------------------------------------------
# --------------------  Neural Network  Cross validation ------------------------
#----------------------------------------------------------------------
train_NN <- train_full %>% select(year,surface_total3, rooms3, bathrooms3, estrato, n_pisos_numerico, zona_t_g, Dist_pol, dist_parque, lat, lon, localidad, price)

# Convertir la variable 'localidad' en factores
train_NN$localidad <- as.factor(train_NN$localidad)
train_NN$year <- as.factor(train_NN$year)


# Paso 2: Definir la receta de preprocesamiento
recipe_nnet <- recipe(price ~ ., data = train_NN) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors()) 

# Paso 3: Convertir los datos a formato espacial
train_sf <- st_as_sf(train_NN, coords = c("lon", "lat"), crs = 4326, remove= FALSE)

# Paso 4: Definir los folds espaciales
set.seed(86936)
block_folds <- spatial_block_cv(train_sf, v = 5)

# Paso 5: Definir la especificación del modelo con tuning
nnet_tune <- 
  mlp(hidden_units = tune(), epochs = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("keras")

# Definir la grilla de hiperparámetros para ajustar
grid_values <- crossing(
  hidden_units = seq(from = 10, to = 100, by = 10),
  epochs = seq(from = 100, to = 500, by = 100)
)

#Definamos la grilla de parámetros que vamos a usar en el ejercicio de validación cruzada espacial:

grid_values <- crossing( #`crossing` nos permite generar una grilla rectangular con la combinación de todos los hiperparámetros. 
  hidden_units = seq(from= 5, to=60, by = 5),
  epochs =  seq(from= 300, to=500, by = 100)
)

# Paso 6: Preparar el flujo de trabajo
workflow_tune <- workflow() %>% 
  add_recipe(recipe_nnet) %>%
  add_model(nnet_tune)

# Realizar la validación cruzada espacial
tune_nnet <- tune_grid(
  workflow_tune, 
  resamples = block_folds, 
  grid = grid_values, 
  metrics = metric_set(mae)
)

# Obtener los mejores hiperparámetros
best_params <- select_best(tune_nnet, "mae")

# Paso 7: Entrenar el modelo final con los mejores hiperparámetros
final_nnet <- mlp(
  hidden_units = best_params$hidden_units,
  epochs = best_params$epochs
) %>% 
  set_mode("regression") %>% 
  set_engine("keras")

workflow_final <- workflow() %>% 
  add_recipe(recipe_nnet) %>%
  add_model(final_nnet)

final_fit <- fit(workflow_final, data = train_full)

# Paso 8: Realizar predicciones con el conjunto de datos de prueba
Tests <- Tests %>% 
  select(surface_total3, surface_covered3, rooms3, bathrooms3, estrato, n_pisos_numerico, zona_t_g, Dist_pol, dist_parque, lat, lon, localidad, property.id)

Tests$localidad <- as.factor(Tests$localidad)

predictions <- predict(final_fit, new_data = Tests) %>%
  bind_cols(Tests %>% select(property.id))

# Paso 9: Crear un nuevo data frame con las predicciones
results <- predictions %>% 
  select(property.id, .pred) %>% 
  rename(predicted_price = .pred)

print(results)
