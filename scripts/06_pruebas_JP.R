#05_Pruebas_Julian
#Neural Networks
rm(list = ls())
library(pacman)  # Importemos Tydiverse que será necesario para lo que viene: 

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load( ISLR2,
        glmnet,
        rio, ## read datasets
        tidyverse, # Manipular dataframes
        tm,   # para Text Mining
        tidytext, #Para tokenización
        stopwords,  # consultar stopwords
        tidymodels,
        sf,
        spatialsample,
        xgboost, #xgboosting
        adabag, #adaboosting  
        dummy #para crear dummys
) 

wd <- ("C:/Users/Juan/Documents/Problem_set_3")
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
# -------------------- Modelos lineales -------------------------------
#----------------------------------------------------------------------

# JP - Eliminar 1 ------
{
  #full_formula_jp1<- as.formula(" Log_Precio ~ property_type + rooms3 + bathrooms3 + 
  #          surface_covered3 + n_pisos_numerico + localidad + Dist_pol + dist_parque + estrato + zona_t_g + abiert + ascensor + bbq + parqueader + gimnasi + deposit + chimene + comunal")
  full_formula_jp1<- as.formula(
    paste("Log_Precio ~ property_type + rooms3 + bathrooms3 + 
            surface_covered3 + n_pisos_numerico + localidad + Dist_pol + dist_parque + estrato + zona_t_g + abiert + ascensor + bbq + parqueader + gimnasi + deposit + chimene + comunal",
          paste(pc_vars_train, collapse = "+"),  sep = "+"))
  
  # ,
  # paste(pc_vars_train, collapse = "+"), 
  # paste(bow_vars_train, collapse = "+"),  sep = "+"))
  
  # elastic net
  elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("glmnet")
  
  grid_values <- grid_regular(penalty(range = c(-1,2)), levels = 50) %>% # penalidad va de 10^-1 a 10^2 
    expand_grid(mixture = c(0,0.25,0.5,0.75,1))
  
  rec_full_jp1 <- recipe(full_formula_jp1, data = train_full) %>%
    step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
    step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
    step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
    step_normalize(all_predictors())  # normaliza los predictores. 
  
  workflow_full_jp1 <- workflow() %>% 
    add_recipe(rec_full_jp1) %>%
    add_model(elastic_net_spec)
  
  train_sf <- st_as_sf(
    train_full, 
    # "coords" is in x/y order -- so longitude goes first!
    coords = c("lon", "lat"),
    # Set our coordinate reference system to EPSG:4326,
    # the standard WGS84 geodetic coordinate reference system
    crs = 4326)
  
  set.seed(86936)
  block_folds <- spatial_block_cv(train_sf, v = 5)
  autoplot(block_folds)
  
  set.seed(86936)
  tune_full_jp1 <- tune_grid(
    workflow_full_jp1,         # El flujo de trabajo que contiene: receta y especificación del modelo
    resamples = block_folds,  # Folds de validación cruzada espacial
    grid = grid_values,        # Grilla de valores de penalización
    metrics = metric_set(mae)  # metrica
  )
  
  best_tune_full_jp1 <- select_best(tune_full_jp1, metric = "mae")
  
  full_final_jp1 <- finalize_workflow(workflow_full_jp1, best_tune_full_jp1)
  full_final_fit_jp1 <- fit(full_final_jp1, data = train_full)
  
  augment(full_final_fit_jp1, new_data = train_full) %>%
    mae(truth = price, estimate = .pred) #predicción en train: mae = 180012424
  
  #sacar exp 
  pred_train_jp1 <- augment(full_final_fit_jp1, new_data = train_full)
  pred_train_jp1$price1 <- round(exp(pred_train_jp1$.pred),-6)
  mae(data = pred_train_jp1,truth = price,estimate = price1) #mae = 179321902
  
  #predicción en test (modelo2)
  predicted_jp1 <- augment(full_final_fit_jp1, new_data = test_full)
  predicted_jp1$price1 <- round(exp(predicted_jp1$.pred),-6)
  modeljp1_elastic_net_text_regressors_logprice <- predicted_jp1[,c("property_id","price1")]
  colnames(modeljp1_elastic_net_text_regressors_logprice) <- c("property_id","price")
  write.csv(modeljp1_elastic_net_text_regressors_logprice,"modeljp2_elastic_net_logprice.csv",row.names = F)
}

# JP - Eliminar 2 ------
{
  full_formula_jp2<- as.formula(
    paste("Log_Precio ~ property_type + rooms3 + bathrooms3 + 
            surface_covered3 + n_pisos_numerico + localidad + Dist_pol + dist_parque + estrato + zona_t_g + abiert + ascensor + bbq + parqueader + gimnasi + deposit + chimene + comunal",
          paste(pc_vars_train, collapse = "+"),  sep = "+"))
  
  # elastic net
  elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("glmnet")
  
  grid_values <- grid_regular(penalty(range = c(-1,2)), levels = 50) %>% # penalidad va de 10^-1 a 10^2 
    expand_grid(mixture = c(0,0.25,0.5,0.75,1))
  
  rec_full_jp2 <- recipe(full_formula_jp2, data = train_full) %>%
    step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
    step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
    step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
    step_normalize(all_predictors())  # normaliza los predictores. 
  
  workflow_full_jp2 <- workflow() %>% 
    add_recipe(rec_full_jp2) %>%
    add_model(elastic_net_spec)
  
  train_sf <- st_as_sf(
    train_full, 
    # "coords" is in x/y order -- so longitude goes first!
    coords = c("lon", "lat"),
    # Set our coordinate reference system to EPSG:4326,
    # the standard WGS84 geodetic coordinate reference system
    crs = 4326)
  
  set.seed(86936)
  block_folds <- spatial_block_cv(train_sf, v = 5)
  autoplot(block_folds)
  
  set.seed(86936)
  tune_full_jp2 <- tune_grid(
    workflow_full_jp2,         # El flujo de trabajo que contiene: receta y especificación del modelo
    resamples = block_folds,  # Folds de validación cruzada espacial
    grid = grid_values,        # Grilla de valores de penalización
    metrics = metric_set(mae)  # metrica
  )
  
  best_tune_full_jp2 <- select_best(tune_full_jp2, metric = "mae")
  
  full_final_jp2 <- finalize_workflow(workflow_full_jp2, best_tune_full_jp2)
  full_final_fit_jp2 <- fit(full_final_jp2, data = train_full)
  
  augment(full_final_fit_jp2, new_data = train_full) %>%
    mae(truth = price, estimate = .pred) #predicción en train: mae = 180012424
  
  #sacar exp 
  pred_train_jp2 <- augment(full_final_fit_jp2, new_data = train_full)
  pred_train_jp2$price1 <- round(exp(pred_train_jp2$.pred),-6)
  mae(data = pred_train_jp2,truth = price,estimate = price1) #mae = 179321902
  
  #predicción en test (modelo2)
  predicted_jp2 <- augment(full_final_fit_jp2, new_data = test_full)
  predicted_jp1$price1 <- round(exp(predicted_jp2$.pred),-6)
  modeljp2_elastic_net_text_regressors_logprice <- predicted_jp2[,c("property_id","price1")]
  colnames(modeljp2_elastic_net_text_regressors_logprice) <- c("property_id","price")
  write.csv(modeljp2_elastic_net_text_regressors_logprice,"modeljp2_elastic_net_logprice.csv",row.names = F)
}

#### Mi manera ------------------------
model_form <- Ln_Ing_hog ~ PerXCuarto + 
  nocupados +
  as.factor(Head_Mujer)+
  Head_Cot_pension+
  Head_Rec_subsidio


#----------------------------------------------------------------------
# -------------------- Neural Network ---------------------------------
#----------------------------------------------------------------------

#Formula para predicción
formula_n <- as.formula("Log_Precio ~ property_type + rooms3 + bathrooms3 + 
            surface_covered3 + n_pisos_numerico + localidad + Dist_pol + dist_parque + estrato + zona_t_g + abiert + ascensor + bbq + parqueader + gimnasi + deposit + chimene + comunal")
formula_n_1 <- as.formula("Log_Precio ~ property_type + rooms3 + bathrooms3 + 
            surface_covered3 + n_pisos_numerico + localidad + Dist_pol + dist_parque + estrato + zona_t_g + abiert + ascensor + bbq + parqueader + gimnasi + deposit + chimene + comunal")


train_jp <- na.omit(train_full)
n <- nrow(train_jp)

lfit <-  lm(formula = formula_n, data = train_full)
lpred <- predict(lfit, test_full)

x <- scale(model.matrix(Log_Precio ~ property_type + rooms3 + bathrooms3 + 
                          surface_covered3 + n_pisos_numerico + localidad + Dist_pol + dist_parque + estrato + zona_t_g + abiert + ascensor + bbq + parqueader + gimnasi + deposit + chimene + comunal - 1 , data = train_full))
y <- train_full$Log_Precio

cvfit <- cv.glmnet(x, y, type.measure = "mae")
cpred <- predict(cvfit, x, s = "lambda.min")
mean(abs(y - cpred))
plot(cvfit)

install.packages("keras")
library(keras)


library(reticulate)
use_condaenv("r-keras", required = TRUE)



x <- scale(model.matrix(Log_Precio ~ property_type + rooms3 + bathrooms3 + 
                          surface_covered3 + n_pisos_numerico + localidad + Dist_pol + dist_parque + estrato + zona_t_g + abiert + ascensor + bbq + parqueader + gimnasi + deposit + chimene + comunal - 1 , data = train_full))



#Receta asociada - Investigar
recipe_nnet <- recipe( formula  , data = train_full) %>%
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%    # elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores. 

#Definamos la estructura de red 
nnet_base <- 
  mlp(hidden_units = 6, epochs = 500) %>% 
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
predicted <- augment(base_final_fit, new_data = train_full) %>% mae(truth = price, estimate = .pred)

#predicción en test (NN-modelo1)
predicted <- augment(base_final_fit, new_data = test_full)
NN_model1 <- predicted[,c("property_id",".pred")]
colnames(NN_model1) <- c("property_id","price")
write.csv(NN_model1,"NN_model1.csv",row.names = F)


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
  epochs =  seq(from= 300, to=500, by = 100)
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
mae<- augment(nnet_tuned_final_fit, new_data = train_full) %>%
  mae(truth = price, estimate = .pred) 
mae[1,3]

#predicted <- augment(base_final_fit, new_data = test_full)
predicted1 <- augment(nnet_tuned_final_fit,new_data = test_full)
NN_model2 <- predicted1[,c("property_id",".pred")]
colnames(NN_model2) <- c("property_id","price")
write.csv(NN_model2,"NN_model1.csv",row.names = F)
