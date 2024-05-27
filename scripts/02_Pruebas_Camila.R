options(scipen=999)

setwd(paste0(wd,"/stores"))
load("data_final.RData")

#clase de Andres - comentarios como regresores ------------------------------------------------------------
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
  select(-property_id) %>%
  colnames()

pc_vars_train <- zdes_train %>%
  select(-property_id) %>%
  colnames()

#precio normal
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


# elastic net
elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

grid_values <- grid_regular(penalty(range = c(-1,2)), levels = 50) %>% # penalidad va de 10^-1 a 10^2 
  expand_grid(mixture = c(0,0.25,0.5,0.75,1))

rec_full <- recipe(full_formula, data = train_full) %>%
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores. 

rec_full1 <- recipe(full_formula1, data = train_full) %>%
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores. 

rec_full2 <- recipe(full_formula2, data = train_full) %>%
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores. 

workflow_full <- workflow() %>% 
  add_recipe(rec_full) %>%
  add_model(elastic_net_spec)

workflow_full1 <- workflow() %>% 
  add_recipe(rec_full1) %>%
  add_model(elastic_net_spec)

workflow_full2 <- workflow() %>% 
  add_recipe(rec_full1) %>%
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
tune_full <- tune_grid(
  workflow_full,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  grid = grid_values,        # Grilla de valores de penalización
  metrics = metric_set(mae)  # metrica
)

set.seed(86936)
tune_full1 <- tune_grid(
  workflow_full1,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  grid = grid_values,        # Grilla de valores de penalización
  metrics = metric_set(mae)  # metrica
)

set.seed(86936)
tune_full2 <- tune_grid(
  workflow_full2,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  grid = grid_values,        # Grilla de valores de penalización
  metrics = metric_set(mae)  # metrica
)

best_tune_full <- select_best(tune_full, metric = "mae")
best_tune_full1 <- select_best(tune_full1, metric = "mae")
best_tune_full2 <- select_best(tune_full1, metric = "mae")

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
full_final <- finalize_workflow(workflow_full, best_tune_full)
full_final_fit <- fit(full_final, data = train_full)

full_final1 <- finalize_workflow(workflow_full1, best_tune_full1)
full_final_fit1 <- fit(full_final1, data = train_full)

full_final2 <- finalize_workflow(workflow_full2, best_tune_full2)
full_final_fit2 <- fit(full_final2, data = train_full)

augment(full_final_fit, new_data = train_full) %>%
  mae(truth = price, estimate = .pred) #predicción en train: mae = 180012424

#predicción en test (modelo1)
predicted <- augment(full_final_fit, new_data = test_full)
model1_elastic_net_text_regressors <- predicted[,c("property_id",".pred")]
colnames(model1_elastic_net_text_regressors) <- c("property_id","price")
write.csv(model1_elastic_net_text_regressors,"model1_elastic_net_text_regressors.csv",row.names = F)
#puntaje Kaggle: 283469139.92881

#sacar exp 
pred_train1 <- augment(full_final_fit1, new_data = train_full)
pred_train1$price1 <- round(exp(pred_train1$.pred),-6)
mae(data = pred_train1,truth = price,estimate = price1) #mae = 179321902

#predicción en test (modelo2)
predicted1 <- augment(full_final_fit1, new_data = test_full)
predicted1$price1 <- round(exp(predicted1$.pred),-6)
model2_elastic_net_text_regressors_logprice <- predicted1[,c("property_id","price1")]
colnames(model2_elastic_net_text_regressors_logprice) <- c("property_id","price")
write.csv(model2_elastic_net_text_regressors_logprice,"model2_elastic_net_text_regressors_logprice.csv",row.names = F)
#puntaje Kaggle: 317925082.55955

augment(full_final_fit2, new_data = train_full) %>%
  mae(truth = price, estimate = .pred) #predicción en train: mae = 654534655

#predicción en test (modelo3)
predicted2 <- augment(full_final_fit2, new_data = test_full)
model3_elastic_net_text_regressors <- predicted2[,c("property_id",".pred")]
colnames(model3_elastic_net_text_regressors) <- c("property_id","price")
write.csv(model3_elastic_net_text_regressors,"model3_elastic_net_text_regressors.csv",row.names = F)
#puntaje Kaggle: 873559529,54767

#modelo 4 - regresión lineal ---------------------------------------------------
model1 <- lm(price~surface_total3+surface_covered3+rooms3+
               bathrooms3+estrato+n_pisos_numerico+zona_t_g+Dist_pol+
               dist_parque+lat+lon+localidad,data=train)
train$y_pred <- predict(model1,newdata = train)
test$y_pred <- predict(model1,newdata = test)
mae(data = train,truth = price, estimate = y_pred) #predicción en train: mae = 164128735
model4_linear_regression <- test[,c("property_id","y_pred")]
colnames(model4_linear_regression) <- c("property_id","price")
write.csv(model4_linear_regression,"model4_linear_regression.csv",row.names = F)

model4 <- lm(price~surface_covered3+bedrooms+
               bathrooms3+estrato+n_pisos_numerico+zona_t_g+Dist_pol+
               dist_parque+lat+lon+localidad,data=train)
train$y_pred <- predict(model4,newdata = train)
test$y_pred <- predict(model4,newdata = test)
mae(data = train,truth = price, estimate = y_pred) #predicción en train: mae = 186363914
model5_linear_regression <- test[,c("property_id","y_pred")]
colnames(model5_linear_regression) <- c("property_id","price")
write.csv(model5_linear_regression,"model4_linear_regression.csv",row.names = F)


#Boosting ----------------------------------------------------------------------

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

fitControl <- trainControl(method ="cv",number=5)
grid_xbgoost <- expand.grid(nrounds = c(500),
                            max_depth = c(4), 
                            eta = c(0.25,0.5), 
                            gamma = c(0), 
                            min_child_weight = c(50),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4))

#xgboost con variables y componentes principales
set.seed(6392)
model1 <- train(price~.,
                data=train2[-1], #excluye variable de property_id
                method = "xgbTree", 
                trControl = fitControl,
                tuneGrid=grid_xbgoost)        
model1

train2 <- train2 %>% 
  mutate(price_pred = predict(model1, newdata = train2))  
mae(truth = price, estimate = price_pred, data = train2) #predicción en train: mae = 112882917

xgboost1 <- test2 %>% 
  mutate(price = predict(model1, newdata = test2)) %>% 
  select(property_id,price) 

write.csv(xgboost1,"xgboost1.csv",row.names = F) #Puntaje Kaggle: 289553937

#xgboost con variables
set.seed(6392)
model2 <- train(price~.,
                data=train1[-1], #excluye variable de property_id
                method = "xgbTree", 
                trControl = fitControl,
                tuneGrid=grid_xbgoost)        
model2

train1 <- train1 %>% 
  mutate(price_pred = predict(model2, newdata = train1))  
mae(truth = price, estimate = price_pred, data = train1) #predicción en train: mae = 124417720

xgboost2 <- test1 %>% 
  mutate(price = predict(model1, newdata = test1)) %>% 
  select(property_id,price) 

write.csv(xgboost2,"xgboost2.csv",row.names = F)

#xgboost con 10 componentes principales
set.seed(6392)
model3 <- train(price~.,
                data=train3[-ncol(train3)], #excluye variable de property_id
                method = "xgbTree", 
                trControl = fitControl,
                tuneGrid=grid_xbgoost)        
model3

train3 <- train3 %>% 
  mutate(price_pred = predict(model3, newdata = train3))  
mae(truth = price, estimate = price_pred, data = train3) #predicción en train: mae = 131904294

xgboost3 <- test3 %>% 
  mutate(price = predict(model3, newdata = test3)) %>% 
  select(property_id,price) 

write.csv(xgboost3,"xgboost3.csv",row.names = F) 

