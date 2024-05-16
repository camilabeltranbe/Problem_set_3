
require(pacman)
p_load(rio, ## read datasets
       tidyverse, # Manipular dataframes
       tm,   # para Text Mining
       tidytext, #Para tokenización
       stopwords,  # consultar stopwords
       tidymodels,
       sf,
       spatialsample)

wd <- ("/Users/camilabeltran/OneDrive/Educación/PEG - UniAndes/BDML/Problem_set_3")
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

#modelo 1 - Elastic Net - variables importantes, texto como regresores y componentes principales de texto --------------------------
bow_vars_train <- des_train %>%
  select(-property_id) %>%
  colnames()

pc_vars_train <- zdes_train %>%
  select(-property_id) %>%
  colnames()

full_formula<- as.formula(
  paste("price ~ property_type + rooms3 + bathrooms3 + surface_total3 +
          surface_covered3 + n_pisos_numerico + zona_t_g ",
        paste(pc_vars_train, collapse = "+"), 
        paste(bow_vars_train, collapse = "+"),  sep = "+"))

# elastic net
elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

grid_values <- grid_regular(penalty(range = c(-1,2)), levels = 50) %>% # penalidad va de 10^-1 a 10^2 
  expand_grid(mixture = c(0,  0.5,  1))

rec_full <- recipe(full_formula, data = train_full) %>%
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores. 

workflow_full <- workflow() %>% 
  add_recipe(rec_full) %>%
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

best_tune_full <- select_best(tune_full, metric = "mae")

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
full_final <- finalize_workflow(workflow_full, best_tune_full)
full_final_fit <- fit(full_final, data = train_full)

augment(full_final_fit, new_data = train_full) %>%
  mae(truth = price, estimate = .pred)

#predicción en train: mae = 180012424

#predicción en test
predicted <- augment(full_final_fit, new_data = test_full)
model1_elastic_net_text_regressors <- predicted[,c("property_id",".pred")]
colnames(model1_elastic_net_text_regressors) <- c("property_id","price")
write.csv(model1_elastic_net_text_regressors,"model1_elastic_net_text_regressors.csv",row.names = F)
