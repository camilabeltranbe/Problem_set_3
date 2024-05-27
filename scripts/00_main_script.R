#Main Script
#Este código contiene todos los scripts utilizados para el problem set 3


#se borra la memoria
rm(list = ls())
#se cargan los paquetes
library(pacman)
p_load(
  rio, # importación/exportación de datos
  tidyverse, # datos ordenados (ggplot y Tidyverse)
  skimr, # datos de resumen
  visdat, # visualización de datos faltantes
  corrplot, # gráficos de correlación
  stargazer, # tablas/salida a TEX.
  rvest, # web-scraping
  readxl, # importar Excel
  writexl, # exportar Excel
  boot, # bootstrapping
  ggpubr, # extensiones de ggplot2
  WVPlots, # gráficos de variables ponderadas
  patchwork, # para combinar gráficos
  gridExtra, # para combinar gráficos
  ggplot2, # gráficos
  caret, # para evaluación de modelos predictivos
  data.table, # para manipulación de datos
  glmnet, # glmnet
  smotefamily, # remuestreo SMOTE
  dplyr, # dplyr
  dummy, # crear dummys
  Metrics, # evaluation metrics for ML
  MLeval, # Machine Learning Model Evaluation
  pROC, # pROC
  ROSE, # remuestreo ROSE
  ranger, # random forest
  xgboost, # xgboosting
  tm, # para Text Mining
  tidytext, # Para tokenización
  stopwords, # consultar stopwords
  tidymodels, # para modelos de ML
  sf, # Leer/escribir/manipular datos espaciales
  spatialsample, # validación cruzada espacial
  adabag, # adaboosting
  nnet, # redes neuronales de una sola capa
  keras, # keras
  tensorflow, # tensorflow
  recipes, # recipes
  plotly, # Gráficos interactivos
  leaflet, # Mapas interactivos
  tmaptools, # geocode_OSM()
  osmdata, # Get OSM's data 
  modeest, # Mode estimation
  blockCV, # Spatial cross-validation
  terra, # Spatial data handling
  rpart, # Recursive Partition and Regression Trees (To run Trees)
  rpart.plot, # for trees graphs
  ipred, # For Bagging 
  DiagrammeR, # DiagrammeR
  ISLR2 # ISLR2
  )

#se define la ruta de trabajo
ifelse(grepl("camilabeltran", getwd()),
       wd <- "/Users/camilabeltran/OneDrive/Educación/PEG - UniAndes/BDML/Problem_set_3_BDML",
       ifelse(grepl("Juan",getwd()),
              wd <- "C:/Users/Juan/Documents/Problem_set_3",
              ifelse(grepl("juanp.rodriguez",getwd()),
                     wd <- "C:/Users/juanp.rodriguez/Documents/GitHub/Problem_set_3",
                     ifelse(grepl("C:/Users/User",getwd()),
                            wd <- "C:/Users/User/OneDrive - Universidad de los andes/Big Data y Machine Learning/Problem_set_3/Problem_set_3",
                            ifelse(grepl("/Users/aleja/",getwd()),
                                   wd <- "/Users/aleja/Documents/Maestría Uniandes/Clases/Big Data y Machine Learning/Repositorios Git Hub/Problem_set3)",
                                   wd <- "otro_directorio")))))

#INSERTE SU RUTA AQUÍ:
#wd<-("")


#Script: "01_data.R". Realiza el proceso de limpieza, estdísticas descriptivas y se establece la base de datos final.
setwd(paste0(wd,"/scripts"))
source("01_data.R")

#Script: "02_Pruebas_Camila.R". Modelos de predicción de precios de vivienda
setwd(paste0(wd,"/scripts"))
source("02_Pruebas_Camila.R")

#Script: "03_Pruebas_Camila.R". Modelos de predicción de precios de vivienda
setwd(paste0(wd,"/scripts"))
source("03_Pruebas_Julian.R")

#Script: "04_Pruebas_Julian.R". Modelos de predicción de precios de vivienda
setwd(paste0(wd,"/scripts"))
source("04_Pruebas_Alejandra.R")

#Script: "05_Pruebas_Juan.R". Predicciones de Clasificación directa e indirecta
setwd(paste0(wd,"/scripts"))
source("05_Pruebas_Juan.R")
