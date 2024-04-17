#Main Script
#Este código contiene todos los scripts utilizados para el problem set 2


#se borra la memoria
rm(list = ls())
#se cargan los paquetes
library(pacman)
p_load(rio, # importación/exportación de datos
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
       data.table,# para manipulación de datos
       glmnet,
       caret,
       smotefamily, #remuestreo SMOTE
       dplyr,
       dummy, #crear dummys
       Metrics, #evaluation metrics for ML
       MLeval, #Machine Learning Model Evaluation
       pROC,
       ROSE, #remuestreo ROSE
       ranger,#random forest
       xgboost) #xgboosting

#se define la ruta de trabajo
ifelse(grepl("camilabeltran", getwd()),
       wd <- "/Users/camilabeltran/OneDrive/Educación/PEG - UniAndes/BDML/Problem_set_2_BDML",
       ifelse(grepl("Juan",getwd()),
              wd <- "C:/Users/Juan/Documents/Problem_set_1",
              ifelse(grepl("juanp.rodriguez",getwd()),
                     wd <- "C:/Users/juanp.rodriguez/Documents/GitHub/Problem_set_1",
                     ifelse(grepl("C:/Users/User",getwd()),
                            wd <- "C:/Users/User/OneDrive - Universidad de los andes/Big Data y Machine Learning/Problem_set_1/Problem_set_1",
                            ifelse(grepl("/Users/aleja/",getwd()),
                                   wd <- "/Users/aleja/Documents/Maestría Uniandes/Clases/Big Data y Machine Learning/Repositorios Git Hub/Problem_set_1)",
                                   wd <- "otro_directorio")))))

#INSERTE SU RUTA AQUÍ:
wd<-("C:/Users/user/OneDrive - Universidad de los andes/Big Data y Machine Learning/Problem_set_2_BDML")


#Script: "01_Data.R". Realiza el proceso de limipeza y unión de bases de datos.
setwd(paste0(wd,"/scripts"))
source("01_Data.R")

#Script: "02_Estadística descriptica.R". Realiza gráficos de las relaciones de las variables relevantes dentro de la muestra
setwd(paste0(wd,"/scripts"))
source("02_data.R")

#Script: "03_Pruebas_Camila.R". Predicciones de Clasificación directa e indirecta
setwd(paste0(wd,"/scripts"))
source("03_Pruebas_Camila.R")

#Script: "04_Pruebas_Julian.R". Predicciones de Clasificación directa e indirecta
setwd(paste0(wd,"/scripts"))
source("04_Pruebas_Julian.R")


#Script: "05_Pruebas_Alejandra.R". Predicciones de Clasificación directa e indirecta
setwd(paste0(wd,"/scripts"))
source("05_Pruebas_Alejandra.R")

#Script: "06_Pruebas_Jun_Pablo.R". Predicciones de Clasificación directa e indirecta
setwd(paste0(wd,"/scripts"))
source("06_Pruebas_Juan_Pablo.R")
