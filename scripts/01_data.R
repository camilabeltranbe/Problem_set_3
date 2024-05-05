#####Datos - problem set 3######
rm(list = ls())
install.packages("pacman")
require(pacman)
# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Get OSM's data 
       tidymodels,#para modelos de ML
       visdat) #gráfica para missings 

#wd <- "C:/Users/camib/OneDrive/Educación/PEG - UniAndes/BDML/Problem_set_3"
wd <-("C:/Users/User/OneDrive - Universidad de los andes/Big Data y Machine Learning/Problem_set_3")
wd <- ("/Users/camilabeltran/OneDrive/Educación/PEG - UniAndes/BDML/Problem_set_3")

#se define la ruta
setwd(paste0(wd,"/stores"))

# se cargan las bases de datos -------------------------------------------------
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#dimensiones
dim(train) #386644 obs y 16 variables
dim(test) #10286 y 16 variables (var de precio está en NA)

#variables
colnames(train)
colnames(test)

# datos geoespaciales ----------------------------------------------------------

# primera visualización de datos (train)
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon, 
             lat = train$lat)

# primera visualización de datos (test)
leaflet() %>%
  addTiles() %>%
  addCircles(lng = test$lon, 
             lat = test$lat)

# missing values ---------------------------------------------------------------
vis_dat(train)
vis_dat(test)
# hay NA en "surface_total", "surface_covered","rooms","bathrooms"

# asignar los NA de "rooms" como la suma de baños y habitaciones - test
test$rooms2 <- rowSums(test[c("bedrooms","bathrooms")],na.rm=T)
test$rooms <- ifelse(is.na(test$rooms),test$rooms2,test$rooms)
test$rooms <- ifelse(test$rooms==0,NA,test$rooms) # poner NA cuando da como resultado 0 habitaciones

# asignar los NA de "rooms" como la suma de baños y habitaciones - train
train$rooms2 <- rowSums(train[c("bedrooms","bathrooms")],na.rm=T)
train$rooms <- ifelse(is.na(train$rooms),train$rooms2,train$rooms)
train$rooms <- ifelse(train$rooms==0,NA,train$rooms) # poner NA cuando da como resultado 0 habitaciones

#D|elimitando los datos a solamente chapinero (JULIAN SUJETO A REVISION)---------------------
#Problema que se indetifico: existian datos outliers como aptos y casas en Suba,
#que no corresponden a predecir chapinero
lim_chapinero<- getbb("Chapinero, Bogotá, Colombia")
lim_chapinero
train <- train %>%
  filter(
    between(lon, lim_chapinero[1, "min"], lim_chapinero[1, "max"]) & 
      between(lat, lim_chapinero[2, "min"], lim_chapinero[2, "max"])
  )
test <- test %>%
  filter(
    between(lon, lim_chapinero[1, "min"], lim_chapinero[1, "max"]) & 
      between(lat, lim_chapinero[2, "min"], lim_chapinero[2, "max"])
  )


# georeferencia x localidad ----------------------------------------------------
# fuente: https://bogota-laburbano.opendatasoft.com/explore/dataset/poligonos-localidades/export/
localidades <- st_read("poligonos-localidades.geojson")
localidades <- subset(localidades, !(Nombre.de.la.localidad == "SUMAPAZ")) #quitar Sumapaz
localidades <- st_transform(localidades,4626)
sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4626)
sf_test<- st_as_sf(test, coords = c("lon", "lat"),  crs = 4626)

# imputación de datos ----------------------------------------------------------

# creación, modificación de variables ------------------------------------------

# hay q correr esto cuando imputemos datos de área
train <- train %>%
  mutate(precio_mt2 = round(price/surface_total, 0))%>%
  mutate(precio_mt2 = precio_mt2/1000000 )  # precio x Mt2 en millones

# quitar outliers
# filtramos ese outlier que resulta no ser real
train <- train %>%
  filter(between(precio_mt2, 0.10,  30))

# gráficas de ubicación geográfica x localidad ---------------------------------
# (train)
ggplot()+
  geom_sf(data=localidades, color = "darkred") + 
  geom_sf(data=sf_train,shape=15, size=0.3
          ,aes(color= precio_mt2)) + 
  theme_bw()

# (test)
ggplot()+
  geom_sf(data=localidades, color = "darkred") + 
  geom_sf(data=sf_test,shape=15, size=0.3,color="darkblue") +
  theme_bw()


# estadísticas descriptivas ----------------------------------------------------
stargazer(train,type="text")
stargazer(test,type="text")
