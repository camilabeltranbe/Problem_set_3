#####Datos - problem set 3######
rm(list = ls())
#install.packages("pacman")
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
       visdat,#gráfica para missings
       ggplot2,
       stargazer,
       modeest)  

#wd <- "C:/Users/camib/OneDrive/Educación/PEG - UniAndes/BDML/Problem_set_3"
#wd <-("C:/Users/User/OneDrive - Universidad de los andes/Big Data y Machine Learning/Problem_set_3")
wd <- ("/Users/camilabeltran/OneDrive/Educación/PEG - UniAndes/BDML/Problem_set_3")
#wd <- ("C:/Users/Juan/Documents/Problem_set_3")
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

# missing values ---------------------------------------------------------------
vis_dat(train)
vis_dat(test)
# hay NA en "surface_total", "surface_covered","rooms","bathrooms"

# imputación de datos ----------------------------------------------------------
# rooms, surface_total y surface_covered
train <- train %>%
  group_by(property_type) %>%
  mutate(rooms2 = ifelse(is.na(rooms), mlv(rooms,na.rm = T), rooms),
         bathrooms2 = ifelse(is.na(bathrooms), mlv(rooms,na.rm = T), bathrooms), #moda de cuartos
         surface_total2 = ifelse(is.na(surface_total), mean(surface_total,na.rm = T), surface_total), #media de superficie total
         surface_covered2 = ifelse(is.na(surface_covered), mean(surface_covered,na.rm = T), surface_covered)) #media de superficie cubierta

test <- test %>%
  group_by(property_type) %>%
  mutate(rooms2 = ifelse(is.na(rooms), mlv(rooms,na.rm = T), rooms),
         bathrooms2 = ifelse(is.na(bathrooms), mlv(rooms,na.rm = T), bathrooms), #moda de cuartos
         surface_total2 = ifelse(is.na(surface_total), mean(surface_total,na.rm = T), surface_total), #media de superficie total
         surface_covered2 = ifelse(is.na(surface_covered), mean(surface_covered,na.rm = T), surface_covered)) #media de superficie cubierta

# creación, modificación de variables ------------------------------------------
## Transformacion Log(Precio)
train$Log_Precio <- log(train$price)

# precio x metro cuadrado
train <- train %>%
  mutate(precio_mt2 = round(price/surface_total2,0))%>%
  mutate(precio_mt2 = precio_mt2/1000000)  # precio x Mt2 en millones
summary(train$precio_mt2)

# estadísticas descriptivas ----------------------------------------------------
stargazer(as.data.frame(train),type="text")
stargazer(as.data.frame(test),type="text")

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

#Delimitando los datos a solamente chapinero (JULIAN SUJETO A REVISION)---------------------
#Problema que se indetifico: existian datos outliers como aptos y casas en Suba,
#que no corresponden a predecir chapinero
#lim_chapinero<- getbb("Chapinero, Bogotá, Colombia")
#lim_chapinero
#train <- train %>%
#  filter(
#   between(lon, lim_chapinero[1, "min"], lim_chapinero[1, "max"]) & 
#     between(lat, lim_chapinero[2, "min"], lim_chapinero[2, "max"])
# )
#test <- test %>%
#  filter(
#   between(lon, lim_chapinero[1, "min"], lim_chapinero[1, "max"]) & 
#     between(lat, lim_chapinero[2, "min"], lim_chapinero[2, "max"])
# )

# georeferencia x localidad ----------------------------------------------------
# fuente: https://bogota-laburbano.opendatasoft.com/explore/dataset/poligonos-localidades/export/
localidades <- st_read("poligonos-localidades.geojson")
localidades <- subset(localidades, !(Nombre.de.la.localidad == "SUMAPAZ")) #quitar Sumapaz
localidades <- st_transform(localidades,4626)
sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4626)
sf_test<- st_as_sf(test, coords = c("lon", "lat"),  crs = 4626)
# Realizar la unión espacial basada en la proximidad de coordenadas
sf_train <- st_join(sf_train, localidades, join = st_intersects)
sf_test <- st_join(sf_test, localidades, join = st_intersects)
# Agregar a train y test tambien 
train$localidad <- sf_train$Nombre.de.la.localidad
test$localidad <- sf_test$Nombre.de.la.localidad

# gráficas de ubicación geográfica x localidad ---------------------------------
# (train)
ggplot()+
  geom_sf(data=localidades, color = "darkred") + 
  geom_sf(data=sf_train,shape=15, size=0.3,aes(color= precio_mt2)) + 
  theme_bw()

# (test)
ggplot()+
  geom_sf(data=localidades, color = "darkred") + 
  geom_sf(data=sf_test,shape=15, size=0.3,color="darkblue") +
  theme_bw()

#gráficas de ubicación geográfica en chapinero x tipo apto o casa---------------
#CAMBIEN LOS COLORES. SOY TERRIBLE CON LOS COLORES- by JULIAN
ggplot() +
  geom_sf(data = localidades, color = "darkgrey") + 
  geom_sf(data = sf_train %>% filter(property_type == "Apartamento"), aes(color = "Apartamento"), shape = 15, size = 0.3) +
  geom_sf(data = sf_train %>% filter(property_type == "Casa"), aes(color = "Casa"), shape = 15, size = 0.3) +
  scale_color_manual(name = "Tipo de Propiedad", 
                     values = c(Apartamento = "red", Casa = "darkblue")) +
  labs(x = "Longitud", y = "Latitud")+
    theme_bw()

ggplot() +
  geom_sf(data = localidades, color = "darkgrey") + 
  geom_sf(data = sf_test %>% filter(property_type == "Apartamento"), aes(color = "Apartamento"), shape = 15, size = 0.3) +
  geom_sf(data = sf_test %>% filter(property_type == "Casa"), aes(color = "Casa"), shape = 15, size = 0.3) +
  scale_color_manual(name = "Tipo de Propiedad", 
                     values = c(Apartamento = "red", Casa = "darkblue")) +
  labs(x = "Longitud", y = "Latitud")+
  theme_bw()

# a. Importar los datos de bogota
p_load(tidyverse, sf, tmaptools) 

print(available_features()) # para ver todas las categorias

bogota<-opq(bbox = getbb("Bogotá Colombia"))
bogota

# b. Obtenenemos las estaciones de policia
police <- bogota %>% 
  add_osm_feature(key="amenity",value="police") %>% #amenities disponibles
  osmdata_sf() #transformamos a un objeto sf

# Centroides de los puntos 
puntos_police<-police$osm_point
head(puntos_police)

# Situar los datos en la misma proyeccion

#  Policia
police <- st_transform(police$osm_points, 4686)
#  Transformación de Train
train_st<-st_as_sf(train, coords=c('lon','lat'),crs=4326)
train_st<-st_transform(train_st,4686)
st_crs(train_st)

# calculo distancia
train$Dist_pol <- st_distance(train_st, police)

# La distancia mas cercana 
train$Dist_pol <- apply(train$Dist_pol, 1, min)

#Adicionando las 4 variables  de OPEN STREET MAPS------------------------------
# Obtener las etiquetas disponibles para el ocio
datos_osm <- available_tags("leisure")

# Extraemos la info de todos los parques de chapinero en bogotá
parques <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 
# Cambiamos el formato para que sea un objeto sf (simple features)
parques_sf <- osmdata_sf(parques)

# De las features del parque nos interesa su geomoetría y donde estan ubicados 
parques_geometria <- parques_sf$osm_polygons %>% 
  dplyr::select(osm_id, name) 
# Guardemos los poligonos de los parques 
parques_geometria <- st_as_sf(parques_sf$osm_polygons)
# Calculamos el centroide de cada parque para aproximar su ubciacion como un solo punto 
centroides <- st_centroid(parques_geometria, byid = T)
centroides <- centroides %>%
  mutate(x=st_coordinates(centroides)[, "X"]) %>%
  mutate(y=st_coordinates(centroides)[, "Y"]) 

centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
#modificacion de la codificciòn de sf_train
sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)

dist_matrix <- st_distance(x = sf_train, y = centroides_sf)
dim(dist_matrix)
dist_min <- apply(dist_matrix, 1, min)  
#Vamos a tomar una muestra dada la cantidad de los datos 
ggplot(sf_train%>%sample_n(1000), aes(x = distancia_parque, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros (log-scale)", 
       y = "Valor de venta  (log-scale)",
       title = "Relación entre la proximidad a un parque y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()

# Ahora vamos a evaluar si el tamaño del parque más cercano influye
posicion <- apply(dist_matrix, 1, function(x) which(min(x) == x))
# De la geometria de los parques extraemos el área
areas <- st_area(parques_geometria)
#Agregamos la variable a nuestra base de datos original
sf_train <- sf_train %>%
  mutate(area_parque = as.numeric(areas[posicion]))

# Ploteamos la relación 
ggplot(sf_train%>%sample_n(1000), aes(x = area_parque, y = precio_mt2)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Área del parque más cercano (log-scale)", 
       y = "Valor del arriendo (log-scale)",
       title = "Relación entre área de un parque y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)

# Datos abiertos Bogotá --------------------------------------------------------
barrios <- st_read("SECTOR.GEOJSON")
barrios <- st_transform(barrios,4626)
sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4626)
sf_test<- st_as_sf(test, coords = c("lon", "lat"),  crs = 4626)

# Verificar y corregir geometrías inválidas
if (!all(st_is_valid(barrios))) {
  barrios <- st_make_valid(barrios)}

# Realizar la unión espacial basada en la proximidad de coordenadas
sf_train <- st_join(sf_train, barrios, join = st_intersects)
sf_test <- st_join(sf_test, barrios, join = st_intersects)
# Agregar a train y test tambien 
train$barrio <- sf_train$SCANOMBRE
test$barrio <- sf_test$SCANOMBRE
