#####Datos - problem set 3######

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
vis_dat(train) +
  scale_fill_manual(values = c(
    "character" = "thistle1",
    "integer" = "plum",
    "numeric" = "mediumpurple",
    "NA" = "azure3"
  ))
vis_dat(test)
# hay NA en "surface_total", "surface_covered","rooms","bathrooms"
sum(is.na(train$surface_total)) #30.790
sum(is.na(train$surface_covered)) #30.079
sum(is.na(train$rooms)) #18.260
sum(is.na(train$bathrooms)) #10.071

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

train <- as.data.frame(train)
test <- as.data.frame(test)
# creación, modificación de variables ------------------------------------------
## Transformacion Log(Precio)
train$Log_Precio <- log(train$price)

# precio x metro cuadrado
train <- train %>%
  mutate(precio_mt2 = round(price/surface_total2,0))%>%
  mutate(precio_mt2 = precio_mt2/1000000)  # precio x Mt2 en millones
summary(train$precio_mt2)

# estadísticas descriptivas ----------------------------------------------------
stargazer(train)
stargazer(tr,type="text")


# Histograma ----------------------------------------------------
hist((train$price/1000000), probability = TRUE, col = 'cadetblue4', border = "white", breaks = 25)
abline(v = mean((train$price/1000000)), col='red', lwd = 3)
lines(density((train$price/1000000)), col = 'cadetblue1', lwd = 3)

media_apto <- mean((train$price[train$property_type == "Apartamento"])/1000000)
media_casa <- mean((train$price[train$property_type == "Casa"])/1000000)
mean(train$surface_total[train$property_type == "Apartamento"], na.rm = TRUE)
mean(train$surface_total[train$property_type == "Casa"], na.rm = TRUE)

(media_casa-media_apto)/media_apto

ggplot(train, aes(x = (price/1000000), fill = property_type)) +
  geom_histogram(color="white", alpha = 0.6, position = 'identity', binwidth = 50) +
  geom_vline(aes(xintercept = media_apto), color = "darkgreen", linetype = "dashed")+
  geom_vline(aes(xintercept = media_casa), color = "darkorange3", linetype = "dashed")+
  labs(x = 'Precio de Viviendas ($ - Millones)', y ="Frecuencia")+
  scale_fill_manual(values=c("cadetblue4", "darkorange"))+
  guides(fill = guide_legend(title = "Inmueble"))+
  facet_wrap(~property_type)

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

# delimitando los datos a solamente chapinero (JULIAN SUJETO A REVISION)---------------------
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
# Agregar variable a train y test  
train$localidad <- sf_train$Nombre.de.la.localidad
test$localidad <- sf_test$Nombre.de.la.localidad

# gráficas de ubicación geográfica x localidad ---------------------------------
# (train)
ggplot()+
  geom_sf(data=localidades, color = "darkgrey") + 
  geom_sf(data=sf_train,shape=15, size=0.3,aes(color= precio_mt2)) + 
  theme_bw()

# (test)
ggplot()+
  geom_sf(data=localidades, color = "darkgrey") + 
  geom_sf(data=sf_test,shape=15, size=0.3,color="cadetblue4") +
  theme_bw()

# gráficas de ubicación geográfica en chapinero x tipo apto o casa---------------
#CAMBIEN LOS COLORES. SOY TERRIBLE CON LOS COLORES- by JULIAN
ggplot() +
  geom_sf(data = localidades, color = "darkgrey") + 
  geom_sf(data = sf_train %>% filter(property_type == "Apartamento"), aes(color = "Apartamento"), shape = 15, size = 0.3) +
  geom_sf(data = sf_train %>% filter(property_type == "Casa"), aes(color = "Casa"), shape = 15, size = 0.3) +
  scale_color_manual(name = "Tipo de Propiedad", 
                     values = c(Apartamento = "cadetblue4", Casa = "darkorange")) +
  labs(x = "Longitud", y = "Latitud")+
    theme_bw()

ggplot() +
  geom_sf(data = localidades, color = "darkgrey") + 
  geom_sf(data = sf_test %>% filter(property_type == "Apartamento"), aes(color = "Apartamento"), shape = 15, size = 0.3) +
  geom_sf(data = sf_test %>% filter(property_type == "Casa"), aes(color = "Casa"), shape = 15, size = 0.3) +
  scale_color_manual(name = "Tipo de Propiedad", 
                     values = c(Apartamento = "cadetblue4", Casa = "darkorange")) +
  labs(x = "Longitud", y = "Latitud")+
  theme_bw()

# variable de estacion de policia -----------------------------------------------
# a. Importar los datos de bogota

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
police <- st_transform(police$osm_points, 4326)
#  Transformación de Train
train_st<-st_as_sf(train, coords=c('lon','lat'),crs=4326)
train_st<-st_transform(train_st,4326)
st_crs(train_st)

test_st<-st_as_sf(test, coords=c('lon','lat'),crs=4326)
test_st<-st_transform(test_st,4326)
st_crs(test_st)

# calculo distancia
test$Dist_pol <- st_distance(test_st, police)
train$Dist_pol <- st_distance(train_st, police)


# La distancia mas cercana 
train$Dist_pol <- apply(train$Dist_pol, 1, min)
test$Dist_pol <- apply(test$Dist_pol, 1, min)
# adicionando las 4 variables  de OPEN STREET MAPS------------------------------
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

# datos abiertos Bogotá --------------------------------------------------------

#variable de barrio 
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
# Agregar variable a train y test  
train$barrio <- sf_train$SCANOMBRE
test$barrio <- sf_test$SCANOMBRE

# nuevas variables de rooms, surface_total y surface_covered (usando barrio) ----
train <- train %>%
  group_by(property_type,barrio) %>%
  mutate(rooms3 = ifelse(is.na(rooms), mlv(rooms,na.rm = T), rooms),
         bathrooms3 = ifelse(is.na(bathrooms), mlv(rooms,na.rm = T), bathrooms), #moda de cuartos
         surface_total3 = ifelse(is.na(surface_total), mean(surface_total,na.rm = T), surface_total), #media de superficie total
         surface_covered3 = ifelse(is.na(surface_covered), mean(surface_covered,na.rm = T), surface_covered)) #media de superficie cubierta

test <- test %>%
  group_by(property_type,barrio) %>%
  mutate(rooms3 = ifelse(is.na(rooms), mlv(rooms,na.rm = T), rooms),
         bathrooms3 = ifelse(is.na(bathrooms), mlv(rooms,na.rm = T), bathrooms), #moda de cuartos
         surface_total3 = ifelse(is.na(surface_total), mean(surface_total,na.rm = T), surface_total), #media de superficie total
         surface_covered3 = ifelse(is.na(surface_covered), mean(surface_covered,na.rm = T), surface_covered)) #media de superficie cubierta

# Quedan unos poquitos NA cuando se agrupa por barrio entonces a ese le pongo el de la imputación inicial que solo agrupaba por tipo de propiedad
train <- train %>% 
  mutate(rooms3=ifelse(is.na(rooms3),rooms2,rooms3),
         bathrooms3=ifelse(is.na(bathrooms3),bathrooms2,bathrooms3),
         surface_total3=ifelse(is.na(surface_total3),surface_total2,surface_total3),
         surface_covered3=ifelse(is.na(surface_covered3),surface_covered2,surface_covered3))
test <- test %>% 
  mutate(rooms3=ifelse(is.na(rooms3),rooms2,rooms3),
         bathrooms3=ifelse(is.na(bathrooms3),bathrooms2,bathrooms3),
         surface_total3=ifelse(is.na(surface_total3),surface_total2,surface_total3),
         surface_covered3=ifelse(is.na(surface_covered3),surface_covered2,surface_covered3))

train <- as.data.frame(train)
test <- as.data.frame(test)

# variable de texto (no. pisos)-------------------------------------------------
train <- train %>%
  mutate(n_pisos = str_extract(description, "(\\w+|\\d+) pisos"))

test <- test %>%
  mutate(n_pisos = str_extract(description, "(\\w+|\\d+) pisos"))

numeros_escritos <- c( "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", 
                       "diez","once","doce","trece","catorce","quince")
numeros_numericos <- as.character(2:15)

train <- train %>%
  mutate(n_pisos = str_replace_all(n_pisos, setNames(numeros_numericos,numeros_escritos)))

test <- test %>%
  mutate(n_pisos = str_replace_all(n_pisos, setNames(numeros_numericos,numeros_escritos)))

train <- train %>%
  mutate(n_pisos_numerico = as.integer(str_extract(n_pisos, "\\d+")))  %>%
  mutate(n_pisos_numerico = if_else(is.na(n_pisos_numerico), 1, n_pisos_numerico)) %>%
  mutate(n_pisos_numerico = if_else(n_pisos_numerico>15, 1, n_pisos_numerico)) ### quedarnos propiedades de hasta 10 pisos. 

test <- test %>%
  mutate(n_pisos_numerico = as.integer(str_extract(n_pisos, "\\d+")))  %>%
  mutate(n_pisos_numerico = if_else(is.na(n_pisos_numerico), 1, n_pisos_numerico)) %>%
  mutate(n_pisos_numerico = if_else(n_pisos_numerico>15, 1, n_pisos_numerico)) ### quedarnos propiedades de hasta 10 pisos. 

# variable de texto (zona T o zona G) ------------------------------------------
train <- train %>%
  mutate(zonat_g = if_else(grepl("\\b(zona t|zona g)\\b", description, ignore.case = TRUE), 1, 0))
test <- test %>%
  mutate(zonat_g = if_else(grepl("\\b(zona t|zona g)\\b", description, ignore.case = TRUE), 1, 0))
