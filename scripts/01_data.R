#####Datos - problem set 3######

wd <- setwd("C:/Users/camib/OneDrive/Educación/PEG - UniAndes/BDML/Problem_set_3")

#se define la ruta
setwd(paste0(wd,"/stores"))

# se cargan las bases de datos --------------
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#dimensiones
dim(train) #386644 obs y 16 variables
dim(test) #10286 y 16 variables (var de precio está en NA)

#variables
colnames(train)
colnames(test)

# missing values ----------------------------
