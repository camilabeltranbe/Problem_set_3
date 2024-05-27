# Problem Set 3: Big Data y Machine Learning para Economía Aplicada
***26 de mayo de 2024***  
***Universidad de los Andes***  
***Camila Beltran (cod. 202213110)***  
***Julian Pulido (cod. 201814833)***  
***Alejandra Guevara (cod. 202324570)***  
***Juan Pablo Rodríguez (cod. 201715671)***  

Este repositorio almacena toda la información utilizada para la solución del problem set 3 de la materia de Big Data y Machine Learning para Economía Aplicada. La información se encuentra separada en cuatro carpetas:

* `document`: En esta carpeta está el documento final en  _.pdf_, dado que el trabajo se realizó en grupo, decidimos utilizar el compilador de texto en línea _overleaf_. Por lo tanto, este repositorio no permite la reproducibilidad del documento final. 

* `scripts`: En esta carpeta están los _scripts_ utilizados para generar los resultados. 

  * `00_main_script`: Contiene el código principal, se determina el directorio de trabajo, se cargan los paquetes necesarios y llama a los otros scripts. Para su reproducibilidad es importante determinar la ruta de trabajo. Entre la línea 51 y 61 se puede cambiar la ruta para replicar los modelos.
  * `01_data`:  Este código realiza incorpora las dos bases de datos (train y test) de los precios y características de los precios de las viviendas en Bogotá. También se encuentra la limpieza de la base de datos, y se realiza la imputación de datos de los _missing values_. Más adelante se realizan las estadísticas descriptivas, con un enfasis en la ubicación geográfica de las propiedades y la distribución del precio. Además, guarda los resultados en formato _.RData_ en la carpeta de `stores`.
  * `02_Pruebas_Camila`:  Se realizan modificaciones a la base de datos para extraer la raíz de las palabras claves que se encuentran en la descripción de cada vivienda y se realiza modelos lineales con regulación de Elastic net.


  * `03_Pruebas_Julia`: Este código realiza las estimaciones de la sección 3 del problem set 2 (Modelos de clasificación). Se exploran prediccion lineales, elastic Net y boosting. Asimismo, se explora una regresión con xgboost para clasificación indirecta.
  * `04_Pruebas_Alejandra`: Este código realiza prediciones tanto de la sección 3 (modelo de clasificación utilizando logit son SMOTE y predicción de la sección 4 del problem set 2 (Clasificación indirecta) por medio de Random Forest.
  * `05_Pruebas_Juan`: Este código realiza las Prediciones de la sección 3 (modelo de clasificación directa) se realizan predicciones por medio de random Forest y boosting. Adicionalmente, se explora Prediccióndes para clasificación indirecta por medio de boosting.



* `Data`: En esta carpeta se encuentran las bases de datos que se encuentran en Kaggle y los resultados de cada de predicción de cada script en formato (.csv)
  
* `views`: En esta carpeta están las tablas y figuras del documento final.

  * `views/tables`
	-Tabla 1- Descripción de Variables.
	-Tabla 2- Modelos de Calsificación
	-Tabla 3 - Comparación entre modelos de clasificación indirecta
	-Tabla 4- Comparación de los coeficientes de un modelo lineal y uno equivalente con un método de regulación

  * `views/figures'
	-Figura 1 Histograma del ingreso de los hogares clasificados por pobreza.
	-Figura 2 Árbol 1. Clasificación con XGBoosting
        -Figura 3 Árbol 499. Clasificación con XGBoosting
    
