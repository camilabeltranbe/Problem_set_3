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
  * `03_Pruebas_Julian`: Este código estima el precio de las viviendas por el método de redes neuronales artificiales (RNA).
  * `04_Pruebas_Alejandra`: Este código realiza prediciones de precios de las vivienda utilizando el método de Random Forest y XG-Boost.
  * `05_Pruebas_Juan`: De igual manera, este código realiza la predición de los precios usando un modelo líneal con un número reducido de palabras clave de la descripción de las viviendas y RNA.

* `stores`: En esta carpeta se encuentran las bases de datos que se encuentran en Kaggle y los resultados de cada de predicción de cada script en formato (.csv)
  
* `views`: En esta carpeta están las tablas y figuras del documento final.

  * `views/Tables`
	-Tabla 1 - Estad ́ısticas descriptivas de las principales variables en la base de datos de entrenamiento.
	-Tabla 2 - Resumen de los modelos de Predicción.


  * `views/Figures'
	-Figura 1 - Ubicación geográfica de las propiedades en Bogotá.
	-Figura 2 - Missing values en la base de entrenamiento por categoría de variable.
        -Figura 3 - Histograma del precio del inmueble en millones de pesos por tipo de vivienda.
	-Figura 4 - Validación cruzada espacial XGBoost.
    
