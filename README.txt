# Problem Set 2: Big Data y Machine Learning para Economía Aplicada
***14 de Abril de 2024***  
***Universidad de los Andes***  
***Camila Beltran (cod. 202213110)***  
***Julian Pulido (cod. 201814833)***  
***Alejandra Guevara (cod. 202324570)***  
***Juan Pablo Rodríguez (cod. 201716771)***  

Este repositorio almacena toda la información utilizada para la solución del problem set 2 de la materia de Big Data y Machine Learning para Economía Aplicada. La información se encuentra separada en cuatro carpetas:

* `document`: En esta carpeta está el documento final en  _.pdf_, dado que el trabajo se realizó en grupo, decidimos utilizar el compilador de texto en línea _overleaf_. Por lo tanto, este repositorio no permite la reproducibilidad del documento final. 

* `scripts`: En esta carpeta están los _scripts_ utilizados para generar los resultados. 

  * `00_main_script`: Contiene el código principal, se determina el directorio de trabajo, se cargan los paquetes necesarios y llama a los otros scripts. Para su reproducibilidad es importante determinar la ruta de trabajo. En la línea 51 puede cambiar por la ruta en donde replico el repositorio.
  * `01_Data`:  Este código realiza la unión de las bases train de hogares y personas, y la de test de hogares y personas, que se encuentran en formato csv. Se realiza la limpieza de la base de datos, mantiene las variables de interés y realiza imputación de datos. Además, guarda los resultados en formato _.RData_ en la carpeta de `stores`.
  * `02_Estadística descriptiva`:  Se realiza gráfico para la sección de estadística descriptiva y se guarda la figura en views.
  * `03_Pruebas_Camila`: Este código realiza las estimaciones de la sección 3 del problem set 2 (Modelos de clasificación). Se exploran prediccion lineales, elastic Net y boosting. Asimismo, se explora una regresión con xgboost para clasificación indirecta.
  * `04_Pruebas Julian`: Este código realiza prediciones tanto de la sección 3 (modelo de clasificación utilizando logit son SMOTE y predicción de la sección 4 del problem set 2 (Clasificación indirecta) por medio de Random Forest.
  * `05_Pruebas_Alejandra`: Este código realiza las Prediciones de la sección 3 (modelo de clasificación directa) se realizan predicciones por medio de random Forest y boosting. Adicionalmente, se explora Prediccióndes para clasificación indirecta por medio de boosting.
  * `06_Pruebas_Juan_Pablo:En este código se realizan las predicciones lineales tanto de la sección 3 (modelos de clasificación directas) como de la sección 4 (Modelos de clasificación indirecta)


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
    
