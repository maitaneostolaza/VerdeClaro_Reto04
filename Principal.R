#1 Preporcesamiento de datos 

#1-1  Año 2021 Limpieza,Duplicados,Missing
#
source("Scripts_preprocesamiento/Preprocesamiento_2021.R", encoding = "UTF-8")
#
rm(list=ls())


#1-2  Año 2022 Limpieza,Duplicados,Missing
#
source("Scripts_preprocesamiento/Preprocesamiento_2022.R", encoding = "UTF-8")
#
rm(list=ls())


#1-3  Año 2023 Limpieza,Duplicados,Missing
#
source("Scripts_preprocesamiento/Preprocesamiento_2023.R", encoding = "UTF-8")
#
rm(list=ls())


#1-4  Creacion Ratios 
#
source("Analisis_Exploratorio/Ratios.R", encoding = "UTF-8")
#
rm(list=ls())


#1-5  Outliers Para los ficheros de todos los años
#
source("Scripts_preprocesamiento/Outliers.R", encoding = "UTF-8")
#
rm(list=ls())



#2 Clusters, Api Lista y Recomendaciones

#2-1 Clustering
source("Modelos/Clusters.R", encoding = "UTF-8")
#
rm(list=ls())


#2-2 API ventana
source("Analisis_Exploratorio/EjecutarAPI.R", encoding = "UTF-8")
#
rm(list=ls())


#2-3 Listas de los clusters con las empresas recomendadas
source("Modelos/Listas.R", encoding = "UTF-8")
#
rm(list=ls())

#2-3 Recomendaciones Empresas
source("Modelos/Recomendaciones.R", encoding = "UTF-8")
#
rm(list=ls())



#3 Otros Scripts como Analisis Exploratorio, Graficos, Mate...

#3-1 Analisis Exploratorio
#
source("Analisis_Exploratorio/AnalisisExploratorio.R", encoding = "UTF-8")
#
rm(list=ls())


#3-2  Analisis Ratios
#
source("Analisis_Exploratorio/AnalisisRatios.R", encoding = "UTF-8")
#
rm(list=ls())

#3-3  Graficos Para el analisis 2021
#
source("Scripts_EDA/analisis graficos 2021.R", encoding = "UTF-8")
#
rm(list=ls())


#3-4  Graficos Para el analisis 2022
#
source("Scripts_EDA/analisis graficos 2022.R", encoding = "UTF-8")
#
rm(list=ls())


#3-5  Graficos Para el analisis 2023
#
source("Scripts_EDA/analisis graficos 2023.R", encoding = "UTF-8")
#
rm(list=ls())

#3-6  Matematica
#
source("Scripts_EDA/matematicas Reto3.R", encoding = "UTF-8")
#
rm(list=ls())



