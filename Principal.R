##Antes de comenzar todos los scripts contienen este source, para llamar a todas
## las librerias que contiene el proyecto, y caso de no tener alguna, la instala.
source("Scripts_preprocesamiento/Librerias.R", encoding = "UTF-8")
rm(list=ls())


#1 Preporcesamiento de datos 

#1-1  Limpieza de datos Duplicados
#
source("Scripts_preprocesamiento/Limpieza de datos.R", encoding = "UTF-8")
#
rm(list=ls())


#1-2 Limpieza de datos Reduccion de dataset
#
source("Scripts_preprocesamiento/Limpieza de datos Reduccion.R", encoding = "UTF-8")
#
rm(list=ls())


#1-3  Clustering
#
source("Scripts_preprocesamiento/Clustering.R", encoding = "UTF-8")
#
rm(list=ls())


#1-4  Matriz
#
source("Scripts_preprocesamiento/matriz.R", encoding = "UTF-8")
#
rm(list=ls())



#2 Modelos

#2-1 Algoritmos
source("Modelos/Comparando algoritmos.R", encoding = "UTF-8")
#
rm(list=ls())


#2-2 Recomendadores binarizadas
source("Modelos/Recomendadores binarizadas.R", encoding = "UTF-8")
#
rm(list=ls())


#2-3 Reglas de asociacion
source("Modelos/reglas_de_asociacion.R", encoding = "UTF-8")
#
rm(list=ls())

#2-4 Objetivo 4 en modo Lista
lista_obj_4 <- readRDS("Datos/Resultados/Comprobacion_objetivo4.rds")
lista_obj_4
#
rm(list=ls())



#3 Otros Scripts como Aplicaciones, Graficos...

#3-1 Api con los objetivos recomendadores
#
source("Aplicaciones/EjecutarApi.R", encoding = "UTF-8")
#
rm(list=ls())


#3-2  App Shiny
#
app <- source("Aplicaciones/ShinyApp.R", local = TRUE)$value
runApp(app)
#
rm(list=ls())

#3-3  Graficos para analisis descriptivo
#
source("Scripts_Estadisticos/AnalisisDescriptivos.R", encoding = "UTF-8")
#
rm(list=ls())
