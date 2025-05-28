################################# 
library(ggplot2)
library(dplyr)
library(tidyr)


############################### CREACION MATRIZ ################################
# VAMOS A HACER UNA MATRIZ POR CLUSTER PARA QUE LAS RECOMENDACIONES SEAN MÁS ESPECIFICAS
df <- readRDS("Datos\\Transformados\\tickets_Reducidos.rds")
str(df)

# --------------------------------- MATRIZ CON TODOS LOS DATOS
general <- df %>% 
  group_by(id_cliente_enc,cod_est) %>% 
  summarise(Cantidad = n())


matriz_general <- pivot_wider(general,
                              names_from = cod_est,
                              values_from = Cantidad)

matriz_general <- as.matrix(matriz_general)

saveRDS(matriz_general,"Datos/Resultados/Matriz.rds")


# ------------------------- MATRIZ SIN NA'S
matriz_general <- readRDS("Datos\\Resultados\\Matriz.rds")
rownames(matriz_general) <- matriz_general[,1]
matriz_general <- matriz_general[,-c(1)]

# ------------ cambiamos los NA's por 0
matriz_general <- as(matriz_general, "matrix")
matriz_general[is.na(matriz_general)] <- 0

saveRDS(matriz_general,"Datos\\Resultados\\Matriz_sinNA.rds")
