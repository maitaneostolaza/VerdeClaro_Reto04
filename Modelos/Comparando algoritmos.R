library(recommenderlab)
library(dplyr)
library(tidyr)
library(Matrix)

# DESCARGAMOS LA MATRIZ
matriz_general <- readRDS("Datos\\Resultados\\Matriz.rds")


# convertimos a data frame para que podamos convertir la columna cliente_id a nombres de fila
df <- as.data.frame(matriz_general)
rownames(df) <- df$id_cliente_enc
df <- df[,-1]
# matriz_general <- as(df, "matrix")
# matriz_rec <- as(matriz_general,"realRatingMatrix")

# como no nos deja convertirlo a matriz, vamos a filtrar directamente el data frame: 
# 1. SACAMOS ESTADISTICOS 
df_numeric <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))
rownames(df_numeric) <- rownames(df)

# Total de productos comprados por cada cliente
compras_por_cliente <- rowSums(df_numeric, na.rm = TRUE)
media_clientes <- mean(compras_por_cliente) #104.4187
max(compras_por_cliente) # 1919
min(compras_por_cliente) # 1

# Total de veces que se compró cada producto (columnas)
compras_por_producto <- colSums(df_numeric, na.rm = TRUE)
media_productos <- mean(compras_por_producto) #1409.497
min(compras_por_producto) # 1
max(compras_por_producto) #22228

# ------------------------- FILTRAMOS = REDUCIMOS
# filtramos df porque minimo los clientes hayan comprado la media de compras por cliente
# y los productos porque minimo hayan comprado la media de compras por producto
# ---------- CLIENTES
clientes_validos <- compras_por_cliente >= 120
df_reducido_clien <- df_numeric[clientes_validos, ]

# Recalcular compras por producto tras filtrar clientes
compras_por_producto_filtrado <- colSums(df_reducido_clien, na.rm = TRUE)

#---------- PRODUCTOS
<<<<<<< HEAD
productos_validos <- compras_por_producto_filtrado >= 1000
=======
<<<<<<< HEAD
productos_validos <- compras_por_producto_filtrado >= 2000
=======
productos_validos <- compras_por_producto_filtrado >= 1000
>>>>>>> 35f53198c60e3923c78bd5b09f2808c5679f1232
>>>>>>> 94f7eae52ba500b036b068d404c34f2879d1ad89
df_reducido <- df_reducido_clien[, productos_validos]
dim(df_reducido)


# ahora si, convertimos a matriz
matriz <- as.matrix(df_reducido)
saveRDS(matriz,"Datos/Resultados/Matriz_red_comp_algos.rds")

# hay que preparar ese archivo para llevarlo a weka

library(foreign)
matriz<- as.data.frame(matriz)
codigos <- gsub("^X", "", colnames(matriz))

tabla_nombres <- data.frame(
  original = colnames(matriz),
  cod_est = codigos
) %>%
  left_join(productos, by = "cod_est")

nuevos_nombres <- ifelse(
  is.na(tabla_nombres$descripcion),
  tabla_nombres$original,
  tabla_nombres$descripcion
)

colnames(matriz) <- nuevos_nombres
matriz <- select(matriz, -TOSTADA)
write.arff(matriz,file = "Datos/Resultados/Matriz_red_comp_algos.arff")



########################## COMPARACION ALGORITMOS ##############################

matriz <- readRDS("Datos\\Resultados\\Matriz_red_comp_algos.rds")

# convertimos a realratingmatrix
storage.mode(matriz) <- "numeric"
matriz_sparse <- as(as.matrix(matriz), "dgCMatrix")
matriz_rec <- as(matriz_sparse, "realRatingMatrix")

set.seed(8)
# --------------------------- TRAIN Y TEST
str(matriz_rec)
min(rowCounts(matriz_rec))
eval_scheme <- evaluationScheme(matriz_rec, method = "split",
                                train = 0.8, given= 5,
                                goodRating = 1)

# -------------------------- ENTRENAR MODELOS :
algos <- list("random" = list(name = "RANDOM", param = NULL),
              "UBCF_10nn" = list(name = "UBCF", param = list(nn = 10)), # vecinos mas cercanos 
              "UBCF_50nn" = list(name = "UBCF", param = list(nn = 50)),
              "IBCF_Pearson" = list(name = "IBCF", param = list(method = "Pearson")),
              "popular" = list(name = "POPULAR" , param = NULL),
              "svdf_10" = list(name = "SVDF", param = list(k=10)))

# ----------- TOPNLIST
eval <- evaluate(eval_scheme, algos, type = "topNList", n = c(1,3,5,10,15,20))
plot(eval) # a priori no hay mucha diferencia entre los algoritmos

# ---- SACAMOS PARAMETROS PARA COMPRAR ALGORITMOS: 

CM_Random <- getConfusionMatrix(eval[["random"]])[[1]]
CM_UBCF_10 <- getConfusionMatrix(eval[["UBCF_10nn"]])[[1]]
CM_UBCF_50 <- getConfusionMatrix(eval[["UBCF_50nn"]])[[1]]
CM_IBCF <- getConfusionMatrix(eval[["IBCF_Pearson"]])[[1]]
CM_Popular <- getConfusionMatrix(eval[["popular"]])[[1]]
CM_SVDF_10 <- getConfusionMatrix(eval[["svdf_10"]])[[1]]

# ----------------- RATINGS
eval_ratings <- evaluate(eval_scheme, algos, type = "ratings", n = c(1,3,5,10,15,20))

avg(eval_ratings[["random"]])
avg(eval_ratings[["UBCF_10nn"]])
avg(eval_ratings[["UBCF_50nn"]])
avg(eval_ratings[["IBCF_Pearson"]])
avg(eval_ratings[["popular"]])
avg(eval_ratings[["svdf_10"]])


################### GRAFICOS PARA LA COMPARACION DE ALGORITMOS: 
sum(!is.na(matriz_rec)) / (nrow(matriz_rec) * ncol(matriz_rec))
recs <- predict(Recommender(getData(eval_scheme, "train"), method = "UBCF", param = list(nn = 50)),
                getData(eval_scheme, "known"), type = "topNList", n = 10)

# Verifica cuántas recomendaciones hay por usuario
summary(recs)









