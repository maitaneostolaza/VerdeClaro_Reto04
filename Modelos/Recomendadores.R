library(recommenderlab)
library(dplyr)
library(tidyr)
library(rsparse)
library(ggplot2)
library(Matrix)
library(stringr)
library(tibble)

# cargamos los datos
tickets <- readRDS("Datos\\Transformados\\tickets_Reducidos.rds")
clusteres <- readRDS("Datos\\Transformados\\df_con_clusteres.rds")
productos <- readRDS("Datos\\Originales\\maestroestr.RDS")
objetivos <- readRDS("Datos\\Originales\\objetivos.RDS")

##################################### OBJETIVO 1 ###############################
objetivo1 <- as.data.frame(objetivos[[1]])
objetivo2 <- as.data.frame(objetivos[[2]])

# ------------------------ como en este caso lo que queremos es recomendar a los 10
# usuarios mas relevantes un producto en especifico, el proceso a seguir es este: 
# Entrenar el modelo
# cargar la matriz
matriz_general <- readRDS("Datos\\Resultados\\Matriz.rds")
rownames(matriz_general) <- matriz_general$id_cliente_enc
matriz_general <- matriz_general[,-1]
matriz_general <- as.matrix(matriz_general)

matriz_rec <- as(matriz_general,"realRatingMatrix")

# --------------------- ESTADÍSTICOS DE LA MATRIZ
matriz <- as(matriz_general,"matrix")
recuentoF <- rowCounts(matriz_rec) # cuantas celditas de una fila son diferentes de NA (cuantas pelis ha valorado cada usuario)
recuentoC <- colCounts(matriz_rec) # cuantas celditas de una columna son diferentes de NA

max(matriz,na.rm=T) ; min(matriz, na.rm=T)

# MEDIAS 
media_columnas <- colMeans(matriz_rec,na.rm=T)
media_filas <- rowMeans(matriz_rec,na.rm=T)
hist(media_filas)
hist(media_columnas)

# PARA SABER EL GOODRATING Y EL GIVEN
min(recommenderlab::rowCounts(matriz_rec,na.rm=T))

######################### RECOMENDADORES CON RECOMMENDERLAB ####################
set.seed(8)
# --------------------------- TRAIN Y TEST
eval_scheme <- evaluationScheme(matriz_rec, method = "split",
                                train = 0.8, given= 1,
                                goodRating = 2)

# -------------------------- ENTRENAR MODELOS :
algos <- list("random" = list(name = "RANDOM", param = NULL),
              "UBCF_10nn" = list(name = "UBCF", param = list(nn = 10)), # vecinos mas cercanos 
              "UBCF_50nn" = list(name = "UBCF", param = list(nn = 50)),
              "IBCF_Pearson" = list(name = "IBCF", param = list(method = "Pearson")),
              "popular" = list(name = "POPULAR" , param = NULL),
              "svdf_50" = list(name = "SVDF",param = list(k=50)),
              "svdf_100" = list(name = "SVDF",param = list(k=100)),
              "svdf_200" = list(name = "SVDF",param = list(k=200)))

# ----------- TOPNLIST
eval <- evaluate(eval_scheme, algos, type = "topNList", n = c(1,3,5,10,15,20))
plot(eval)


getConfusionMatrix(eval[["random"]])
grafico2 <- plot(eval[-c(2, 3)]) 
grafico4 <- plot(eval,"prec/rec")


CM_Random <- getConfusionMatrix(eval[["random"]])[[1]]
CM_UBCF_10 <- getConfusionMatrix(eval[["UBCF_10nn"]])[[1]]
CM_UBCF_50 <- getConfusionMatrix(eval[["UBCF_50nn"]])[[1]]
CM_IBCF <- getConfusionMatrix(eval[["IBCF_Pearson"]])[[1]]
CM_Popular <- getConfusionMatrix(eval[["popular"]])[[1]]
CM_SVDF_50 <- getConfusionMatrix(eval[["svdf_50"]])[[1]]
CM_SVDF_100 <- getConfusionMatrix(eval[["svdf_100"]])[[1]]
CM_SVDF_200 <- getConfusionMatrix(eval[["svdf_200"]])[[1]]

# ----------------- RATINGS
eval_ratings <- evaluate(eval_scheme, algos, type = "ratings", n = c(1,3,5,10,15,20))

avg(eval_ratings[["random"]])
avg(eval_ratings[["UBCF_10nn"]])
avg(eval_ratings[["UBCF_50nn"]])
avg(eval_ratings[["IBCF_Pearson"]])
avg(eval_ratings[["popular"]])
avg(eval_ratings[["svdf_50"]])
avg(eval_ratings[["svdf_100"]])
avg(eval_ratings[["svdf_200"]])



############################### RSPARSE ########################################

############################### OBJETIVO 1 #####################################
# Creamos el modelo WRMF -------------------------------------------------------
matriz_general <- readRDS("Datos\\Resultados\\Matriz_sinNA.rds")
storage.mode(matriz_general) <- "numeric"

# para el objetivo 1 cambiamos filas por columnas
matriz_alreves <- t(matriz_general)

# -- MODELO: 
modelo_wrmf_alreves <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_alreves$fit_transform(matriz_alreves, n_iter = 1000L, convergence_tol=0.000001)

# para el objetivo 1 filtramos por el producto que nos interesa --> masa de pizza

matriz_obj1 <- matriz_alreves[rownames(matriz_alreves) %in% objetivos$objetivo1$obj, , drop=F] 
matriz_obj1 <- as(matriz_obj1,"sparseMatrix")

preds_1 <- modelo_wrmf_alreves$predict(matriz_obj1, k = 10) # para que nos de 10 usuarios
preds_1
lista_1 <- attr(preds_1,'ids')

# guardamos el df en resultados
objretivo1_resultado <- as.data.frame(lista_1)
rownames(objretivo1_resultado)<- c("MASAS DE PIZZA")
saveRDS(objretivo1_resultado, "Datos\\Resultados\\Objetivo1_resultado.rds")

################################## OBJETIVO 2 ##################################
# modelo para el objetivo 2 y 4: 
modelo_wrmf <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf$fit_transform(matriz_general, n_iter = 1000L, convergence_tol=0.000001)

matriz_obj2 <- matriz_general[rownames(matriz_general) %in% objetivos$objetivo2$obj,]
matriz_obj2 <- as(matriz_obj2,"sparseMatrix")

preds_2 <- modelo_wrmf$predict(matriz_obj2, k = 1, not_recommend = matriz_obj2)
preds_2
lista_2 <- attr(preds_2,'ids')

# guardamos en un data frame
objetivo2_resultado <- as.data.frame(lista_2)
colnames(objetivo2_resultado) <- "cod_est"
objetivo2_resultado <- rownames_to_column(objetivo2_resultado, var = "Id_cliente")
objetivo2_resultado <- inner_join(objetivo2_resultado,productos,by="cod_est")

saveRDS(objetivo2_resultado,"Datos\\Resultados\\Objetivo2_resultado.rds")

################################# OBJETIVO 3 ###################################
# el modelo es el mismo que para el objetivo 1 
matriz_obj3 <- matriz_general
matriz_obj3 <- as(matriz_obj3,"sparseMatrix")

# cogemos los items que no queremos que recomiente para el predict
matriz_no_recomendados <- matriz_general [,!colnames(matriz_general) %in% objetivos$objetivo3$obj]

items_no_recomendados <- colnames(matriz_no_recomendados)

preds_3 <- modelo_wrmf$predict(matriz_obj3, k = 1, items_exclude = items_no_recomendados)
preds_3

lista_3 <- attr(preds_3,'ids')

# guardamos en un data frame con el nombre del producto
objetivo3_resultado <- as.data.frame(lista_3)
colnames(objetivo3_resultado) <- "cod_est"
objetivo3_resultado <- rownames_to_column(objetivo3_resultado, var = "Id_cliente")
objetivo3_resultado <- inner_join(objetivo3_resultado,productos,by="cod_est")

saveRDS(objetivo3_resultado,"Datos\\Resultados\\Objetivo3_resultado.rds")

################################# OBJETIVO 4 ###################################
obj4<-objetivos[[4]]$obj

# Creo una lista vacia y en el bucle itero cada usuario del objetivo buscando 
# la última compra realizada y guardando en la lista un df con la cantidad de 
# cada producto comprado ese dia
ultimas_compras <- list()
for (c in obj4) {
  tickets_matriz <- tickets %>% 
    filter(id_cliente_enc == c) %>% 
    filter(dia == max(dia))
  
  tickets_matriz_agrupado <- tickets_matriz %>% 
    group_by(cod_est) %>% 
    summarise(dia, N_compras = n()) %>% 
    select(cod_est, N_compras)
  
  ultimas_compras[[c]] <- tickets_matriz_agrupado
}

# Creo una matriz vacia con las mismas columnas que se ha entrenado el modelo
# y le pongo los nombres de las columnas y filas que les corresponden
mo4 <- matrix(0, ncol = ncol(matriz_general), nrow = length(obj4))
colnames(mo4) <- colnames(matriz_general)
rownames(mo4) <- obj4

# Con un for hago que para cada cliente y para cada producto comprado, ponga en
# la matriz en la fila y columna correspondiente la cantidad comprada
for (cliente in obj4) {
  compra <- ultimas_compras[[cliente]]
  for (prod in compra$cod_est) {
    producto <- filter(compra, cod_est == prod)
    mo4[cliente, prod] <- producto$N_compras
  }
}

mo4 <- as(mo4, "sparseMatrix")

modelo_wrmf$fit_transform(matriz_general, n_iter = 1000L, convergence_tol=0.000001) 
preds_o4 <- modelo_wrmf$predict(mo4, k = 1, not_recommend = mo4)
preds_o4
attr_preds_o4 <- attr(preds_o4,'ids')
preds_o4_df <- data.frame(id_cliente_enc = rownames(attr_preds_o4), 
                          cod_est = attr_preds_o4, row.names = c()) 

preds_o4_nombres <- inner_join(preds_o4_df, productos, by = "cod_est")
preds_o4_nombres
saveRDS(preds_o4_nombres, "Datos/Resultados/Objetivo4_resultado.rds")


############################### COMPROBACIONES #################################
# 1. CREAMOS LAS MATRICES DE FACTORES LATENTES: 
user_emb <- modelo_wrmf$fit_transform(matriz_general) # matriz de factores de los usuarios
user_emb; dim(user_emb) #22718 cliente 10 productos

su <- similarity(as(user_emb,"realRatingMatrix"))
item_emb <- modelo_wrmf$components # matriz de factores de los items
item_emb
dim(item_emb)
item_emb_n <- t(modelo_wrmf$components)


# -------------------------------- OBJETIVO 1: 
# para este objetivo como el modelo se ha realizado con la matriz alreves, creamos
# de nuevo las matrices de usuarios y productos para este objetivo
item_emb_obj1 <- modelo_wrmf_alreves$fit_transform(matriz_alreves)
user_emb_obj1 <- modelo_wrmf_alreves$components

user_emb_obj1_n <- t(user_emb_obj1)
item_emb_obj1_n <- t(item_emb_obj1)

# 10 usuarios para recomendar masa de pizza --> 14351005
objetivos$objetivo1$obj

comprobacion1 <- user_emb_obj1_n%*%item_emb_obj1_n[,colnames(item_emb_obj1_n)=="14351005"]%>% 
  sort(decreasing=TRUE, index.return = T)
comprobacion1$ix[1:10]
comprobacion1_df <- data.frame(valoracion = comprobacion1$x, usuario = rownames(user_emb_obj1_n)[comprobacion1$ix])
comprobacion1_df[1:10,] # usuarios que comprarian la masa de pizza

objetivo1 <- readRDS("Datos\\Resultados\\Objetivo1_resultado.rds")

# respuesta: mitad si mitad no

# --------------------------------------- OBJETIVO 2: 
comprobacion2 <- user_emb[rownames(user_emb) %in% objetivos$objetivo2$obj,] %*% item_emb[] %>% 
  sort(decreasing = T, index.return = T)
comprobacion2_df <- data.frame(valoracion = comprobacion2$x, usuario = colnames(item_emb)[comprobacion2$ix])









