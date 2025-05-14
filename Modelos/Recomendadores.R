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

<<<<<<< HEAD
tickets_filtrados <- tickets[tickets$id_cliente_enc %in% obj4, ]

ultimos_tickets <- tickets_filtrados %>%
  group_by(id_cliente_enc) %>%
  filter(dia == max(dia)) %>%
  ungroup()

historial_tickets <- anti_join(tickets_filtrados, ultimos_tickets, by = "num_ticket")


tickets_matriz_agrupado <- historial_tickets %>%
  group_by(id_cliente_enc, cod_est) %>%
  summarise(N_compras = n(), .groups = "drop")

df_matriz <- pivot_wider(
  tickets_matriz_agrupado, 
  names_from = "cod_est", 
  values_from = "N_compras", 
  values_fill = 0,
  names_prefix = "id_"
)

matriz_sparse_o4 <- as(as.matrix(df_matriz[,-1]), "dgCMatrix")
rownames(matriz_sparse_o4) <- df_matriz$id_cliente_enc


modelo_wrmf_o4 <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_o4$fit_transform(matriz_sparse_o4, n_iter = 1000L, convergence_tol = 1e-6)

preds_o4 <- modelo_wrmf_o4$predict(matriz_sparse_o4, k = 1)

clientes <- rownames(matriz_sparse_o4)
productos_predichos <- attr(preds_o4, "ids")

recomendaciones_o4 <- data.frame(
  id_cliente_enc = clientes,
  producto_olvidado = productos_predichos
)

recomendaciones_o4 <- recomendaciones_o4 %>%
  mutate(cod_est = str_remove(producto_olvidado, "id_")) %>%
  left_join(productos %>% select(cod_est, descripcion), by = "cod_est") %>%
  select(id_cliente_enc, cod_est, descripcion)

# guardamos el df
saveRDS(recomendaciones_o4,"Datos\\Resultados\\Objetivo4_resultado.rds")


##################### segunda prueba objetivo 4
=======
>>>>>>> 036150d3d568744f25f0e069b471cb99fdb14f67
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

preds_o4 <- modelo_wrmf$predict(mo4, k = 1, not_recommend = mo4)
preds_o4
attr_preds_o4 <- attr(preds_o4,'ids')
preds_o4_df <- data.frame(id_cliente_enc = rownames(attr_preds_o4), 
                          cod_est = attr_preds_o4, row.names = c()) 

preds_o4_nombres <- inner_join(preds_o4_df, productos, by = "cod_est")
preds_o4_nombres
saveRDS(preds_o4_nombres, "Datos/Resultados/Objetivo4_resultado.rds")
