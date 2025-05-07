# OBJETIVO 1: encontrar los 10 usuarios que más se parezcan a los usuarios que 
# mas masas de pizzas han comprado para recomendarles a ellos la masa de pizza
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

# --------------------- REDUCIMOS LA MATRIZ
# CLIENTES QUE AL MENOS HAYAN COMPRADO DOS VECES
matriz_rec <- as.matrix(matriz_general)
matriz_rec <- as(matriz_rec,"realRatingMatrix")
matriz_reducido <- matriz_rec [recuentoF>15,]
matriz_reducido <- matriz_reducido[,colCounts(matriz_reducido)>100]
dim(matriz_general)
dim(matriz_reducido)
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
eval_reducido <- evaluate(eval_scheme, algos, type = "topNList", n = c(1,3,5,10,15,20))

# new: habitos de compra de los clientes se van a decidir dependiendo del numero de caracteristicas que eligas en K
# todo lo demás lo dejamos igual

# ---- en el predict metemos: 
# matriz sobre los clientes que queremos predecir y productos sobre los que queremos predecir
# k= cuantos productos a cada cliente
# not_recommend= que productos no queremos que recomeinte
# preds: attr(preds, 'ids')

