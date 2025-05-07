library(recommenderlab)
library(dplyr)
library(tidyr)
library(rsparse)
library(ggplot2)

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
matriz_rec <- as.matrix(matriz_general)
matriz_rec <- as(matriz_rec,"realRatingMatrix")

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
ggsave("Graficos/Comparacion_algoritmos_evaluate.png", 
       plot = grafico_eval, width = 10, height = 6, dpi = 300)

getConfusionMatrix(eval[["random"]])
grafico2 <- plot(eval[-c(2, 3)]) 
grafico4 <- plot(eval,"prec/rec")
ggsave("Graficos/Comparacion_algoritmos_evaluate2.png", 
       plot = grafico2, width = 10, height = 6, dpi = 300)

ggsave("Graficos/Comparacion_algoritmos_evaluate3.png", 
       plot = grafico4, width = 10, height = 6, dpi = 300)

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


# ---------------------- ALS
model_als <- WRMF$new(rank = 10, lambda = 0.1, feedback = "explicit")

model_als$fit(x = matriz_ui, n_iter = 10)

# Predicción de ratings para toda la matriz usuario-item
pred <- model_als$predict(matriz_ui)

# Top-3 recomendaciones por usuario
top3 <- model_als$top_n(matriz_ui, n = 3)

# Ver para un usuario
top3[[1]]  # ítems recomendados para el usuario 1


# Supongamos que el producto es "itemX"
producto <- objetivo1[,2]

# Obtener los usuarios con mayor predicción para ese producto
scores <- pred_matrix[, producto]
top10_clientes <- names(sort(scores, decreasing = TRUE))[1:10]



# ----------- EVAL SCHEME DE NUEVO
set.seed(7)
train<- evaluationScheme(matriz_rec, method= "split",
                         train= 0.8, 
                         k=1,
                         given=15, goodRating=2)

paraentrenar<- getData(train, "train") #se valoraran las predicciones respecto a estas
as(getData(train, "unknown"), "matrix")

paradar<- getData(train, "known") #se le dan estas
as(getData(train, "known"),"matrix")

parapredecir<-getData(train, "unknown") # se valoran las predicciones respecto a estas
as(getData(train, "unknown"),"matrix")

# entrenar modelos : Recommender (1 solo algortimo);
#                     evaluate (multiples algoritmos)
#                     parametrizar algoritmos

# Lista de algoritmos a evaluar
algos <- list("random" = list(name = "RANDOM", param = NULL),
              "UBCF" = list(name = "UBCF"),
              "IBCF" = list(name = "IBCF"),
              "POPULAR" = list(name = "POPULAR", param = NULL))
#vamos a evaluar los algoritmos pero
#ahora solicitamos los articulos topn en lugar de una estimacion de las valoraciones
#Se evaluarán los algoritmos para n = 1,3,5,10,15,20.
#La función eval entrena los algoritmos, #predice y entrega la evaluación para todos los algoritmos

eval <- evaluate(train, algos, type = "ratings")
plot(eval);plot(eval,"prec/rec") 
getConfusionMatrix(eval[["random"]])
getConfusionMatrix(eval[["IBCF"]])

# Evaluar usando top-N
results <- evaluate(train, method = algos, type = "topNList", n = c(1, 3, 5, 10))

plot(results)
usuarios <-  c(
  "b51353fcf07cb61280eda45e33679871",
  "02ff5edaa057b63ea0a0010c5402205c",
  "25d259d32a2bc254343715f2e347c518",
  "53ffb83e85fd51cf1ec2fdef3c78b4fd",
  "26f424b3bba6aaf97952ac599ed39f75",
  "32cc820ac27ff143c3ea976f3fe69d34",
  "a57938025d714b65612bf2cfde12136d",
  "af30d404e282749ccd5f5ad0c8e834c7",
  "8b9aa623b654a8be21b316a5fdf41007",
  "e27ceb0a1576648212c4325fdf7d8002"
)
com <- df %>% filter(id_cliente_enc %in% usuarios)
u <- unique(com$id_cliente_enc)
