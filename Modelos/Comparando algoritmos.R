#Librerias
source("Scripts_preprocesamiento/Librerias.R", encoding = "UTF-8")
source("Scripts_preprocesamiento/Funciones.R", encoding = "UTF-8")

################################## REDUCCION DE MATRIZ #########################
# DESCARGAMOS LA MATRIZ
# matriz_general <- readRDS("Datos\\Resultados\\Matriz.rds")
# rownames(matriz_general) <- matriz_general[,1]
# matriz_general <- matriz_general[,-1]
# storage.mode(matriz_general) <- "numeric"
# matriz_sparse <- as(as.matrix(matriz_general), "dgCMatrix")
# matriz_rec <- as(matriz_sparse, "realRatingMatrix")
# 
# 
# dim(matriz_rec)
# summary(colSums(matriz_general, na.rm=T))
# hist(colSums(matriz_general,na.rm=T))
# hist(rowSums(matriz_general,na.rm=T))
# 
# # sacamos estadisticos
# colCounts(matriz_rec) %>% 
#   as("matrix") 
# hist(getRatings(matriz_rec))
# recuentoF <- rowCounts(matriz_rec) # cuantas celditas de una fila son diferentes de NA (cuantas pelis ha valorado cada usuario)
# recuentoC <- mean(colCounts(matriz_rec))
# 
# # convertimos a data frame para que podamos convertir la columna cliente_id a nombres de fila
# df <- as.data.frame(matriz_general)
# rownames(df) <- df$id_cliente_enc
# df <- df[,-1]
# # matriz_general <- as(df, "matrix")
# # matriz_rec <- as(matriz_general,"realRatingMatrix")
# 
# # como no nos deja convertirlo a matriz, vamos a filtrar directamente el data frame: 
# # 1. SACAMOS ESTADISTICOS 
# df_numeric <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))
# rownames(df_numeric) <- rownames(df)
# 
# # Total de productos comprados por cada cliente
# compras_por_cliente <- rowSums(df_numeric, na.rm = TRUE)
# media_clientes <- mean(compras_por_cliente) #104.4187
# max(compras_por_cliente) # 1919
# min(compras_por_cliente) # 1
# 
# # Total de veces que se compró cada producto (columnas)
# compras_por_producto <- colSums(df_numeric, na.rm = TRUE)
# media_productos <- mean(compras_por_producto) #1409.497
# min(compras_por_producto) # 1
# max(compras_por_producto) #22228
# 
# # ------------------------- FILTRAMOS = REDUCIMOS
# # filtramos df porque minimo los clientes hayan comprado la media de compras por cliente
# # y los productos porque minimo hayan comprado la media de compras por producto
# # ---------- CLIENTES
# clientes_validos <- compras_por_cliente >= 120
# df_reducido_clien <- df_numeric[clientes_validos, ]
# 
# # Recalcular compras por producto tras filtrar clientes
# compras_por_producto_filtrado <- colSums(df_reducido_clien, na.rm = TRUE)
# 
# #---------- PRODUCTOS
# productos_validos <- compras_por_producto_filtrado >= 1000
# df_reducido <- df_reducido_clien[, productos_validos]
# dim(df_reducido)
# 
# 
# # ahora si, convertimos a matriz
# matriz <- as.matrix(df_reducido)
# saveRDS(matriz,"Datos/Resultados/Matriz_red_comp_algos.rds")


########################## COMPARACION ALGORITMOS ##############################

matriz <- readRDS("Datos\\Resultados\\Matriz_red_comp_algos.rds")
matriz_rec <- as(matriz, "realRatingMatrix")


set.seed(8)
# --------------------------- TRAIN Y TEST


eval_scheme <- evaluationScheme(matriz_rec, method = "split",
                                train = 0.8, given= 20,
                                goodRating = 1)

# -------------------------- ENTRENAR MODELOS :
algos <- list(
  "random" = list(name = "RANDOM", param = NULL),
  
  "UBCF_10nn" = list(name = "UBCF", param = list( nn = 10)),
  
  "UBCF_5" = list(name = "UBCF", param = list( nn = 5)),
  
  "IBCF" = list(name = "IBCF", param = NULL),
  
  "popular" = list(name = "POPULAR", param = NULL),
  
  "svdf_10" = list(name = "SVDF", param = list(k = 10)),
  
  "svdf_40" = list(name = "SVDF", param = list (k = 40)),
  
  "ALS" = list(name = "ALS", param = list(lambda = 0.1, n_factors = 10, n_iterations = 10)))



# ----------- TOPNLIST

# eval <- evaluate(eval_scheme, algos, type = "topNList", n = c(1,3,5,10,15,20))
# saveRDS(eval,"Datos\\Resultados\\Comparando_algoritmos_topNlist_eval.rds")
  
# ---- SACAMOS PARAMETROS PARA COMPRAR ALGORITMOS: 
# cargamos el eval
eval <- readRDS("Datos\\Resultados\\Comparando_algoritmos_topNlist_eval.rds")
plot(eval) 
Save_pdf("GraficoModelos_TRP_FPR", quote(plot(eval)))
CM_Random <- getConfusionMatrix(eval[["random"]])[[1]]
CM_UBCF_10 <- getConfusionMatrix(eval[["UBCF_10nn"]])[[1]]
CM_UBCF_50 <- getConfusionMatrix(eval[["UBCF_5"]])[[1]]
CM_IBCF <- getConfusionMatrix(eval[["IBCF"]])[[1]]
CM_Popular <- getConfusionMatrix(eval[["popular"]])[[1]]
CM_SVDF_10 <- getConfusionMatrix(eval[["svdf_10"]])[[1]]
CM_SVDF_40 <- getConfusionMatrix(eval[["svdf_40"]])[[1]]
CM_ALS <- getConfusionMatrix(eval[["ALS"]]) [[1]]

# ----------------- RATINGS
#  eval_ratings <- evaluate(eval_scheme, algos, type = "ratings", n = c(1,3,5,10,15,20))
# saveRDS(eval_ratings,"Datos\\Resultados\\Comparando_algoritmos_ratings_eval.rds")

# --------------- SACAMOS PARAMETROS PARA COMPARAR ALGORITMOS: 
# cargamos datos
eval_ratings <- readRDS("Datos\\Resultados\\Comparando_algoritmos_ratings_eval.rds")
random_ratings <- avg(eval_ratings[["random"]])
UBCF_10nn_ratings <- avg(eval_ratings[["UBCF_10nn"]])
UBCF_5nn_ratings <- avg(eval_ratings[["UBCF_5"]])
IBCF_ratings <- avg(eval_ratings[["IBCF"]])
popular_ratings <- avg(eval_ratings[["popular"]])
svdf_ratings_10 <- avg(eval_ratings[["svdf_10"]])
SVDF_ratings_40 <- avg(eval_ratings[["svdf_40"]])
ALS_ratings <- avg(eval_ratings[["ALS"]]) 


################################# GRAFICAMOS ###################################
# --------------------------------- RATINGS

# Crear el data frame con las métricas (con random)
comparativa_errores <- data.frame(
  Modelo = c("POPULAR",  "UBCF_5n","UBCF_10n","RANDOM", "IBCF", "SVDF_10","SVDF_40", "ALS"),
  RMSE = c(popular_ratings[1],
           UBCF_5nn_ratings[1],
           UBCF_10nn_ratings[1],
           random_ratings[1],
           IBCF_ratings[1],
           svdf_ratings_10[1],
           SVDF_ratings_40[1],
           ALS_ratings[1]),
  MAE = c(popular_ratings[3],
          UBCF_5nn_ratings[3],
          UBCF_10nn_ratings[3],
          random_ratings[3],
          IBCF_ratings[3],
          svdf_ratings_10[3],
          SVDF_ratings_40[3],
          ALS_ratings[3]),
  MSE = c(popular_ratings[2],
          UBCF_5nn_ratings[2],
          UBCF_10nn_ratings[2],
          random_ratings[2],
          IBCF_ratings[2],
          svdf_ratings_10[2],
          SVDF_ratings_40[2],
          ALS_ratings[2])
)

#Graficarlo (RMSE,MSE,MAE)


comparativa_larga <- pivot_longer(comparativa_errores,
                                  cols = c("RMSE", "MAE", "MSE"),
                                  names_to = "Metrica",
                                  values_to = "Valor")

Save_pdf("Comparacion_Error_Ratings", quote({
ggplot(comparativa_larga, aes(x = Modelo, y = Valor, fill = Metrica)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Comparación de errores por modelo (Ratings)",
       y = "Valor del error",
       x = "Modelo",
       fill = "Métrica") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("RMSE" = "#B00A1C", "MAE" = "#005B92", "MSE" = "#FFD5D1")) +
  theme(legend.position = "top")
}))



# Crear el data frame con las métricas (sin random)
comparativa_errores <- data.frame(
  Modelo = c("POPULAR",  "UBCF_5n","UBCF_10n", "IBCF", "SVDF_10","SVDF_40", "ALS"),
  RMSE = c(popular_ratings[1],
           UBCF_5nn_ratings[1],
           UBCF_10nn_ratings[1],
           IBCF_ratings[1],
           svdf_ratings_10[1],
           SVDF_ratings_40[1],
           ALS_ratings[1]),
  MAE = c(popular_ratings[3],
          UBCF_5nn_ratings[3],
          UBCF_10nn_ratings[3],
          IBCF_ratings[3],
          svdf_ratings_10[3],
          SVDF_ratings_40[3],
          ALS_ratings[3]),
  MSE = c(popular_ratings[2],
          UBCF_5nn_ratings[2],
          UBCF_10nn_ratings[2],
          IBCF_ratings[2],
          svdf_ratings_10[2],
          SVDF_ratings_40[2],
          ALS_ratings[2])
)

#Graficarlo (RMSE,MSE,MAE)


comparativa_larga <- pivot_longer(comparativa_errores,
                                  cols = c("RMSE", "MAE", "MSE"),
                                  names_to = "Metrica",
                                  values_to = "Valor")

Save_pdf("Comparacion_Error_ratings2", quote({
ggplot(comparativa_larga, aes(x = Modelo, y = Valor, fill = Metrica)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Comparación de errores por modelo (Ratings)",
       y = "Valor del error",
       x = "Modelo",
       fill = "Métrica") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("RMSE" = "#B00A1C", "MAE" = "#005B92", "MSE" = "#FFD5D1")) +
  theme(legend.position = "top")
}))

# ----------------------------------------- TOP N LIST
# Obtener matriz de confusión para n=5 de cada algoritmo
conf_matrix_n5_list <- lapply(eval, function(res) getConfusionMatrix(res, n = 5))

# Ver estructura
str(conf_matrix_n5_list)

# Convertir cada matriz en dataframe y añadir nombre del modelo
df_list <- lapply(names(conf_matrix_n5_list), function(modelo){
  df <- as.data.frame(conf_matrix_n5_list[[modelo]])
  df$Modelo <- modelo
  df
})

# Unir todo en un solo dataframe
df_conf <- do.call(rbind, df_list)

# Mostrar primeras filas para revisar
head(df_conf)


df_long <- pivot_longer(df_conf,
                        cols = -Modelo,
                        names_to = "Metrica",
                        values_to = "Valor")


df_roc <- df_long %>%
  filter(Metrica %in% c("TPR", "FPR")) %>%
  mutate(Metrica = tolower(Metrica))

# 2. Añadir columna 'Paso' para diferenciar múltiples puntos por modelo
df_roc <- df_roc %>%
  group_by(Modelo, Metrica) %>%
  mutate(Paso = row_number()) %>%
  ungroup()

# 3. Convertir a formato ancho para tener columnas 'tpr' y 'fpr'
df_roc_wide <- df_roc %>%
  pivot_wider(names_from = Metrica, values_from = Valor)

# 4. Graficar la curva ROC: TPR vs FPR por modelo, conectando puntos en orden de 'Paso'
Save_pdf("CurvaRoc_TPR_vs_FPR", quote({
ggplot(df_roc_wide, aes(x = fpr, y = tpr, color = Modelo, group = Modelo)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Curva ROC: TPR vs FPR por Modelo",
       x = "FPR (False Positive Rate)",
       y = "TPR (True Positive Rate)",
       color = "Modelo") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
}))

