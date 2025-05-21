########################## RECOMENDACIONES BINARIZADAS #########################
library(rsparse)
library(dplyr)
library(tidyr)
library(tibble)

tickets <- readRDS("Datos\\Transformados\\tickets_Reducidos.rds")
objetivos <- readRDS("Datos\\Originales\\objetivos.RDS")
matriz_general <- readRDS("Datos\\Resultados\\Matriz_sinNA.rds")
productos <- readRDS("Datos\\Originales\\maestroestr.RDS")

# ------------------ BINARIZAMOS LA MATRIZ
storage.mode(matriz_general) <- "numeric"
matriz <- ifelse(matriz_general > 0,1,0)
matriz_general <- as(matriz,"sparseMatrix")

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
objetivo1_resultado <- as.data.frame(lista_1)
rownames(objetivo1_resultado)<- c("MASAS DE PIZZA")
objetivo1_resultado <- t(objetivo1_resultado)
saveRDS(objetivo1_resultado, "Datos\\Resultados\\Objetivo1_resultado.rds")

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
objetivo3_resultado1 <- rownames_to_column(objetivo3_resultado, var = "Id_cliente")
objetivo3_resultado2 <- inner_join(objetivo3_resultado1,productos,by="cod_est")

fila_faltante <- anti_join(objetivo3_resultado1,objetivo3_resultado2, by = "cod_est")
saveRDS(objetivo3_resultado2,"Datos\\Resultados\\Objetivo3_resultado.rds")
length(unique(objetivo3_resultado2$cod_est))
objetivos$objetivo3$obj

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

# -------------------------------- OBJETIVO 1: 
# para este objetivo como el modelo se ha realizado con la matriz alreves, creamos
# de nuevo las matrices de usuarios y productos para este objetivo
item_emb_obj1 <- modelo_wrmf_alreves$fit_transform(matriz_alreves)
user_emb_obj1 <- modelo_wrmf_alreves$components


comp1 <- item_emb_obj1[rownames(item_emb_obj1) == "14351005"]%*%user_emb_obj1 %>% 
  sort(decreasing=TRUE, index.return = T)
comp1_df <- data.frame(valoracion = comp1$x, usuario = colnames(user_emb_obj1)[comp1$ix])
comp1_df[1:10,]
objetivo1_resultado

# ------------- CALCULAMOS SIMILITUD ENTRE USUARIOS PARA VER QUE AUNQUE EL PREDICT
# Y LA MULTIPLICACION NO DE LO MISMO, LOS USUARIOS QUE PREDICE SE PARECEN CON LOS USUARIOS DE LA MULTIPLICACION
matriz_similarity <- user_emb_obj1 %*% item_emb_obj1




# --------------------------------------- OBJETIVO 2: 
user_emb <- modelo_wrmf$fit_transform(matriz_general) # matriz de factores de los usuarios
user_emb; dim(user_emb) #22718 cliente 10 productos

item_emb <- modelo_wrmf$components # matriz de factores de los items
item_emb
dim(item_emb)
item_emb_n <- t(modelo_wrmf$components)

comprobacion2 <- user_emb[rownames(user_emb) %in% objetivos$objetivo2$obj,] %*% item_emb[] 


verificar_predicciones <- function(matriz, users, items, prediccion, rerecomend) {
  justificacion <- users %*% items
  comprobaciones <- c()
  items_max_list <- c()
  
  for (i in rownames(matriz)) {
    usuarios <- justificacion[i, , drop = FALSE]
    
    if (!rerecomend) {
      productos_comprados <- colnames(matriz)[as.logical(matriz[i, ])]
      usuarios <- usuarios[, colnames(usuarios) %in% productos_comprados, drop = FALSE]
    }
    
    items_max <- colnames(usuarios)[max.col(usuarios)]
    prediccion_usuario <- prediccion[prediccion$Id_cliente == i, "cod_est", drop = TRUE]
    
    comprobaciones <- c(comprobaciones, prediccion_usuario == items_max)
    items_max_list <- c(items_max_list, items_max)
  }
  
  return(list(comprobaciones = comprobaciones, items_max = items_max_list))
}


matriz_obj2 <- as.matrix(matriz_obj2)
verificar_predicciones(matriz_obj2,user_emb,item_emb, objetivo2_resultado, rerecomend = F)

# mirar que items se parecen a los que ha predecido el usuario
comp2_item <- item_emb[,colnames(item_emb) %in% objetivo2_resultado$cod_est] %*% item_emb[] %>% 
  sort(decreasing = T, index.return = T)

data.frame(valoracion = comp2_item$x, item = colnames(item_emb)[comp2_item$ix])

# Función de normalización
normalize <- function(x) x / sqrt(sum(x^2))

# Normalizamos todas las columnas de item_emb
item_emb_norm <- apply(item_emb, 2, normalize)

# Ítems objetivos
target_items <- colnames(item_emb) %in% objetivo2_resultado$cod_est
filtered_items <- colnames(item_emb)[target_items]

# Inicializamos resultado
resultados <- data.frame(
  item_origen = character(),
  item_mas_parecido = character(),
  similitud = numeric(),
  stringsAsFactors = FALSE
)

# Recorremos cada ítem objetivo
for (item in filtered_items) {
  origen_vec <- item_emb_norm[, item]
  
  # Calculamos similitud coseno con todos los demás ítems
  sim <- apply(item_emb_norm, 2, function(x) sum(x * origen_vec))
  
  # Evitamos que se compare consigo mismo
  sim[item] <- NA
  
  # Obtenemos el ítem más parecido
  mejor_item <- names(which.max(sim))
  mejor_score <- max(sim, na.rm = TRUE)
  
  # Añadimos al resultado
  resultados <- rbind(resultados, data.frame(
    item_origen = item,
    item_mas_parecido = mejor_item,
    similitud = mejor_score,
    stringsAsFactors = FALSE
  ))
}

# Mostramos el resultado
print(resultados)

# ahora miramos si este cliente ha comprado estos productos -->
# añadimos a resultados el cliente:
colnames(objetivo2_resultado) <- c("cliente", "item_origen", "descripcion") 
df2 <- left_join(resultados,objetivo2_resultado, by = c("item_origen"))
matriz_general %>% filter(cliente)


# ----------------------------------- OBJETIVO 3: 
# las matrices de usuario e item son iguales que para el objetivo 2
# recomendarle de 20 productos uno de ellos al usuario
user_emb <- user_emb[!rownames(user_emb) == "7776d22e65bc7a561b34457b4effb747",]

scores <- user_emb %*% item_emb[, colnames(item_emb) %in% objetivos$objetivo3$obj]
top_items_per_user <- apply(scores, 1, function(x) names(x)[which.max(x)])
comprobacion_obj3 <- data.frame(
  cliente = rownames(user_emb),
  producto_recomendado = top_items_per_user
)

comprobacion_obj3 == objetivo3_resultado2[,1:2]


funcion_comprobacion(matriz_obj3,user_emb, item_emb, objetivo3_resultado,rerecomend = F)

# ---------------------------------- OBJETIVO 4: 

