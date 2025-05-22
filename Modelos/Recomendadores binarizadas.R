########################## RECOMENDACIONES BINARIZADAS #########################
library(rsparse)
library(dplyr)
library(tidyr)
library(tibble)
library(recommenderlab)
library(dplyr)
library(reshape2)

tickets <- readRDS("Datos\\Transformados\\tickets_Reducidos.rds")
objetivos <- readRDS("Datos\\Originales\\objetivos.RDS")
matriz_general <- readRDS("Datos\\Resultados\\Matriz_sinNA.rds")
productos <- readRDS("Datos\\Originales\\maestroestr.RDS")

# ------------------ BINARIZAMOS LA MATRIZ
storage.mode(matriz_general) <- "numeric"
matriz_general <- as(matriz_general, "realRatingMatrix")
matriz_binarizada <- binarize(matriz_general, minRating = 1)
matriz_general <- as(matriz_binarizada,"sparseMatrix")

# para el objetivo 1 cambiamos filas por columnas
matriz_alreves <- t(matriz_general)


saveRDS(matriz_general, "Datos\\Resultados\\matriz_general.rds")
saveRDS(matriz_alreves, "Datos\\Resultados\\matriz_alreves.rds")

matriz_general <- readRDS("Datos\\Resultados\\matriz_general.rds")
matriz_alreves <- readRDS("Datos\\Resultados\\matriz_alreves.rds")

# -- MODELO: 
set.seed(12)
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
item_emb_obj1_n <- t(item_emb_obj1)

# ------------- vamos a ver si los clientes han comprado otros productos parecidos a la masa de pizza
# calcular productos similares a la masa de pizza
items_parecidos_masa_pizza <- item_emb_obj1[rownames(item_emb_obj1) == "14351005"]%*%item_emb_obj1_n %>% 
  sort(decreasing = T,index.return = T)
productos_masa_pizza_parecido <- colnames(item_emb_obj1_n)[items_parecidos_masa_pizza$ix[1:15]]
productos_masa_pizza_parecido <- c(productos_masa_pizza_parecido,"08100110")
productos_masa_pizza_parecido <- productos %>% filter(cod_est%in%productos_masa_pizza_parecido)
productos_definitivos <- productos_masa_pizza_parecido %>% 
  filter(descripcion %in% c("CHAMPIÑONES","LONCHAS PORCION INTERNACIONAL",
                            "LONCHAS PORCION NACIONAL","QUESO RALLADO MOZZARELLA",
                            "MEXICANA MASAS", "PECHUGA DE PAVO",
                            "TOMATE TRITURADO HASTA 500G"))

# ver si a los clientes que se le ha recomendado la masa de pizza efectivamente
# han comprado los productos que mas se parecen
COMPROBACION_DEFINITIVA_OBJ1 <- as.matrix(matriz_general[rownames(matriz_general) %in% objetivo1_resultado, colnames(matriz_general) %in% productos_definitivos$cod_est])
COMPROBACION_DEFINITIVA_OBJ1 <- as.data.frame(COMPROBACION_DEFINITIVA_OBJ1)
colnames(COMPROBACION_DEFINITIVA_OBJ1) <- c( "LONCHAS PORCION INTERNACIONAL",	
                                            "MEXICANA MASAS", "QUESO RALLADO MOZZARELLA",
                                            "TOMATE TRITURADO HASTA 500G", 
                                            "CHAMPIÑONES")
COMPROBACION_DEFINITIVA_OBJ1 <- ifelse(COMPROBACION_DEFINITIVA_OBJ1 == T, 1, 0)

# guardamos rds: 
saveRDS(COMPROBACION_DEFINITIVA_OBJ1,"Datos\\Resultados\\comprobacion_objetivo1.rds")

# --------------------------------------- OBJETIVO 2: 
user_emb <- modelo_wrmf$fit_transform(matriz_general) # matriz de factores de los usuarios
user_emb; dim(user_emb) #22718 cliente 10 productos

item_emb <- modelo_wrmf$components # matriz de factores de los items
item_emb
dim(item_emb)
item_emb_n <- t(modelo_wrmf$components)

comprobacion2 <- user_emb[rownames(user_emb) %in% objetivos$objetivo2$obj,] %*% item_emb[] 




# mirar que items se parecen a los que ha predecido el usuario (cogemos los 15 productos mas similares)
colnames(objetivo2_resultado) <- c("cliente", "item_origen", "descripcion")
# Función de normalización
normalize <- function(x) x / sqrt(sum(x^2))

# Normalizamos todas las columnas de item_emb
item_emb_norm <- apply(item_emb, 2, normalize)

# Ítems objetivos
target_items <- colnames(item_emb) %in% objetivo2_resultado$item_origen
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
  mejor_item <- names(sort(sim, decreasing = T))[1:20]
  
  # Añadimos al resultado
  resultados <- rbind(resultados, data.frame(
    item_origen = item,
    item_mas_parecido = mejor_item,
    stringsAsFactors = FALSE
  ))
}

# Mostramos el resultado
print(resultados)

# ahora miramos si este cliente ha comprado estos productos -->
# añadimos a resultados el cliente:
 
df2 <- left_join(resultados,objetivo2_resultado, by = c("item_origen"))
comprobacion_items <- as.matrix(matriz_general[rownames(matriz_general) %in% unique(df2$cliente), colnames(matriz_general) %in% df2$item_mas_parecido])

# Creamos una lista donde cada elemento es un cliente con los productos comprados
lista_compras <- apply(comprobacion_items, 1, function(x) {
  names(x)[x == 1]
})

# Convertimos en lista nombrada (por si pierde nombres)
names(lista_compras) <- rownames(comprobacion_items)
print(lista_compras)

# lo convertimos a data frame: 

# Convertimos la lista en formato largo
df_largo <- stack(lista_compras)
colnames(df_largo) <- c("producto", "usuario")

# Formato ancho con 1 y 0
df_binario <- dcast(df_largo, usuario ~ producto, fun.aggregate = length, fill = 0)

# Resultado
print(df_binario)

saveRDS(df_binario, "Datos\\Resultados\\comprobacion_obj2.rds")


# ----------------------------------- OBJETIVO 3: 
# las matrices de usuario e item son iguales que para el objetivo 2
# recomendarle de 20 productos uno de ellos al usuario
user_emb_obj3 <- user_emb[!rownames(user_emb) == "7776d22e65bc7a561b34457b4effb747",]

scores <- user_emb %*% item_emb[, colnames(item_emb) %in% objetivos$objetivo3$obj]
top_items_per_user <- apply(scores, 1, function(x) names(x)[which.max(x)])
comprobacion_obj3 <- data.frame(
  cliente = rownames(user_emb),
  producto_recomendado = top_items_per_user
)

# elegimos 10 clientes para comprobar si anteriormente han comprado ese producto
# que les ha recomendado el algoritmo
similarity <- t(item_emb[]) %*% item_emb[, colnames(item_emb) == "05030101", drop = F]

# Asegúrate de que las filas de item_emb tienen nombres de ítems
item_names <- colnames(item_emb)  # o colnames(item_emb) si la orientación es distinta

# Convertimos la matriz de similitud a vector y le asignamos nombres
similaridades <- as.vector(similarity)
names(similaridades) <- item_names

# Ordenamos las similitudes de mayor a menor
similares_ordenados <- sort(similaridades, decreasing = TRUE)

# Mostrar los 10 ítems más similares
productos_similares_obj3 <- head(similares_ordenados, 10)

# mirar si el cliente ha comprado alguno de estos productos: 
clientes_comprar_obj3_producto_analizar <- rownames(objetivo3_resultado %>% filter(cod_est == "05030101"))
comprobacion_obj3 <- as.matrix(matriz_general[rownames(matriz_general) %in% clientes_comprar_obj3_producto_analizar, colnames(matriz_general) %in% c("05030101",
            "05040180", "12650101", "05040181", "09090503", "09070103", "12410315", "10080301", "10200101", "12670111")])

comprobacion_obj31 <- ifelse(comprobacion_obj3 == T, 1, 0)
comprobacion_obj32 <- comprobacion_obj31[rowSums(comprobacion_obj31) >= 7, ]

saveRDS(comprobacion_obj32, "Datos\\Resultados\\Comprobacion_objetivo3.rds")

# ---------------------------------- OBJETIVO 4: 
biderketa4 <- user_emb[rownames(user_emb) %in% objetivos$objetivo4$obj,] %*% item_emb[] 
# Obtener los nombres de los ítems (columnas de item_emb)
item_names <- colnames(item_emb)

# Para cada usuario (fila), obtener el nombre del ítem con mayor score
mejor_item_por_usuario <- apply(biderketa4, 1, function(fila) {
  item_names[which.max(fila)]
})

# Resultado: named vector, donde nombre = usuario, valor = ítem más valorado
as.data.frame(mejor_item_por_usuario)

# mirar items de ultima compra --> calcular similaridad
# mirar si los items recomendados los han comprado anteriormente alguna vez
cliente1 <- as.data.frame(ultimas_compras[1])
comprobacion_cliente1_obj4 <- inner_join(cliente1, productos, by = c("fe234baf66f020e01feb5253dfb398f0.cod_est" ="cod_est"))
comprobacion_cliente1_obj4 <- comprobacion_cliente1_obj4 %>% mutate(asociacion = c(1,1,1,1,1,1,1,1,0,1,1,1,1,0,0,0,1,1,1,1,1,0,1,0,1,1,1,0,0,1,0,1,1))
comprobacion_cliente1_obj4 <- comprobacion_cliente1_obj4[,-2]
colnames(comprobacion_cliente1_obj4) <- c("PLATANO DE CANARIAS_cliente 1","descripcion", "asociacion")

cliente2 <- as.data.frame(ultimas_compras[2])
comprobacion_cliente2_obj4 <- inner_join(cliente2, productos, by = c("d85ceefcf666f2b27e3e1e1252e5a1ac.cod_est" ="cod_est"))
comprobacion_cliente2_obj4 <- comprobacion_cliente2_obj4 %>% mutate(asociacion = c(1,1,1,1,1,1,1,1,0,1,1,1,0,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1))
comprobacion_cliente2_obj4 <- comprobacion_cliente2_obj4[,-2]
colnames(comprobacion_cliente2_obj4) <- c("BROCOLI_cliente 2","descripcion", "asociacion")

cliente3 <- as.data.frame(ultimas_compras[3])
comprobacion_cliente3_obj4 <- inner_join(cliente3, productos, by = c("a8a16b0b76cb14783348e920a59588ed.cod_est" ="cod_est"))
comprobacion_cliente3_obj4 %>% mutate(asociacion = c(1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,1))
colnames(comprobacion_cliente3_obj4) <- c("LECHE_SEMIDESNATADA_cliente 3","descripcion", "asociacion")
dim(comprobacion_cliente1_obj4)
dim(comprobacion_cliente2_obj4)
dim(comprobacion_cliente3_obj4)

comprobacion_objetivo4 <- list(comprobacion_cliente1_obj4,
     comprobacion_cliente2_obj4,
     comprobacion_cliente3_obj4)
names(comprobacion_objetivo4) <- c("fe234baf66f020e01feb5253dfb398f0",
                                   "d85ceefcf666f2b27e3e1e1252e5a1ac",
                                   "a8a16b0b76cb14783348e920a59588ed")

saveRDS(comprobacion_objetivo4,"Datos\\Resultados\\Comprobacion_objetivo4.rds")
