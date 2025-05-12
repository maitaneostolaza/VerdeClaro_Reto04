################################# 
library(ggplot2)
library(dplyr)
library(tidyr)

# cargamos los datos
tickets <- readRDS("Datos\\Transformados\\tickets_Reducidos.rds")
clusteres <- readRDS("Datos\\Transformados\\df_con_clusteres.rds")
productos <- readRDS("Datos\\Originales\\maestroestr.RDS")

# para poder hacer la matriz, añadimos la COLUMNA DE CLUSTERES al data frame original
df <- left_join(tickets,clusteres,by="id_cliente_enc")
df <- df[,-c(5,6,7)]  
  
# por otro lado, agregamos una columna con el producto en general, y el producto en especifico
# ------- PRODUCTO GENERAL
df <- df %>%
  mutate(
    cod_est = as.character(cod_est),  # Asegura que tenga formato string
    producto_general = case_when(
      grepl("^010\\d+", cod_est) ~ "Fruta y verdura",
      grepl("^0125\\d+", cod_est) ~ "Legumbres y frutos secos a granel",
      grepl("^02\\d+", cod_est) ~ "Carniceria",
      grepl("^03\\d+", cod_est) ~ "Congelados",
      grepl("^04\\d+", cod_est) ~ "Charcuteria",
      grepl("^050\\d+", cod_est) ~ "Lacteos",
      grepl("^051\\d+", cod_est) ~ "Lacteos y postres",
      grepl("^052\\d+", cod_est) ~ "Huevos y leche fresco",
      grepl("^06\\d+", cod_est) ~ "Panaderia",
      grepl("^07\\d+", cod_est) ~ "Carniceria",
      grepl("^081\\d+", cod_est) ~ "Latas o mermeladas", 
      grepl("^082\\d+", cod_est) ~ "Latas",
      grepl("^083140\\d+", cod_est) ~ "Conservas",
      grepl("^083145\\d+", cod_est) ~ "Alimentacion animales",
      grepl("^0833\\d+", cod_est) ~ "Snacks",
      grepl("^084\\d+", cod_est) ~ "Snacks",
      grepl("^080\\d+", cod_est) ~ "Snacks",
      grepl("^090\\d+", cod_est) ~ "Salsas y arroces/pastas",
      grepl("^091\\d+", cod_est) ~ "Salsas y arroces/pastas", 
      grepl("^092\\d+", cod_est) ~ "Frigorifico (sin productos de origen animal)",
      grepl("^093\\d+", cod_est) ~ "Especies",
      grepl("^10\\d+", cod_est) ~ "Panaderia",
      grepl("^11\\d+", cod_est) ~ "Alcoholes",
      grepl("^1213\\d+", cod_est) ~ "Higiene",
      grepl("^13\\d+", cod_est) ~ "Higiene",
      grepl("^14\\d+", cod_est) ~ "Platos preparados",
      TRUE ~ "Otros"
    )
  )
  
# ---------- PRODUCTO ESPECIFICO (del df maestrostr)
df <- left_join(df,productos,by="cod_est")

# ------------- PARA LA CARACTERIZACIÓN DE CLUSTERES
productos_generales_menos_comprados <- df %>% 
  group_by(cluster,producto_general) %>% 
  summarise(cantidad_producto = n()) %>% 
  slice_min(order_by = cantidad_producto, n = 5, with_ties = F)

gf_productos_generales_menos_comprados<-ggplot(productos_generales_menos_comprados, aes(x = cantidad_producto, y = reorder(producto_general, cantidad_producto))) +
  geom_segment(aes(x = 0, xend = cantidad_producto, yend = producto_general), color = "#0074B5",size= 1.5) +
  geom_point(color =  "#E10A23", size = 5) +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(
    title = "Productos generales menos comprados por cluster",
    x = "Cantidad",
    y = "Productos generales"
  ) +
  theme_minimal()

ggsave("Graficos/gf_productos_generales_menos_comprados.png", plot = gf_productos_generales_menos_comprados,width = 10, height = 6, dpi = 300) 



productos_generales_mas_comprados <- df %>% 
  group_by(cluster,producto_general) %>% 
  summarise(cantidad_producto = n()) %>% 
  slice_max(order_by = cantidad_producto, n = 5, with_ties = F)

gf_productos_generales_mas_comprados<-ggplot(productos_generales_mas_comprados, aes(x = cantidad_producto, y = reorder(producto_general, cantidad_producto))) +
  geom_segment(aes(x = 0, xend = cantidad_producto, yend = producto_general), color = "#0074B5",size= 1.5) +
  geom_point(color =  "#E10A23", size = 5) +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(
    title = "Productos generales más comprados por cluster",
    x = "Cantidad",
    y = "Productos generales"
  ) +
  theme_minimal()

ggsave("Graficos/gf_productos_generales_mas_comprados.png", plot = gf_productos_generales_mas_comprados,width = 10, height = 6, dpi = 300) 


productos_menos_comprados <- df %>% 
  group_by(cluster,descripcion) %>% 
  summarise(cantidad_producto = n()) %>% 
  slice_min(order_by = cantidad_producto, n = 5, with_ties = F)

gf_productos_menos_comprados<-ggplot(productos_menos_comprados, aes(x = cantidad_producto, y = reorder(descripcion, cantidad_producto))) +
  geom_segment(aes(x = 0, xend = cantidad_producto, yend = descripcion), color = "#0074B5",size= 1.5) +
  geom_point(color =  "#E10A23", size = 5) +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(
    title = "Productos menos comprados por cluster",
    x = "Cantidad",
    y = "Productos"
  ) +
  theme_minimal()

ggsave("Graficos/gf_productos_menos_comprados.png", plot = gf_productos_menos_comprados,width = 10, height = 6, dpi = 300) 


productos_mas_comprados <- df %>% 
  group_by(cluster,descripcion) %>% 
  summarise(cantidad_producto = n()) %>% 
  slice_max(order_by = cantidad_producto, n = 5, with_ties = F)

gf_productos_mas_comprados<-ggplot(productos_mas_comprados, aes(x = cantidad_producto, y = reorder(descripcion, cantidad_producto))) +
  geom_segment(aes(x = 0, xend = cantidad_producto, yend = descripcion), color = "#0074B5",size= 1.5) +
  geom_point(color =  "#E10A23", size = 5) +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(
    title = "Productos más comprados por cluster",
    x = "Cantidad",
    y = "Productos"
  ) +
  theme_minimal()

ggsave("Graficos/gf_productos_mas_comprados.png", plot = gf_productos_mas_comprados,width = 10, height = 6, dpi = 300) 



top20_productos <- df %>%
  count(descripcion, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  pull(descripcion)  # Vector con nombres

productos_mas_comprados <- df %>% 
  group_by(cluster,descripcion) %>% 
  summarise(cantidad_producto = n())
top20_cluster <- productos_mas_comprados %>%
  filter(descripcion %in% top20_productos)

top20prods_total_por_cluster<-ggplot(top20_cluster, aes(x = cantidad_producto, y = reorder(descripcion, cantidad_producto), fill = cluster)) +
  geom_col(position = "stack") +
  labs(
    title = "Top 20 productos más vendidos y su distribución por cluster",
    x = "Cantidad total",
    y = "Producto"
  ) +
  scale_fill_manual(values = c("#E10A23",  "#F0928E", "#0074B5","#A2CBE8")) +
  theme_minimal()

ggsave("Graficos/top20prods_total_por_cluster.png", plot = top20prods_total_por_cluster,width = 10, height = 6, dpi = 300) 


producto_mas_comprado <- df %>%
  group_by(id_cliente_enc, producto_general,num_ticket) %>%
  summarise(veces_comprado = n(), .groups = "drop") %>%
  group_by(id_cliente_enc) %>%
  slice_max(order_by = veces_comprado, n = 1, with_ties = FALSE)

producto_mas_comprado <- producto_mas_comprado %>% 
  select(id_cliente_enc,producto_general)

############################### CREACION MATRIZ ################################
# VAMOS A HACER UNA MATRIZ POR CLUSTER PARA QUE LAS RECOMENDACIONES SEAN MÁS ESPECIFICAS
str(df)
cluster1 <- df %>%
  filter(cluster == 1) %>% 
  group_by(id_cliente_enc,cod_est) %>% 
  summarise(Cantidad = n())

matriz1 <- pivot_wider(cluster1,
                       names_from = cod_est,
                       values_from = Cantidad)

cluster2 <- df %>%
  filter(cluster == 2) %>% 
  group_by(id_cliente_enc,cod_est) %>% 
  summarise(Cantidad = n())

matriz2 <- pivot_wider(cluster2,
                       names_from = cod_est,
                       values_from = Cantidad)
cluster3 <- df %>%
  filter(cluster == 3) %>% 
  group_by(id_cliente_enc,cod_est) %>% 
  summarise(Cantidad = n())

matriz3 <- pivot_wider(cluster3,
                       names_from = cod_est,
                       values_from = Cantidad)

cluster4 <- df %>%
  filter(cluster == 4) %>% 
  group_by(id_cliente_enc,cod_est) %>% 
  summarise(Cantidad = n())

matriz4 <- pivot_wider(cluster4,
                       names_from = cod_est,
                       values_from = Cantidad)


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
