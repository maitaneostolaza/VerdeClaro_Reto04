library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(purrr)
library(plotly)
library(ggplot2)
library(gridExtra)
library(cowplot)

# cargamos los datos ya limpios
tickets <- readRDS("Datos\\Transformados\\tickets_limpios.rds")

# CREAMOS COLUMNAS PARA EL CLUSTERING Y PARA CARACTERIZAR LOS CLIENTES
# ---------------------- PRODUCTOS POR COMPRA
cantidad_productos <- tickets %>% 
  group_by(num_ticket,id_cliente_enc) %>% 
  distinct () %>% 
  summarise(cantidad_productos=n())

# ahora la media por cliente
cantidad_productos <- cantidad_productos %>% 
  group_by(id_cliente_enc) %>% 
  summarise(media_unidades_por_compra = round(mean(cantidad_productos),0))

# ------------------------- CANTIDAD DE VECES QUE HA COMPRADO
media_veces_compra <- tickets %>% 
  group_by(id_cliente_enc) %>% 
  summarise(total_veces_que_ha_comprado = n())

# ------------------------ DIAS QUE PASAN DESDE UNA COMPRA A OTRA
numero_dias <- tickets %>%
  select(id_cliente_enc, dia,num_ticket) %>%
  distinct() %>%  # Nos quedamos con un registro por cliente y día
  arrange(id_cliente_enc, dia) %>%
  group_by(id_cliente_enc) %>%
  mutate(n_compras=n(),
         next_sale = lead(dia),
         sale_diff = as.numeric(difftime(next_sale, dia, units = "days"))
  )

# los que tienen mas de una compra borramos la ultima fila despues calculamos la media
# de los que tienen solo una compra; sin borrar nada la media sera NA
cada_cuanto_compras <- numero_dias %>% 
  group_by(id_cliente_enc) %>% 
  summarise(
    media_de_dias_pasadas_por_compras = round(mean(sale_diff, na.rm = TRUE))
  )



# GRUPO DE PRODUCTO MÁS COMPRADO
# añadimos columna del grupo de producto al que pertenece para poder sacar el 
# tipo de producto que más compra el cliente
tickets <- tickets %>%
  mutate(
    cod_est = as.character(cod_est),  # Asegura que tenga formato string
    producto_general = case_when(
      grepl("^010\\d+", cod_est) ~ "Fruta y verdura",
      grepl("^0125\\d+", cod_est) ~ "Legumbres y frutos secos",
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
      grepl("^092\\d+", cod_est) ~ "Frigorifico",
      grepl("^093\\d+", cod_est) ~ "Especies",
      grepl("^10\\d+", cod_est) ~ "Panaderia",
      grepl("^11\\d+", cod_est) ~ "Alcoholes",
      grepl("^1213\\d+", cod_est) ~ "Higiene",
      grepl("^13\\d+", cod_est) ~ "Higiene",
      grepl("^14\\d+", cod_est) ~ "Platos preparados",
      TRUE ~ "Otros"
    )
  )

producto_mas_comprado <- tickets %>%
  group_by(id_cliente_enc, producto_general,num_ticket) %>%
  summarise(veces_comprado = n(), .groups = "drop") %>%
  group_by(id_cliente_enc) %>%
  slice_max(order_by = veces_comprado, n = 1, with_ties = FALSE)

producto_mas_comprado <- producto_mas_comprado %>% 
  select(id_cliente_enc,producto_general)

# JUNTAR LAS COLUMNAS 
df_clustering <- inner_join(cantidad_productos,cada_cuanto_compras, by = "id_cliente_enc")
df_clustering <- inner_join(df_clustering,media_veces_compra, by = "id_cliente_enc") 
# las filas que son NA es porque han comprado una vez 

# GUARDAMOS 
saveRDS(df_clustering, file = "Datos/Transformados/df_clustering_entero.rds")


############################### CLUSTERING #####################################
# cargamos los datos: 
df_entero <- readRDS("Datos\\Transformados\\df_clustering_entero.rds")

# LOS CLIENTES QUE HAN COMPRADO UNA VEZ APARECEN COMO NA EN LA COLUMNA DE CADA CUANTO
# COMPRAS POR LO QUE LOS TRANFORMAMOS A 0 PARA PODER HACER EL METODO DEL CODO Y EL CLUSTERING
df_entero <- df_entero %>% 
  mutate(media_de_dias_pasadas_por_compras = if_else(is.nan(media_de_dias_pasadas_por_compras), 0, media_de_dias_pasadas_por_compras))

# nos quedamos solo con las columnas numericas para el clustering: 
df_clustering <- df_entero[,-1]


# metodo del codo para elegir la K 
tot_withinss <- map_dbl(1:10, function(k) {
  set.seed(12); model <- kmeans(x = df_clustering, centers = k)
  model$tot.withinss
})

# Crear dataframe con resultados
elbow_df <- data.frame(k = 1:10, tot_withinss = tot_withinss)

# Graficar curva del método del codo

metodo_codo <- plot_ly(elbow_df, x = ~k, y = ~tot_withinss, type = "scatter",
                       mode = "lines+markers",
                       line = list(color = "#8c94a4"),  # Color de la línea
                       marker = list(color = "#0d7b7c")) %>%
  add_trace(x = 4, y = elbow_df$tot_withinss[elbow_df$k == 4],
            type = "scatter",
            mode = "markers",
            marker = list(color = "red", size = 10)) %>%
  layout(
    title = "Método del Codo para Determinar k",
    xaxis = list(title = "Número de Clusters (k)", tickvals = 1:10),
    yaxis = list(title = "Suma de Distancias Intra-cluster"),
    showlegend = FALSE  # Esto es para ocultar la leyenda
  )

# elegimos el k = 4 porque baja significativamente hasta ese numero

## K MEANS CON EL K=4
# HACEMOS EL K MEANS CON EL K 4
set.seed(7); 
KMEANS <-kmeans(df_clustering,4)

#valores de los clusters
cluster_KM <- KMEANS$cluster
table(cluster_KM)
# añadirle al df
df_clustering$cluster_KM <- as.factor(KMEANS$cluster)

# Centroides
centroides_kmean <- as.data.frame(KMEANS$centers) %>%
  mutate(cluster_KM = as.factor(as.character(1:length(unique(df_clustering$cluster_KM)))))

# Graficar centroides
plot_ly(data = centroides_kmean,
        x = ~media_unidades_por_compra  ,
        y = ~media_de_dias_pasadas_por_compras ,
        z = ~total_veces_que_ha_comprado ,
        type = "scatter",
        color = ~cluster_KM,
        colors = c( 
          "#E10A23",  
          "#A2CBE8", 
          "#005B92",
          "#F0928E"
        ))

# Graficar todos los puntos y el cluster al que pertenecen
grafico_distribucion_centroides <- plot_ly(data = df_clustering,
        x = ~media_unidades_por_compra,
        y = ~total_veces_que_ha_comprado,
        type = "scatter",
        color = ~cluster_KM,
        colors = c( 
          "#E10A23",  
          "#A2CBE8", 
          "#005B92",
          "#F0928E"
        ))


#suma de distancias intra cluster
intra_cluster_KM <- KMEANS$tot.withinss

# suma de distancias inter cluster
inter_clusterKM <- KMEANS$betweenss

# añadimos al df original
df_entero$cluster <- df_clustering$cluster_KM

saveRDS(df_entero, file = "Datos/Transformados/df_con_clusteres.rds")

# ------------------------------ CENTROIDES
# calculamos las medias por cluster para poder caracterizar los clusteres creados
media_clusteres <- df_entero %>%
  mutate(cluster = as.factor(as.character(cluster))) %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))

productos_menos_comprados <- df_entero %>% 
  group_by(cluster,producto_general) %>% 
  summarise(cantidad_producto = n()) %>% 
  slice_min(order_by = cantidad_producto, n = 3, with_ties = TRUE)

productos_mas_comprados <- df_entero %>% 
  group_by(cluster,producto_general) %>% 
  summarise(cantidad_producto = n()) %>% 
  slice_max(order_by = cantidad_producto, n = 3, with_ties = TRUE)


# --------------------------- GRAFICOS 
# paleta de colores
paletteer_c("ggthemes::Red-Green Diverging", 30)

# grafico de barras por columna  
media_unidades <- media_clusteres[,c(1,2)]
media_dias <- media_clusteres[,c(1,3)]
total_compra <- media_clusteres[c(1,4)]

# Crear gráfico de barras facetado
media_unidades_gf <- ggplot(media_unidades, aes(x = cluster, 
                           y = media_unidades_por_compra,
                           fill = cluster)) +
  geom_col() +
  labs(title = "Media de unidades por compra",
       x = "clusteres", y = "unidades medias") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c(
    "1" = "steelblue",  # azul
    "2" = "grey",  # naranja
    "3" = "green4",  # verde
    "4" = "#d62728"   # rojo
  )) 

media_diasgf <- ggplot(media_dias, aes(x = cluster, 
                                           y = media_de_dias_pasadas_por_compras,
                                           fill = cluster)) +
  geom_col() +
  labs(title = "Media de dias transcurridas por compra",
       x = "clusteres", y = "media de dias") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c(
    "1" = "steelblue",  # azul
    "2" = "grey",  # naranja
    "3" = "green4",  # verde
    "4" = "#d62728"   # rojo
  ))  

total_comprasgf <- ggplot(total_compra, aes(x = cluster, 
                           y = total_veces_que_ha_comprado,
                           fill = cluster)) +
  geom_col() +
  labs(title = "Total veces que han comprado",
       x = "clusteres", y = "Total compras") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c(
    "1" = "steelblue",  # azul
    "2" = "grey",  # gris
    "3" = "green4",  # verde
    "4" = "#d62728"   # rojo
  ))         

# Desactivar leyenda interna en cada gráfico
media_unidades_gf_nolegend <- media_unidades_gf + theme(legend.position = "none")
media_diasgf_nolegend     <- media_diasgf     + theme(legend.position = "none")
total_comprasgf_nolegend  <- total_comprasgf  + theme(legend.position = "none")


# Extraer la leyenda desde uno de los originales (con leyenda activa)
shared_legend <- get_legend(media_unidades_gf + theme(legend.position = "right",
                                                      legend.direction = "horizontal"))

# Combinar
final_plot <- plot_grid(
  plot_grid(media_unidades_gf_nolegend, media_diasgf_nolegend,
            total_comprasgf_nolegend, ncol = 3),
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.1)
)


print(final_plot)
ggsave("Graficos/Analisis_exporatorio.png", plot = final_plot,width = 10, height = 6, dpi = 300) 
