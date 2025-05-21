library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)
library(purrr)
library(DT)
library(lubridate)
library(scales)

options(scipen = 999)

# ---------------- CARGA DE DATOS ---------------- #
df_entero <- readRDS("Datos/Transformados/df_con_clusteres.rds")
tickets <- readRDS("Datos/Transformados/tickets_limpios.rds")
productos <- readRDS("Datos/Originales/maestroestr.RDS")
tickets_enc <- readRDS("Datos\\Originales\\tickets_enc.RDS")
eroski_palette_extended <- c(
  "#E10A23", "#B00A1C", "#F0928E", "#FFD5D1",
  "#0074B5", "#005B92", "#A2CBE8", "#FADED6"
)

datos <- tickets_enc
colnames(datos) <- c("fecha", "ticket", "cod_est", "id_cliente")
datos$fecha <- ymd(datos$fecha)

datos_enriquecidos <- left_join(datos, productos, by = "cod_est")

# Unimos clusters al df de tickets
df <- left_join(tickets, df_entero, by = "id_cliente_enc")
df <- df[,-c(5,6,7)]

# Clasificación de productos generales
df <- df %>%
  mutate(
    cod_est = as.character(cod_est),
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
    ),
    fecha = ymd(dia),
    dia_semana = wday(dia, label = TRUE, abbr = FALSE, week_start = 1),
    dia_semana = factor(dia_semana, levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))
  )


df <- left_join(df, productos, by = "cod_est")

# 1. Media por cluster
media_clusteres <- df_entero %>%
  mutate(cluster = as.factor(as.character(cluster))) %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")


media_unidades <- media_clusteres[, c(1, 2)]
media_dias <- media_clusteres[, c(1, 3)]
total_compra <- media_clusteres[, c(1, 4)]

media_unidades_gf <- ggplot(media_unidades, aes(x = cluster, y = media_unidades_por_compra, fill = cluster)) +
  geom_col() +
  labs(title = "Media de unidades por compra", x = "Cluster", y = "Unidades medias") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("1" = "steelblue", "2" = "grey", "3" = "green4", "4" = "#d62728"))

media_diasgf <- ggplot(media_dias, aes(x = cluster, y = media_de_dias_pasadas_por_compras, fill = cluster)) +
  geom_col() +
  labs(title = "Media de días transcurridos por compra", x = "Cluster", y = "Media de días") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("1" = "steelblue", "2" = "grey", "3" = "green4", "4" = "#d62728"))

total_comprasgf <- ggplot(total_compra, aes(x = cluster, y = total_veces_que_ha_comprado, fill = cluster)) +
  geom_col() +
  labs(title = "Total veces que han comprado", x = "Cluster", y = "Total compras") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("1" = "steelblue", "2" = "grey", "3" = "green4", "4" = "#d62728"))

media_unidades_gf_nolegend <- media_unidades_gf + theme(legend.position = "none")
media_diasgf_nolegend <- media_diasgf + theme(legend.position = "none")
total_comprasgf_nolegend <- total_comprasgf + theme(legend.position = "none")
shared_legend <- get_legend(media_unidades_gf + theme(legend.position = "right", legend.direction = "horizontal"))

final_plot <- plot_grid(
  plot_grid(media_unidades_gf_nolegend, media_diasgf_nolegend, total_comprasgf_nolegend, ncol = 3),
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.1)
)

# 2. Otros gráficos por cluster
productos_generales_mas_comprados <- df %>%
  group_by(cluster, producto_general) %>%
  summarise(cantidad_producto = n()) %>%
  slice_max(order_by = cantidad_producto, n = 5, with_ties = FALSE)

gf_productos_generales_mas_comprados <- ggplot(productos_generales_mas_comprados, aes(x = cantidad_producto, y = reorder(producto_general, cantidad_producto))) +
  geom_segment(aes(x = 0, xend = cantidad_producto, yend = producto_general), color = "#0074B5", size = 1.5) +
  geom_point(color = "#E10A23", size = 5) +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(title = "Productos generales más comprados por cluster", x = "Cantidad", y = "Producto general") +
  theme_minimal()

productos_generales_menos_comprados <- df %>%
  group_by(cluster, producto_general) %>%
  summarise(cantidad_producto = n()) %>%
  slice_min(order_by = cantidad_producto, n = 5, with_ties = FALSE)

gf_productos_generales_menos_comprados <- ggplot(productos_generales_menos_comprados, aes(x = cantidad_producto, y = reorder(producto_general, cantidad_producto))) +
  geom_segment(aes(x = 0, xend = cantidad_producto, yend = producto_general), color = "#0074B5", size = 1.5) +
  geom_point(color = "#E10A23", size = 5) +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(title = "Productos generales menos comprados por cluster", x = "Cantidad", y = "Producto general") +
  theme_minimal()

productos_mas_comprados <- df %>%
  group_by(cluster, descripcion) %>%
  summarise(cantidad_producto = n()) %>%
  slice_max(order_by = cantidad_producto, n = 5, with_ties = FALSE)

gf_productos_mas_comprados <- ggplot(productos_mas_comprados, aes(x = cantidad_producto, y = reorder(descripcion, cantidad_producto))) +
  geom_segment(aes(x = 0, xend = cantidad_producto, yend = descripcion), color = "#0074B5", size = 1.5) +
  geom_point(color = "#E10A23", size = 5) +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(title = "Productos más comprados por cluster", x = "Cantidad", y = "Producto") +
  theme_minimal()

productos_menos_comprados <- df %>%
  group_by(cluster, descripcion) %>%
  summarise(cantidad_producto = n()) %>%
  slice_min(order_by = cantidad_producto, n = 5, with_ties = FALSE)

gf_productos_menos_comprados <- ggplot(productos_menos_comprados, aes(x = cantidad_producto, y = reorder(descripcion, cantidad_producto))) +
  geom_segment(aes(x = 0, xend = cantidad_producto, yend = descripcion), color = "#0074B5", size = 1.5) +
  geom_point(color = "#E10A23", size = 5) +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(title = "Productos menos comprados por cluster", x = "Cantidad", y = "Producto") +
  theme_minimal()

top20_productos <- df %>%
  count(descripcion, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  pull(descripcion)

top20_cluster <- df %>%
  group_by(cluster, descripcion) %>%
  summarise(cantidad_producto = n()) %>%
  filter(descripcion %in% top20_productos)

top20prods_total_por_cluster <- ggplot(top20_cluster, aes(x = cantidad_producto, y = reorder(descripcion, cantidad_producto), fill = cluster)) +
  geom_col(position = "stack") +
  labs(title = "Top 20 productos más vendidos y su distribución por cluster", x = "Cantidad total", y = "Producto") +
  scale_fill_manual(values = c("#E10A23", "#F0928E", "#0074B5", "#A2CBE8")) +
  theme_minimal()
# --- UI ---
ui <- fluidPage(
  titlePanel("App con Pestañas y Desplegables"),
  tabsetPanel(
    tabPanel("Objetivos",
             sidebarLayout(
               sidebarPanel(
                 selectInput("objetivo_select", "Selecciona un objetivo:", choices = c("Objetivo 1", "Objetivo 2", "Objetivo 3", "Objetivo 4")),
                 
                 # Inputs condicionales por objetivo
                 conditionalPanel(condition = "input.objetivo_select == 'Objetivo 1'",
                                  selectInput("grafico1_select", "Selecciona un gráfico:", choices = c("Gráfico A1", "Gráfico A2", "Gráfico B1", "Gráfico B2"))
                 ),
                 conditionalPanel(condition = "input.objetivo_select == 'Objetivo 2'",
                                  selectInput("grafico2_select", "Selecciona un gráfico:", choices = c("Gráfico 2A", "Grafico 2B"))
                 ),
                 
                 conditionalPanel(condition = "input.objetivo_select == 'Objetivo 3'",
                                  selectInput("grafico3_select", "Selecciona un gráfico:", choices = c("Gráfico 3A", "Gráfico 3B"))
                 ),
                 conditionalPanel(condition = "input.objetivo_select == 'Objetivo 4'",
                                  selectInput("grafico4_select", "Selecciona un gráfico:", choices = c("Gráfico 4A", "Gráfico 4B"))
                 )
               ),
               mainPanel(
                 conditionalPanel(condition = "input.objetivo_select == 'Objetivo 1'", plotOutput("plot_obj1")),
                 conditionalPanel(condition = "input.objetivo_select == 'Objetivo 2'", plotlyOutput("plot_obj2")),
                 conditionalPanel(condition = "input.objetivo_select == 'Objetivo 3'", plotOutput("plot_obj3")),
                 conditionalPanel(condition = "input.objetivo_select == 'Objetivo 4'", plotOutput("plot_obj4"))
               )
             )),
    
    tabPanel("Análisis Exploratorio",
             sidebarLayout(
               sidebarPanel(
                 selectInput("grafico_seleccionado", "Selecciona un gráfico:",
                             choices = c(
                               "Top 10 Productos" = "gx_top10_prod",
                               "Tickets por Día" = "gx_tickets_por_dia",
                               "Productos por Día de la Semana" = "gx_prod_por_dia_semana",
                               "Productos por Semana" = "gx_prod_por_semana",
                               "Top 10 Clientes" = "gx_top10_clientes",
                               "Top 5 por Día con Cluster" = "gx_top5_por_dia_con_cluster"
                             )
                 )
               ),
               mainPanel(
                 plotOutput("grafico_dinamico", height = "600px")
               )
             )),
    
    tabPanel("Clusters",
             sidebarLayout(
               sidebarPanel(
                 selectInput("cluster_plot_select", "Selecciona gráfico a visualizar:", choices = c(
                   "Resumen de medias" = "medias",
                   "Productos más comprados" = "prod_mas",
                   "Productos menos comprados" = "prod_menos",
                   "Productos generales más comprados" = "prod_gen_mas",
                   "Productos generales menos comprados" = "prod_gen_menos",
                   "Top 20 por cluster" = "top20"
                 ))
               ),
               mainPanel(
                 plotOutput("plot_clusters_dinamico", height = "600px")
               )
             )),
    tabPanel("Centroides",
             sidebarLayout(
               sidebarPanel(
                 selectInput("cluster", "Selecciona un cluster", choices = NULL)
               ),
               mainPanel(
                 DTOutput("tabla_centroides_clusteres")
               )
             )),
    
    tabPanel("Gráficos para Modelos",
             sidebarLayout(
               sidebarPanel(
                 selectInput("grafico_modelo_select", "Selecciona un gráfico:", choices = c(
                   "Método del codo" = "codo",
                   "Distribución por cluster" = "distribucion",
                   "Centroides en 3D" = "centroides"
                 ))
               ),
               mainPanel(
                 plotlyOutput("grafico_modelos")
               )
             ))
  )
)

server <- function(input, output, session) {
  
  output$grafico_dinamico <- renderPlot({
    grafico <- input$grafico_seleccionado
    
    plot_obj <- switch(grafico,
                       "gx_top10_prod" = {
                         datos_enriquecidos %>%
                           count(descripcion, sort = TRUE) %>%
                           slice_max(n, n = 10) %>%
                           ggplot(aes(x = reorder(descripcion, n), y = n)) +
                           geom_col(fill = eroski_palette_extended[1]) +
                           coord_flip() +
                           labs(title = "Top 10 productos más vendidos", x = NULL, y = "Unidades") +
                           theme_minimal()
                       },
                       
                       "gx_tickets_por_dia" = {
                         datos %>%
                           group_by(fecha) %>%
                           summarise(tickets = n_distinct(ticket)) %>%
                           ggplot(aes(x = fecha, y = tickets)) +
                           geom_line(color = eroski_palette_extended[5]) +
                           labs(title = "Número de tickets por día", x = "Fecha", y = "Tickets") +
                           theme_minimal()
                       },
                       
                       "gx_prod_por_dia_semana" = {
                         datos %>%
                           mutate(
                             dia_semana = wday(fecha, label = TRUE, abbr = FALSE, week_start = 1),
                             dia_semana = factor(dia_semana,
                                                 levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))
                           ) %>%
                           filter(dia_semana != "domingo") %>%
                           count(dia_semana) %>%
                           ggplot(aes(x = dia_semana, y = n)) +
                           geom_col(fill = eroski_palette_extended[7]) +
                           labs(
                             title = "Productos vendidos por día de la semana (sin domingos)",
                             x = NULL, y = "Unidades"
                           ) +
                           scale_y_continuous(labels = label_comma()) +
                           theme_minimal()
                       },
                       
                       "gx_prod_por_semana" = {
                         datos %>%
                           mutate(semana = floor_date(fecha, "week", week_start = 1)) %>%
                           count(semana) %>%
                           ggplot(aes(x = semana, y = n)) +
                           geom_col(fill = eroski_palette_extended[7], color = eroski_palette_extended[6]) +
                           labs(
                             title = "Productos vendidos por semana del año",
                             x = "Semana", y = "Unidades"
                           ) +
                           scale_x_date(labels = date_format("%U"), breaks = "1 week") +
                           theme_minimal()
                       },
                       
                       "gx_top10_clientes" = {
                         datos %>%
                           count(id_cliente, name = "compras") %>%
                           slice_max(compras, n = 10) %>%
                           ggplot(aes(x = reorder(id_cliente, compras), y = compras)) +
                           geom_col(fill = eroski_palette_extended[2]) +
                           coord_flip() +
                           labs(
                             title = "Top 10 clientes con más unidades compradas",
                             x = "ID Cliente", y = "Unidades"
                           ) +
                           theme_minimal()
                       },
                       
                       "gx_top5_por_dia_con_cluster" = {
                         df %>%
                           filter(dia_semana != "domingo") %>%
                           group_by(dia_semana, producto_general) %>%
                           summarise(cantidad_vendida = n(), .groups = "drop") %>%
                           group_by(dia_semana) %>%
                           slice_max(order_by = cantidad_vendida, n = 5) %>%
                           ungroup() %>%
                           ggplot(aes(x = dia_semana, y = cantidad_vendida, fill = producto_general)) +
                           geom_col() +
                           coord_flip() +
                           labs(
                             title = "Top 5 productos más vendidos por día de la semana (sin domingos)",
                             x = "Día de la semana",
                             y = "Cantidad vendida",
                             fill = "Producto general"
                           ) +
                           scale_fill_manual(values = eroski_palette_extended) +
                           theme_minimal() +
                           theme(axis.text.x = element_text(angle = 45, hjust = 1))
                       }
                       
    )
    
    plot_obj
  })
  
  
  # --- Preprocesamiento para clustering ---
  
  # Reemplazar NaN por 0 en la variable de días por compras
  df_entero <- df_entero %>%
    mutate(media_de_dias_pasadas_por_compras = if_else(is.nan(media_de_dias_pasadas_por_compras), 0, media_de_dias_pasadas_por_compras))
  
  # Quedarse solo con columnas numéricas para clustering (omitimos la primera que probablemente es un ID)
  df_clustering <- df_entero[, -1]
  
  # Método del codo
  tot_withinss <- map_dbl(1:10, function(k) {
    set.seed(12)
    model <- kmeans(x = df_clustering, centers = k)
    model$tot.withinss
  })
  
  elbow_df <- data.frame(k = 1:10, tot_withinss = tot_withinss)
  
  metodo_codo <- plot_ly(elbow_df, x = ~k, y = ~tot_withinss, type = "scatter",
                         mode = "lines+markers",
                         line = list(color = "#8c94a4"),
                         marker = list(color = "#0d7b7c")) %>%
    add_trace(x = 4, y = elbow_df$tot_withinss[elbow_df$k == 4],
              type = "scatter", mode = "markers",
              marker = list(color = "red", size = 10)) %>%
    layout(
      title = "Método del Codo para Determinar k",
      xaxis = list(title = "Número de Clusters (k)", tickvals = 1:10),
      yaxis = list(title = "Suma de Distancias Intra-cluster"),
      showlegend = FALSE
    )
  
  # KMeans con k = 4
  set.seed(7)
  KMEANS <- kmeans(df_clustering, 4)
  df_clustering$cluster_KM <- as.factor(KMEANS$cluster)
  
  # Centroides
  centroides_kmean <- as.data.frame(KMEANS$centers) %>%
    mutate(cluster_KM = as.factor(as.character(1:length(unique(df_clustering$cluster_KM)))))
  
  # Gráfico 3D de centroides
  plot_centroides_kmeans <- plot_ly(data = centroides_kmean,
                                    x = ~media_unidades_por_compra,
                                    y = ~media_de_dias_pasadas_por_compras,
                                    z = ~total_veces_que_ha_comprado,
                                    type = "scatter3d",
                                    mode = "markers",
                                    color = ~cluster_KM,
                                    colors = c("#E10A23", "#A2CBE8", "#005B92", "#F0928E"))
  
  # Distribución de todos los puntos por cluster
  grafico_distribucion_centroides <- plot_ly(data = df_clustering,
                                             x = ~media_unidades_por_compra,
                                             y = ~total_veces_que_ha_comprado,
                                             type = "scatter",
                                             color = ~cluster_KM,
                                             colors = c("#E10A23", "#A2CBE8", "#005B92", "#F0928E"))
  
  # Objetivo 1
  grafico1_list <- list(
    "Gráfico A1" = ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + ggtitle("Objetivo 1 - A1"),
    "Gráfico A2" = ggplot(mtcars, aes(x = wt, y = qsec)) + geom_point(color = "blue") + ggtitle("Objetivo 1 - A2"),
    "Gráfico B1" = ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + geom_boxplot() + ggtitle("Objetivo 1 - B1"),
    "Gráfico B2" = ggplot(mtcars, aes(x = factor(cyl), y = hp)) + geom_boxplot(fill = "orange") + ggtitle("Objetivo 1 - B2")
  )
  output$plot_obj1 <- renderPlot({ grafico1_list[[input$grafico1_select]] })
  
  # Objetivo 2
  output$plot_obj2 <- renderPlotly({
    if (input$grafico2_select == "Gráfico 2A") {
      ggplotly(ggplot(mtcars, aes(x = mpg)) +
                 geom_histogram(bins = 10, fill = "skyblue") +
                 ggtitle("Objetivo 2 - Gráfico A"))
    } else if (input$grafico2_select == "Grafico 2B") {
      ggplotly(ggplot(mtcars, aes(x = hp, y = mpg)) +
                 geom_point(color = "red") +
                 ggtitle("Objetivo 2 - Gráfico B"))
    }
  })
  
  
  # Objetivo 3
  grafico3_list <- list(
    "Gráfico 3A" = ggplot(mtcars, aes(x = gear, y = mpg)) + geom_bar(stat = "identity") + ggtitle("Objetivo 3A"),
    "Gráfico 3B" = ggplot(mtcars, aes(x = drat, y = mpg)) + geom_line() + ggtitle("Objetivo 3B")
  )
  output$plot_obj3 <- renderPlot({ grafico3_list[[input$grafico3_select]] })
  
  # Objetivo 4
  grafico4_list <- list(
    "Gráfico 4A" = ggplot(mtcars, aes(x = disp, y = drat)) + geom_point(color = "purple") + ggtitle("Objetivo 4A"),
    "Gráfico 4B" = ggplot(mtcars, aes(x = wt, y = qsec)) + geom_smooth() + ggtitle("Objetivo 4B")
  )
  output$plot_obj4 <- renderPlot({ grafico4_list[[input$grafico4_select]] })
  
  # Exploratorio
  output$summary_exploratorio <- renderPrint({ summary(mtcars) })
  output$plot_exploratorio <- renderPlot({ pairs(mtcars, main = "Matriz de Dispersión") })
  
  # Clusters
  
  output$plot_clusters_dinamico <- renderPlot({
    switch(input$cluster_plot_select,
           "medias" = final_plot,
           "prod_mas" = gf_productos_mas_comprados,
           "prod_menos" = gf_productos_menos_comprados,
           "prod_gen_mas" = gf_productos_generales_mas_comprados,
           "prod_gen_menos" = gf_productos_generales_menos_comprados,
           "top20" = top20prods_total_por_cluster,
           plot.new() + text(0.5, 0.5, "Gráfico no disponible")
    )
  })
  
  # Centroides
  updateSelectInput(session, "cluster",
                    choices = c("Todos", levels(media_clusteres$cluster)),
                    selected = "Todos")
  
  output$tabla_centroides_clusteres <- renderDT({
    df <- if (input$cluster == "Todos") {
      media_clusteres
    } else {
      media_clusteres %>% filter(cluster == input$cluster)
    }
    
    datatable(df, options = list(paging = nrow(df) > 10))
  })
  
  output$grafico_modelos <- renderPlotly({
    req(input$grafico_modelo_select) # Asegura que haya un valor seleccionado
    switch(input$grafico_modelo_select,
           "codo" = metodo_codo,
           "distribucion" = grafico_distribucion_centroides,
           "centroides" = plot_centroides_kmeans
    )
  })
  
  
} # <-- FIN de la función server

# --- Lanzar app ---
shinyApp(ui = ui, server = server)