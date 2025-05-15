library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(cowplot)
library(grid)

# --- Cargar datos ---
df_entero <- readRDS("Datos\\Transformados\\df_con_clusteres.rds")
tickets <- readRDS("Datos\\Transformados\\tickets_limpios.rds")
productos <- readRDS("Datos\\Originales\\maestroestr.RDS")

# --- Datos para clustering (medias por cluster) ---
media_clusteres <- df_entero %>%
  mutate(cluster = as.factor(as.character(cluster))) %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")

media_unidades <- media_clusteres[, c("cluster", "media_unidades_por_compra")]
media_dias     <- media_clusteres[, c("cluster", "media_de_dias_pasadas_por_compras")]
total_compra   <- media_clusteres[, c("cluster", "total_veces_que_ha_comprado")]

media_unidades_gf <- ggplot(media_unidades, aes(x = cluster, y = media_unidades_por_compra, fill = cluster)) +
  geom_col() +
  labs(title = "Media de unidades por compra", x = "Clusters", y = "Unidades medias") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("1" =  "#E10A23", "2" = "#F0928E", "3" = "#FADED6", "4" = "#005B92"))

media_diasgf <- ggplot(media_dias, aes(x = cluster, y = media_de_dias_pasadas_por_compras, fill = cluster)) +
  geom_col() +
  labs(title = "Media de días transcurridos por compra", x = "Clusters", y = "Media de días") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("1" =  "#E10A23", "2" = "#F0928E", "3" = "#FADED6", "4" = "#005B92"))

total_comprasgf <- ggplot(total_compra, aes(x = cluster, y = total_veces_que_ha_comprado, fill = cluster)) +
  geom_col() +
  labs(title = "Total de veces que han comprado", x = "Clusters", y = "Total compras") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("1" =  "#E10A23", "2" = "#F0928E", "3" = "#FADED6", "4" = "#005B92"))

media_unidades_gf_nolegend <- media_unidades_gf + theme(legend.position = "none")
media_diasgf_nolegend     <- media_diasgf     + theme(legend.position = "none")
total_comprasgf_nolegend  <- total_comprasgf  + theme(legend.position = "none")

shared_legend <- get_legend(media_unidades_gf + theme(legend.position = "right", legend.direction = "horizontal"))

final_plot <- plot_grid(
  plot_grid(media_unidades_gf_nolegend, media_diasgf_nolegend, total_comprasgf_nolegend, ncol = 3),
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.1)
)

# --- Enriquecimiento de datos para productos por cluster ---
df <- left_join(tickets, df_entero, by = "id_cliente_enc") %>%
  mutate(cod_est = as.character(cod_est)) %>%
  select(-c(5,6,7)) %>%
  mutate(producto_general = case_when(
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
  ))

df <- left_join(df, productos, by = "cod_est")

# --- Gráficos de productos por cluster ---
productos_generales_mas_comprados <- df %>%
  group_by(cluster, producto_general) %>%
  summarise(cantidad_producto = n()) %>%
  slice_max(order_by = cantidad_producto, n = 5, with_ties = FALSE)

productos_generales_menos_comprados <- df %>%
  group_by(cluster, producto_general) %>%
  summarise(cantidad_producto = n()) %>%
  slice_min(order_by = cantidad_producto, n = 5, with_ties = FALSE)

productos_mas_comprados <- df %>%
  group_by(cluster, descripcion) %>%
  summarise(cantidad_producto = n()) %>%
  slice_max(order_by = cantidad_producto, n = 5, with_ties = FALSE)

productos_menos_comprados <- df %>%
  group_by(cluster, descripcion) %>%
  summarise(cantidad_producto = n()) %>%
  slice_min(order_by = cantidad_producto, n = 5, with_ties = FALSE)

top20_productos <- df %>%
  count(descripcion, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  pull(descripcion)

top20_cluster <- df %>%
  group_by(cluster, descripcion) %>%
  summarise(cantidad_producto = n()) %>%
  filter(descripcion %in% top20_productos)

# --- ggplot objetos ---
gf_productos_generales_mas_comprados <- ggplot(productos_generales_mas_comprados, aes(x = cantidad_producto, y = reorder(producto_general, cantidad_producto))) +
  geom_segment(aes(x = 0, xend = cantidad_producto, yend = producto_general), color = "#0074B5", size = 1.5) +
  geom_point(color = "#E10A23", size = 5) +
  facet_wrap(~cluster, scales = "free_y") +
  labs(title = "Productos generales más comprados por cluster", x = "Cantidad", y = "Producto") +
  theme_minimal()

gf_productos_generales_menos_comprados <- ggplot(productos_generales_menos_comprados, aes(x = cantidad_producto, y = reorder(producto_general, cantidad_producto))) +
  geom_segment(aes(x = 0, xend = cantidad_producto, yend = producto_general), color = "#0074B5", size = 1.5) +
  geom_point(color = "#E10A23", size = 5) +
  facet_wrap(~cluster, scales = "free_y") +
  labs(title = "Productos generales menos comprados por cluster", x = "Cantidad", y = "Producto") +
  theme_minimal()

gf_productos_mas_comprados <- ggplot(productos_mas_comprados, aes(x = cantidad_producto, y = reorder(descripcion, cantidad_producto))) +
  geom_segment(aes(x = 0, xend = cantidad_producto, yend = descripcion), color = "#0074B5", size = 1.5) +
  geom_point(color = "#E10A23", size = 5) +
  facet_wrap(~cluster, scales = "free_y") +
  labs(title = "Productos más comprados por cluster", x = "Cantidad", y = "Producto") +
  theme_minimal()

gf_productos_menos_comprados <- ggplot(productos_menos_comprados, aes(x = cantidad_producto, y = reorder(descripcion, cantidad_producto))) +
  geom_segment(aes(x = 0, xend = cantidad_producto, yend = descripcion), color = "#0074B5", size = 1.5) +
  geom_point(color = "#E10A23", size = 5) +
  facet_wrap(~cluster, scales = "free_y") +
  labs(title = "Productos menos comprados por cluster", x = "Cantidad", y = "Producto") +
  theme_minimal()

top20prods_total_por_cluster <- ggplot(top20_cluster, aes(x = cantidad_producto, y = reorder(descripcion, cantidad_producto), fill = cluster)) +
  geom_col(position = "stack") +
  labs(title = "Top 20 productos más vendidos y su distribución por cluster", x = "Cantidad total", y = "Producto") +
  scale_fill_manual(values = c("#E10A23", "#F0928E", "#0074B5", "#A2CBE8")) +
  theme_minimal()

# --- UI ---
ui <- fluidPage(
  titlePanel("App con Pestañas y Desplegables"),
  tabPanel("Objetivos",
           sidebarLayout(
             sidebarPanel(
               selectInput("objetivo_select", "Selecciona un objetivo:", choices = c("Objetivo 1", "Objetivo 2", "Objetivo 3", "Objetivo 4")),
               
               # Inputs condicionales por objetivo
               conditionalPanel(condition = "input.objetivo_select == 'Objetivo 1'",
                                selectInput("grafico1_select", "Selecciona un gráfico:", choices = c("Gráfico A1", "Gráfico A2", "Gráfico B1", "Gráfico B2"))
               ),
               conditionalPanel(condition = "input.objetivo_select == 'Objetivo 2'",
                                actionButton("anterior", "Anterior"),
                                actionButton("siguiente", "Siguiente")
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
           ),
           tabPanel("Análisis Exploratorio",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("tipo_analisis", "Selecciona tipo de análisis:", choices = c("Resumen", "Matriz de Dispersión"))
                      ),
                      mainPanel(
                        conditionalPanel(condition = "input.tipo_analisis == 'Resumen'", verbatimTextOutput("summary_exploratorio")),
                        conditionalPanel(condition = "input.tipo_analisis == 'Matriz de Dispersión'", plotOutput("plot_exploratorio"))
                      )
                    )),
           tabPanel("Clusters",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("cluster_plot_select", "Selecciona gráfico a visualizar:", choices = c(
                          "Resumen de medias" = "medias",
                          "Productos más comprados" = "prod_mas",
                          "Productos menos comprados" = "prod_menos",
                          "Productos generales más comprados" = "prodgen_mas",
                          "Productos generales menos comprados" = "prodgen_menos",
                          "Top 20 por cluster" = "top20"
                        ))
                      ),
                      mainPanel(
                        plotOutput("plot_clusters_dinamico", height = "600px")
                      )
                    ))
  )
)

# --- Server ---
server <- function(input, output, session) {
  grafico1_list <- list(
    "Gráfico A1" = ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + ggtitle("Objetivo 1 - A1"),
    "Gráfico A2" = ggplot(mtcars, aes(x = wt, y = qsec)) + geom_point(color = "blue") + ggtitle("Objetivo 1 - A2"),
    "Gráfico B1" = ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + geom_boxplot() + ggtitle("Objetivo 1 - B1"),
    "Gráfico B2" = ggplot(mtcars, aes(x = factor(cyl), y = hp)) + geom_boxplot(fill = "orange") + ggtitle("Objetivo 1 - B2")
  )
  output$plot_obj1 <- renderPlot({ grafico1_list[[input$grafico1_select]] })
  
  grafico2_list <- list(
    ggplotly(ggplot(mtcars, aes(x = mpg)) + geom_histogram(bins = 10, fill = "skyblue") + ggtitle("Objetivo 2 - Gráfico A")),
    ggplotly(ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point(color = "red") + ggtitle("Objetivo 2 - Gráfico B"))
  )
  indice <- reactiveVal(1)
  observeEvent(input$siguiente, { if (indice() < length(grafico2_list)) indice(indice() + 1) })
  observeEvent(input$anterior, { if (indice() > 1) indice(indice() - 1) })
  output$plot_obj2 <- renderPlotly({ grafico2_list[[indice()]] })
  
  grafico3_list <- list(
    "Gráfico 3A" = ggplot(mtcars, aes(x = gear, y = mpg)) + geom_bar(stat = "identity") + ggtitle("Objetivo 3A"),
    "Gráfico 3B" = ggplot(mtcars, aes(x = drat, y = mpg)) + geom_line() + ggtitle("Objetivo 3B")
  )
  output$plot_obj3 <- renderPlot({ grafico3_list[[input$grafico3_select]] })
  
  grafico4_list <- list(
    "Gráfico 4A" = ggplot(mtcars, aes(x = disp, y = drat)) + geom_point(color = "purple") + ggtitle("Objetivo 4A"),
    "Gráfico 4B" = ggplot(mtcars, aes(x = wt, y = qsec)) + geom_smooth() + ggtitle("Objetivo 4B")
  )
  output$plot_obj4 <- renderPlot({ grafico4_list[[input$grafico4_select]] })
  
  output$summary_exploratorio <- renderPrint({ summary(mtcars) })
  output$plot_exploratorio <- renderPlot({ pairs(mtcars, main = "Matriz de Dispersión") })
  
  output$plot_clusters_dinamico <- renderPlot({
    switch(input$cluster_plot_select,
           "medias" = final_plot,
           "prod_mas" = gf_productos_mas_comprados,
           "prod_menos" = gf_productos_menos_comprados,
           "prodgen_mas" = gf_productos_generales_mas_comprados,
           "prodgen_menos" = gf_productos_generales_menos_comprados,
           "top20" = top20prods_total_por_cluster
    )
  })
}

# --- Lanzar app ---
shinyApp(ui = ui, server = server)



# -------------------------- TABLA CLUSTERES CENTROIDES
library(DT)
media_clusteres <- df_entero %>%
  mutate(cluster = as.factor(as.character(cluster))) %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))

ui <- fluidPage(
  titlePanel("Centroides de los clusteres"),
  selectInput (inputId='cluster',label="Selecciona un cluster",choices = unique(media_clusteres$cluster)),
  DTOutput("tabla_centroides_clusteres"))

server <- function(input, output, session) {
  updateSelectInput(session, "cluster",
                    choices = c("Todos", levels(media_clusteres$cluster)),
                    selected = "Todos")
  
  output$tabla_centroides_clusteres <- renderDT({
    if (input$cluster == "Todos") {
      media_clusteres
    } else {
      media_clusteres %>% filter(cluster == input$cluster)
    }
  })
}
shinyApp(ui = ui, server = server)

 

library(shiny)
library(purrr)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)

df_entero <- readRDS("C:/Users/gorra/OneDrive - Mondragon Unibertsitatea/Escritorio/ultimo intento git/VerdeClaro_Reto04/Datos/Transformados/df_con_clusteres.rds")
tickets <- readRDS("C:/Users/gorra/OneDrive - Mondragon Unibertsitatea/Escritorio/ultimo intento git/VerdeClaro_Reto04/Datos/Transformados/tickets_limpios.rds")
productos <- readRDS("C:/Users/gorra/OneDrive - Mondragon Unibertsitatea/Escritorio/ultimo intento git/VerdeClaro_Reto04/Datos/Originales/maestroestr.RDS")


media_clusteres <- df_entero %>%
  mutate(cluster = as.factor(as.character(cluster))) %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")

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
                 selectInput("tipo_analisis", "Selecciona tipo de análisis:", choices = c("Resumen", "Matriz de Dispersión"))
               ),
               mainPanel(
                 conditionalPanel(condition = "input.tipo_analisis == 'Resumen'", verbatimTextOutput("summary_exploratorio")),
                 conditionalPanel(condition = "input.tipo_analisis == 'Matriz de Dispersión'", plotOutput("plot_exploratorio"))
               )
             )),
    tabPanel("Clusters",
             sidebarLayout(
               sidebarPanel(
                 selectInput("cluster_plot_select", "Selecciona gráfico a visualizar:", choices = c(
                   "Resumen de medias" = "medias",
                   "Productos más comprados" = "prod_mas",
                   "Productos menos comprados" = "prod_menos",
                   "Productos generales más comprados" = "prodgen_mas",
                   "Productos generales menos comprados" = "prodgen_menos",
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
df_entero <- readRDS("Datos\\Transformados\\df_clustering_entero.rds")

# --- Server ---
server <- function(input, output, session) {
  
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
  # Aquí deberías definir estos objetos antes de ejecutar la app:
  # final_plot, gf_productos_mas_comprados, etc.
  output$plot_clusters_dinamico <- renderPlot({
    switch(input$cluster_plot_select,
           "medias" = final_plot,
           "prod_mas" = gf_productos_mas_comprados,
           "prod_menos" = gf_productos_menos_comprados,
           "prodgen_mas" = gf_productos_generales_mas_comprados,
           "prodgen_menos" = gf_productos_generales_menos_comprados,
           "top20" = top20prods_total_por_cluster
           
           
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
    req(input$grafico_modelo_select)  # Asegura que haya un valor seleccionado
    switch(input$grafico_modelo_select,
           "codo" = metodo_codo,
           "distribucion" = grafico_distribucion_centroides,
           "centroides" = plot_centroides_kmeans
    )
  })
  
}  # <-- FIN de la función server

# --- Lanzar app ---
shinyApp(ui = ui, server = server)
library(DT)  
  
library(purrr)
  
  
