library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)
library(purrr)
library(DT)
library(lubridate)
library(recommenderlab)
library(scales)
library(shinydashboard)

options(scipen = 999)

# ---------------- CARGA DE DATOS ---------------- #
eval <- readRDS("Datos/Resultados/Comparando_algoritmos_topNlist_eval.rds")
eval_ratings <- readRDS("Datos/Resultados/Comparando_algoritmos_ratings_eval.rds")
df_entero <- readRDS("Datos/Transformados/df_con_clusteres.rds")
tickets <- readRDS("Datos/Transformados/tickets_limpios.rds")
productos <- readRDS("Datos/Originales/maestroestr.RDS")
tickets_enc <- readRDS("Datos\\Originales\\tickets_enc.RDS")
matriz <- readRDS("Datos/Resultados/Matriz_red_comp_algos.rds")
matriz_rec <- as(matriz, "realRatingMatrix")
objetivo1_data <- readRDS("Datos/Resultados/Objetivo1_resultado.rds")
objetivo2_data <- readRDS("Datos/Resultados/Objetivo2_resultado.rds")
objetivo3_data <- readRDS("Datos/Resultados/Objetivo3_resultado.rds")
objetivo4_data <- readRDS("Datos/Resultados/Objetivo4_resultado.rds")

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
  scale_fill_manual(values = c("#E10A23", "#F0928E", "#0074B5", "#A2CBE8"))

media_diasgf <- ggplot(media_dias, aes(x = cluster, y = media_de_dias_pasadas_por_compras, fill = cluster)) +
  geom_col() +
  labs(title = "Media de días transcurridos por compra", x = "Cluster", y = "Media de días") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#E10A23", "#F0928E", "#0074B5", "#A2CBE8"))

total_comprasgf <- ggplot(total_compra, aes(x = cluster, y = total_veces_que_ha_comprado, fill = cluster)) +
  geom_col() +
  labs(title = "Total veces que han comprado", x = "Cluster", y = "Total compras") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#E10A23", "#F0928E", "#0074B5", "#A2CBE8"))

media_unidades_gf_nolegend <- media_unidades_gf + theme(legend.position = "none")
media_diasgf_nolegend <- media_diasgf + theme(legend.position = "none")
total_comprasgf_nolegend <- total_comprasgf + theme(legend.position = "none")
shared_legend <- suppressWarnings(
  get_legend(media_unidades_gf + theme(legend.position = "right", legend.direction = "horizontal"))
)

final_plot <- plot_grid(
  plot_grid(media_unidades_gf_nolegend, media_diasgf_nolegend, total_comprasgf_nolegend, ncol = 3),
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.1)
)

# 2. Otros gráficos por cluster
productos_generales_mas_comprados <- df %>%
  group_by(cluster, producto_general) %>%
  summarise(cantidad_producto = n(), .groups = "drop") %>%
  slice_max(order_by = cantidad_producto, n = 5, with_ties = FALSE)

gf_productos_generales_mas_comprados <- ggplot(productos_generales_mas_comprados, aes(x = cantidad_producto, y = reorder(producto_general, cantidad_producto))) +
  geom_segment(aes(x = 0, xend = cantidad_producto, yend = producto_general), color = "#0074B5", size = 1.5) +
  geom_point(color = "#E10A23", size = 5) +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(title = "Productos generales más comprados por cluster", x = "Cantidad", y = "Producto general") +
  theme_minimal()

productos_generales_menos_comprados <- df %>%
  group_by(cluster, producto_general) %>%
  summarise(cantidad_producto = n(), .groups = "drop") %>%
  slice_min(order_by = cantidad_producto, n = 5, with_ties = FALSE)

gf_productos_generales_menos_comprados <- ggplot(productos_generales_menos_comprados, aes(x = cantidad_producto, y = reorder(producto_general, cantidad_producto))) +
  geom_segment(aes(x = 0, xend = cantidad_producto, yend = producto_general), color = "#0074B5", size = 1.5) +
  geom_point(color = "#E10A23", size = 5) +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(title = "Productos generales menos comprados por cluster", x = "Cantidad", y = "Producto general") +
  theme_minimal()

productos_mas_comprados <- df %>%
  group_by(cluster, descripcion) %>%
  summarise(cantidad_producto = n(), .groups = "drop") %>%
  slice_max(order_by = cantidad_producto, n = 5, with_ties = FALSE)

gf_productos_mas_comprados <- ggplot(productos_mas_comprados, aes(x = cantidad_producto, y = reorder(descripcion, cantidad_producto))) +
  geom_segment(aes(x = 0, xend = cantidad_producto, yend = descripcion), color = "#0074B5", size = 1.5) +
  geom_point(color = "#E10A23", size = 5) +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(title = "Productos más comprados por cluster", x = "Cantidad", y = "Producto") +
  theme_minimal()

productos_menos_comprados <- df %>%
  group_by(cluster, descripcion) %>%
  summarise(cantidad_producto = n(), .groups = "drop") %>%
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
  summarise(cantidad_producto = n(), .groups = "drop") %>%
  filter(descripcion %in% top20_productos)

top20prods_total_por_cluster <- ggplot(top20_cluster, aes(x = cantidad_producto, y = reorder(descripcion, cantidad_producto), fill = cluster)) +
  geom_col(position = "stack") +
  labs(title = "Top 20 productos más vendidos y su distribución por cluster", x = "Cantidad total", y = "Producto") +
  scale_fill_manual(values = c("#E10A23", "#F0928E", "#0074B5", "#A2CBE8")) +
  theme_minimal()

# --- UI ---
resumen_unidades <- df %>%
  count(descripcion) %>%
  summarise(
    minimo = min(n),
    maximo = max(n)
  )
min_unidades_vendidas <- resumen_unidades$minimo
max_unidades_vendidas <- resumen_unidades$maximo

ui <- tagList(
  tags$head(
    tags$style(HTML("
    .navbar {
      background-color: #E10A23 !important;
    }
    .navbar-default .navbar-brand {
      background-color: #0074B5 !important;
      color: white !important;
      padding: 15px 15px !important;
      font-weight: bold;
      display: flex;
      align-items: center;
      height: 60px;
    }
    .navbar-default .navbar-nav > li > a {
      color: white !important;
    }
    .navbar-default .navbar-nav > .active > a,
    .navbar-default .navbar-nav > .active > a:focus,
    .navbar-default .navbar-nav > .active > a:hover {
      background-color: #B00A1C !important;
      color: white !important;
      height: 100%;
      padding-top: 15px;
      padding-bottom: 15px;
    }
    .navbar-right-custom {
      position: absolute;
      right: 20px;
      top: 12px;
    }
  "))
  ),
  
  navbarPage(
    title = div(
      "Reto 4 Eroski",
      tags$div(class = "navbar-right-custom",
               tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/13/Eroski_logo.svg/2560px-Eroski_logo.svg.png",
                        height = "35px"))
    ),
    
  
  # 1. Análisis Exploratorio
  tabPanel("1 - Análisis Exploratorio",
           sidebarLayout(
             sidebarPanel(
               dateRangeInput("filtro_fecha", "Rango de fechas:",
                              start = min(datos$fecha),
                              end = max(datos$fecha)),
               selectInput("filtro_dia", "Día de la semana:",
                           choices = c("Todos", levels(df$dia_semana)[levels(df$dia_semana) != "domingo"])),
               selectInput("filtro_producto", "Producto general:",
                           choices = c("Todos", unique(df$producto_general))),
               sliderInput("filtro_min_compras", "Mínimo de unidades vendidas:",
                           min = min_unidades_vendidas,
                           max = max_unidades_vendidas,
                           value = min_unidades_vendidas,
                           step = 1),
               actionButton("ejecutar_filtros", "Aplicar filtros", icon = icon("play"), class = "btn-primary"),
               actionButton("reset_filtros", "Resetear filtros", icon = icon("redo"), class = "btn-danger")
               
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Top 10 Productos", plotOutput("grafico_top10_prod")),
                 tabPanel("Tickets por Día", plotOutput("grafico_tickets_por_dia")),
                 tabPanel("Productos por Día de la Semana", plotOutput("grafico_prod_dia_semana")),
                 tabPanel("Productos por Semana", plotOutput("grafico_prod_semana")),
                 tabPanel("Top 10 Clientes", plotOutput("grafico_top10_clientes")),
                 tabPanel("Top 5 por Día con Cluster", plotOutput("grafico_top5_dia_cluster"))
               )
             )
           )
  ),
  
  # 2. Clusterización
  tabPanel("2 - Clusterización",
           tabsetPanel(
             tabPanel("Resumen de Medias", plotOutput("grafico_resumen_medias")),
             tabPanel("Productos más comprados", plotOutput("grafico_prod_mas")),
             tabPanel("Productos menos comprados", plotOutput("grafico_prod_menos")),
             tabPanel("Productos generales más comprados", plotOutput("grafico_prod_gen_mas")),
             tabPanel("Productos generales menos comprados", plotOutput("grafico_prod_gen_menos")),
             tabPanel("Top 20 por cluster", plotOutput("grafico_top20_cluster")),
             tabPanel("Centroides", 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("cluster", "Selecciona un cluster", choices = NULL)
                        ),
                        mainPanel(DT::dataTableOutput("tabla_centroides_clusteres"))
                      )
             ),
             tabPanel("Centroides 3D", plotlyOutput("grafico_centroides_3d")),
             tabPanel("Modelos",
                      tabsetPanel(
                        tabPanel("Método del Codo", plotlyOutput("grafico_codo")),
                        tabPanel("Distribución Compras", plotlyOutput("grafico_distribucion"))
                      )
             )
           )
  ),
  
  # 3. Resultados de Modelos
  tabPanel("3 - Modelos",
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("metricas_modelos", "Métricas a mostrar:",
                                  choices = c("RMSE", "MAE", "MSE"),
                                  selected = c("RMSE", "MAE", "MSE")),
               helpText("Puedes seleccionar qué métricas comparar."),
               selectInput("modelos_roc", "Modelos en curva ROC:",
                           choices = NULL, multiple = TRUE)
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Errores con Random", plotOutput("grafico_errores_con_random")),
                 tabPanel("Errores sin Random", plotOutput("grafico_errores_sin_random")),
                 tabPanel("Curva ROC", plotOutput("grafico_roc"))
               )
             )
           )
  ) ,
  
  # 4. Resultados Objetivos
  tabPanel("4 - Objetivos",
           sidebarLayout(
             sidebarPanel(
               selectInput("objetivo_select", "Selecciona objetivo:",
                           choices = c("Objetivo 1", "Objetivo 2", "Objetivo 3", "Objetivo 4")),
               uiOutput("opciones_objetivo")
             ),
             mainPanel(
               conditionalPanel(condition = "input.objetivo_select == 'Objetivo 1'", DT::dataTableOutput("tabla_obj1")),
               conditionalPanel(condition = "input.objetivo_select == 'Objetivo 2'", DT::dataTableOutput("tabla_obj2")),
               conditionalPanel(condition = "input.objetivo_select == 'Objetivo 3'", DT::dataTableOutput("tabla_obj3")),
               conditionalPanel(condition = "input.objetivo_select == 'Objetivo 4'", DT::dataTableOutput("tabla_obj4"))
             )
           )
  )
)
)

server <- function(input, output, session) {
  # ---- PESTAÑA 1: Análisis Exploratorio ----
  output$grafico_top10_prod <- renderPlot({
    req(filtros$df)
    
    filtros$df %>%
      count(descripcion, sort = TRUE) %>%
      slice_max(n, n = 10) %>%
      ggplot(aes(x = reorder(descripcion, n), y = n)) +
      geom_col(fill = "#E10A23") +
      coord_flip() +
      labs(title = "Top 10 productos más vendidos", x = NULL, y = "Unidades") +
      theme_minimal()
  })
  
  
  filtros <- reactiveValues(df = NULL)
  
  observeEvent(input$ejecutar_filtros, {
    df_filtrado <- df
    
    if (!is.null(input$filtro_fecha)) {
      df_filtrado <- df_filtrado %>%
        filter(fecha >= input$filtro_fecha[1] & fecha <= input$filtro_fecha[2])
    }
    
    if (input$filtro_dia != "Todos") {
      df_filtrado <- df_filtrado %>% filter(dia_semana == input$filtro_dia)
    }
    
    if (input$filtro_producto != "Todos") {
      df_filtrado <- df_filtrado %>% filter(producto_general == input$filtro_producto)
    }
    
    if (!is.null(input$filtro_min_compras)) {
      df_filtrado <- df_filtrado %>%
        group_by(descripcion) %>%
        filter(n() >= input$filtro_min_compras) %>%
        ungroup()
    }
    
    filtros$df <- df_filtrado
  })
  
  observeEvent(input$reset_filtros, {
  updateDateRangeInput(session, "filtro_fecha",
                       start = min(df$fecha),
                       end = max(df$fecha))
  updateSelectInput(session, "filtro_dia", selected = "Todos")
  updateSelectInput(session, "filtro_producto", selected = "Todos")
  updateSliderInput(session, "filtro_min_compras", value = min_unidades_vendidas)
  
  filtros$df <- df
})

  
  output$grafico_tickets_por_dia <- renderPlot({
    req(filtros$df)
    
    filtros$df %>%
      group_by(fecha) %>%
      summarise(tickets = n_distinct(id_cliente_enc), .groups = "drop") %>%
      ggplot(aes(x = fecha, y = tickets)) +
      geom_line(color = "#0074B5") +
      labs(title = "Número de tickets por día", x = "Fecha", y = "Clientes únicos") +
      theme_minimal()
  })
  
  
  output$grafico_prod_dia_semana <- renderPlot({
    req(filtros$df)
    
    filtros$df %>%
      filter(dia_semana != "domingo") %>%
      count(dia_semana) %>%
      ggplot(aes(x = dia_semana, y = n)) +
      geom_col(fill = "#A2CBE8") +
      labs(title = "Productos vendidos por día de la semana (sin domingos)", x = NULL, y = "Unidades") +
      scale_y_continuous(labels = label_comma()) +
      theme_minimal()
  })
  
  
  output$grafico_prod_semana <- renderPlot({
    req(filtros$df)
    
    filtros$df %>%
      mutate(semana = floor_date(fecha, "week", week_start = 1)) %>%
      count(semana) %>%
      ggplot(aes(x = semana, y = n)) +
      geom_col(fill = "#A2CBE8", color = "#005B92") +
      labs(title = "Productos vendidos por semana del año", x = "Semana", y = "Unidades") +
      scale_x_date(labels = date_format("%U"), breaks = "1 week") +
      theme_minimal()
  })
  
  
  output$grafico_top10_clientes <- renderPlot({
    req(filtros$df)
    
    filtros$df %>%
      count(id_cliente_enc, name = "compras") %>%
      slice_max(compras, n = 10) %>%
      ggplot(aes(x = reorder(id_cliente_enc, compras), y = compras)) +
      geom_col(fill = "#B00A1C") +
      coord_flip() +
      labs(title = "Top 10 clientes con más unidades compradas", x = "ID Cliente (enc)", y = "Unidades") +
      theme_minimal()
  })
  
  
  output$grafico_top5_dia_cluster <- renderPlot({
    req(filtros$df)
    
    filtros$df %>%
      filter(dia_semana != "domingo") %>%
      group_by(dia_semana, producto_general) %>%
      summarise(cantidad_vendida = n(), .groups = "drop") %>%
      group_by(dia_semana) %>%
      slice_max(order_by = cantidad_vendida, n = 5) %>%
      ungroup() %>%
      ggplot(aes(x = dia_semana, y = cantidad_vendida, fill = producto_general)) +
      geom_col() +
      scale_fill_manual(values = eroski_palette_extended) +
      coord_flip() +
      labs(title = "Top 5 productos más vendidos por día de la semana (sin domingos)",
           x = "Día de la semana", y = "Cantidad vendida", fill = "Producto general") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  # ---- PESTAÑA 2: Clusterización ----
  
  # Preprocesamiento
  df_clustering <- df_entero[, -1]
  
  # Cálculo del método del codo
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
  
  # Cálculo de centroides para gráfico 3D
  set.seed(7)
  KMEANS <- kmeans(df_clustering, 4)
  df_clustering$cluster_KM <- as.factor(KMEANS$cluster)
  
  centroides_kmean <- as.data.frame(KMEANS$centers) %>%
    mutate(cluster_KM = as.factor(as.character(1:length(unique(df_clustering$cluster_KM)))))
  
  plot_centroides_kmeans <- plot_ly(data = centroides_kmean,
                                    x = ~media_unidades_por_compra,
                                    y = ~media_de_dias_pasadas_por_compras,
                                    z = ~total_veces_que_ha_comprado,
                                    type = "scatter3d",
                                    mode = "markers",
                                    color = ~cluster_KM,
                                    colors = c("#E10A23", "#A2CBE8", "#005B92", "#F0928E"))
  
  grafico_distribucion_centroides <- plot_ly(data = df_clustering,
                                           x = ~media_unidades_por_compra,
                                           y = ~total_veces_que_ha_comprado,
                                           type = "scatter",
                                           color = ~cluster_KM,
                                           colors = c("#E10A23", "#A2CBE8", "#005B92", "#F0928E"))

  output$grafico_resumen_medias <- renderPlot({ final_plot })
  
  output$grafico_prod_mas <- renderPlot({ gf_productos_mas_comprados })
  output$grafico_prod_menos <- renderPlot({ gf_productos_menos_comprados })
  output$grafico_prod_gen_mas <- renderPlot({ gf_productos_generales_mas_comprados })
  output$grafico_prod_gen_menos <- renderPlot({ gf_productos_generales_menos_comprados })
  output$grafico_top20_cluster <- renderPlot({ top20prods_total_por_cluster })
  
  output$tabla_centroides_clusteres <- renderDT({
    df <- if (input$cluster == "Todos") {
      media_clusteres
    } else {
      media_clusteres %>% filter(cluster == input$cluster)
    }
    
    datatable(
      df,
      extensions = c('Buttons'),
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 10
      ),
      filter = "top",
      rownames = FALSE
    )
  })
  
  
  updateSelectInput(session, "cluster",
                    choices = c("Todos", levels(media_clusteres$cluster)),
                    selected = "Todos")
  
  output$grafico_centroides_3d <- renderPlotly({ plot_centroides_kmeans })
  output$grafico_codo <- renderPlotly({ metodo_codo })
  output$grafico_distribucion <- renderPlotly({ grafico_distribucion_centroides })
  
  
  # ---- PESTAÑA 3: Variables auxiliares ----
  
  get_df_errores <- function(modelos) {
    map_dfr(modelos, ~{
      rt <- ratings[[.x]]
      if (is.null(rt) || length(rt) < 3) {
        rt <- rep(NA_real_, 3)
      }
      tibble(
        Modelo = toupper(.x),
        RMSE   = rt[1],
        MAE    = rt[3],
        MSE    = rt[2]
      )
    })
  }
  
  CMs <- lapply(eval, function(x) getConfusionMatrix(x)[[1]])
  ratings <- lapply(eval_ratings, avg)
  names(ratings) <- tolower(names(ratings))
  
  modelos_con_random <- names(ratings)
  modelos_sin_random <- setdiff(modelos_con_random, "random")
  
  conf_matrix_n5_list <- lapply(eval, function(res) getConfusionMatrix(res, n = 5))
  
  df_list <- lapply(names(conf_matrix_n5_list), function(modelo){
    df <- as.data.frame(conf_matrix_n5_list[[modelo]])
    df$Modelo <- modelo
    df
  })
  
  df_conf <- do.call(rbind, df_list)
  
  df_conf_long <- pivot_longer(df_conf, cols = -Modelo, names_to = "Metrica", values_to = "Valor")
  
  df_roc <- df_conf_long %>%
    filter(Metrica %in% c("TPR", "FPR")) %>%
    mutate(Metrica = tolower(Metrica)) %>%
    group_by(Modelo, Metrica) %>%
    mutate(Paso = row_number()) %>%
    ungroup() %>%
    pivot_wider(names_from = Metrica, values_from = Valor)
  
  observe({
    updateSelectInput(session, "modelos_roc",
                      choices = unique(df_roc$Modelo),
                      selected = unique(df_roc$Modelo))
  })
  
  # ---- PESTAÑA 3: Resultados de Modelos ----
  output$grafico_errores_con_random <- renderPlot({
    df <- get_df_errores(modelos_con_random) %>%
      pivot_longer(-Modelo, names_to = "Metrica", values_to = "Valor") %>%
      filter(Metrica %in% input$metricas_modelos)
    
    ggplot(df, aes(x = Modelo, y = Valor, fill = Metrica)) +
      geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
      labs(title = "Comparación de errores por modelo (con RANDOM)", y = "Valor del error", x = "Modelo") +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("RMSE" = "#B00A1C", "MAE" = "#005B92", "MSE" = "#FFD5D1")) +
      theme(legend.position = "top")
  })
  
  output$grafico_errores_sin_random <- renderPlot({
    df <- get_df_errores(modelos_sin_random) %>%
      pivot_longer(-Modelo, names_to = "Metrica", values_to = "Valor") %>%
      filter(Metrica %in% input$metricas_modelos)
    
    ggplot(df, aes(x = Modelo, y = Valor, fill = Metrica)) +
      geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
      labs(title = "Comparación de errores por modelo (sin RANDOM)", y = "Valor del error", x = "Modelo") +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("RMSE" = "#B00A1C", "MAE" = "#005B92", "MSE" = "#FFD5D1")) +
      theme(legend.position = "top")
  })
  
  output$grafico_roc <- renderPlot({
    df_roc_filtrado <- df_roc %>% filter(Modelo %in% input$modelos_roc)
    
    ggplot(df_roc_filtrado, aes(x = fpr, y = tpr, color = Modelo, group = Modelo)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(title = "Curva ROC: TPR vs FPR por Modelo", x = "FPR", y = "TPR") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")
  })
  # ---- PESTAÑA 4: Resultados Objetivos ----
  
  # --- CÓDIGO ACTUALIZADO PARA TABLAS CON BOTONES EN PESTAÑA OBJETIVOS ---
  
  # OBJETIVO 1
  output$tabla_obj1 <- DT::renderDT({
    tabla <- objetivo1_data
    
    datatable(tabla,
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                pageLength = 10
              ),
              rownames = FALSE)
  })
  
  # OBJETIVO 2
  output$tabla_obj2 <- DT::renderDT({
    tabla <- objetivo2_data
    
    datatable(tabla,
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                pageLength = 10
              ),
              rownames = FALSE)
  })
  
  # OBJETIVO 3
  output$tabla_obj3 <- DT::renderDT({
    tabla <- objetivo3_data 
    
    datatable(tabla,
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                pageLength = 20,
                lengthMenu = c(10, 20, 50, 100, nrow(tabla))
              ),
              filter = "top",
              rownames = FALSE)
  })
  
  
  # OBJETIVO 4
  output$tabla_obj4 <- DT::renderDT({
    tabla <- objetivo4_data
    
    datatable(tabla,
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                pageLength = 10
              ),
              rownames = FALSE)
  })
  
  
}


shinyApp(ui = ui, server = server)
