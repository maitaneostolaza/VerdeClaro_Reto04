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

  
  
  
  
  
