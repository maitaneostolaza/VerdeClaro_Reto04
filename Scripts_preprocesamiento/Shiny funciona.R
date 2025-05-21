library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)

df_entero <- readRDS("C:/Users/gorra/OneDrive - Mondragon Unibertsitatea/Escritorio/ultimo intento git/VerdeClaro_Reto04/Datos/Transformados/df_con_clusteres.rds")
tickets <- readRDS("C:/Users/gorra/OneDrive - Mondragon Unibertsitatea/Escritorio/ultimo intento git/VerdeClaro_Reto04/Datos/Transformados/tickets_limpios.rds")
productos <- readRDS("C:/Users/gorra/OneDrive - Mondragon Unibertsitatea/Escritorio/ultimo intento git/VerdeClaro_Reto04/Datos/Originales/maestroestr.RDS")


# Datos de ejemplo para tabla de centroides

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
                             selectInput("grafico1_select", "Selecciona un gráfico:", choices = c("Gráfico 2A", "Grafico 2B"))
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
                 h4("Aquí puedes añadir controles más adelante")
               ),
               mainPanel(
                 h4("Espacio para gráficos de modelos")
               )
             ))
    
  )
)

# --- Server ---
server <- function(input, output, session) {
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
    if (input$grafico1_select == "Gráfico 2A") {
      ggplotly(ggplot(mtcars, aes(x = mpg)) +
                 geom_histogram(bins = 10, fill = "skyblue") +
                 ggtitle("Objetivo 2 - Gráfico A"))
    } else if (input$grafico1_select == "Grafico 2B") {
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
}  # <-- FIN de la función server

# --- Lanzar app ---
shinyApp(ui = ui, server = server)
