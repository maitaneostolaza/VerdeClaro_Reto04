library(shiny)
library(ggplot2)
library(plotly)  # <-- nuevo

# --- Tus gráficos por objetivo ---
# Objetivo 1 - múltiples gráficos por subpestaña (ggplot2)
grafico1A_list <- list(
  "Gráfico A1" = ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + ggtitle("Objetivo 1 - A1"),
  "Gráfico A2" = ggplot(mtcars, aes(x = wt, y = qsec)) + geom_point(color = "blue") + ggtitle("Objetivo 1 - A2")
)

grafico1B_list <- list(
  "Gráfico B1" = ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + geom_boxplot() + ggtitle("Objetivo 1 - B1"),
  "Gráfico B2" = ggplot(mtcars, aes(x = factor(cyl), y = hp)) + geom_boxplot(fill = "orange") + ggtitle("Objetivo 1 - B2")
)

# Objetivo 2 - gráficos interactivos con plotly
grafico2_1 <- ggplotly(
  ggplot(mtcars, aes(x = mpg)) +
    geom_histogram(bins = 10, fill = "skyblue") +
    ggtitle("Objetivo 2 - Gráfico A")
)

grafico2_2 <- ggplotly(
  ggplot(mtcars, aes(x = hp, y = mpg)) +
    geom_point(color = "red") +
    ggtitle("Objetivo 2 - Gráfico B")
)

grafico2_list <- list(grafico2_1, grafico2_2)

# --- UI ---
ui <- fluidPage(
  titlePanel("App con Pestañas, Subpestañas y Desplegables"),
  
  tabsetPanel(
    # PESTAÑA 1: Subpestañas con ggplot
    tabPanel("Objetivo 1",
             tabsetPanel(
               # Subpestaña A
               tabPanel("Vista A",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("grafico1A_select", "Selecciona un gráfico A:",
                                        choices = names(grafico1A_list))
                          ),
                          mainPanel(
                            plotOutput("plot_obj1A")  # ggplot2
                          )
                        )
               ),
               # Subpestaña B
               tabPanel("Vista B",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("grafico1B_select", "Selecciona un gráfico B:",
                                        choices = names(grafico1B_list))
                          ),
                          mainPanel(
                            plotOutput("plot_obj1B")  # ggplot2
                          )
                        )
               )
             )
    ),
    
    # PESTAÑA 2: Plotly + botones
    tabPanel("Objetivo 2",
             sidebarLayout(
               sidebarPanel(
                 actionButton("anterior", "Anterior"),
                 actionButton("siguiente", "Siguiente")
               ),
               mainPanel(
                 plotlyOutput("plot_obj2")  # Plotly output
               )
             )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  # Objetivo 1: ggplot2 estático
  output$plot_obj1A <- renderPlot({
    grafico1A_list[[input$grafico1A_select]]
  })
  
  output$plot_obj1B <- renderPlot({
    grafico1B_list[[input$grafico1B_select]]
  })
  
  # Objetivo 2: plotly interactivo con navegación
  indice <- reactiveVal(1)
  
  observeEvent(input$siguiente, {
    nuevo <- indice() + 1
    if (nuevo <= length(grafico2_list)) indice(nuevo)
  })
  
  observeEvent(input$anterior, {
    nuevo <- indice() - 1
    if (nuevo >= 1) indice(nuevo)
  })
  
  output$plot_obj2 <- renderPlotly({
    grafico2_list[[indice()]]
  })
}

# Ejecutar app
shinyApp(ui = ui, server = server)

