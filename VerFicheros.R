usar_librerias <- function(paquetes) {
  for (p in paquetes) {
    if (!requireNamespace(p, quietly = TRUE)) {
      message(paste("Instalando paquete:", p))
      install.packages(p, dependencies = TRUE)
    } else {
      message(paste("Ya instalado:", p))
    }
    library(p, character.only = TRUE)
  }
}

mis_librerias <- c("ggplot2", "dplyr", "tidyr", "stringr","tidyverse","purrr",
                   "plotly","naniar","VIM","lubridate","rsparse")
usar_librerias(mis_librerias)

beepr::beep(sound = 2)  # Hay sonidos del 1 al 11


Objetivos <- readRDS("Datos\\Originales\\objetivos.RDS")
#Objetivos <- as.data.frame(Objetivos)

Maestroestr <- readRDS("Datos\\Originales\\maestroestr.RDS")
Maestroestr <- as.data.frame(Maestroestr)

tickets_enc <- readRDS("Datos\\Originales\\tickets_enc.RDS")
tickets_enc <- as.data.frame(tickets_enc)

# Cargar librería
library(ggplot2)

# Definir paleta de colores
eroski_colors <- c(
  "#e60026",  # rojo
  "#0066b3",  # azul
  "#ffffff",  # blanco
)

# Crear datos de ejemplo
datos <- data.frame(
  categoria = c("Fruta", "Verdura", "Pan", "Lácteos", "Bebidas"),
  ventas = c(120, 95, 80, 65, 110)
)

# Gráfico de barras
ggplot(datos, aes(x = categoria, y = ventas, fill = categoria)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = eroski_colors[1:5]) +
  theme_dark() +
  labs(
    title = "Ventas por categoría",
    x = "Categoría",
    y = "Unidades vendidas"
  )
