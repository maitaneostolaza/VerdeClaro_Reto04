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

mis_librerias <- c(
  "ggplot2", "dplyr", "tidyr", "stringr", "tidyverse", "purrr",
  "plotly", "naniar", "VIM", "lubridate", "rsparse", "shiny", "cowplot",
  "DT", "scales", "RColorBrewer", "gridExtra", "Matrix", "recommenderlab",
  "tibble", "plumber", "readr", "reshape2"
)

usar_librerias(mis_librerias)
