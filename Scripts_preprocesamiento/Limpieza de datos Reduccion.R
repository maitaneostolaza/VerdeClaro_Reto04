library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(tidyverse)
library(purrr)
library(plotly)
library(naniar)
library(VIM)
# Cargamos los ficheros previamente limpios
Maestroestr <- readRDS("Datos\\Originales\\maestroestr.RDS")


df_entero <- readRDS("Datos\\Transformados\\df_clustering_entero.rds")
tickets_enc <- readRDS("Datos\\Originales\\tickets_enc.RDS")

sum(is.na(df_entero))
miss_var_summary(df_entero)

str(df_entero)
colnames(df_entero)[1] <- "id_cliente_enc"




#PRUEBA
# --- PRIMER FILTRO: cod_est ---

data <- readRDS("Datos/Transformados/tickets_enc.rds")

# Agrupar por cod_est y contar la cantidad de apariciones
conteo_cod <- data %>%
  group_by(cod_est) %>%
  summarise(cantidad = n(), .groups = "drop")

q1_cod <- quantile(conteo_cod$cantidad, 0.25, na.rm = TRUE)
q3_cod <- quantile(conteo_cod$cantidad, 0.75, na.rm = TRUE)

# Filtrar los cod_est fuera del rango intercuartílico
cod_filtrados <- conteo_cod %>%
  filter(cantidad > q3_cod | cantidad <= q1_cod) %>%
  pull(cod_est)


data_filtrada <- data %>%
  filter(cod_est %in% cod_filtrados)

objetivos <- readRDS("Datos/Originales/objetivos.RDS")
clientes_objetivo <- purrr::map(objetivos, "obj") %>% unlist() %>% unique()

# Recuperar registros de clientes objetivo que fueron eliminados por cod_est
clientes_rescatados <- data %>%
  filter(id_cliente_enc %in% clientes_objetivo)

data_filtrada <- bind_rows(data_filtrada, clientes_rescatados) %>%
  distinct()  

saveRDS(data_filtrada, "Datos/Transformados/tickets_filtrados_cod_est.rds")



# --- SEGUNDO FILTRO: id_cliente_enc ---

data_cod <- readRDS("Datos/Transformados/tickets_filtrados_cod_est.rds")

# Agrupar por id_cliente_enc y contar
conteo_cli <- data_cod %>%
  group_by(id_cliente_enc) %>%
  summarise(cantidad = n(), .groups = "drop")

q1_cli <- quantile(conteo_cli$cantidad, 0.25, na.rm = TRUE)
q3_cli <- quantile(conteo_cli$cantidad, 0.75, na.rm = TRUE)

# Filtrar los id_cliente_enc fuera del rango intercuartílico
clientes_filtrados <- conteo_cli %>%
  filter(cantidad > q3_cli | cantidad <= q1_cli) %>%
  pull(id_cliente_enc)

# Clientes filtrados por IQR + los que deben mantenerse sí o sí
objetivos <- readRDS("Datos//Originales//objetivos.RDS")

# Unir todos los clientes objetivo en un único vector
clientes_objetivo <- purrr::map(objetivos, "obj") %>%
  unlist() %>%
  unique()

clientes_finales <- union(clientes_filtrados, clientes_objetivo)

data_final <- data_cod %>%
  filter(id_cliente_enc %in% clientes_finales)


saveRDS(data_final, "Datos/Transformados/tickets_Reducidos.rds")





#VERIFICAR QUE SE HAN QUITADO 50% de clientes y productos
data_original <- readRDS("Datos/Transformados/tickets_enc.rds")

data_Reducida <- readRDS("Datos/Transformados/tickets_Reducidos.rds")

# Total de cod_est únicos en original y en filtrado
n_total_cod <- n_distinct(data_original$cod_est)
n_filtrado_cod <- n_distinct(data_Reducida$cod_est)

# Calcular % eliminados
porcentaje_cod_est_eliminados <- 100 * (1 - n_filtrado_cod / n_total_cod)

cat("Se han eliminado aproximadamente", round(porcentaje_cod_est_eliminados, 2), "% de cod_est\n")



data_filtrada_cod <- readRDS("Datos/Transformados/tickets_filtrados_cod_est.rds")

# Cargar datos finales
data_final <- readRDS("Datos/Transformados/tickets_Reducidos.rds")

# Total de clientes únicos antes y después
n_total_cli <- n_distinct(data_filtrada_cod$id_cliente_enc)
n_filtrado_cli <- n_distinct(data_final$id_cliente_enc)

# Calcular % eliminados
porcentaje_clientes_eliminados <- 100 * (1 - n_filtrado_cli / n_total_cli)

cat("Se han eliminado aproximadamente", round(porcentaje_clientes_eliminados, 2), "% de clientes\n")
a
