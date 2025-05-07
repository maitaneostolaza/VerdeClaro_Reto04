# ================================
# CARGA DE LIBRERÍAS
# ================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# ================================
# CARGA DE DATOS
# ================================
# Datos base
maestroestr <- readRDS("Datos\\Originales\\maestroestr.RDS")
tickets_enc <- readRDS("Datos\\Originales\\tickets_enc.RDS")
tickets_limpios <- readRDS("Datos\\Transformados\\tickets_limpios.rds")
clusteres <- readRDS("Datos\\Transformados\\df_con_clusteres.rds")

# ================================
# PREPROCESAMIENTO DE DATOS
# ================================
# Tickets originales
datos <- tickets_enc
colnames(datos) <- c("fecha", "ticket", "cod_est", "id_cliente")
datos$fecha <- ymd(datos$fecha)

# Enriquecemos datos con descripción
datos_enriquecidos <- left_join(datos, maestroestr, by = "cod_est")

# ================================
# ANÁLISIS INICIAL
# ================================

# Top 10 productos más vendidos
datos_enriquecidos %>%
  count(descripcion, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(descripcion, n), y = n)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(title = "Top 10 productos más vendidos", x = "Producto")

# Tickets por día
datos %>%
  group_by(fecha) %>%
  summarise(tickets = n_distinct(ticket)) %>%
  ggplot(aes(x = fecha, y = tickets)) +
  geom_line(color = "steelblue") +
  labs(title = "Número de tickets por día", x = "Fecha", y = "Tickets")

# Productos vendidos por día de la semana
datos <- datos %>%
  mutate(
    fecha = ymd(fecha),
    dia_semana = wday(fecha, label = TRUE, abbr = FALSE, week_start = 1)
  )

ventas_por_dia <- datos %>%
  count(dia_semana)

ggplot(ventas_por_dia, aes(x = dia_semana, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Cantidad total de productos vendidos por día de la semana",
    x = "Día de la semana",
    y = "Productos vendidos"
  ) +
  theme_minimal()

# Productos vendidos por semana
# Mutar la columna para que 'semana' sea el número de la semana del año
datos <- datos %>%
  mutate(semana = floor_date(fecha, unit = "week", week_start = 1),
         semana_numero = format(semana, "%U"))  # Esto extrae el número de semana del año

# Crear el gráfico
ventas_por_semana <- datos %>%
  count(semana)

ggplot(ventas_por_semana, aes(x = semana, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    title = "Productos vendidos por semana del año",
    x = "Semana (fecha de inicio)",
    y = "Cantidad de productos"
  ) +
  theme_minimal() +
  scale_x_date(labels = scales::date_format("%U"), breaks = "1 week") # Muestra el número de la semana

top_10 <- compras_por_cliente %>% slice_head(n = 10)

ggplot(top_10, aes(x = reorder(id_cliente, productos_comprados), y = productos_comprados)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 clientes más que mas unidades compran",
       x = "ID Cliente", y = "Productos comprados") +
  theme_minimal()

# ================================
# ANÁLISIS CON CLÚSTERES Y CATEGORÍAS
# ================================

# Unir clusters con tickets limpios
df <- left_join(tickets_limpios, clusteres, by = "id_cliente_enc")
df <- df[ , -c(5,6,7)]  # Eliminar columnas innecesarias

# Clasificación general de productos
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
    )
  )

# Asegurar tipo de fecha y ordenar días
df$fecha <- ymd(df$dia)
df <- df %>%
  mutate(
    dia_semana = wday(dia, label = TRUE, abbr = FALSE, week_start = 1),
    dia_semana = factor(dia_semana, levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))
  )

# Agrupar por día y categoría
ventas_dia_producto <- df %>%
  group_by(dia_semana, producto_general) %>%
  summarise(cantidad_vendida = n(), .groups = "drop")
library(RColorBrewer)

# Agrupar y seleccionar top 5 por día
top5_dia <- ventas_dia_producto %>%
  group_by(dia_semana) %>%
  slice_max(order_by = cantidad_vendida, n = 5) %>%
  ungroup()

# Paleta de colores brillantes
colores_vivos <- brewer.pal(n = 8, name = "Set2")

# Gráfico final
ggplot(top5_dia, aes(x = dia_semana, y = cantidad_vendida, fill = producto_general)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 5 productos más vendidos por día de la semana",
    x = "Día de la semana",
    y = "Cantidad vendida",
    fill = "Producto general"
  ) +
  scale_fill_manual(values = colores_vivos) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


