# ================================
# CARGA DE LIBRERÍAS
# ================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(scales)
library(RColorBrewer)

# ================================
# DEFINICIÓN DE PALETA DE COLORES
# ================================
eroski_palette_extended <- c(
  "#E10A23", # Rojo corporativo
  "#B00A1C", # Rojo oscuro
  "#F0928E", # Rosa claro
  "#FFD5D1", # Rosa muy claro
  "#0074B5", # Azul corporativo
  "#005B92", # Azul oscuro
  "#A2CBE8", # Azul claro
  "#FADED6"  # Beige claro
)

# ================================
# CARGA DE DATOS
# ================================
maestroestr <- readRDS("Datos\\Originales\\maestroestr.RDS")
tickets_enc <- readRDS("Datos\\Originales\\tickets_enc.RDS")
tickets_limpios <- readRDS("Datos\\Transformados\\tickets_limpios.rds")
clusteres <- readRDS("Datos\\Transformados\\df_con_clusteres.rds")

# ================================
# PREPROCESAMIENTO DE DATOS
# ================================
datos <- tickets_enc
colnames(datos) <- c("fecha", "ticket", "cod_est", "id_cliente")
datos$fecha <- ymd(datos$fecha)

datos_enriquecidos <- left_join(datos, maestroestr, by = "cod_est")

# ================================
# ANÁLISIS INICIAL
# ================================

# Top 10 productos más vendidos
datos_enriquecidos %>%
  count(descripcion, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(descripcion, n), y = n)) +
  geom_col(fill = eroski_palette_extended[1]) +
  coord_flip() +
  labs(title = "Top 10 productos más vendidos", x = "Producto") +
  theme_minimal()

# Tickets por día
datos %>%
  group_by(fecha) %>%
  summarise(tickets = n_distinct(ticket)) %>%
  ggplot(aes(x = fecha, y = tickets)) +
  geom_line(color = eroski_palette_extended[5]) +
  labs(title = "Número de tickets por día", x = "Fecha", y = "Tickets") +
  theme_minimal()

# Productos vendidos por día de la semana
datos <- datos %>%
  mutate(
    fecha = ymd(fecha),
    dia_semana = wday(fecha, label = TRUE, abbr = FALSE, week_start = 1),
    dia_semana = factor(dia_semana, levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))
  )

ventas_por_dia <- datos %>%
  filter(dia_semana != "domingo") %>%
  count(dia_semana)

ggplot(ventas_por_dia, aes(x = dia_semana, y = n)) +
  geom_bar(stat = "identity", fill = eroski_palette_extended[7]) +
  labs(
    title = "Cantidad total de productos vendidos por día de la semana",
    x = "Día de la semana",
    y = "Productos vendidos"
  ) +
  scale_y_continuous(labels = label_comma()) +
  theme_minimal()

# Productos vendidos por semana
datos <- datos %>%
  mutate(semana = floor_date(fecha, unit = "week", week_start = 1),
         semana_numero = format(semana, "%U"))

ventas_por_semana <- datos %>%
  count(semana)

options(scipen = 999)

ggplot(ventas_por_semana, aes(x = semana, y = n)) +
  geom_bar(stat = "identity", fill = eroski_palette_extended[7], color = eroski_palette_extended[6]) +
  labs(
    title = "Productos vendidos por semana del año",
    x = "Semana (fecha de inicio)",
    y = "Cantidad de productos"
  ) +
  theme_minimal() +
  scale_x_date(labels = date_format("%U"), breaks = "1 week")

# Top 10 clientes con más unidades compradas
compras_por_cliente <- datos %>%
  count(id_cliente, name = "productos_comprados") %>%
  arrange(desc(productos_comprados))

top_10 <- compras_por_cliente %>% slice_head(n = 10)

ggplot(top_10, aes(x = reorder(id_cliente, productos_comprados), y = productos_comprados)) +
  geom_bar(stat = "identity", fill = eroski_palette_extended[2]) +
  coord_flip() +
  labs(title = "Top 10 clientes que más unidades compran",
       x = "ID Cliente", y = "Productos comprados") +
  theme_minimal()

# ================================
# ANÁLISIS CON CLÚSTERES Y CATEGORÍAS
# ================================

df <- left_join(tickets_limpios, clusteres, by = "id_cliente_enc")
df <- df[ , -c(5,6,7)]

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

df$fecha <- ymd(df$dia)
df <- df %>%
  mutate(
    dia_semana = wday(dia, label = TRUE, abbr = FALSE, week_start = 1),
    dia_semana = factor(dia_semana, levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))
  )

ventas_dia_producto <- df %>%
  filter(dia_semana != "domingo") %>%
  group_by(dia_semana, producto_general) %>%
  summarise(cantidad_vendida = n(), .groups = "drop")

top5_dia <- ventas_dia_producto %>%
  group_by(dia_semana) %>%
  slice_max(order_by = cantidad_vendida, n = 5) %>%
  ungroup()

ggplot(top5_dia, aes(x = dia_semana, y = cantidad_vendida, fill = producto_general)) +
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
