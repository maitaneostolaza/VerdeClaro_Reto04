Maestroestr <- readRDS("Datos\\Originales\\maestroestr.RDS")
options(scipen=999)
lineas <- readRDS("Datos\\Originales\\tickets_enc.RDS")

str(lineas)
datos<-lineas
library(lubridate)
datos<-lineas
colnames(datos)<-c("fecha", "ticket", "cod_est", "id_cliente")
datos
datos$fecha<-ymd(datos$fecha)
datos$fecha
str(Maestroestr)

library(ggplot2)
library(dplyr)

#

# Top 10 establecimientos más frecuentes
datos$cod_est
a <- left_join(datos, Maestroestr, by = "cod_est")

# 2. Verificar las columnas (opcional)
colnames(a)

# 3. Contar productos más vendidos y graficar
a %>%
  count(descripcion, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(descripcion, n), y = n)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(title = "Top 10 productos más vendidos", x = "Producto")
       

#top 10 productos 
#cuanto han comprado por cada cliente
#cuanto tiempo ha estado comprando en eroski
#frequencia de compra

#Tickets por día
datos %>%
  group_by(fecha) %>%
  summarise(tickets = n_distinct(ticket)) %>%
  ggplot(aes(x = fecha, y = tickets)) +
  geom_line(color = "steelblue") +
  labs(title = "Número de tickets por día", x = "Fecha", y = "Tickets")
 #Productos comprados por cliente

#####################
library(tidyverse)
library(lubridate)

# Convertir fecha y obtener día de la semana
datos <- datos %>%
  mutate(
    fecha = ymd(fecha),
    dia_semana = wday(fecha, label = TRUE, abbr = FALSE, week_start = 1) # lunes = 1
  )

datos
# Contar productos vendidos por día de la semana
ventas_por_dia <- datos %>%
  count(dia_semana)
library(tidyverse)  # o al menos: library(dplyr)

# count()# Graficar
ggplot(ventas_por_dia, aes(x = dia_semana, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Cantidad total de productos vendidos por día de la semana",
    x = "Día de la semana",
    y = "Productos vendidos"
  ) +
  theme_minimal()


datos<-datos%>%
  mutate(
  fecha = ymd(fecha),
  semana = floor_date(fecha, unit = "week", week_start = 1)  # Agrupamos por inicio de semana (lunes)
)

ventas_por_semana <- datos %>%
  count(semana)

# Graficar
ggplot(ventas_por_semana, aes(x = semana, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    title = "Productos vendidos por semana",
    x = "Semana (fecha de inicio)",
    y = "Cantidad de productos"
  ) +
  theme_minimal()

##############
compras_por_cliente <- datos %>%
  group_by(id_cliente) %>%
  summarise(productos_comprados = n()) %>%
  arrange(desc(productos_comprados))  # Orden descendente

# Mostrar los 10 más compradores
top_10 <- compras_por_cliente %>% slice_head(n = 10)


# Graficar TOP 10
ggplot(top_10, aes(x = reorder(id_cliente, productos_comprados), y = productos_comprados)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 clientes más compradores",
       x = "ID Cliente", y = "Productos comprados") +
  theme_minimal()
#los liunes y asi que se compra? 




