library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(tidyverse)
library(purrr)
library(plotly)

# Cargamos los ficheros
Maestroestr <- readRDS("Datos\\Originales\\maestroestr.RDS")

tickets_enc <- readRDS("Datos\\Originales\\tickets_enc.RDS")

########################## DATA DISCOVERING
str(tickets_enc)
summary(tickets_enc)

# Pasar a fecha la columna dia
tickets_enc$dia <- ymd(tickets_enc$dia)

# Los tickets solo pueden aparecer para el mismo cliente y para una sola fecha
cuantos <- tickets_enc %>% group_by(dia,num_ticket,id_cliente_enc) %>% 
  summarise(cuantos = n())

# como hay tickets que estan mal, juntamos la columna del clienteID con el numero del ticket
tickets_enc <- tickets_enc %>% 
  mutate(num_ticket = str_c(num_ticket,id_cliente_enc))
colnames(tickets_enc)


# quitamos duplicados sobre fichero original
Duplicados2 <- tickets_enc %>% #Ver duplicados 
  group_by(num_ticket, cod_est, id_cliente_enc,dia) %>%
  summarise(NumRegistros = n(), .groups = "keep") %>%
  filter(NumRegistros > 1)
print(Duplicados2)

sum(Duplicados2$NumRegistros)


tickets_enc_sin_duplicados <- tickets_enc %>%
  distinct(dia,num_ticket, cod_est, id_cliente_enc, .keep_all=TRUE)


# guardar el fichero en transformados
saveRDS(tickets_enc_sin_duplicados, file = "Datos/Transformados/tickets_limpios.rds")



