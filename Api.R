library(plumber)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# Cargar datos
tickets <- readRDS("Datos/Transformados/tickets_Reducidos.rds")
objetivos <- readRDS("Datos/Originales/objetivos.RDS")
maestro <- readRDS("Datos/Originales/maestroestr.RDS")

# Extraer objetivos
objetivo1 <- objetivos$objetivo1$obj
objetivo2 <- objetivos$objetivo2$obj
objetivo3 <- objetivos$objetivo3$obj
objetivo4 <- objetivos$objetivo4$obj


#* Recomendación: artículo promocionado
#* @get /recomendar_promocion
function(){
  prod <- objetivo1[1]
  
  top_clientes <- tickets %>%
    filter(cod_est == prod) %>%
    count(id_cliente_enc, sort = TRUE) %>%
    slice_head(n = 10) %>%
    pull(id_cliente_enc)
  
  nombre_prod <- maestro %>%
    filter(cod_est == prod) %>%
    pull(descripcion)
  
  return(list(
    producto_promocionado_cod = prod,
    producto_promocionado_nombre = nombre_prod,
    clientes_recomendados = top_clientes
  ))
}


#* Recomendación: otros como tú han comprado
#* @param cliente_id ID del cliente
#* @get /recomendar_otros_como_tu
function(cliente_id){
  if (!(cliente_id %in% objetivo2)) {
    return(list(error = "Cliente no está en la lista objetivo2."))
  }
  
  productos_cliente <- tickets %>%
    filter(id_cliente_enc == cliente_id) %>%
    pull(cod_est) %>% unique()
  
  productos_similares <- tickets %>%
    filter(id_cliente_enc %in% objetivo2, !(cod_est %in% productos_cliente)) %>%
    count(cod_est, sort = TRUE)
  
  if (nrow(productos_similares) == 0) {
    return(list(error = "No hay productos nuevos para recomendar a este cliente."))
  }
  
  cod_recomendado <- productos_similares %>%
    slice_head(n = 1) %>%
    pull(cod_est)
  
  nombre_prod <- maestro %>%
    filter(cod_est == cod_recomendado) %>%
    pull(descripcion)
  
  return(list(
    cliente = cliente_id,
    producto_recomendado_cod = cod_recomendado,
    producto_recomendado_nombre = nombre_prod
  ))
}


#* Recomendación: producto en oferta más adecuado
#* @param cliente_id ID del cliente
#* @get /recomendar_oferta
function(cliente_id){
  productos_cliente <- tickets %>%
    filter(id_cliente_enc == cliente_id) %>%
    pull(cod_est) %>% unique()
  
  productos_posibles <- setdiff(objetivo3, productos_cliente)
  
  recomendacion <- tickets %>%
    filter(cod_est %in% productos_posibles) %>%
    count(cod_est, sort = TRUE) %>%
    slice_head(n = 1) %>%
    pull(cod_est)
  
  if (length(recomendacion) == 0) {
    return(list(cliente = cliente_id, mensaje = "No se ha encontrado ningún producto en oferta adecuado."))
  }
  
  nombre_prod <- maestro %>%
    filter(cod_est == recomendacion) %>%
    pull(descripcion)
  
  return(list(
    cliente = cliente_id,
    producto_en_oferta_cod = recomendacion,
    producto_en_oferta_nombre = nombre_prod
  ))
}


#* Recomendación: ítem olvidado (con nombre del producto)
#* @param cliente_id ID del cliente
#* @get /recomendar_olvido
function(cliente_id){
  if (!(cliente_id %in% objetivo4)) {
    return(list(error = "Cliente no en lista de objetivo 4"))
  }
  
  historial <- tickets %>%
    filter(id_cliente_enc == cliente_id)
  
  ultima_fecha <- max(historial$dia)
  ultima_cesta <- historial %>%
    filter(dia == ultima_fecha) %>%
    pull(cod_est) %>%
    unique()
  
  productos_anteriores <- historial %>%
    filter(dia < ultima_fecha) %>%
    pull(cod_est) %>%
    unique()
  
  olvidados <- setdiff(productos_anteriores, ultima_cesta)
  
  olvidados_freq <- historial %>%
    filter(cod_est %in% olvidados, dia < ultima_fecha) %>%
    count(cod_est) %>%
    filter(n >= 2) %>%
    arrange(desc(n))
  
  if (nrow(olvidados_freq) == 0) {
    return(list(cliente = cliente_id, mensaje = "No se ha detectado ningún producto olvidado."))
  }
  
  producto_final_cod <- olvidados_freq %>%
    slice_head(n = 1) %>%
    pull(cod_est)
  
  nombre_prod <- maestro %>%
    filter(cod_est == producto_final_cod) %>%
    pull(descripcion)
  
  return(list(
    cliente = cliente_id,
    producto_olvidado_cod = producto_final_cod,
    producto_olvidado_nombre = nombre_prod
  ))
}
