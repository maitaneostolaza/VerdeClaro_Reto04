library(plumber)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# Cargar datos de resultados finales
resultado1 <- readRDS("Datos/Resultados/Objetivo1_resultado.rds")
resultado2 <- readRDS("Datos/Resultados/Objetivo2_resultado.rds")
resultado3 <- readRDS("Datos/Resultados/Objetivo3_resultado.rds")
resultado4 <- readRDS("Datos/Resultados/Objetivo4_resultado.rds")
maestro    <- readRDS("Datos/Originales/maestroestr.RDS")


#* Recomendación: artículo promocionado (Objetivo 1 adaptado)
#* @get recomendar_promocion
function(){
  # El nombre del producto está como rowname o primera columna
  nombre_prod <- rownames(resultado1)[1]
  if (is.null(nombre_prod)) {
    nombre_prod <- resultado1[1, 1]
  }
  
  # Obtener los clientes (valores de la fila, excepto la 1ª si es nombre)
  clientes <- as.character(resultado1[1, ])
  clientes <- clientes[clientes != nombre_prod]  # eliminar el nombre del producto si aparece como valor
  
  # Buscar código del producto a partir del nombre (en maestro)
  cod_prod <- maestro %>%
    filter(descripcion == nombre_prod) %>%
    pull(cod_est)
  
  return(list(
    producto_promocionado_cod = cod_prod,
    producto_promocionado_nombre = nombre_prod,
    clientes_recomendados = clientes
  ))
}



#* Recomendación: otros como tú han comprado (Objetivo 2)
#* @param cliente_id ID del cliente
#* @get recomendar_otros_como_tu
function(cliente_id){
  if (!"Id_cliente" %in% colnames(resultado2) || !"cod_est" %in% colnames(resultado2)) {
    return(list(error = "Estructura incorrecta del archivo Objetivo2_resultado.rds"))
  }
  
  fila <- resultado2 %>% filter(Id_cliente == cliente_id)
  
  if (nrow(fila) == 0) {
    return(list(error = "Cliente no encontrado en el objetivo 2."))
  }
  
  cod_prod <- fila$cod_est[1]
  nombre_prod <- fila$descripcion[1]
  
  return(list(
    cliente = cliente_id,
    producto_recomendado_cod = cod_prod,
    producto_recomendado_nombre = nombre_prod
  ))
}


#* Recomendación: producto en oferta más adecuado (Objetivo 3)
#* @param cliente_id ID del cliente
#* @get recomendar_oferta
function(cliente_id){
  if (!"Id_cliente" %in% colnames(resultado3) || !"cod_est" %in% colnames(resultado3)) {
    return(list(error = "Estructura incorrecta del archivo Objetivo3_resultado.rds"))
  }
  
  fila <- resultado3 %>% filter(Id_cliente == cliente_id)
  
  if (nrow(fila) == 0) {
    return(list(error = "Cliente no encontrado en el objetivo 3."))
  }
  
  cod_prod <- fila$cod_est[1]
  nombre_prod <- fila$descripcion[1]
  
  return(list(
    cliente = cliente_id,
    producto_en_oferta_cod = cod_prod,
    producto_en_oferta_nombre = nombre_prod
  ))
}


#* Recomendación: ítem olvidado (Objetivo 4)
#* @param cliente_id ID del cliente
#* @get recomendar_olvido
function(cliente_id){
  fila <- resultado4 %>% filter(id_cliente_enc == cliente_id)
  
  if (nrow(fila) == 0) {
    return(list(error = "Cliente no encontrado en el objetivo 4."))
  }
  
  cod_prod <- fila$cod_est[1]
  nombre_prod <- maestro %>%
    filter(cod_est == cod_prod) %>%
    pull(descripcion)
  
  return(list(
    cliente = cliente_id,
    producto_olvidado_cod = cod_prod,
    producto_olvidado_nombre = nombre_prod
  ))
}

Subir a main
