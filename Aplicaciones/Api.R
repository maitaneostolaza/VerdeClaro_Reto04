#Librerias
source("../Scripts_preprocesamiento/Librerias.R", encoding = "UTF-8")

#* @apiTitle API de recomendación de productos
#* @apiDescription Esta es una API que recomienda productos a clientes.<br><br>
#* <b>Ejemplos para probar los endpoints:</b><br><br>
#* <b>Objetivo 2 - Otros como tú han comprado</b><br>
#* 26f424b3bba6aaf97952ac599ed39f75<br>
#* 8b9aa623b654a8be21b316a5fdf41007<br>
#* 339d3222968f1b34f7ef40429a179efd<br><br>
#* <b>Objetivo 3 - Producto en oferta más adecuado</b><br>
#* 000c75b30bb0b237ec817d203e88a54f<br>
#* 00322c1d439808b60d1e5717956afa8e<br>
#* 503a6539df48964124fe026b9deb5d13<br><br>
#* <b>Objetivo 4 - Ítem olvidado</b><br>
#* 503a6539df48964124fe026b9deb5d13<br>
#* 1d98f84a5f074ed9c7a47515d4f5f329<br>
#* 26f424b3bba6aaf97952ac599ed39f75<br>

# Cargar datos de resultados finales
resultado1 <- readRDS("../Datos/Resultados/Objetivo1_resultado.rds")
resultado2 <- readRDS("../Datos/Resultados/Objetivo2_resultado.rds")
resultado3 <- readRDS("../Datos/Resultados/Objetivo3_resultado.rds")
resultado4 <- readRDS("../Datos/Resultados/Objetivo4_resultado.rds")
maestro    <- readRDS("../Datos/Originales/maestroestr.RDS")


#* Recomendación: artículos promocionados (Objetivo 1)
#* @get /recomendar_promocion
function() {
  resultado1_df <- as.data.frame(resultado1)
  
  nombre_prod <- colnames(resultado1_df)[1]
  
  clientes <- resultado1_df[[1]] %>% as.character()
  
  return(list(
    producto_promocionado_nombre = list(nombre_prod),
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
