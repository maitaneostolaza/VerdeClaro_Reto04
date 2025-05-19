library(dplyr)
library(tidyr)

tickets <- readRDS("Datos\\Transformados\\tickets_limpios.rds")
productos <- readRDS("Datos\\Originales\\maestroestr.RDS")
compras<-left_join(tickets, productos, by= "cod_est")
compras<- compras[, c(2,5)]
compras <- compras %>%
  mutate(presente = 1) %>%  # Añadimos una columna para marcar presencia
  distinct() %>%            # Eliminamos duplicados si existen
  pivot_wider(
    names_from = descripcion,
    values_from = presente,
    values_fill = list(presente = 0))


library(foreign)
codigos <- gsub("^X", "", colnames(compras))

tabla_nombres <- data.frame(
  original = colnames(compras),
  cod_est = codigos
) %>%
  left_join(productos, by = "cod_est")

nuevos_nombres <- ifelse(
  is.na(tabla_nombres$descripcion),
  tabla_nombres$original,
  tabla_nombres$descripcion
)

colnames(compras) <- nuevos_nombres

library(arules)
transacciones <- as(compras, "transactions")

reglas <- apriori(transacciones, parameter = list(
  support = 0.1,  # Aparece en al menos 14.678  tickets
  confidence = 0.8,
  minlen = 2,  #reglas con al menos 2 items
  maxlen= 4, #maximo 4 items por regla
  target = "rules"
))


inspect(head(reglas, 10))
inspect(reglas)

reglas<- as(reglas, "data.frame")


library(dplyr)
library(stringr)

# todas estas son reglas de asociacion con una confianza mayor al 80%

# extraer solo las reglas (filtrando líneas que contengan "==>")
reglas <- grep("==>", reglas, value = TRUE)

# Paso 3: extraer y limpiar los componentes de cada regla
datos <- data.frame(
  regla = reglas,
  stringsAsFactors = FALSE
) %>%
  mutate(
    # antecedente
    antecedente = str_trim(str_extract(regla, "^(.*?)==>")),
    antecedente = str_remove(antecedente, "==>$"),
    
    # Consecuente
    consecuente = str_trim(str_extract(regla, "(?<===> )(.*?)(?= \\d)")),
    
    # Lista de antecedentes reales (ítems que acaban en =t o =f)
    antecedentes_lista = str_extract_all(antecedente, ".?=t|.?=f")
  )

# Expandir los antecedentes a columnas (el maximo de antecedentes es 4)
max_items <- max(sapply(datos$antecedentes_lista, length))
ante_cols <- do.call(rbind, lapply(datos$antecedentes_lista, function(x) {
  length(x) <- max_items
  x
}))
colnames(ante_cols) <- paste0("antecedente_", 1:max_items)

reglas <- cbind(datos, ante_cols) #para juntar todo

reglas<- reglas[, -c(1,2,4)]
reglas<-reglas[, c(2,3,4,5,1)]

reglas$antecedente_1<-str_remove(reglas$antecedente_1, "=t")
reglas$antecedente_2<-str_remove(reglas$antecedente_2, "=t")
reglas$antecedente_3<-str_remove(reglas$antecedente_3, "=t")
reglas$antecedente_4<-str_remove(reglas$antecedente_4, "=t")
reglas$consecuente<-str_remove(reglas$consecuente, "=t")
reglas$antecedente_1 <- sub("^\\d+\\.\\s+", "", reglas$antecedente_1)

reglas

saveRDS(reglas,file = "Datos/Resultados/Reglas_de_asociacion.rds")