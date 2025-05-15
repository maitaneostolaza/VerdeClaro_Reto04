library(dplyr)
library(stringr)
lineas <- readLines("Datos/Resultados/reglas_de_asociacion.txt")

# todas estas son reglas de asociacion con una confianza mayor al 90%

# extraer solo las reglas (filtrando líneas que contengan "==>")
reglas <- grep("==>", lineas, value = TRUE)

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
    antecedentes_lista = str_extract_all(antecedente, ".*?=t|.*?=f")
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
