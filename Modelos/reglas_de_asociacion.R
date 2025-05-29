#Librerias
source("Scripts_preprocesamiento/Librerias.R", encoding = "UTF-8")


tickets <- readRDS("Datos\\Originales\\tickets_enc.RDS")
productos <- readRDS("Datos\\Originales\\maestroestr.RDS")
compras<-left_join(tickets, productos, by= "cod_est")
conteo_cod <- compras %>%
  group_by(cod_est) %>%
  summarise(cantidad = n(), .groups = "drop")

q1_cod <- quantile(conteo_cod$cantidad, 0.25, na.rm = TRUE)
q3_cod <- quantile(conteo_cod$cantidad, 0.75, na.rm = TRUE)

cod_filtrados <- conteo_cod %>%
  filter(cantidad > q3_cod | cantidad <= q1_cod) %>%
  pull(cod_est)

compras <- compras %>%
  filter(cod_est %in% cod_filtrados)

compras<- compras[, c(2,5)]
compras <- compras %>%
  mutate(presente = 1) %>%  # Añadimos una columna para marcar presencia
  distinct() %>%            # Eliminamos duplicados si existen
  pivot_wider(
    names_from = descripcion,
    values_from = presente,
    values_fill = list(presente = 0))


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


item_freq <- colSums(compras[,-1])
productos_frecuentes <- names(item_freq[item_freq >= 100])

compras_filtradas <- compras %>%
  select(num_ticket, all_of(productos_frecuentes))


df_binaria <- as.data.frame(compras_filtradas[,-1])
mat_binaria <- as.matrix(df_binaria)
mat_binaria <- mat_binaria == 1  # Convertir a TRUE/FALSE

# Crear objeto transacciones
transacciones <- as(mat_binaria, "transactions")

# Inspeccionar las transacciones
print(class(transacciones))
summary(transacciones)
inspect(head(transacciones, 5))


# Generar reglas de asociación
reglas <- apriori(transacciones, parameter = list(
  support = 0.01,       # al menos el 1% de los tickets
  confidence = 0.6,
  minlen = 2,
  maxlen = 3,
  target = "rules"
))

summary(quality(reglas))
# Extraer las reglas en formato texto
reglas_filtradas <- subset(reglas, subset = support > 0.01 & confidence > 0.8 & lift > 1.1)
reglas_txt <- labels(reglas_filtradas)

# Separar antecedente y consecuente
split_rules <- strsplit(reglas_txt, " => ", fixed = TRUE)
antecedentes <- sapply(split_rules, `[[`, 1)
consecuentes <- sapply(split_rules, `[[`, 2)

# Separar los antecedentes en lista
ante_list <- strsplit(antecedentes, ",\\s*")

# Determinar automáticamente cuántos ítems máximos hay en un antecedente
max_items <- max(sapply(ante_list, length))

# Prealocar matriz de antecedentes
ante_mat <- matrix(NA_character_, nrow = length(ante_list), ncol = max_items)
for (i in seq_along(ante_list)) {
  n_items <- length(ante_list[[i]])
  ante_mat[i, 1:n_items] <- ante_list[[i]]
}

# Asignar nombres de columna dinámicamente
colnames(ante_mat) <- paste0("antecedente_", seq_len(max_items))

# Crear el data.frame final
reglas_df <- data.frame(
  ante_mat,
  consecuente = consecuentes,
  stringsAsFactors = FALSE
)
reglas_df<- reglas_df[,-c(3:6)] #maximo hay dos antecedentes

# Elimina todas las llaves { y } de la columna
reglas_df$antecedente_1 <- gsub("[\\{\\}]", "", reglas_df$antecedente_1)
reglas_df$antecedente_2 <- gsub("[\\{\\}]", "", reglas_df$antecedente_2)
reglas_df$consecuente <- gsub("[\\{\\}]", "", reglas_df$consecuente)


# Guardar data.frame como RDS
# saveRDS(reglas_df, file = "Datos/Resultados/Reglas_de_asociacion.rds")
# me dice que el archivo es demasiado grando y no me deja hacer push;
# entonces voy a subirlo directo a git