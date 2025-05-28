## 🗂️ Estructura del proyecto

Este repositorio contiene todo el trabajo desarrollado durante el Reto 4, centrado en construir un sistema de recomendación basado en datos reales de compra facilitados por EROSKI. La organización de carpetas permite localizar fácilmente cada componente del proyecto:

```plaintext
├── Aplicaciones/                  # Aplicaciones ejecutables del proyecto
│   ├── Api.R                      # Código principal de la API (Plumber)
│   ├── EjecutarApi.R              # Script para lanzar la API localmente
│   └── ShinyApp.R                 # Aplicación Shiny para visualización interactiva
│
├── Datos/
│   ├── Originales/                # Datos originales proporcionados (.RDS)
│   ├── Transformados/             # Datos filtrados y preparados para análisis
│   └── Resultados/                # Resultados generados por los distintos modelos
│
├── Graficos/                      # Imágenes y visualizaciones generadas
│
├── Modelos/                       # Scripts con lógica de recomendación
│   ├── Recomendadores binarizadas.R       # Alternativa usando binarización
│   ├── Comparando algoritmos.R            # Evaluación de distintos algoritmos
│   └── reglas_de_asociacion.R             # Reglas de asociación (Apriori, Eclat)
│
├── Scripts_Estadisticos/
│   └── AnalisisDescriptivos.R             # Análisis exploratorios y gráficos
│
├── Scripts_preprocesamiento/
│   ├── Limpieza de datos.R                # Comprobaciones y transformaciones iniciales
│   ├── Limpieza de datos Reduccion.R      # Aplicación de filtros y reducción de datos
│   ├── Clustering.R                       # Agrupación de clientes con k-means
│   ├── matriz.R                           # Generación de matriz cliente-producto
│   └── Librerias.R                        # Carga de paquetes utilizados
│
├── Principal.R                   # Script integrador general del proyecto
├── RETO04_VerdeClaro_R.Rproj     # Proyecto RStudio
└── README.md                     # Este documento
```

🌿 Ramas del repositorio
Durante el desarrollo del proyecto se han utilizado diferentes ramas para organizar el trabajo por bloques funcionales:

main: rama principal y estable del proyecto.

Recomendadores: desarrollo de los distintos sistemas de recomendación.

Mate: análisis estadístico, cálculos exploratorios y validaciones.

Shinny: implementación de la app interactiva en Shiny.

Api: construcción y pruebas de la API REST con Plumber.

Una vez finalizado el proyecto, se procederá a hacer merge de los archivos más relevantes de cada rama en main y se eliminarán las ramas auxiliares.

🧭 Cómo probar la API de Recomendaciones
Una vez tengas abierta la interfaz de la API en tu navegador, verás una pantalla con todas las funcionalidades disponibles. Desde ahí puedes probarlas fácilmente sin necesidad de escribir código.

A continuación, te explicamos cómo funciona cada una:

🔹 Recomendación de producto promocionado (/recomendar_promocion)
Devuelve el producto promocionado y los 10 clientes seleccionados como receptores ideales.

Haz clic en recomendar_promocion.

Pulsa “Try it out” y después “Execute”.

Verás el código y nombre del producto, y la lista de clientes recomendados.

🔹 Recomendación basada en clientes similares (/recomendar_otros_como_tu)
Devuelve el producto que otros clientes similares han comprado, pero que el cliente indicado aún no ha adquirido.

Haz clic en recomendar_otros_como_tu.

Pulsa “Try it out”.

Introduce un cliente_id del objetivo 2.

Pulsa “Execute” y consulta el resultado.

🔹 Recomendación de producto en oferta (/recomendar_oferta)
Devuelve qué producto en oferta se le asigna a un cliente específico.

Haz clic en recomendar_oferta.

Pulsa “Try it out”.

Introduce un cliente_id del objetivo 3.

Pulsa “Execute” para ver el producto recomendado.

🔹 Recomendación de producto olvidado (/recomendar_olvido)
Identifica qué producto ha podido olvidar el cliente en su última compra.

Despliega recomendar_olvido.

Pulsa “Try it out”.

Introduce un cliente_id del objetivo 4.

Pulsa “Execute” y obtendrás el producto olvidado (si lo hay).
