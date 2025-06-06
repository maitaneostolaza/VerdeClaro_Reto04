## 📂 Estructura del Proyecto de Data Science - Reto 4

Este repositorio contiene todo el trabajo desarrollado durante el Reto 4, centrado en construir un sistema de recomendación basado en datos reales de compra facilitados por EROSKI. La organización de carpetas permite localizar fácilmente cada componente del proyecto:


```plaintext
├── Aplicaciones/                  # Aplicaciones ejecutables
│   ├── Api.R                      # Código principal de la API con Plumber
│   ├── EjecutarApi.R              # Script para lanzar la API localmente
│   └── ShinyApp.R                 # Aplicación Shiny visualización interactiva
│
├── Datos/
│   ├── Originales/                # Datos originales entregados por EROSKI (.RDS)
│   ├── Transformados/             # Datos filtrados, limpiados y estructurados
│   └── Resultados/                # Salidas de modelos, matrices y comprobaciones
│
├── Graficos/                      # Visualizaciones y gráficos generados
│
├── Modelos/                       # Scripts de modelos y recomendadores
│   ├── Comparando algoritmos.R
│   ├── Recomendadores binarizadas.R
│   └── reglas_de_asociacion.R
│
├── Scripts_Estadisticos/          # Análisis descriptivo y exploratorio
│   └── AnalisisDescriptivos.R
│
├── Scripts_preprocesamiento/      # Limpieza, reducción, librerias, Funciones y clustering
│   ├── Limpieza de datos.R
│   ├── Limpieza de datos Reduccion.R
│   ├── Clustering.R
│   ├── matriz.R
│   ├── Funciones.R
│   └── Librerias.R
│
├── Principal.R                    # Script central de integración y ejecución
├── .gitignore                     # Archivo para omitir archivos
├── RETO04_VerdeClaro_R.Rproj      # Archivo de proyecto RStudio
└── README.md                      # Este documento
```

---

💻 Cómo descargar el proyecto en tu equipo

Si quieres probar el proyecto en tu propio ordenador, puedes clonar este repositorio desde GitHub. 
### 👉 Pasos:

1. Abre una terminal o consola (por ejemplo, Git Bash en Windows).
2. Ejecuta el siguiente comando para clonar el repositorio:

```
git clone https://github.com/maitaneostolaza/VerdeClaro_Reto04
```

---

## 🌿 Ramas del repositorio
Durante el desarrollo del proyecto se han utilizado diferentes ramas para organizar el trabajo por bloques funcionales:

- `main`: rama principal y estable del proyecto.  
- `Recomendadores`: desarrollo de los distintos sistemas de recomendación.  
- `Shinny`: implementación de la app interactiva en Shiny.  
- `Api`: construcción y pruebas de la API REST con Plumber.

Una vez finalizado el proyecto, se procederá a hacer merge de las ramas.

---

## 🧭 Cómo probar la API de Recomendaciones
Una vez tengas abierta la **interfaz de la API en tu navegador**, verás una pantalla con todas las funcionalidades disponibles. Hemos dejado preparados algunos identificadores de cliente para que puedas probar fácilmente cómo funciona cada recomendador.
Puedes verlos directamente en la parte superior de la interfaz, dentro de la descripción de la API.

📝 Algunos de estos clientes obtendrán una recomendación, y otros no, simplemente porque no seran de ese recomendador. Así puedes ver cómo responde la API en ambos casos.

A continuación, te explicamos cómo funciona cada una:


### 🔹 **/recomendar_promocion**  
📌 Devuelve el producto promocionado y los **10 clientes seleccionados** como receptores ideales.

1. Haz clic en `recomendar_promocion`.  
2. Pulsa **“Try it out”** y después **“Execute”**.  
3. Verás el **código y nombre del producto**, y la lista de **clientes recomendados**.



### 🔹 **/recomendar_otros_como_tu**  
📌 Devuelve el producto que otros clientes similares han comprado, pero que el cliente indicado aún no ha adquirido.

1. Haz clic en `recomendar_otros_como_tu`.  
2. Pulsa **“Try it out”**.  
3. Introduce un `cliente_id` del objetivo 2.  
4. Pulsa **“Execute”** y consulta el resultado.



### 🔹 **/recomendar_oferta**  
📌 Devuelve qué producto en oferta se le asigna a un cliente específico.

1. Haz clic en `recomendar_oferta`.  
2. Pulsa **“Try it out”**.  
3. Introduce un `cliente_id` del objetivo 3.  
4. Pulsa **“Execute”** para ver el **producto recomendado**.



### 🔹 **/recomendar_olvido**  
📌 Identifica qué producto ha podido olvidar el cliente en su última compra.

1. Despliega `recomendar_olvido`.  
2. Pulsa **“Try it out”**.  
3. Introduce un `cliente_id` del objetivo 4.  
4. Pulsa **“Execute”** y obtendrás el **producto olvidado** (si lo hay).


