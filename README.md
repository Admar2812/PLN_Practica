# PLN_Practica

🔵 Parte 1 – Descarga y extracción de enlaces a todos los artículos (Persona 1)

Objetivos:

Descargar el HTML del Código Penal desde
https://www.conceptosjuridicos.com/codigo-penal/

Identificar y extraer los enlaces a todos los artículos (1 a 616 quater).

Crear una tabla/tibble con:

URL del artículo

Número del artículo (p. ej., “39” o “616 quater”)

Su jerarquía textual: Libro / Título / Capítulo / Sección (cuando exista)

Esta persona solo entrega un dataset limpio con todos los links y jerarquías detectadas.

Salida esperada:

tabla_articulos.csv con columnas:

libro, titulo, capitulo, seccion, articulo, url

🟢 Parte 2 – Web scraping individual de artículos y limpieza del texto (Persona 2)

Objetivos:

Usar la tabla generada en la Parte 1 para visitar cada URL.

Extraer el texto del artículo, limpiarlo:

Eliminar menús, notas, anuncios, etc.

Insertar \n cada ~50 caracteres (no estricto).

Generar el contexto concatenado, p. ej.:

LIBRO I: Disposiciones generales…
Título III: De las penas
Capítulo I: De las penas, sus clases…
Sección III: De las penas privativas…


Preparar un tibble final con:

nombre_doc, texto_articulo, contexto, libro, titulo, capitulo, seccion, articulo


Salida esperada:

articulos_limpios.rds

🟡 Parte 3 – Construcción del corpus quanteda y docvars (Persona 3)

Objetivos:

Cargar el dataset limpio creado en la Parte 2.

Crear el corpus quanteda.

Asignar nombres de documento usando formato:

LIBRO I.Título III.Capítulo I.Sección III.Artículo 39


Añadir las docvars requeridas:

libro

titulo

capitulo

seccion

articulo

contexto

Guardar el corpus como:

corpus_codigo_penal.rds

Salida esperada:

corpus_codigo_penal.rds listo para análisis.

🟠 Parte 4 – Similaridad, distancias y dendrogramas (Persona 4)

Objetivos:

Cargar corpus_codigo_penal.rds.

Crear una matriz de documentos → dfm → distancias euclídeas.

Construir dendrograma:

Solo texto del artículo

Texto del artículo + contexto concatenado

Identificar:

Los dos artículos más similares en cada caso.

Generar gráficos y tablas con distancias mínimas.

Salida esperada:

dendrograma_articulos.png

dendrograma_articulos_contexto.png

Un archivo .txt indicando qué artículos son más parecidos.

🔴 Parte 5 – Extracción de entidades nombradas con spacyr y udpipe (Persona 5)

Objetivos:

Cargar el corpus.

Para cada documento del corpus:

Extraer entidades nominales con spacyr
(por ejemplo usando spacy_extract_entity() o spacy_parse() + filtrado)

Extraer keywords nominales con udpipe usando RAKE

Crear tablas de frecuencia.

Mostrar:

Top 20 entidades más frecuentes (spacyr)

Top 20 entidades más frecuentes (udpipe)

Número de entidades comunes entre ambas listas.

Guardar resultados en:

entidades_spacyr.csv

entidades_udpipe.csv

entidades_comunes.csv

Salida esperada:

Tres archivos con las comparaciones de entidades.

✔️ Resultado final

Con esta división, cada persona tiene una tarea claramente separada:

Persona	Parte	Enfoque principal
1	Extracción de enlaces	Web scraping + jerarquías
2	Descarga y limpieza	Scraping profundo + procesamiento
3	Corpus quanteda	Corpus + docvars
4	Dendrogramas	Distancias + clustering
5	Named Entities	NLP (spacyr + udpipe)
