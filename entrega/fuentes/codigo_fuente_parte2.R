library(rvest)
library(dplyr)
library(stringr)
library(purrr)

articulos <- read.csv("~/Downloads/tabla_articulos.csv", stringsAsFactors = FALSE)
articulos$articulo <- gsub("/", "", articulos$articulo) #normalizamos a 1 (venía con formato tipo /1)

extraer_texto_articulo <- function(url) {
  pagina <- rvest::read_html(url)
  
  texto <- pagina %>%
    rvest::html_elements("p") %>%
    rvest::html_text() %>%
    paste(collapse = " ") %>%
    stringr::str_squish()
  
  texto <- stringr::str_split(texto, "\\bart\\s*\\d+\\s*cp\\b", n = 2)[[1]][1]
  texto <- stringr::str_split(texto, "\\bEl artículo\\b", n = 2)[[1]][1]
  texto <- stringr::str_squish(texto)
  
  texto
}

insertar_saltos <- function(texto, n = 50) {
  stringr::str_wrap(texto, width = n)
}

crear_contexto <- function(libro, titulo, capitulo, seccion) {
  partes <- c(libro, titulo, capitulo, seccion)
  partes <- partes[!is.na(partes) & partes != ""]
  paste(partes, collapse = "\n")
}

crear_nombre_doc <- function(libro, titulo, capitulo, seccion, articulo) {
  partes <- c(libro, titulo, capitulo, seccion, paste("Artículo", articulo))
  partes <- partes[!is.na(partes) & partes != ""]
  paste0("art.", articulo, ":", paste(partes, collapse = ":"))
}

test <- articulos %>%
  slice(1:3) %>%
  mutate(
    texto_articulo = map_chr(url, extraer_texto_articulo),
    texto_articulo = map_chr(texto_articulo, insertar_saltos),
    contexto = pmap_chr(list(libro, titulo, capitulo, seccion), crear_contexto),
    nombre_doc = pmap_chr(list(libro, titulo, capitulo, seccion, articulo), crear_nombre_doc)
  )

cat(test$nombre_doc[1], "\n\n")
cat(test$contexto[1], "\n\n")
cat(test$texto_articulo[1], "\n")

articulos_limpios <- articulos %>%
  mutate(
    texto_articulo = map_chr(url, extraer_texto_articulo),
    texto_articulo = map_chr(texto_articulo, insertar_saltos),
    contexto = pmap_chr(list(libro, titulo, capitulo, seccion), crear_contexto),
    nombre_doc = pmap_chr(list(libro, titulo, capitulo, seccion, articulo), crear_nombre_doc)
  )

saveRDS(articulos_limpios, "articulos_limpios.rds")
write.csv(articulos_limpios, "articulos_limpios.csv", row.names = FALSE)