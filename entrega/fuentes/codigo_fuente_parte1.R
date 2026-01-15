
#Funcion para cargar la pagina web del codigo penal
cargar_texto<-function(){
  #URL de la pagina del codigo penal
  URL_codigo_penal<-"https://www.conceptosjuridicos.com/codigo-penal/"
  #Leemos la pagina del codigo penal
  codigo_penal_pagina <- paste(readLines(URL_codigo_penal,encoding = "UTF-8"),collapse="\n")
  return(codigo_penal_pagina)
}

#Funcion para dada una pagina web devolver todos los links de los articulos de esta
buscar_links_articulos<-function(pagina_web_codigo_penal){
  #Buscamos en la pagina todos los links
  library(stringr)
  #patron_solo_articulo<-"https://www.conceptosjuridicos.com/codigo-penal-articulo-[1-90]*/"
  patron<-"https://www.conceptosjuridicos.com/codigo-penal-articulo-[1-90]*[-[:alnum:]]*/"
  articulos <- str_extract_all(pagina_web_codigo_penal,patron)
  return(unlist(articulos))
}

#Dada una pagina web de un articulo y la uri de esta devuelve la estructura del articulo
obtener_estructura<-function(URL_articulo,pagina_web_articulo){
  library(stringr)
  
  #Vemos el numero del articulo para ver como recoger la estructura
  numero_articulo <- str_extract(URL_articulo,'[0-9]+')
  
  #Inicializamos las variables para que luego se devuelva el mismo tipo de lista
  titulo_articulo<-"NA"
  libro_articulo<-"NA"
  capitulo_articulo<-"NA"
  seccion_articulo<-"NA"
  
  #Vemos si el titulo es preliminar
  if (nchar(numero_articulo)<2){
    titulo_articulo<-str_extract(pagina_web_articulo,"TÍTULO PRELIMINAR: [[:alnum:][:punct:] ]*")
  }else{
    #Sacamos el libro
    libro_articulo<-str_extract(pagina_web_articulo,"LIBRO [I]*")
    #Sacamos el titulo
    titulo_articulo<-str_extract(pagina_web_articulo,"T[ií]tulo [IV]*")
    
    #Sacamos el capitulo
    capitulo_articulo<-str_extract(pagina_web_articulo,"Cap[ií]tulo [IVX]*")
    
    #Sacamos la seccion si hay
    seccion_articulo<-str_extract(pagina_web_articulo,"Sección [IVX]*")
  }
  
  lista_resultado<-list(libro=libro_articulo,titulo=titulo_articulo,capitulo=capitulo_articulo,seccion=seccion_articulo)
  return(lista_resultado)
}


#Funcion que se encarga de los bucles repetidos
sacar_contenido <- function(inicio_f, final_f, articulos_f, tabla_articulos_f) {
  library(stringr)
  
  # Extraemos los links del rango solicitado
  links_rango <- unlist(articulos_f)[inicio_f:final_f]
  
  #leemos cada uno de los articulos
  for (articulo in links_rango) {
    numero_articulo <- str_extract(articulo, '[0-9]+.*')
    
    #datos iniciales del bucle while    
    pagina_web <- NULL
    intento <- 1
    
    #forzamos a que saque el contenido aunque salgan errores
    while (is.null(pagina_web)) {
      pagina_web <- tryCatch({
        # Pausa pequeña antes de cada lectura (0.5 a 1.5 segundos)
        
        message(paste("Leyendo artículo", numero_articulo, "- Intento:", intento))
        
        # Intentamos leer la página
        paste(readLines(articulo, encoding = "UTF-8", warn = FALSE), collapse = "\n")
        
      }, error = function(e) {
        message(paste("Fallo en el intento", intento, "para el artículo", numero_articulo))
        return(NULL) # Si hay error, devuelve NULL para que el while siga
      })
      
      intento <- intento + 1
    }
    
    #si se han sacado lo datos guardarlos en el df
    if (!is.null(pagina_web)) {
      lista <- obtener_estructura(articulo, pagina_web)
      
      nueva_fila <- data.frame(
        libro = as.character(lista["libro"]),
        titulo = as.character(lista["titulo"]),
        capitulo = as.character(lista["capitulo"]),
        seccion = as.character(lista["seccion"]),
        articulo = numero_articulo,
        url = articulo,
        stringsAsFactors = FALSE
      )
      
      tabla_articulos_f <- rbind(tabla_articulos_f, nueva_fila)
      
    }
  }
  #retornar el df
  return(tabla_articulos_f)
}

#Primera parte del trabajo
primera_parte<-function(){
  
  tabla_articulos <- data.frame(
    libro = character(),
    titulo = character(),
    capitulo = character(),
    seccion = character(),
    articulo = character(),
    url = character(),
    stringsAsFactors = FALSE
  )
  
  
  web<-cargar_texto()
  articulos<-buscar_links_articulos(web)
  
  #Llamamos a la funcion sacar_contenido en varias ocasiones para reducir el posible error causado por tener que leer paginas
  #web
  tabla_articulos <- rbind(tabla_articulos,sacar_contenido(0,50,articulos,tabla_articulos))
  tabla_articulos <- rbind(tabla_articulos,sacar_contenido(50,100,articulos,tabla_articulos))
  tabla_articulos <- rbind(tabla_articulos,sacar_contenido(100,150,articulos,tabla_articulos))
  tabla_articulos <- rbind(tabla_articulos,sacar_contenido(150,200,articulos,tabla_articulos))
  tabla_articulos <- rbind(tabla_articulos,sacar_contenido(200,250,articulos,tabla_articulos))
  tabla_articulos <- rbind(tabla_articulos,sacar_contenido(250,300,articulos,tabla_articulos))
  tabla_articulos <- rbind(tabla_articulos,sacar_contenido(300,350,articulos,tabla_articulos))
  tabla_articulos <- rbind(tabla_articulos,sacar_contenido(350,400,articulos,tabla_articulos))
  tabla_articulos <- rbind(tabla_articulos,sacar_contenido(400,450,articulos,tabla_articulos))
  tabla_articulos <- rbind(tabla_articulos,sacar_contenido(450,500,articulos,tabla_articulos))
  tabla_articulos <- rbind(tabla_articulos,sacar_contenido(500,550,articulos,tabla_articulos))
  tabla_articulos <- rbind(tabla_articulos,sacar_contenido(550,600,articulos,tabla_articulos))
  tabla_articulos <- rbind(tabla_articulos,sacar_contenido(600,650,articulos,tabla_articulos))
  tabla_articulos <- rbind(tabla_articulos,sacar_contenido(650,708,articulos,tabla_articulos))
  
  
  #Guardo los datos en un csv
  write.csv(tabla_articulos, "tabla_articulos.csv", row.names = FALSE)
}

primera_parte()
print("Funciono")


