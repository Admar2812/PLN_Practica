# Cargar el corpus
corpus_cp <- readRDS("corpus_codigo_penal.rds")
# Cargar librerías
library(quanteda)
library(spacyr)
library(dplyr)
spacy_initialize(model = "es_core_news_sm")
library(udpipe)
modelo_ud <- udpipe_download_model(language = "spanish")
ud_model <- udpipe_load_model(modelo_ud$file_model)
# Extraer los documentos del corpus
textos <- as.character(corpus_cp)
# Buscamos entidades nombradas con spacyr
ent_spacy <- spacy_extract_entity(textos, type = "named")
# 20 entidades más frecuentes encontradas por spacyr
top20_spacy <- count(ent_spacy, text, sort = TRUE)
top20_spacy <- slice_head(top20_spacy, n = 20)
# Guardar top20 de spacyr
write.csv(top20_spacy, file = "top20_spacy.csv", row.names = FALSE)
# Buscamos entidades nombradas con udpipe
texto_udpipe <- udpipe_annotate(ud_model, x = textos)
texto_udpipe_df <- as.data.frame(texto_udpipe)
ent_udpipe <- keywords_rake(x = texto_udpipe_df,term = "lemma", group = "doc_id", 
                           relevant = texto_udpipe_df$upos == "PROPN")
# 20 entidades más frecuentes encontradas por udpipe
top20_udpipe <- count(ent_udpipe, keyword, sort = TRUE)
top20_udpipe <- slice_head(top20_udpipe, n = 20)
# Guardar top20 de udpipe
write.csv(top20_udpipe, file = "top20_udpipe.csv", row.names = FALSE)
# Buscar comunes
spacy_vec <- tolower(top20_spacy$text)
udpipe_vec <- tolower(top20_udpipe$keyword)
comunes <- intersect(spacy_vec, udpipe_vec)
write.csv(comunes, file = "comunes.csv", row.names = FALSE)

