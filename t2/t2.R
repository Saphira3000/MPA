# Tarea 2: Frecuencias e histogramas


if (!require('tidytext')) {
  install.packages('tidytext')
}
if (!require('dplyr')) {
  install.packages('dplyr')
}
if (!require('gutenbergr')) {
  install.packages('gutenbergr')
}


library(gutenbergr)
library(tidytext)
library(dplyr)


# Obtenemos el libro ID Ann of Green Gables
libro = gutenberg_download(c(45))


# Obtenemos las letras 
letras = libro %>% unnest_tokens(chars, text, "characters")
barplot(table(letras$chars))
Tletras = as.data.frame(table(letras$chars))
colnames(Tletras) = c("letra", "frec")
Tletras = Tletras[!(Tletras$letra == "|"), ]

letraso = Tletras[order(Tletras$frec, decreasing = T),]
png('letras_decreciente.png', width=2600, height=1600, res=300)
barplot(letraso$frec, ylab = "Frecuencia", xlab = "Letra", names.arg = letraso$letra)
dev.off()

png('letras_decreciente_log.png', width=2600, height=1600, res=300)
barplot(letraso$frec, names.arg = letraso$letra, log = "y", ylab = "Frecuencia", xlab = "Letra")
dev.off()


# Obtenemos las palabras
palabras = libro %>% unnest_tokens(word, text, "words")
barplot(table(palabras$word))

# filtro 1: stop words
palabras =  palabras %>% anti_join(stop_words)

# filtro 2: frecuencias
palabrasf = as.data.frame(table(palabras$word))
colnames(palabrasf) = c("palabra", "frec")
palabrasf = palabrasf[palabrasf$frec > 1,]

palabraso = palabrasf[order(palabrasf$frec, decreasing = T),]
head(palabraso, 10)
png('palabras_decreciente_log3.png', width=2600, height=1600, res=300)
barplot(palabraso$frec[11:30], names.arg = palabraso$palabra[11:30], ylab = "Frecuencia", las=2)
dev.off()

# sentimientos sin filtro de frecuencias
bing_count <- palabras %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()

positive = bing_count[bing_count$sentiment == "positive", ]
positive = positive[positive$n > 1, ] # filtro frec > 1
library(RColorBrewer)
png('palabras_positivas.png', width=1600, height=1400, res=300)
barplot(positive$n[1:10], names.arg = positive$word[1:10], ylab = "Frecuencia", las = 2, 
        col = brewer.pal(10, "Set3"))
dev.off()

negative = bing_count[bing_count$sentiment == "negative", ]
negative = negative[negative$n > 1, ] # filtro frec > 1
png('palabras_negativas.png', width=1600, height=1400, res=300)
barplot(negative$n[1:10], names.arg = negative$word[1:10], ylab = "Frecuencia", 
        las = 2, col = grey.colors(10))
dev.off()

