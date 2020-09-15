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

head(palabraso, 10)

palabraso = palabrasf[order(palabrasf$frec, decreasing = T),]
png('palabras_decreciente_log3.png', width=2600, height=1600, res=300)
barplot(palabraso$frec[11:30], names.arg = palabraso$palabra[11:30], ylab = "Frecuencia", las=2)
dev.off()


palabraso = Tpalabras[order(Tpalabras$frec, decreasing = T),]
barplot(palabraso$frec, names.arg = palabraso$palabra, log = "y")

