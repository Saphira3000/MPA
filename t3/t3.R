# Tarea 3: Distribuciones geometrica, binomial y binomial negativa

if (!require('tidytext')) {
  install.packages('tidytext')
}
if (!require('gutenbergr')) {
  install.packages('gutenbergr')
}
if (!require('tidyr')) {
  install.packages('tidyr')
}
if (!require('dplyr')) {
  install.packages('dplyr')
}
if (!require('corpus')) {
  install.packages('corpus')
}
library(corpus)
library(gutenbergr)
library(tidytext)
library(dplyr)
library(stringr)


#####################################
#     HISTOGRAMAS DE LOS DATOS      #
#####################################

anne = gutenberg_download(c(45)) # Anne Of Green Gables
anne = anne[anne$text != "",] # elimina lineas vacias
anne = anne[42:dim(anne)[1], ] # elimina indice
anne = lapply(anne, gsub, pattern="_", replacement="") # elimina guión bajo 
anne = as.data.frame(anne)

peter = gutenberg_download(c(16)) # Peter Pan para la comparación
peter = peter[peter$text != "",] 
peter = peter[23:dim(peter)[1], ] 
peter = as.data.frame(peter)

s_anne = paste(anne$text, collapse = " ")
anne_oraciones = text_split(s_anne, "sentences")
anne_oraciones = data.frame(sapply(anne_oraciones, as.character), stringsAsFactors = FALSE)

s_peter = paste(peter$text, collapse = " ")
peter_oraciones = text_split(s_peter, "sentences")
peter_oraciones = data.frame(sapply(peter_oraciones, as.character), stringsAsFactors = FALSE)

# palabras, comas y, puntos y comas por oración
anne_oraciones$palabras = str_count(anne_oraciones$text, "\\S+")
anne_oraciones$comas = str_count(anne_oraciones$text, ',')
anne_oraciones$pcomas = str_count(anne_oraciones$text, ';')

peter_oraciones$palabras = str_count(peter_oraciones$text, "\\S+") 
peter_oraciones$comas = str_count(peter_oraciones$text, ',')
peter_oraciones$pcomas = str_count(peter_oraciones$text, ';')

# hist palabras por oracion
# setEPS() 
# postscript("hist_ppo_anne.eps") 
png('hist_ppo_anne.png', width=2600, height=1600, res=300)
hist(anne_oraciones$palabras, ylab = "Frecuencia", xlab = "Cantidad de palabras por oración", main = "")
dev.off()

png("hist_ppo_peter.png", width=2600, height=1600, res=300)
hist(peter_oraciones$palabras, ylab = "Frecuencia", xlab = "Cantidad de palabras por oración", main = "")
dev.off()

# hist comas por oracion
png("hist_cpo_anne.png", width=2600, height=1600, res=300)
hist(anne_oraciones$comas, ylab = "Frecuencia", xlab = "Cantidad de comas por oración", main = "")
dev.off()
png("hist_cpo_peter.png", width=2600, height=1600, res=300)
hist(peter_oraciones$comas, ylab = "Frecuencia", xlab = "Cantidad de comas por oración", main = "")
dev.off()

# hist puntos y comas por oracion
png("hist_pcpo_anne.png", width=2600, height=1600, res=300)
hist(anne_oraciones$pcomas, ylab = "Frecuencia", xlab = "Cantidad de comas por oración", main = "")
dev.off()
png("hist_pcpo_peter.png", width=2600, height=1600, res=300)
hist(peter_oraciones$pcomas, ylab = "Frecuencia", xlab = "Cantidad de comas por oración", main = "")
dev.off()

# largo de las palabras
anne_palabras = anne %>% unnest_tokens(word, text, "words")
tp_anne = nchar(anne_palabras$word)
peter_palabras = peter %>% unnest_tokens(word, text, "words")
tp_peter = nchar(peter_palabras$word)

png("hist_tpalabras_anne.png", width=2600, height=1600, res=300)
hist(tp_anne, ylab = "Frecuencia", xlab = "Largo de las palabras", main = "", breaks = seq(1: max(tp_anne)))
dev.off()
png("hist_tpalabras_peter.png", width=2600, height=1600, res=300)
hist(tp_peter, ylab = "Frecuencia", xlab = "Largo de las palabras", main = "", breaks = seq(1: max(tp_peter)))
dev.off()



#####################################
#     EXPERIMENTOS DE BERNOULLI     #
#####################################

# Obtenemos las letras 
letras = anne %>% unnest_tokens(chars, text, "characters")
letras = letras[!(letras$chars =="|"), ]

# geometrica: exito, obtener una "e"
resultados = numeric()
while ( length(resultados) < 10000) {
  counter = 0
  while (TRUE) {
    counter = counter + 1
    letra = sample(letras$chars, 1)
    if (letra == "e") {
      break;
    }
  }
  resultados = c(resultados, counter)
}

png("geom_e.png", width=2600, height=1600, res=300)
hist(resultados, ylab = "Frecuencia", xlab = "Número de fracasos", main = "")
dev.off()

# binomial negativa: ¿cuántos intentos necesito para obtener 7 t (exitos)?
resultados = numeric()
exitos = 7
while ( length(resultados) < 10000) {
  counter = 0
  kex = 0
  while (TRUE) {
    counter = counter + 1
    letra = sample(letras$chars, 1)
    if (letra == "t") {
      kex = kex + 1
      if (kex == exitos) {
        break
      }
    }
  }
  resultados = c(resultados, counter)
}
png("bin_neg_t7.png", width=2600, height=1600, res=300)
hist(resultados, ylab = "Frecuencia", xlab = "Número de intentos", main = "")
dev.off()

# Obtenemos las palabras
palabras = anne %>% unnest_tokens(word, text, "words") %>% mutate(word = str_extract(word, "[a-z']+"))

# binomial: ¿cuántos éxitos se obtienen en 1000 replicas?
m = 30 # intentos
replicas = 1000
resultados = numeric()
n1 = 5 # tamano de la palabra
while ( length(resultados) < replicas) {
  exitos = 0
  for (i in 1:m){
    palabra = sample(palabras$word, 1)
    if (nchar(palabra) <= n1) {
      exitos = exitos + 1
    }
  }
  resultados = c(resultados, exitos)
}
png("bin_5.png", width=2600, height=1600, res=300)
hist(resultados, ylab = "Frecuencia", xlab = "Número de éxitos", main = "", breaks = seq(0, max(resultados), 1))
dev.off()

