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

library(gutenbergr)
library(tidytext)
library(dplyr)


# Obtenemos el libro ID Ann of Green Gables
libro = gutenberg_download(c(45))
libro = libro[libro$text != "",] # elimina lineas vacias
libro = libro[42:dim(libro)[1], ] # elimina indice


#####################################
#     EXPERIMENTOS DE BERNOULLI     #
#####################################

# Obtenemos las letras 
letras = libro %>% unnest_tokens(chars, text, "characters")
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

setEPS()
postscript("geom_e.eps")
hist(resultados, ylab = "Frecuencia", xlab = "Número de fracasos", main = "")
dev.off()

# binomial negativa: ¿cuántos intentos necesito para obtener 7 t (exitos)? 
resultados2 = numeric()
exitos = 7
while ( length(resultados2) < 10000) {
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
  resultados2 = c(resultados2, counter)
}
setEPS()
postscript("bin_neg_t7.eps")
hist(resultados2, ylab = "Frecuencia", xlab = "Número de intentos", main = "")
dev.off()

# Obtenemos las palabras
library(stringr)
palabras = libro %>% unnest_tokens(word, text, "words") %>% mutate(word = str_extract(word, "[a-z']+"))

# binomial
m = 30 # experimentos
replicas = 1000
resultados2 = numeric()
n = 6 # tamano de la palabra
while ( length(resultados2) < replicas) {
    exitos = 0
    for (i in 1:m){
      palabra = sample(palabras$word, 1)
      if (nchar(palabra) < n) {
        exitos = exitos + 1
     }
   }
   resultados2 = c(resultados2, exitos)
}
hist(resultados2, breaks = seq(0,m))

# palabras
tp = nchar(palabras$word) # tamaño de las palabras
#tp = as.data.frame(table(palabras$word, nchar(palabras$word)))
hist(tp, ylab = "Frecuencia", xlab = "Tamaño de palabra", main = "")

# oraciones
libro$text = str_replace_all(libro$text, "MRS.", "Mrs")
libro$text = str_replace_all(libro$text, "Mrs.", "Mrs")
libro$text = str_replace_all(libro$text, "J.", "J")
libro$text = str_replace_all(libro$text, "St.", "St")
libro$text = str_replace_all(libro$text, "Mr.", "Mr")
oraciones <- libro %>% unnest_tokens(sentence, text, token = "sentences") # separa por puntuacion

# cantidad de palabras por oracion
ppo = str_count(oraciones$sentence, "\\S+")
setEPS()
postscript("hist_ppo.eps")
hist(ppo, ylab = "Frecuencia", xlab = "Cantidad de palabras por oración", main = "")
dev.off()

# cantidad de comas por oracion
cpo = str_count(oraciones$sentence, ',')
cpo = as.data.frame(table(cpo))

# cantidad de comas por oracion
cpo = str_count(oraciones$sentence, c(',', ':', ';', '?', '!'))
cpo = as.data.frame(table(cpo))


