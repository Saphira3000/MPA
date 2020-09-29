# Tarea 4: Distribución de Poisson

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
library(stringr)

# Distribución de Poisson en el texto 
anne = gutenberg_download(c(45)) # Anne Of Green Gables
anne = anne[anne$text != "",] # elimina lineas vacias
anne = anne[42:dim(anne)[1], ] # elimina indice

letras = anne %>% unnest_tokens(chars, text, "characters")
letras = letras[!(letras$chars =="|"), ]

# Hay algo que esta mal planteado (revisar nuevamente)
# library(tidyr)
# library(stringr)
# anne2 = anne %>% unnest_tokens(word, text, "words") %>% mutate(word = str_extract(word, "[a-z']+"))
# ngrams <- anne %>% unnest_tokens(bigram, text, token = "ngrams", n = 20)
# 
# resultados = numeric()
# k = 3
# n = 0
# while ( length(resultados ) < 100) {
#   counter = 0
#   while (TRUE) {
#     ngram = sample(ngrams$bigram, 1)
#     cuantos = str_count(ngram, "w")
#     counter = counter + cuantos
#     n = n + 1
#     if(counter >= k) {
#       break
#     }
#     resultados = c(resultados, n)
#   }
# }
# png("poisson_w.png", width=2600, height=1600, res=300)
# hist(resultados, ylab = "Frecuencia", xlab = "Número de fracasos", main = "")
# dev.off()


# Aproximación a Poisson a partir de una distribución binomial
br = 5
n = 10000
p = 0.001
me = 80
la = n*p
repl = 1000
cb = numeric()
for (replica in 1:repl) {
  db = numeric()
  while (sum(db) < me) {
    db = c(db, rbinom(1, n, p))
  }
  cb = c(cb, length(db))
}
png('poisson_binom-p001-n1000-m80-r1000.png', width=2000, height=1600, res=300)
par(mfrow = c(1, 2))
hist(cb, breaks = br, xlab = "", ylab = "Frecuencia", main = "")
hist(rpois(repl, la), breaks = br, xlab = "", ylab = "Frecuencia", main = "")
dev.off()


# Aproximación a Poisson a través de la distribución normal
r = 1000
meta = 10000
la = 100
br = 7
cn = numeric()
for (replica in 1:r) {
  a_normal = numeric()
  while (sum(a_normal) < meta) {
    a_normal = c(a_normal, rnorm(1, la, sqrt(la)))
  }
  cn = c(cn, length(a_normal))
}
cp = rpois(r, la)
png('poisson_normal.png', width=2000, height=1600, res=300)
par(mfrow = c(1, 2))
hist(cn, breaks = seq(min(cn), max(cn), length.out = br), ylab = "Frecuencia", xlab = "", main = "")
hist(cp, breaks = seq(min(cp), max(cp), length.out = br), ylab = "Frecuencia", xlab = "", main = "")
dev.off()

p1 = rpois(10000, 1)
p10 = rpois(10000, 10)
p50 = rpois(10000, 50)
p100 = rpois(10000, 100)
# mar (bottom, left, top, right)
br = 10
png('poisson_normal_vl1.png', width=2200, height=1600, res=300)
par(mar=c(3,3,1,1), mfrow = c(1, 2))
hist(rpois(10000, 1), main = "", ylab = "Frecuencia", xlab = "", breaks = br)
hist(rnorm(10000,1,1),  main = "", ylab = "Frecuencia", xlab = "", breaks = br)
dev.off()
png('poisson_normal_vl10.png', width=2200, height=1600, res=300)
par(mar=c(3,3,1,1), mfrow = c(1, 2))
hist(rpois(10000, 10), main = "", ylab = "Frecuencia", xlab = "", breaks = br)
hist(rnorm(10000, 10, sqrt(10)),  main = "", ylab = "Frecuencia", xlab = "", breaks = br)
dev.off()
png('poisson_normal_vl100.png', width=2200, height=1600, res=300)
par(mar=c(3,3,1,1), mfrow = c(1, 2))
hist(rpois(10000, 100), main = "", ylab = "Frecuencia", xlab = "")
hist(rnorm(10000, 100, sqrt(100)),  main = "", ylab = "Frecuencia", xlab = "")
dev.off()

# trred <- rgb(1, 0, 0, 0.5) ## transparent red
# trblue <- rgb(0, 0, 1, 0.5) ## transparent blue
# hist(rpois(10000, 1), col = trred, breaks = 10, main = "")
# par(new = TRUE)
# hist(rpois(10000,10), col = trblue, breaks = 10, main = "")
# 
# png('poisson_l50.png', width=1800, height=1500, res=300)
# par(mar=c(3,3,1,1))
# hist(p50, width=1000, height=800, res=100)
# dev.off()
# png('poisson_l100.png', width=1800, height=1500, res=300)
# par(mar=c(3,3,1,1))
# hist(p100, width=1000, height=800, res=100)
# dev.off()
# 
# n1 = rnorm(10000,1,1)
# n10 = rnorm(10000, 10, sqrt(10))
# n50 = rnorm(10000, 50, sqrt(50))
# n100 = rnorm(10000, 100, sqrt(100))

# Aproximación a Poisson a partir de una distribución exponencial
la = 1
ce = numeric()
repl = 10000
br = 5
me = 1
for (replica in 1:repl) {
  de = numeric()
  while (sum(de) < me) {
    de = c(de, rexp(1, la))
  }
  ce = c(ce, length(de))
}
png('poisson_exponencial.png', width=2000, height=1600, res=300)
par(mfrow = c(1, 2), mar=c(3,3,1,1))
hist(ce, breaks = br, main = "", ylab = "Frecuencia", xlab = "")
hist(rpois(repl, la), breaks = br, main = "", ylab = "Frecuencia", xlab = "") # la*me ???
dev.off()

