library('stringr')

n = c(10, 100, 1000, 10000, 100000)
moneda = c(0,1)
resultados = numeric()
for (i in n) {
  for (r in 100) {
    lanzamientos = sample(moneda, i, replace = T)
    resultados = c(resultados, mean(lanzamientos))
  }
  nombre_png = str_c('moneda_', i, ".png")
  png(nombre_png, width=2500, height=1600, res=300)
  par(mar=c(3,4,2,1))
  hist(resultados, freq = FALSE, ylab = "", xlab = "", main = "") 
  dev.off()
}

moneda = c(0,1)
resultados = numeric()
for (i in n) {
  lanzamientos = sample(moneda, i, replace = T)
  resultados = c(resultados, mean(lanzamientos))
}
datos = table(resultados)



# dados
dado = c(1,2,3,4,5,6)
resultados = numeric()
for (i in n) {
  for (r in 1000) {
    lanza = sample(dado, i, replace = T)
    resultados = c(resultados, mean(lanza))
  }
  nombre_png = str_c('dado_', i, ".png")
  png(nombre_png, width=2500, height=1600, res=300)
  par(mar=c(3,4,2,1))
  hist(resultados, breaks = 5, ylab = "", xlab = "", main = "") 
  dev.off()
}

dado = c(1,2,3,4,5,6)
resultados = numeric()
for (i in n) {
  lanzamientos = sample(dado, i, replace = T)
  print(mean(lanzamientos))
  resultados = c(resultados, mean(lanzamientos))
}
datos = table(resultados)
