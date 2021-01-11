# tarea 14

library(stringr)
prueba <- read.csv(file = 'datos_tarea14.csv')
datos = prueba$gap

hist(datos)
tam_muestra = c(10, 50, 80)
for (i in tam_muestra) {
  medias = numeric()
  for (r in 1:100) {
    muestra = sample(datos, i, replace = F)
    medias = c(medias, mean(muestra))
  }
  nombre = str_c('meangap_', i, ".png")
  png(nombre, width=2500, height=1600, res=300)
  par(mar=c(3,4,2,1))
  hist(medias, main = "", ylab = "Frecuencia", xlab = " ", col = "lightpink") 
  dev.off()
}

mean(datos)
