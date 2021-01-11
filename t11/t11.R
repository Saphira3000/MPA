
# apoyo numerico para demostrar la primera propiedad 
a = 3
b = 1
c = 5
d = 2

cov_abcd = numeric()
cov_ac = numeric()
for(r in 1:100) {
  X = rnorm(1000, 0, 5)
  Y = runif(1000)
  cov_abcd = c(cov_abcd, cov(a*X+b, c*Y+d))
  cov_ac = c(cov_ac, a*c*cov(X, Y))
}
png("cov_abcd.png", width=2000, height=1600, res=300)
par(mar=c(3,4,1,1))
hist(cov_abcd, main = "", ylab = "Frecuencia", xlab = " ", col = "slategray1")
dev.off()
png("cov_ac.png", width=2000, height=1600, res=300)
par(mar=c(3,4,1,1))
hist(cov_ac, main = "", ylab = "Frecuencia", xlab = " ", col = "lightpink")
dev.off()


# apoyo numerico para demostrar la segunda propiedad 
var_suma = numeric()
resultado = numeric()
for(r in 1:100) {
  X = rnorm(1000, 0, 5)
  Y = runif(1000)
  var_suma = c(var_suma, var(X+Y))
  resultado = c(resultado, var(X) + var(Y) + 2*cov(X,Y))  
}
png("var_suma.png", width=2000, height=1600, res=300)
par(mar=c(3,4,1,1))
hist(var_suma, main = "", ylab = "Frecuencia", xlab = " ", col = "slategray1")
dev.off()
png("var_resultado.png", width=2000, height=1600, res=300)
par(mar=c(3,4,1,1))
hist(resultado, main = "", ylab = "Frecuencia", xlab = " ")
dev.off()


# prueba chi cuadrada
muy_efectivo = c(16.3, 8.7)
efectov = c(49.7, 45.1)
prueba = as.table(cbind(muy_efectivo, efectov))
colnames(prueba) <- c("muy efectivo","algo efectivo")
rownames(prueba) <- c("Policia federal","policia estatal")
chisq.test(prueba, correct = F)


# convolucion
r_separado = numeric()
r_conv = numeric()
for (r in 1:100) {
  x1 = rnorm(1000, 0, 3)
  x2 = rnorm(1000, 1, 5)
  x_conv = rnorm(1000, 1, sqrt(9+25))
  r_conv = c(r_conv, convolve(x1, x2))
}
