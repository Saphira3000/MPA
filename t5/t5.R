# Tarea 5: Distribución normal y generación pseudoaleatoria
# Octubre 2020
# Gabriela Sánchez Y.

library('swfscMisc')

uniforme2 = function(n, semilla, a, c, m) {
  datos = numeric()
  x = semilla
  while (length(datos) < n) {
    x = (a * x + c) %% m
    datos = c(datos, x)
  }
  return(datos)
}

######################################################
################ Generación pseudoaleatoria ##########

n = 3000 # cuantos numeros
semilla = c(5, 59, 103, 541, 1117)

# primos de una cifra
pvalor = numeric()
unicos = numeric()
for (r in 1:length(semilla)) {
  numeros = uniforme2(n, semilla[r], 3, 5, 7)
  unicos = c(unicos, length(unique(numeros)))
  numeros = numeros/(6)
  pvalor = c(pvalor, uniform.test(hist(numeros))$p.value)
}
png("hist_1117-3-5-7", width=2000, height=1600, res=300)
par(mar=c(4,4,1,1))
hist(numeros, main = "", ylab = "Frecuencia", xlab = "Número", col = "plum2")
dev.off()
print("primos de una cifra 3-5-7")
print(pvalor)
print(unicos)


# primos de dos cifras
pvalor = numeric()
unicos = numeric()
for (r in 1:length(semilla)) {
  numeros = uniforme2(n, semilla[r], 11, 43, 97)
  unicos = c(unicos, length(unique(numeros)))
  numeros = numeros/(96)
  pvalor = c(pvalor, uniform.test(hist(numeros))$p.value)
  nombre = paste("hist_", semilla[r], "-11-43-97.png", sep = "")
  png(nombre, width=2000, height=1600, res=300)
  par(mar=c(4,4,1,1))
  hist(numeros, main = "", ylab = "Frecuencia", xlab = "", col = "plum2")
  dev.off()
}
print("primos de dos cifras 11-43-97")
print(pvalor)
print(unicos)

pvalor = numeric()
unicos = numeric()
for (r in 1:length(semilla)) {
  numeros = uniforme2(n, semilla[r], 59, 43, 97)
  unicos = c(unicos, length(unique(numeros)))
  numeros = numeros/(96)
  pvalor = c(pvalor, uniform.test(hist(numeros))$p.value)
  nombre = paste("hist_", semilla[r], "-59-43-97.png", sep = "")
  png(nombre, width=2000, height=1600, res=300)
  par(mar=c(4,4,1,1))
  hist(numeros, main = "", ylab = "Frecuencia", xlab = "", col = "skyblue1")
  dev.off()
}
print("primos de dos cifras 59-43-97")
print(pvalor)
print(unicos)


# primos de 3 cifras
pvalor = numeric()
unicos = numeric()
m = 857
for (r in 1:length(semilla)) {
  numeros = uniforme2(n, semilla[r], 613, 919, m)
  unicos = c(unicos, length(unique(numeros)))
  numeros = numeros/(m-1)
  pvalor = c(pvalor, uniform.test(hist(numeros))$p.value)
  nombre = paste("hist_", semilla[r], "-613-919-857.png", sep = "")
  png(nombre, width=2000, height=1600, res=300)
  par(mar=c(4,4,1,1))
  hist(numeros, main = "", ylab = "Frecuencia", xlab = "")
  dev.off()
}
print("primos de tres cifras 613-919-857")
print(pvalor)
print(unicos)


# primos de 4 cifras
pvalor = numeric()
unicos = numeric()
m = 5449
for (r in 1:length(semilla)) {
  numeros = uniforme2(n, semilla[r], 1291, 3821, m)
  unicos = c(unicos, length(unique(numeros)))
  numeros = numeros/(m-1)
  pvalor = c(pvalor, uniform.test(hist(numeros))$p.value)
  nombre = paste("hist_", semilla[r], "-1291-3821-5449.png", sep = "")
  png(nombre, width=2000, height=1600, res=300)
  par(mar=c(4,4,1,1))
  hist(numeros, main = "", ylab = "Frecuencia", xlab = "", col = "lightpink")
  dev.off()
}
print("primos de cuatro cifras 1291-3821-5449")
print(pvalor)
print(unicos)


# pruebas para la variación de la semilla
source('uniforme.R')
numeros1 = uniforme(n, 5, 613, 919, 857)
numeros2 = uniforme(n, 59, 613, 919, 857)
numeros3 = uniforme(n, 103, 613, 919, 857)
numeros4 = uniforme(n, 1117, 613, 919, 857)

dta = data.frame(semilla = c(rep(5,n), rep(59,n), rep(103,n),rep(1117,n)), 
                              valores = c(numeros1, numeros2, numeros3, numeros4))
kruskal.test(valores ~ semilla, data = dta)
png("var_semilla.png", width=2000, height=1600, res=300)
par(mar=c(4,4,1,1))
boxplot(valores ~ semilla, data = dta, main = "", ylab = "Valores", xlab = "Semilla", col = "lightpink")
dev.off()
################################################
########## Generacion pseudonormal ############# 

source('normal2.R')
mu = 0
sigma = 1

elige = uniforme(n, 1117, 3, 5, 7)
pvalor = numeric()
for(r in 1:10) {
  datos = numeric()
  while(length(datos) < n) {
    datos = c(datos, normal2(mu, sigma, sample(elige, 2)))
  }
  pvalor = c(pvalor, shapiro.test(datos)$p.value)
}
print("pvalor para replicas usando uniforme con malos parametros ")
print(pvalor)
png("normal_uniforme_malo.png", width=2000, height=1600, res=300)
par(mar=c(3,4,1,1), mfrow = c(1, 2))
hist(datos, main = "", ylab = "Frecuencia", xlab = "", col = "slategray1")
hist(rnorm(n), main = "", ylab = "Frecuencia", xlab = "")
dev.off()

elige = uniforme(n, 1117, 1291, 3821, 5449)
pvalor = numeric()
for (r in 1:10) {
  datos = numeric()
  while(length(datos) < n) {
    datos = c(datos, normal2(mu, sigma, sample(elige, 2)))
  }
  pvalor = c(pvalor, shapiro.test(datos)$p.value)
}
print("pvalor para replicas usando uniforme con buenos parametros ")
print(pvalor)
png("normal_uniforme_regular.png", width=2000, height=1600, res=300)
par(mar=c(3,4,1,1), mfrow = c(1, 2))
hist(datos, main = "", ylab = "Frecuencia", xlab = "", col = "slategray1")
hist(rnorm(n), main = "", ylab = "Frecuencia", xlab = "")
dev.off()

### z0 - z1 - ambas
normal = function(mu, sigma) {
  u = runif(2);
  z0 = sqrt(-2 * log(u[1])) * cos(2 * pi * u[2]);
  z1 = sqrt(-2 * log(u[1])) * sin(2 * pi * u[2]);
  datos = c(z0, z1);
  return (sigma * datos + mu);
}

datosz0 = numeric()
datosz1 = numeric()
datos_ambos = numeric()
while(length(datos_ambos) < n/2) {
  datosz0 = c(datosz0, normal(mu, sigma)[1])
  datosz1 = c(datosz1,  normal(mu, sigma)[2])
  datos_ambos = c(datos_ambos, normal(mu,sigma))
}
pvalor = c(shapiro.test(datosz0)$p.value, shapiro.test(datosz1)$p.value, shapiro.test(datos_ambos)$p.value)
print("pvalor uso z0, z1, ambos")
print(pvalor)
png("normal.png", width=2000, height=1600, res=300)
par(mar=c(3,4,1,1), mfrow = c(1, 3))
hist(datosz0, main = "", ylab = "Frecuencia", xlab = "", col = "slategray1")
hist(datosz1, main = "", ylab = "Frecuencia", xlab = "", col = "lightpink")
hist(datos_ambos, main = "", ylab = "Frecuencia", xlab = "")
dev.off()

# graficas por separado
png("normalz0.png", width=2000, height=1600, res=300)
par(mar=c(3,4,1,1))
hist(datosz0, main = "", ylab = "Frecuencia", xlab = "", col = "slategray1")
dev.off()

png("normalz1.png", width=2000, height=1600, res=300)
par(mar=c(3,4,1,1))
hist(datosz1, main = "", ylab = "Frecuencia", xlab = "", col = "lightpink")
dev.off()

png("normalambos.png", width=2000, height=1600, res=300)
par(mar=c(3,4,1,1))
hist(datos_ambos, main = "", ylab = "Frecuencia", xlab = "")
dev.off()

dta = data.frame(num = c(rep(0,n), rep(1,n), rep(2,n)), 
                 valores = c(datosz0, datosz1, datos_ambos))
png("normal_numeros.png", width=2000, height=1600, res=300)
par(mar=c(4,4,1,1))
boxplot(valores ~ num, data = dta, main = "", ylab = "Valores", c("z_0", "z_1", "ambos"), col = "lightpink")
dev.off()
# anova <- aov(dta$valores ~ dta$num)
