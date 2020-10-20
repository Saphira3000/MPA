
# ajuste lineal sin ruido
x = runif(200,0,1)
y = 3*x+1

# computing the regression line
mx = mean(x)
my = mean(y)
sx = sd(x)
sy = sd(y)
r = sum((x-mx)*(y-my))/sqrt(sum((x-mx)^2)*sum((y-my)^2))
r2 = cor(x,y)
  
m = r*sy/sx
b = my - m*mx
yp = m*x + b
plot(x~yp)

# agreguemos ruido
x = runif(200,0,10)
y = 3*x + 1 + rnorm(200, 0, 1)
regresion = lm(y~x)
png('linealruido.png',width=2000,height=2000,res=300)
par(mar=c(4,4,1,1))
plot(y~x, main="", col= 'blue', pch = 16)
abline(regresion)
dev.off()

# regresion lineal multiple
x1 = runif(500, 5, 10)
x2 = runif(500, 0, 10)
x3 = runif(500, -10, 10)
y = x1 + x2 + 0.5*x3 + rnorm(500,0,1)
datos = data.frame("x1" = x1, "x2" = x2, "x3" = x3 , "y" = y)
png('linealmultiple.png',width=2000,height=2000,res=300)
pairs(datos)
dev.off()
ajuste = lm(y~x1+x2+x3)
summary(ajuste)

# transformadas
x1 = rnorm(500, 100, 5)
y = 0.2 + 0.03*x1^2
