# Materia: modelos probabilistas aplicados
# Proyecto integrador: analiza las diferencias entre las soluciones obtenidas de dos formulaciones
# Gabriela Sanchez Y.

library(ggplot2)

datos <- read.csv(file = 'proyecto.csv') 

# analiza soluciones de funcion objetivo Max-Min
gap1 <- data.frame(gap = datos$gap1)
gap21 <- data.frame(gap = datos$gap21)
gap1$sol <- 's1'
gap21$sol <- 's21'
# and combine into your new data frame
ob1 <- rbind(gap1, gap21)

# densidades
png("histden_sols_ob1.png", width=2500, height=1600, res=300)
par(mar=c(3,4,2,1))
ggplot(ob1, aes(gap, fill = sol)) + geom_density(alpha = 0.5) + theme_classic() + 
  theme(legend.position=c(0.8, 0.8), legend.background = element_rect(linetype="solid", colour ="black")) + labs(title = " ", x = 'gap', y = 'Densidad')
dev.off()

# histograma
png("hist_sols_ob1.png", width=2500, height=1600, res=300)
par(mar=c(3,4,2,1))
ggplot(ob1, aes(gap, fill = sol)) + geom_histogram(alpha = 0.5, position = 'identity') + theme_classic() +
  theme(legend.position=c(0.8, 0.8), legend.background = element_rect(linetype="solid", colour ="black")) + labs(title = " ", x = 'gap', y = 'Frecuencia')
dev.off()

# media de los gap
mean(datos$gap1)
mean(datos$gap21)


# analiza soluciones de funcion objetivo Min-Max
gap12 <- data.frame(gap = datos$gap12)
gap2 <- data.frame(gap = datos$gap2)
gap2$sol <- 's2'
gap12$sol <- 's12'
gap2$clase = datos$clase
gap12$clase = datos$clase
# and combine into your new data frame
ob2 <- rbind(gap2, gap12)

# densidades
png("histden_sols_ob2.png", width=2500, height=1600, res=300)
par(mar=c(3,4,2,1))
ggplot(ob2, aes(gap, fill = sol)) + geom_density(alpha = 0.5) + theme_classic() + 
  theme(legend.position=c(0.8, 0.8), legend.background = element_rect(linetype="solid", colour ="black")) +
  labs(title = " ", x = 'gap', y = 'Densidad')
dev.off()

# histograma
png("hist_sols_ob2.png", width=2500, height=1600, res=300)
par(mar=c(3,4,2,1))
ggplot(ob2, aes(gap, fill = sol)) + geom_histogram(alpha = 0.5, position = 'identity') + theme_classic() +
  theme(legend.position=c(0.8, 0.8), legend.background = element_rect(linetype="solid", colour ="black")) + labs(title = " ", x = 'gap', y = 'Frecuencia')
dev.off()

#ggplot(ob2, aes(x=clase, y=gap, fill=sol)) + geom_boxplot() + facet_wrap(~sol)

# media de los gap
mean(datos$gap2)
mean(datos$gap12)

# normalidad
shapiro.test(datos$gap1)
shapiro.test(datos$gap21)
shapiro.test(datos$gap2)
shapiro.test(datos$gap12)


png("hist_gap1.png", width=2500, height=1600, res=300)
par(mar=c(4,4,1,1))
hist(datos$gap1, main = " ", xlab = "gap", ylab = "Frecuencia") 
dev.off()

png("hist_gap21.png", width=2500, height=1600, res=300)
par(mar=c(4,4,1,1))
hist(datos$gap21, main = " ", xlab = "gap", ylab = "Frecuencia") 
dev.off()

png("hist_gap2.png", width=2500, height=1600, res=300)
par(mar=c(4,4,1,1))
hist(datos$gap2, main = " ", xlab = "gap", ylab = "Frecuencia") 
dev.off()

png("hist_gap12.png", width=2500, height=1600, res=300)
par(mar=c(4,4,1,1))
hist(datos$gap12, main = " ", xlab = "gap", ylab = "Frecuencia") 
dev.off()

png("box_gap1.png", width=2500, height=1600, res=300)
par(mar=c(4,4,1,1))
boxplot(formula = gap1 ~ clase, data = datos, names = unique(datos$clase), main = " ", ylab = "Gap", col = "slategray1")
dev.off()

png("box_gap21.png", width=2500, height=1600, res=300)
par(mar=c(4,4,1,1))
boxplot(formula = gap21 ~ clase, data = datos, main = " ", names = unique(datos$clase), ylab = "Gap", col = "lightpink")
dev.off()

png("box_gap2.png", width=2500, height=1600, res=300)
par(mar=c(4,4,1,1))
boxplot(formula = gap2 ~ clase, data = datos, names = unique(datos$clase), main = " ", ylab = "Gap", col = "slategray1")
dev.off()

png("box_gap12.png", width=2500, height=1600, res=300)
par(mar=c(4,4,1,1))
boxplot(formula = gap12 ~ clase, data = datos, main = " ", names = unique(datos$clase), ylab = "Gap", col = "lightpink")
dev.off()

# datos con los que se trabajara
s1 = datos$gap1
s2 = datos$gap2
s12 = datos$gap12
s21 = datos$gap21

# wilcoxon 
wilcox.test(s1, s21, paired = TRUE)
wilcox.test(s2, s12, paired = TRUE)
wilcox.test(s21,s12, paired = TRUE)

# wilcoxon considerando datos distintos
prueba = datos[datos$gap1 != datos$gap21, ]
#prueba = datos[datos$gap21 != 0, ]
wilcox.test(prueba$gap1, prueba$gap21, paired = TRUE) #ob1
mean(prueba$gap1) # promedio gap1
mean(prueba$gap21) # promedio gap21

prueba2 = datos[datos$gap2 != datos$gap12, ]
#prueba2 = datos[datos$gap12 != 0, ]
wilcox.test(prueba2$gap2, prueba2$gap12, paired = TRUE) #ob2
mean(prueba$gap2) # promedio gap2
mean(prueba$gap12) # promedio gap12

# wilcoxon para evaluaciones cruzadas (solo entradas distintas)
prueba3 = datos[datos$gap21 != datos$gap12, ]
wilcox.test(prueba2$gap21, prueba2$gap12, paired = TRUE)
mean(prueba$gap12) # promedio gap12
mean(prueba$gap21) # promedio gap21

