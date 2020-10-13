datos <- read.csv("datos.csv", header = TRUE)
datos2 <- datos[,-1]

# one sample t-test 
delitos = c(datos2[,1], datos2[,2], datos2[,3]) # ano 2010-2013
shapiro.test(delitos)
print('-----------------------------------------')
t.test(delitos, mu=23600)

# Wilcoxon Signed Rank Test
todos_delitos = as.vector(t(datos2))
shapiro.test(todos_delitos)
print('-----------------------------------------')
wilcox.test(todos_delitos, mu=23600)

# Two Sample Wilcoxon Signed Rank Test
norte = rbind(datos2[2,], datos2[5,], datos2[8,], datos2[19,], datos2[26,], datos2[28,]) # 2 Baja California, 5 Coahuila, 8 Chihuahua, 19 Nuevo Leon, 26 Sonora, 28 Tamaulipas  
norte = as.vector(t(norte))
centro = rbind(datos2[9,], datos2[11,], datos2[15,], datos2[16,], datos2[22,], datos2[29,]) # 9 Cdmx, 11 Gto, 15 Mexico, 16 Michoacan, 22 Queretaro, 29 Tlaxcala
centro = as.vector(t(centro))
shapiro.test(norte)
shapiro.test(centro)
wilcox.test(norte, centro, alternative = "g")
png('boxplot_norte-centro.png',width=2500,height=2000,res=300)
par(mar=c(3,4,1,1))
boxplot(centro, norte, main = " ", names = c("Zona centro", "Zona norte"), 
        col= c(rgb(0.1,0.1,0.7,0.5) , rgb(0.8,0.1,0.3,0.6)), ylab = "Tasa de prevalencia delictiva")
dev.off()

# Fisher’s F-Test
var.test(centro,norte)

# Chi Squared Test
# nivel_edu = c("primaria", "secundaria", "medio superior", "licenciatura", "posgrado")
# nivel_socioec = c("pobre", "media baja", "media" , "alta", "rica")
# edu = character()
# eco = character()
# for (i in 1:50) {
#   a = sample(nivel_edu, 1)
#   if(a == "licenciatura")
#     b = sample(c("media baja", "media" , "alta", "rica"), 1)
#   if(a == "posgrado")
#     b = sample(c("media baja", "media" , "alta"),1)
#   if(a == "primaria" | a == "secundaria")
#     b = sample(c("pobre", "media baja", "media"),1)
#   if(a == "medio superior")
#     b = sample(c("pobre", "media baja", "media" , "alta", "rica"), 1)
#   edu = c(edu, a)
#   eco = c(eco, b)
# }
# chisq.test(table(edu, eco), correct = FALSE)

muy_efectivo = c(16.3, 8.7)#, 6.3)
efectov = c(49.7, 45.1)#, 40.1)
prueba = as.table(cbind(muy_efectivo, efectov))
colnames(prueba) <- c("muy efectivo","algo efectivo")
rownames(prueba) <- c("Policia federal","policia estatal")#, "policia municipal")
chisq.test(prueba, correct = F)
