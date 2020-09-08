datos <- read.csv("datos.csv", header = TRUE)
datos2 <- datos[,-1]

col = c('Aguascalientes', 'Baja California Norte', 'Baja California Sur', 'Campeche', 'Coahuila', 
        'Colima', 'Chiapas', 'Chihuahua', 'Ciudad de México', 'Durango', 'Guanajuato',
        'Guerrero', 'Hidalgo', 'Jalisco', 'México', 'Michoacán', 'Morelos', 'Nayarit',
        'Nuevo León', 'Oaxaca', 'Puebla', 'Querétaro', 'Quintana Roo', 'San Luis Potosí',
        'Sinaloa', 'Sonora', 'Tabasco', 'Tamaulipas', 'Tlaxcala', 'Veracruz', 'Yucatán',
        'Zacatecas')

png('victimizacion.png',width=3000,height=2500,res=300)
par(mar=c(9,4,2,1))
boxplot(t(datos2), main="Tasa de prevalencia delictiva", cex.names=0.2, names = col, las=2)
dev.off()

# datos edomexico tasa de prevalencia delictiva segun sexo
h <- c(31454, 33889, 45506, 53217, 49874, 51555, 49907, 49177, 44778)
m <- c(24777, 28380, 37105, 42555, 40938, 40653, 45638, 44513, 38297)
em <- data.frame(h,m)
png('edomex.png',width=3000,height=2500,res=300)
boxplot(em, main="Tasa de prevalencia delictiva: Estado de México", names = c("Hombres", "Mujeres"), col= c(rgb(0.1,0.1,0.7,0.5) , rgb(0.8,0.1,0.3,0.6)))
dev.off()

# datos edomexico tasa de prevalencia delictiva segun sexo para edomex, cdmx, baja california nte
entidad = c(rep("Estado de Mexico", 18), rep("Ciudad de Mexico", 18), rep("Baja California Norte", 18))
previo = c(rep("Hombres", 9), rep("Mujeres", 9))
sexo = rep(previo, 3)
edomex = c(31454, 33889, 45506, 53217, 49874, 51555, 49907, 49177, 44778, 24777, 
           28380, 37105, 42555, 40938, 40653, 45638, 44513, 38297)
cdmx  = c(36308, 35662,	33860, 35861,	35949, 40515,	36428, 46343,	45130, 28795,
          31081, 29470,	30816, 36079,	36767, 35639, 40301, 40473)
bcn = c(32097, 30340, 36046, 41483, 36532, 29854, 40536,	36138, 32897, 29701,	
        30751, 37059, 37655, 38550,	31612, 39244,	36985, 34695)
tasas = c(edomex, cdmx, bcn)
data = data.frame(entidad, tasas, sexo)

library(ggplot2)
d <- ggplot(data, aes(x=entidad, y=tasas, fill=sexo)) + 
        geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
png('delitos_sexo.png',width=3000,height=2500,res=300)
print(d)
dev.off()

