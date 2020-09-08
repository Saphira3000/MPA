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
h <- c(31454, 33889,	45506, 53217,	49874, 51555, 49907, 49177, 44778)
m <- c(24777, 28380,	37105, 42555, 40938, 40653,	45638, 44513, 38297)
em <- data.frame(h,m)
png('edomex.png',width=3000,height=2500,res=300)
boxplot(em, main="Tasa de prevalencia delictiva: Estado de México", names = c("Hombres", "Mujeres"), col= c(rgb(0.1,0.1,0.7,0.5) , rgb(0.8,0.1,0.3,0.6)))
dev.off()
