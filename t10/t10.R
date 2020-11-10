
if (!require('vecsets')) {
  install.packages('vecsets')
}


# e1, p247
juego = function(n) {
  cards = seq(2,10,1)
  profit = numeric()
  for (i in 1:n) {
    card = sample(cards, 1)
    if(card %% 2 == 0)
      profit = c(profit, -1)
    else 
      profit = c(profit, 1)
  }
  return(profit)
}

juego1 = as.data.frame(table(juego(100)))
juego2 = as.data.frame(table(juego(10000)))

meanp = numeric()
meanp2 = numeric()
for(i in 1:100) {
  meanp = c(meanp, mean(juego(100)))
  meanp2 = c(meanp2, mean(juego(10000)))
}
png("juego_cartas1.png", width=2000, height=1600, res=300)
par(mar=c(3,4,1,1))
hist(meanp, main = "", ylab = "Frecuencia", xlab = " ", col = "slategray1")
dev.off()
png("juego_cartas2.png", width=2000, height=1600, res=300)
par(mar=c(3,4,1,1))
hist(meanp2, main = "", ylab = "Frecuencia", xlab = " ", col = "lightpink")
dev.off()

# extra
# meanp = numeric()
# #for (r in 1:500) {
#   for (i in 1:1000) {
#     card = sample(cards, 1)
#     if(card %% 2 == 0)
#       profit = c(profit, -1)
#     else 
#       profit = c(profit, 1)
#   }
#   #meanp = c(meanp, mean(profit))
# #}


# e15, p249

balls = c("G", "G", "S", "S", "S")
resultados = numeric()
meanr = numeric()
for (r in 1:100) {
  for (i in 1:100) {
    cballs = balls
    profit = 0
    while (profit != 1 & sum(cballs == "G") != 0) {
      ball = sample(cballs, 1)
      cballs = vecsets::vsetdiff(cballs, ball)
      if(ball == "G") {
        profit = profit + 1
      } else {
        profit = profit - 1
      }
    }
    resultados = c(resultados, profit)
  }
  meanr = c(meanr, mean(resultados))
}
png("pelotas.png", width=2000, height=1600, res=300)
par(mar=c(1,4,1,1))
boxplot(meanr, main = "", ylab = "Ganancia promedio", xlab = " ", col = "lavender", )
dev.off()

# se retira al llevar ventaja de 1 o sacar 2 pelotas
resultados2 = numeric()
meanr2 = numeric()
for (r in 1:100) {
  for (i in 1:100) {
    cballs = balls
    profit = 0
    while (profit != 1 & length(cballs) != 3) {
      ball = sample(cballs, 1)
      cballs = vecsets::vsetdiff(cballs, ball)
      if(ball == "G") {
        profit = profit + 1
      } else {
        profit = profit - 1
      }
    }
    resultados2 = c(resultados2, profit)
  }
  meanr2 = c(meanr2, mean(resultados2))
}
png("pelotas_gana1_osale2.png", width=2000, height=1600, res=300)
par(mar=c(1,4,1,1))
boxplot(meanr2, main = "", ylab = "Ganancia promedio", xlab = " ", col = "lavender")
dev.off()


# e18, p249 llaves
keys = c(0,0,0,0,0,1)
tries = numeric()
resultk = numeric()
for (r in 1:500) {
  for(i in 1:100) {
    ckeys = keys
    failure = 0
    key = sample(ckeys, 1)
    while (key != 1) {
      failure = failure + 1
      ckeys = vecsets::vsetdiff(ckeys, key)
      key = sample(ckeys, 1)
    }
    tries = c(tries, failure)
  }
  resultk = c(resultk, mean(tries))
}
png("llaves.png", width=2000, height=1600, res=300)
par(mar=c(1,4,1,1))
boxplot(resultk, main = "", ylab = "Promedio de intentos", xlab = " ", col = "mistyrose")
dev.off()


# e3, p278 light bulb
d = function(t) {
  l = 0.05
  return((l**2)*t*exp(-l*t))
}

t = runif(100000, 0, 2500)
meanlb = numeric()
varlb = numeric()
for (r in 1:100) {
  meanlb = c(meanlb, mean(sample(t, 500, prob = d(t))))
  varlb = c(varlb, var(sample(t, 500, prob = d(t))))
}
png("lb_mean.png", width=2000, height=1600, res=300)
par(mar=c(3,4,1,1))
hist(meanlb, main = "", ylab = "Frecuencia", xlab = "", col = "mistyrose")
dev.off()

png("lb_var.png", width=2000, height=1600, res=300)
par(mar=c(3,4,1,1))
hist(varlb, main = "", ylab = "Frecuencia", xlab = "", col = "mistyrose")
dev.off()
