rm(list = ls())
if (length(dev.list())) {
  dev.off()
}

library("plot3D")

set.seed(203)

n <- 1 # dimensao
N <- 100 # numero de pontos de cada classe

# pro grafico
ylim <- c(-1.5, 1.5)
xlim <- c(0, 5)

# gera os dados
xc1 <- rnorm(N, mean = 1.5, sd = 0.5)
xc2 <- rnorm(N, mean = 3.5, sd = 0.5)

# a priori das duas - p(c), prob de ser da classe
N1 <- length(xc1) # n de amostras da classe 1
N2 <- length(xc2)

pc1 <- N1/(N1 + N2)
pc2 <- N2/(N1 + N2)

# assume que e normal, func normal de 1 variavel
# essa é a funcao que faz o papel da P(x | C)
fnormal1var <- function(x, m, r) {
  y <- (1/(sqrt(2*pi*r*r)))*exp(-0.5 * ((x-m)/r)^2)
  return (y)
}

plot(xc1, matrix(0, nrow = N1, ncol = n), xlim = xlim, ylim = ylim, col = "red")
par(new = T)
plot(xc2, matrix(0, nrow = N2, ncol = n), xlim = xlim, ylim = ylim, col = "blue")

# monta o grid
xgrid = seq(0, 6, 0.05)

# KDE
h = 0.2

pkde_c1 = rep(0, length(xgrid))
for (i in (1:N)) {
  pkde = fnormal1var(xgrid, xc1[i], h) / N # gaussiana especifica da amostra atual
  # divide por N para ficar menor que 1 a probabilidade
  par(new=T)
  # plot(xgrid, pkde, type = 'l', col = 'red', xlim = xlim, ylim = ylim)
  pkde_c1 = pkde_c1 + pkde
}

pkde_c2 = rep(0, length(xgrid))
for (i in (1:N)) {
  pkde = fnormal1var(xgrid, xc2[i], h) / N # gaussiana especifica da amostra atual
  par(new=T)
  # plot(xgrid, pkde, type = 'l', col = 'blue', xlim = xlim, ylim = ylim)
  pkde_c2 = pkde_c2 + pkde
}

par(new = T)
plot(xgrid, pkde_c1, type = 'l', col = 'red', xlim = xlim, ylim = ylim, lwd = 2)
par(new = T)
plot(xgrid, pkde_c2, type = 'l', col = 'blue', xlim = xlim, ylim = ylim, lwd = 2)

# agora usa bayes com essas verossimilhanças para fazer a classificação
pc1x = pkde_c1 * pc1
pc2x = pkde_c2 * pc2
yhat <- sign(pc1x - pc2x)

par(new = T)
plot(xgrid, yhat, xlim = xlim, ylim = ylim, col = "black", type = 'l', lwd = 2)



