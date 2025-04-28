rm(list = ls())
if (length(dev.list())) {
    dev.off()
}

library("plot3D")

set.seed(203)

n <- 1 # dimensao

# pro grafico
ylim <- c(-1.5, 1.5)

# gera os dados
xc1 <- rnorm(100, mean = 3, sd = 0.6)
xc2 <- rnorm(50, mean = 5, sd = 0.6)

# a priori das duas - p(c), prob de ser da classe
N1 <- length(xc1) # n de amostras da classe 1
N2 <- length(xc2)

pc1 <- N1/(N1 + N2)
pc2 <- N2/(N1 + N2)

s1 <- sd(xc1)
m1 <- mean(xc1)

s2 <- sd(xc2)
m2 <- mean(xc2)

# assume que e normal, func normal de 1 variavel
# essa é a funcao que faz o papel da P(x | C)
fnormal1var <- function(x, m, r) {
  y <- (1/(sqrt(2*pi*r*r)))*exp(-0.5 * ((x-m)/r)^2)
  return (y)
}

plot(xc1, matrix(0, nrow = N1, ncol = n), xlim = c(0, 8), ylim = c(0,1.5), col = "red")
par(new = T)
plot(xc2, matrix(0, nrow = N2, ncol = n), xlim = c(0, 8), ylim = c(0,1.5), col = "blue")

# plota as densidades
# monta o grid
xrange <- seq(0, 8, 0.2)

# assume ser normal para ambos -> isso é a parte mais dificil de achar,
# aqui assume para o exercicio
fx1 <- fnormal1var(xrange, m1, s1) # p(x | c1)
fx2 <- fnormal1var(xrange, m2, s2) # p(x | c2)

par(new = T)
plot(xrange, fx1, xlim = c(0, 8), ylim = c(0,1.5), col = "red", type = 'l', lwd = 2)
par(new = T)
plot(xrange, fx2, xlim = c(0, 8), ylim = c(0,1.5), col = "blue", type = 'l', lwd = 2)

# px, no denominador do Bayes, é o mesmo pras duas classes
px <- fx1 * pc1 + fx2 * pc2 # p(x | c1) * p(C1) + p(x | c2) * p(C2) - mutuamente exclusivos 
pc1x <- (fx1 * pc1) / px # p(c1 | x) = ... (regra de Bayes)
pc2x <- (fx2 * pc2) / px

yhat <- sign(pc1x - pc2x)
# positivo classe 1, caso contrario classe 2
# quando a probabilidade de ser a classe 1 é maior que a prob de ser a classe 2,
# classifica como classe 1

par(new = T)
plot(xrange, yhat, xlim = c(0, 8), ylim = c(0,1.5), col = "black", type = 'l', lwd = 2)




