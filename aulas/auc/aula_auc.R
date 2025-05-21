rm(list = ls())
if (length(dev.list())) {
  dev.off()
}

library("plot3D")

# x: argumento que estou avaliando
# m: centro
# r: raio
fnormal1var <- function(x, m, r) {
  y <- (1/(sqrt(2*pi*r*r)))*exp(-0.5 * ((x-m)/r)^2)
  return (y)
}

xlim <- c(0, 6)
ylim <- c(0, 1)

C1_LABEL = 1
C2_LABEL = -1

N1 <- 30
N2 <- 30

xc1 <- rnorm(N1, mean=2, sd = 1)
xc2 <- rnorm(N2, mean=4, sd = 0.1)
yc1_label <- C1_LABEL
yc2_label <- C2_LABEL

# estima os m e s
m1 <- mean(xc1)
m2 <- mean(xc2)
s1 <- sd(xc1)
s2 <- sd(xc2)

yplot1 <- matrix(0, ncol = 1, nrow = N1)
yplot2 <- matrix(0, ncol = 1, nrow = N2)

plot(xc1, yplot1, col="red", xlim = xlim, ylim = ylim, xlab = '', ylab = '')
par(new = T)
plot(xc2, yplot2, col="blue", xlim = xlim, ylim = ylim, xlab = '', ylab = '')

xrange <- seq(0, 6, 0.1)

# tem que estimar m1 m2 s1 s2 a partir dos dados
fN1 <- fnormal1var(xrange, m1, s1)
fN2 <- fnormal1var(xrange, m2, s2)

par(new = T)
plot(xrange, fN1, col="red", xlim = xlim, ylim = ylim, xlab = '', ylab = '', 
     type='l', lwd = 2)
par(new = T)
plot(xrange, fN2, col="blue", xlim = xlim, ylim = ylim, xlab = '', ylab = '',
     type='l', lwd = 2)

# varia o limiar (theta na foto 05/05) de 0 a 6
TN <- matrix() # true negative
TP <- matrix() # true positive

ci <- 1
for (i in xrange)  {
  # i é o limiar 
  TN[ci] <- sum(1 * (xc1 < i))/N1
  TP[ci] <- sum(1 * (xc2 > i))/N2
  ci <- ci + 1
}

par(new = T)
plot(xrange, TN, col="red", xlim = xlim, ylim = ylim, xlab = '', ylab = '',
     type='l', lwd = 2, lty="dashed")
par(new = T)
plot(xrange, TP, col="blue", xlim = xlim, ylim = ylim, xlab = '', ylab = '',
     type='l', lwd = 2, lty="dashed")

# plota a curva ROC
plot(1 - TN, TP, col="orange", xlim = c(0,1), ylim = c(0,1), xlab = '', ylab = '',
     type='l', lwd = 2)
