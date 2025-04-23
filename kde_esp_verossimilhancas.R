rm(list = ls())
if (length(dev.list())) {
  dev.off()
}

library("plot3D")

fnormal1var <- function(x, m, r) {
  y <- (1/(sqrt(2*pi*r*r)))*exp(-0.5 * ((x-m)/r)^2)
  return (y)
}

set.seed(203)

C1_LABEL <- 1
C2_LABEL <- -1

# xi é cada amostra
kdemulti <- function(xi, xall, h) {
  N <- dim(xall)[1]
  n <- dim(xall)[2]
  
  # aplica o xi em todo o xall
  xirow <- matrix(xi, ncol = n, nrow = 1) # garante que é vetor linha
  
  # calcula distancia para todos os xall
  
  # pega o xi e replica N vezes, toma a diferença com o xall, eleva ao quadrado
  xirep <- matrix(xirow, ncol = n, nrow = N, byrow = T)
  # byrow preenche por linha
  
  matdif <- (xall - xirep)*(xall - xirep)
  
  # equacao 1.15 das notas de aula
  
  dximat <- rowSums(matdif) # aqui é a distancia
  
  dximat <- dximat / (h*h)
  
  emat <- exp(-dximat/2)
  
  pxi <- sum(emat)/(N * sqrt(2 * pi) * h)^n # eq 1.15
  
  return (pxi)
}


N <- 120
n <- 2
h <- 0.5

m1 <- c(2,2)
m2 <- c(4,4)
m3 <- c(2,4)
m4 <- c(4,2)

variancia = 0.4

g1 <- matrix(rnorm(2 * N), ncol = n, nrow = N)*variancia + matrix(m1, nrow = N, ncol = n, byrow = T)
g2 <- matrix(rnorm(2 * N), ncol = n, nrow = N)*variancia + matrix(m2, nrow = N, ncol = n, byrow = T)
g3 <- matrix(rnorm(2 * N), ncol = n, nrow = N)*variancia + matrix(m3, nrow = N, ncol = n, byrow = T)
g4 <- matrix(rnorm(2 * N), ncol = n, nrow = N)*variancia + matrix(m4, nrow = N, ncol = n, byrow = T)

xc1 <- rbind(g1, g2)
yc1 <- rep(C1_LABEL, nrow(xc1))
xc2 <- rbind(g3, g4)
yc2 <- rep(C2_LABEL, nrow(xc1))

xall <- rbind(g1, g2, g3, g4)

pc1 <- nrow(xc1) / nrow(xall)
pc2 <- nrow(xc2) / nrow(xall)

plot(xc1[,1], xc1[,2], xlim = c(0,6), ylim=c(0,6), xlab='', ylab='', col = "red")
par(new=T)
plot(xc2[,1], xc2[,2], xlim = c(0,6), ylim=c(0,6), xlab='', ylab='', col = "blue")


# px<-kdemulti(xall[1,], xall, h)
pxc1<-kdemulti(xc2[1,], xc1, h)
pxc2<-kdemulti(xc2[1,], xc2, h)
print(pxc1 / pxc2)


# grid

seqi <- seq(0, 6, 0.1)
seqj <- seq(0, 6, 0.1)
M1 <- matrix(1, nrow =  length(seqi), ncol = length(seqj))

ci <- 0

for (i in seqi) {
  ci <- ci + 1
  cj <- 0
  
  for (j in seqj) {
    cj <- cj +1
    x <- matrix(c(i, j), byrow = T, ncol = 1)
    pxc1 <- kdemulti(x, xc1, h)
    pxc2 <- kdemulti(x, xc2, h)
    
    if (pxc1 * pc1 > pxc2 * pc2) {
      M1[ci, cj] <- 1
    } else {
      M1[ci, cj] <- 0
    }
    
    # M1[ci, cj] <- 1 * (pxc1 > (pc2 / pc1) * pxc2)
  }
}

par(new = T)
contour(seqi, seqj, M1, levels = 1, lwd = 2)
# ribbon3D(seqi, seqj, M1)

Nall <- nrow(xall)

pxc1vec <- matrix()
pxc2vec <- matrix()

for (i in 1:Nall) {
  # para amostra calcular o kde dela e guardar
  pxc1vec[i] <- kdemulti(xall[i,], xc1, h)
  pxc2vec[i] <- kdemulti(xall[i,], xc2, h)
}

pxc1c2 <- cbind(pxc1vec, pxc2vec)
colvec <- c('red', 'blue')
# CORRIGIR CORES
plot(pxc1c2[,1], pxc1c2[,2], col = "red", xlab="pxc1", ylab="pxc2")

