rm(list = ls())
if (length(dev.list())) {
  dev.off()
}

library("plot3D")
source("C:\\dev\\padroes-2025-1\\utils\\mistura_gaussiana.R")

set.seed(203)

N <- 300
n <- 2

m1 <- c(2,2)
m2 <- c(4,4)
m3 <- c(2,4)
m4 <- c(4,2)

variancia = 0.6

g1 <- matrix(rnorm(2 * N), ncol = n, nrow = N)*variancia + matrix(m1, nrow = N, ncol = n, byrow = T)
g2 <- matrix(rnorm(2 * N), ncol = n, nrow = N)*variancia + matrix(m2, nrow = N, ncol = n, byrow = T)
g3 <- matrix(rnorm(2 * N), ncol = n, nrow = N)*variancia + matrix(m3, nrow = N, ncol = n, byrow = T)
g4 <- matrix(rnorm(2 * N), ncol = n, nrow = N)*variancia + matrix(m4, nrow = N, ncol = n, byrow = T)

plot(g1[,1], g1[,2], xlim = c(0,6), ylim=c(0,6), xlab='', ylab='')
par(new=T)
plot(g2[,1], g2[,2], xlim = c(0,6), ylim=c(0,6), xlab='', ylab='')
par(new=T)
plot(g3[,1], g3[,2], xlim = c(0,6), ylim=c(0,6), xlab='', ylab='')
par(new=T)
plot(g4[,1], g4[,2], xlim = c(0,6), ylim=c(0,6), xlab='', ylab='')

# ainda nao sabemos as classes, aplica o kmeans para ele agrupar
# estima as gaussianas da verossimilhanca centradas nos centros que o kmeans achar 


# monta o xall que vamos particionar
k <- 4 # numero de grupos que queremos agrupar
xall <- rbind(g1, g2, g3, g4)
kmeansret <- kmeans(xall, k)
# kmeansret$cluster são os rotulos de cada amostra que ele agrupo (1, 2, 3 ou 4)

plot (xall[,1], xall[,2], col=kmeansret$cluster, pch=kmeansret$cluster, lwd = 2)

# usa silhueta e matriz de distancia para verificar se ficou bom

# ordena via rotulo para plotar a matriz de distancia
# xall_ord <- xall[order(kmeansret$cluster), ]
# image(as.matrix(dist(xall_ord, diag = T, upper = T)))

xclusters <- list() # cada item da lista e uma amostra de cada cluster

# para cada rotulo
for (i in (1:k)) {
  ici <- which(kmeansret$cluster==i)
  xclusters[[i]] <- xall[ici,]
}

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
    
    # agora centra uma gaussiana em cada centro o kmeans e calcula o p()
    M1[ci, cj] <- mymix(x, xclusters)
    
    # mymix(x, xclusters) seria a verossimilhança p(x|C) que nos estimamos
  }
}

persp3D(seqi, seqj, M1)

contour(seqi, seqj, M1) # mostra que é uma funcao continua, que junta as gaussianas todas

