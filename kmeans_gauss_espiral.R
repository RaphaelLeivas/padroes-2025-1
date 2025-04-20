rm(list = ls())
if (length(dev.list())) {
  dev.off()
}

library("plot3D")
library("mlbench")
source("C:\\dev\\padroes-2025-1\\utils\\mistura_gaussiana.R")

xlim <- c(-1.7, 1.7)
ylim <- c(-1.7, 1.7)
grid_seq <- seq(-1.7, 1.7, 0.05)

N <- 1000
k <- 30
n <- 2 # dimensao
C1_LABEL <- 1
C2_LABEL <- 2

n_folds <- 10
fold_size <- floor(N / n_folds)

acc_array <- c()

all_data <- mlbench.spirals(N, cycles=2, sd=0.04)

plot(all_data, xlim = xlim, ylim = ylim, lwd = 2)

xall <- cbind(all_data$x, as.numeric(all_data$classes))

# embaralha a matriz dos dados de entrada - remove bias de coleta
xall <- xall[sample.int(nrow(xall)), ]

n_folds <- 10
fold_size <- floor(N / n_folds)

acc_array <- c()

xall_c1 <- xall[which(xall[,n+1]==C1_LABEL),]
xall_c2 <- xall[which(xall[,n+1]==C2_LABEL),]

pc1 <- nrow(xall_c1) / nrow(xall)
pc2 <- nrow(xall_c2) / nrow(xall)

# BUG()
kmeansret_c1 <- kmeans(xall_c1, k)
kmeansret_c2 <- kmeans(xall_c2, k)

xclusters_c1 <- list() # cada item da lista e uma amostra de cada cluster
xclusters_c2 <- list()

# para cada rotulo
for (i in (1:k)) {
  ici <- which(kmeansret_c1$cluster==i)
  xclusters_c1[[i]] <- xall_c1[ici,1:n]
}

for (i in (1:k)) {
  ici <- which(kmeansret_c2$cluster==i)
  xclusters_c2[[i]] <- xall_c2[ici,1:n]
}

seqi <- grid_seq
seqj <- grid_seq
M1_c1 <- matrix(1, nrow = length(seqi), ncol = length(seqj))
M1_c2 <- matrix(1, nrow = length(seqi), ncol = length(seqj))

ci <- 0

for (i in seqi) {
  ci <- ci + 1
  cj <- 0
  
  for (j in seqj) {
    cj <- cj +1
    x <- matrix(c(i, j), byrow = T, ncol = 1)
    
    # agora centra uma gaussiana em cada centro o kmeans e calcula o p()
    M1_c1[ci, cj] <- mymix(x, xclusters_c1)
    M1_c2[ci, cj] <- mymix(x, xclusters_c2)
    
    # mymix(x, xclusters) seria a verossimilhança p(x|C) que nos estimamos
  }
}

plot(xall_c1[,1], xall_c1[,2], col=kmeansret_c1$cluster, lwd = 2, xlim = xlim, ylim = ylim)
par(new=T)
contour(seqi, seqj, M1_c1, xlim = xlim, ylim = ylim, lwd = 2) # mostra que é uma funcao continua, que junta as gaussianas todas
par(new=T)
points(kmeansret_c1$centers[,1:n], col = "blue", pch = 4, lwd = 4)

plot(xall_c2[,1], xall_c2[,2], col=kmeansret_c2$cluster, lwd = 2, xlim = xlim, ylim = ylim)
par(new=T)
contour(seqi, seqj, M1_c2, xlim = xlim, ylim = ylim,  lwd = 2) # mostra que é uma funcao continua, que junta as gaussianas todas
par(new=T)
points(kmeansret_c2$centers[,1:n], col = "blue", pch = 4, lwd = 4)

# persp3D(seqi, seqj, M1_c1)
# persp3D(seqi, seqj, M1_c2)

# verossimilhanças estimadas: agora é calcular a p(C|x) via regra de Bayes

Mgrid <- matrix(NA, nrow = length(seqi), ncol = length(seqj))
ci <- 0
for (i in seqi) {
  ci <- ci + 1
  cj <- 0

  for (j in seqj) {
    cj <- cj +1
    x <- matrix(c(i, j), byrow = T, ncol = 1)

    # agora centra uma gaussiana em cada centro o kmeans e calcula o p()
    M1_c1[ci, cj] <- mymix(x, xclusters_c1) * pc1
    M1_c2[ci, cj] <- mymix(x, xclusters_c2) * pc2

    if (M1_c1[ci, cj] >= M1_c2[ci, cj]) {
      # esse é classe 1
      Mgrid[ci, cj] <- C1_LABEL
    } else {
      # esse é classe 2
      Mgrid[ci, cj] <- C2_LABEL
    }
  }
}

contour(seqi, seqj, Mgrid, xlim = xlim, ylim = ylim)



