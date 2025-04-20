rm(list = ls())
if (length(dev.list())) {
  dev.off()
}

set.seed(203)

library("plot3D")
library("mlbench")
library("flextable")
source("C:\\dev\\padroes-2025-1\\utils\\mistura_gaussiana.R")

xlim <- c(-1.7, 1.7)
ylim <- c(-1.7, 1.7)
grid_spacing <- 0.05
grid_start <- 1.7
grid_seq <- seq(-grid_start, grid_start, grid_spacing)

N <- 500
k <- 10
n <- 2 # dimensao
C1_LABEL <- 1
C2_LABEL <- 2

n_folds <- 10
fold_size <- floor(N / n_folds)

acc_array <- c()

all_data <- mlbench.spirals(N, cycles=2, sd=0.04)

# plot(all_data, xlim = xlim, ylim = ylim, lwd = 2)

xall <- cbind(all_data$x, as.numeric(all_data$classes))

# embaralha a matriz dos dados de entrada - remove bias de coleta
xall <- xall[sample.int(nrow(xall)), ]

n_folds <- 5
fold_size <- floor(N / n_folds)

acc_array <- c()

for (fold in 1:n_folds) {
  num_of_corrects <- 0
  
  start_index <- fold_size * (fold - 1) + 1
  end_index <- start_index + (fold_size - 1)
  
  data_for_test <- xall[start_index:end_index, ]
  X_test <- data_for_test[, 1:n]
  Y_test <- data_for_test[, n+1]
  
  X_train <- xall[-(start_index:end_index), ]
  
  xall_c1 <- X_train[which(X_train[,n+1]==C1_LABEL),]
  xall_c2 <- X_train[which(X_train[,n+1]==C2_LABEL),]
  
  pc1 <- nrow(xall_c1) / nrow(X_train)
  pc2 <- nrow(xall_c2) / nrow(X_train)
  
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
        Mgrid[ci, cj] = 1
      } else {
        Mgrid[ci, cj] = 2
      }
    }
  }
  
  # agora joga no conjunto de testes
  for (i in 1:nrow(data_for_test)) {
    xtest_point = X_test[i,]
    xtest_label = Y_test[i]
    
    # procura a posição do grid o mais proximo possivel do xtest_point
    # o valor de cada posicao no xgrid é (index - 1) * grid_spacing
    # queremos (index - 1) * grid_spacing = xtest_point => index = xtest_point / grid_spacing + 1
    index_in_xgrid_d1 = round((xtest_point[1] + grid_start) / grid_spacing + 1)
    index_in_xgrid_d2 = round((xtest_point[2] + grid_start) / grid_spacing + 1)
    xtest_label_hat = NA
    
    if (M1_c1[index_in_xgrid_d1, index_in_xgrid_d2] >= M1_c2[index_in_xgrid_d1, index_in_xgrid_d2]) {
      xtest_label_hat = 1
    } else {
      xtest_label_hat = 2
    }
    
    if (xtest_label_hat == xtest_label) {
      num_of_corrects <- num_of_corrects + 1
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
  
  contour(seqi, seqj, Mgrid, lwd = 2)
  
  BUG()
  # persp3D(seqi, seqj, M1_c1)
  # persp3D(seqi, seqj, M1_c2)
  
  # verossimilhanças estimadas: agora é calcular a p(C|x) via regra de Bayes
  
  acc_array <- c(acc_array, num_of_corrects / fold_size * 100)
}

print(paste(mean(acc_array), " +/- ", sd(acc_array)))

df <- data.frame(seq(1, 10, 1), round(acc_array, 2))
colnames(df) <- c("Fold", "Acurácia (%)")
ft <- flextable(df)
ft <- align(ft, align = "center", part = "all")

# *------------------------------------* 



