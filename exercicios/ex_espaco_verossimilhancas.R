rm(list = ls())
if (length(dev.list())) {
  dev.off()
}

library("flextable")
library("plot3D")


fnormal1var <- function(x, m, r) {
  y <- (1/(sqrt(2*pi*r*r)))*exp(-0.5 * ((x-m)/r)^2)
  return (y)
}

distance_two_points <- function(x1, x2) {
  return (sqrt(sum((x1 - x2)^2)))
}

# dimensao 2D
distance_point_line <- function(x0, a, b, c) {
  return (abs(a * x0[1] + b * x0[2] + c) / sqrt(a^2 + b^2))
}

set.seed(203)

C1_LABEL <- 1
C2_LABEL <- -1
C1_LABEL_COL = "red"
C2_LABEL_COL = "blue"
grid_spacing <- 0.1

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


N <- 300
n <- 2
# h <- 0.5

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
yall <- c(yc1, yc2)

# junta tudo na matriz dos dados
all_data <- cbind(xall, yall)

# embaralha a matriz dos dados de entrada - remove bias de coleta
all_data <- all_data[sample.int(nrow(all_data)), ]

n_folds <- 10
fold_size <- floor(N / n_folds)

h_list <- seq(0.1, 1.5, 0.1)
h_counter <- 0
# h_list <- c(0.1, 0.5, 1)

acc_by_h <- c()
dist_index_array <- c()
ratio_corr_mat <- matrix(NA, ncol = 2, nrow = length(h_list))
dpl_mat <- matrix(NA, ncol = 2, nrow = length(h_list))
dpl_ratio <- c()

for (h in h_list) {
  h_counter <- h_counter + 1
  
  acc_array <- c()
  
  for (fold in 1:n_folds) {
    num_of_corrects <- 0
    
    start_index <- fold_size * (fold - 1) + 1
    end_index <- start_index + fold_size
    
    data_for_test <- all_data[start_index:end_index, ]
    X_test <- data_for_test[, 1:n]
    Y_test <- data_for_test[, n+1]
    
    data_for_train <- all_data[-(start_index:end_index), ]
    X_train <- data_for_train[, 1:n]
    Y_train <- data_for_train[, n+1]
    
    xc1_train <- X_train[which(Y_train == C1_LABEL),]
    xc2_train <- X_train[which(Y_train == C2_LABEL),]
    
    pc1 <- nrow(xc1_train) / nrow(X_train)
    pc2 <- nrow(xc2_train) / nrow(X_train)
    
    # plot(xc1_train[,1], xc1_train[,2], xlim = c(0,6), ylim=c(0,6), xlab='', ylab='', col = "red", , main = paste("h = ", h))
    # par(new=T)
    # plot(xc2_train[,1], xc2_train[,2], xlim = c(0,6), ylim=c(0,6), xlab='', ylab='', col = "blue")
    
    # grid
    seqi <- seq(0, 6, grid_spacing)
    seqj <- seq(0, 6, grid_spacing)
    M1 <- matrix(1, nrow =  length(seqi), ncol = length(seqj))
    
    ci <- 0
    
    for (i in seqi) {
      ci <- ci + 1
      cj <- 0
      
      for (j in seqj) {
        cj <- cj +1
        x <- matrix(c(i, j), byrow = T, ncol = 1)
        pxc1 <- kdemulti(x, xc1_train, h)
        pxc2 <- kdemulti(x, xc2_train, h)
        
        if (pxc1 * pc1 > pxc2 * pc2) {
          M1[ci, cj] <- C1_LABEL
        } else {
          M1[ci, cj] <- C2_LABEL
        }
      }
    }
    
    # par(new = T)
    # contour(seqi, seqj, M1, levels = 1, lwd = 2)
    
    # calcula a acuracia no conjunto de testes
    for (i in 1:nrow(data_for_test)) {
      xt <- data_for_test[i, 1:n]
      yt <- data_for_test[i, n+1]
      
      # coloca o xt no grid. ve se esta correto
      # procura a posição do grid o mais proximo possivel do xtest_point
      # o valor de cada posicao no xgrid é (index - 1) * grid_spacing
      # queremos (index - 1) * grid_spacing = xtest_point => index = xtest_point / grid_spacing + 1
      index_in_xgrid = round(xt[1] / grid_spacing + 1)
      index_in_ygrid = round(xt[2] / grid_spacing + 1)
      
      if (M1[index_in_xgrid, index_in_ygrid] == yt) {
        num_of_corrects <- num_of_corrects + 1
      }
    }
    
    # plota o espaço de semelhanças
    
    if (fold == 1) {
      pxc1vec <- c()
      pxc2vec <- c()
      pxlabelvec <- c()
      for (i in 1:nrow(data_for_train)) {
        # para amostra calcular o kde dela e guardar
        pxc1vec <- c(pxc1vec, kdemulti(X_train[i,], xc1_train, h))
        pxc2vec <- c(pxc2vec, kdemulti(X_train[i,], xc2_train, h)) 
        if (Y_train[i] == C1_LABEL) {
          pxlabelvec <- c(pxlabelvec, "red")
        } else {
          pxlabelvec <- c(pxlabelvec, "blue")
        }
      }
      
      pxc1c2 <- cbind(pxc1vec, pxc2vec, pxlabelvec)
      
      plot(pxc1c2[,1], pxc1c2[,2], col = pxc1c2[,3], xlab="pxc1", ylab="pxc2",
           main = paste("h = ", h))
      
      # calcula indices sobre o espaço de verossimilhanças
      # procura algum que se relaciona bem com o h de maior acurácia
      # melhor h: 0.5
      
      # indice 1: distancia entre os centroides
      k = 2 # dois clusters
      kmeansret <- kmeans(pxc1c2[, 1:2], k)
      dist_index <- distance_two_points(kmeansret$centers[1,], kmeansret$centers[2,])
      dist_index_array <- c(dist_index_array, dist_index)
      
      # indice 3: razao da distancias de cada centro à identidade
      # identidade: 1x - 1y + 0 = 0
      dpl_mat[h_counter, 1] <- distance_point_line(kmeansret$centers[1,], 1, -1, 0)
      dpl_mat[h_counter, 2] <- distance_point_line(kmeansret$centers[2,], 1, -1, 0)
      dpl_ratio <- c(dpl_ratio, dpl_mat[h_counter, 1] / dpl_mat[h_counter, 2])
      
      # indice 2: numero de pontos que cruzam a identidade
      # num_corr_c1 <- 0
      # num_corr_c2 <- 0
      # for (i in 1:nrow(pxc1c2)) {
      #   if (pxc1c2[i,3] == C1_LABEL_COL) {
      #     # classe 1
      #     if (as.numeric(pxc1c2[i, 1]) > as.numeric(pxc1c2[i, 2])) {
      #       num_corr_c1 <- num_corr_c1 + 1
      #     }
      #   }
      #   
      #   if (pxc1c2[i,3] == C2_LABEL_COL) {
      #     # classe 2
      #     if (as.numeric(pxc1c2[i, 1]) < as.numeric(pxc1c2[i, 2])) {
      #       num_corr_c2 <- num_corr_c2 + 1
      #     }
      #   }
      # }
      # 
      # ratio_corr_mat[h_counter, 1] <- num_corr_c1 / nrow(pxc1c2[which(pxc1c2[,3] == C1_LABEL_COL),])
      # ratio_corr_mat[h_counter, 2] <- num_corr_c2 / nrow(pxc1c2[which(pxc1c2[,3] == C2_LABEL_COL),])
      
    }
    
    acc_array <- c(acc_array, num_of_corrects / nrow(data_for_test) * 100)
  }
  
  acc_by_h <- c(acc_by_h, mean(acc_array))
  
  print(paste(mean(acc_array), " +/- ", sd(acc_array)))
  
  df <- data.frame(seq(1, 10, 1), round(acc_array, 2))
  colnames(df) <- c("Fold", "Acurácia (%)")
  ft <- flextable(df)
  ft <- align(ft, align = "center", part = "all")
}

# plot(h_list, acc_by_h, lwd = 2, col = "black", type = "b")
# plot(h_list, dist_index_array, lwd = 2, col = "black", type = "b")
# plot(h_list, dpl_ratio, lwd = 2, col = "black", type = "b")
# plot(dist_index_array, acc_by_h, lwd = 2, col = "black", type = "b")

plot(h_list, acc_by_h, lwd = 2, col = "black", type = "b")
par(new=T)
plot(h_list, dpl_ratio, lwd = 2, col = "orange", type = "b")

df <- data.frame(h_list, round(acc_by_h, 2))
colnames(df) <- c("h", "Acurácia (%)")
ft <- flextable(df)
ft <- align(ft, align = "center", part = "all")



