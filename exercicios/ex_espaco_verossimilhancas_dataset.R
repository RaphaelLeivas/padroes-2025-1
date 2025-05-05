rm(list = ls())
if (length(dev.list())) {
  dev.off()
}

library("flextable")
library("mlbench")
library("plot3D")

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

distance_two_points <- function(x1, x2) {
  return (sqrt(sum((x1 - x2)^2)))
}

# dimensao 2D
distance_point_line <- function(x0, a, b, c) {
  return (abs(a * x0[1] + b * x0[2] + c) / sqrt(a^2 + b^2))
}

# pega os dados da package mlbench
data("PimaIndiansDiabetes")
data2 <- PimaIndiansDiabetes

# Realiza o tratamento dos dados para remoção de NA
data2 <- data2[complete.cases(data2),]

# embaralha a matriz dos dados de entrada - remove bias de coleta
data2 <- data2[sample.int(nrow(data2)), ]

start_variables_column <- 1
end_variables_column <- 8
label_column <- 9

X <- data.matrix(data2[, start_variables_column:end_variables_column])

N <- nrow(X) # numero de amostras
n <- ncol(X) # numero de variaveis

Y <- matrix(NA, nrow = N, ncol = 1)

C1_LABEL = 1
C2_LABEL = -1

for (i in 1:N) {
  # ultima coluna é a coluna com a classe
  if (data2[i, label_column] == "pos") {
    Y[i] <- C1_LABEL
  } else {
    Y[i] <- C2_LABEL
  }
}

# junta tudo na matriz dos dados
all_data <- cbind(X, Y)

n_folds <- 2
fold_size <- floor(N / n_folds)

# h_list <- seq(0.6, 3.5, 0.1)
h_list <- seq(0.36, 10, 0.25)
h_counter <- 0

acc_by_h <- c()
dpl_mat <- matrix(NA, ncol = 2, nrow = length(h_list))
dpl_ratio <- c()
dist_index_norm_arr <- c()

for (h in h_list) {
  h_counter <- h_counter + 1
  acc_array <- c()
  
  for (fold in 1:n_folds) {
    num_of_corrects <- 0
    
    start_index <- fold_size * (fold - 1) + 1
    end_index <- start_index + fold_size - 1
    
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
    
    # itera sobre todos os pontos de teste
    for (i in 1:nrow(data_for_test)) {
      xt <- data_for_test[i, 1:n]
      yt <- data_for_test[i, n+1]
      
      # garante que xt é vetor coluna
      xt <- matrix(xt, ncol = 1, nrow = n)
      
      pxc1 <- kdemulti(xt, xc1_train, h)
      pxc2 <- kdemulti(xt, xc2_train, h)
      
      yhat <- NA
      
      if (pxc1 * pc1 > pxc2 * pc2) {
        yhat <- C1_LABEL
      } else {
        yhat <- C2_LABEL
      }
      
      if (yhat == yt) {
        num_of_corrects <- num_of_corrects + 1
      }
    }

    # plota o espaço de semelhanças do conjunto de treinamento
    if (fold == 1) {
      pxc1vec <- c()
      pxc2vec <- c()
      pxlabelvec <- c()
      for (i in 1:nrow(data_for_train)) {
        # para amostra calcular o kde dela e guardar
        # garante vetor coluna
        xt <- matrix(X_train[i,], ncol = 1, nrow = n)
        pxc1vec <- c(pxc1vec, kdemulti(xt, xc1_train, h))
        pxc2vec <- c(pxc2vec, kdemulti(xt, xc2_train, h)) 
        if (Y_train[i] == C1_LABEL) {
          pxlabelvec <- c(pxlabelvec, C1_LABEL_COL)
        } else {
          pxlabelvec <- c(pxlabelvec, C2_LABEL_COL)
        }
      }
      
      pxc1c2 <- cbind(pxc1vec, pxc2vec, pxlabelvec)
      
      
      if (h %% 2 == 0) {
        # plot(as.numeric(pxc1c2[,1]), as.numeric(pxc1c2[,2]),
        #      col = pxc1c2[,3], xlab="pxc1", ylab="pxc2",
        #      main = paste("h = ", h))
      }
      
      # indice 3: razao da distancias de cada centro à identidade
      # identidade: 1x - 1y + 0 = 0
      k = 2 # dois clusters
      kmeansret <- kmeans(pxc1c2[, 1:2], k)
      dpl_mat[h_counter, 1] <- distance_point_line(kmeansret$centers[1,], 1, -1, 0)
      dpl_mat[h_counter, 2] <- distance_point_line(kmeansret$centers[2,], 1, -1, 0)
      dpl_ratio <- c(dpl_ratio, dpl_mat[h_counter, 1] / dpl_mat[h_counter, 2])
      
      
      # indice 4: distancia normalizada entre os centroids
      dist_index_norm <- distance_two_points(
        kmeansret$centers[1,],
        kmeansret$centers[2,]
      ) / min(
        distance_two_points(kmeansret$centers[2,], c(0,0)), 
        distance_two_points(kmeansret$centers[1,], c(0,0))
      )
      dist_index_norm_arr <- c(dist_index_norm_arr, dist_index_norm)
    }
    
    acc_array <- c(acc_array, num_of_corrects / nrow(data_for_test) * 100)
  }
  
  acc_by_h <- c(acc_by_h, mean(acc_array))
  
  print(paste(mean(acc_array), " +/- ", sd(acc_array)))
}


plot(h_list, acc_by_h, type = "b", col = "black", lwd = 3, xlab = "h"
     ,ylab = "Acurácia (%)", main = "Acurácia (%) em função de h") # first plot

par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(h_list, acc_by_h, type = "b", col = "black", lwd = 2, xlab = "h"
     ,ylab = "Acurácia (%)") # first plot
par(new = TRUE)
plot(h_list, dist_index_norm_arr, type = "b", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd = 2,
     col = "orange")
axis(side=4, at = pretty(range(dist_index_norm_arr)))
mtext("Distância Normalizada", side=4, line=3)
legend("bottomleft", legend = c("Acurácia (%)", "Distância Normalizada"),
       col=c("black", "orange"), lty=1)

