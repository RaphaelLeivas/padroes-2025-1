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

n_folds <- 5
fold_size <- floor(N / n_folds)

h_list <- seq(0.5, 1.5, 0.05)

acc_by_h <- c()

for (h in h_list) {
  
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
    
    # itera sobre todos os pontos de teste
    for (i in 1:nrow(data_for_test)) {
      xt <- data_for_test[i, 1:n]
      yt <- data_for_test[i, n+1]
      
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
    
    acc_array <- c(acc_array, num_of_corrects / nrow(data_for_test) * 100)
    
    Nall <- nrow(X_train)

    pxc1vec <- matrix()
    pxc2vec <- matrix()

    for (i in 1:Nall) {
      # para cada amostra calcular o kde dela e guardar
      pxc1vec[i] <- kdemulti(X_train[i,], xc1_train, h)
      pxc2vec[i] <- kdemulti(X_train[i,], xc2_train, h)
    }

    # pxc1c2 <- cbind(pxc1vec, pxc2vec)
    # col_seq <- c("red", "blue")
    # 
    # plot(pxc1c2[,1], pxc1c2[,2], col = col_seq[((Y_train+1)/2) + 1],
    #      xlab="pxc1", ylab="pxc2", main = paste("h = ", h))
  }
  
  acc_by_h <- c(acc_by_h, mean(acc_array))
  
  # print(paste(mean(acc_array), " +/- ", sd(acc_array)))
  
  df <- data.frame(seq(1, 10, 1), round(acc_array, 2))
  colnames(df) <- c("Fold", "Acurácia (%)")
  ft <- flextable(df)
  ft <- align(ft, align = "center", part = "all")
}

plot(h_list, acc_by_h, lwd = 2, col = "black", type = "b")

df <- data.frame(h_list, round(acc_by_h, 2))
colnames(df) <- c("h", "Acurácia (%)")
ft <- flextable(df)
ft <- align(ft, align = "center", part = "all")



