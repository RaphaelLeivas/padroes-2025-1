rm(list = ls())
if (length(dev.list())) {
  dev.off()
}

library("mlbench")
library("flextable")
source("C:\\dev\\padroes-2025-1\\utils\\trainperceptron.R")

set.seed(205)

# pega os dados da package mlbench
data("BreastCancer")
data2 <- BreastCancer

# Realiza o tratamento dos dados para remoção de NA
data2 <- data2[complete.cases(data2),]

start_variables_column <- 2
end_variables_column <- 10
label_column <- 11

X <- data.matrix(data2[, start_variables_column:end_variables_column])

N <- nrow(X) # numero de amostras
n <- ncol(X) # numero de variaveis

Y <- matrix(NA, nrow = N, ncol = 1)

for (i in 1:N) {
  # ultima coluna é a coluna com a classe
  if (data2[i, label_column] == "benign") {
    Y[i] <- 1
  } else {
    Y[i] <- 0
  }
}

# junta tudo na matriz dos dados
all_data <- cbind(X, Y)

# embaralha a matriz dos dados de entrada - remove bias de coleta
all_data <- all_data[sample.int(nrow(all_data)), ]

n_folds <- 10
fold_size <- floor(N / n_folds)

eta <- 0.01
tol <- 0.001 # entre loops de treinamento, para detectar que convergiu
maxepocas <- 10

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
  
  # treina a rede
  retlist <- trainperceptron(X_train, Y_train, eta, tol, maxepocas, 1)
  
  w <- retlist[[1]] # w na posicao 1 é o theta
  evec <- retlist[[2]]
  
  # calcula a saida da rede no conjunto de teste
  Yhat <- cbind(1, X_test) %*% w # add coluna 1 para o theta (polarizacao)
  
  # se é maior que zero, salva como 1 (benigno). se é menor que 0, salva como 0 (maligno)
  for (i in 1:fold_size) {
    if (Yhat[i] >= 0) {
      Yhat[i] <- 1
    } else {
      Yhat[i] <- 0
    }
  }
  
  # compara com a saida de teste e conta os corretos
  for (i in 1:fold_size) {
    if (Yhat[i] == Y_test[i]) {
      num_of_corrects <- num_of_corrects + 1
    } 
  }
  
  acc_array <- c(acc_array, num_of_corrects / fold_size * 100)
  
  if (fold != 8) next
  
  # plota a convergencia em um deles (fold = 8)
  plot(
    evec,
    main = paste("Convergência: fold = ", fold),
    xlab = "Iteração",
    ylab = "Erro", col = "red", lwd = 2,  type="l",
    cex.main = 2,
    cex.axis = 2,
    cex.lab = 2
  )
}

print(paste(mean(acc_array), " +/- ", sd(acc_array)))

df <- data.frame(seq(1, 10, 1), round(acc_array, 2))
colnames(df) <- c("Fold", "Acurácia (%)")
ft <- flextable(df)
ft <- align(ft, align = "center", part = "all")

