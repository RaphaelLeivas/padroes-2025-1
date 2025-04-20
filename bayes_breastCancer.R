rm(list = ls())
if (length(dev.list())) {
  dev.off()
}

library("mlbench")
library("flextable")
source("C:\\dev\\padroes-2025-1\\utils\\trainperceptron.R")

set.seed(222)

pdfnvar <- function(x, m, K, n) {
  # garante que x e m é vetor coluna
  x <- matrix(x, nrow = n, ncol = 1)
  m <- matrix(m, nrow = n, ncol = 1)
  
  return ((1 / (sqrt((
    2 * pi
  )^n * (
    det(K)
  )))) * exp(-0.5 * (t(x - m) %*% (solve(
    K
  )) %*% (x - m))))
}

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

BENIGN_LABEL = 1
MALIGN_LABEL = 0

for (i in 1:N) {
  # ultima coluna é a coluna com a classe
  if (data2[i, label_column] == "benign") {
    Y[i] <- BENIGN_LABEL
  } else {
    Y[i] <- MALIGN_LABEL
  }
}

# junta tudo na matriz dos dados
all_data <- cbind(X, Y)

# embaralha a matriz dos dados de entrada - remove bias de coleta
all_data <- all_data[sample.int(nrow(all_data)), ]

n_folds <- 10
fold_size <- floor(N / n_folds)

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
  
  # calcula as medias das colunas da classe 1
  c1_means <- as.numeric(colMeans(data_for_train[which(data_for_train[,n+1]==BENIGN_LABEL),1:n]))
  k1_cov <- cov(data_for_train[which(data_for_train[,n+1]==BENIGN_LABEL),1:n]) 
  
  # agora para a classe 2
  c2_means <- as.numeric(colMeans(data_for_train[which(data_for_train[,n+1]==MALIGN_LABEL),1:n]))
  k2_cov <- cov(data_for_train[which(data_for_train[,n+1]==MALIGN_LABEL),1:n])

  # calcula a saida do Bayes para cada classe no conj de teste
  num_benings <- 0
  num_maligns <- 0
  for (i in 1:length(Y_test)) {
    if (Y_test[i] == BENIGN_LABEL) num_benings <- num_benings + 1
    else num_maligns <- num_maligns + 1
  }
  
  for (i in 1:nrow(X_test)) {
    current_sample <- X_test[i]
    yhat <- -1
    
    # ve a classificacao dessa amostra:
    Pc1x <- pdfnvar(current_sample, c1_means, k1_cov, n) * num_benings / length(Y_test)
    Pc2x <- pdfnvar(current_sample, c2_means, k2_cov, n) * num_maligns / length(Y_test)
    
    if (Pc1x >= Pc2x) yhat <- BENIGN_LABEL
    else yhat <- MALIGN_LABEL
    
    # verifica se esta certo
    if (yhat == Y_test[i]) num_of_corrects <- num_of_corrects + 1
  }
  
  acc_array <- c(acc_array, num_of_corrects / fold_size * 100)
}

print(paste(mean(acc_array), " +/- ", sd(acc_array)))

df <- data.frame(seq(1, 10, 1), round(acc_array, 2))
colnames(df) <- c("Fold", "Acurácia (%)")
ft <- flextable(df)
ft <- align(ft, align = "center", part = "all")