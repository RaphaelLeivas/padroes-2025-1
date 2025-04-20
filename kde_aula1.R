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

n <- 1 # dimensao
N <- 500 # numero de pontos de cada classe

h = 0.2 # hiperparametro do KDE

# pro grafico
ylim <- c(-1.5, 1.5)
xlim <- c(0, 5)

# gera os dados
xc1 <- rnorm(N, mean = 1.5, sd = 0.5)
xc2 <- rnorm(N, mean = 3.5, sd = 0.5)
yc1_label <- 1
yc2_label <- -1

# junta tudo para o teste e treinamento
c1_all <- cbind(xc1, yc1_label)
c2_all <- cbind(xc2, yc2_label)
all_data <- rbind(c1_all, c2_all)

# embaralha a matriz dos dados de entrada - remove bias de coleta
all_data <- all_data[sample.int(nrow(all_data)), ]

n_folds <- 10
fold_size <- floor(2*N / n_folds)

acc_array <- c()

for (fold in 1:n_folds) {
  num_of_corrects <- 0

  start_index <- fold_size * (fold - 1) + 1
  end_index <- start_index + (fold_size - 1)

  data_for_test <- all_data[start_index:end_index, ]
  X_test <- data_for_test[, 1:n]
  Y_test <- data_for_test[, n+1]

  data_for_train <- all_data[-(start_index:end_index), ]
  X_train <- data_for_train[, 1:n]
  Y_train <- data_for_train[, n+1]
  
  # monta os xc1 e xc2 a partir do Xtrain
  xc1_train <- data_for_train[which(data_for_train[,n+1]==yc1_label),1:n]
  xc2_train <- data_for_train[which(data_for_train[,n+1]==yc2_label),1:n]

  # a priori das duas - p(c), prob de ser da classe
  N1 <- length(xc1_train) # n de amostras da classe 1
  N2 <- length(xc2_train)
  
  pc1 <- N1/(N1 + N2)
  pc2 <- N2/(N1 + N2)
  
  plot(xc1_train, matrix(0, nrow = N1, ncol = n), xlim = xlim, ylim = ylim, col = "red")
  par(new = T)
  plot(xc2_train, matrix(0, nrow = N2, ncol = n), xlim = xlim, ylim = ylim, col = "blue")
  
  # monta o grid
  grid_spacing = 0.05
  xgrid = seq(0, 6, grid_spacing)
  
  pkde_c1 = rep(0, length(xgrid))
  for (i in (1:N1)) {
    pkde = fnormal1var(xgrid, xc1_train[i], h) / N1 # gaussiana especifica da amostra atual
    # divide por N para ficar menor que 1 a probabilidade
    par(new=T)
    # plot(xgrid, pkde, type = 'l', col = 'red', xlim = xlim, ylim = ylim)
    pkde_c1 = pkde_c1 + pkde
  }
  
  pkde_c2 = rep(0, length(xgrid))
  for (i in (1:N2)) {
    pkde = fnormal1var(xgrid, xc2_train[i], h) / N2 # gaussiana especifica da amostra atual
    par(new=T)
    # plot(xgrid, pkde, type = 'l', col = 'blue', xlim = xlim, ylim = ylim)
    pkde_c2 = pkde_c2 + pkde
  }
  
  par(new = T)
  plot(xgrid, pkde_c1, type = 'l', col = 'red', xlim = xlim, ylim = ylim, lwd = 2)
  par(new = T)
  plot(xgrid, pkde_c2, type = 'l', col = 'blue', xlim = xlim, ylim = ylim, lwd = 2)
  
  # agora usa bayes com essas verossimilhanças para fazer a classificação
  pc1x = pkde_c1 * pc1
  pc2x = pkde_c2 * pc2
  yhat <- sign(pc1x - pc2x)
  
  par(new = T)
  plot(xgrid, yhat, xlim = xlim, ylim = ylim, col = "black", type = 'l', lwd = 2)
  
  # agora joga no conjunto de testes
  for (i in 1:nrow(data_for_test)) {
    xtest_point = X_test[i]
    xtest_label = Y_test[i]
    
    # procura a posição do grid o mais proximo possivel do xtest_point
    # o valor de cada posicao no xgrid é (index - 1) * grid_spacing
    # queremos (index - 1) * grid_spacing = xtest_point => index = xtest_point / grid_spacing + 1
    index_in_xgrid = round(xtest_point / grid_spacing + 1)
    xtest_label_hat = sign(pc1x[index_in_xgrid] - pc2x[index_in_xgrid])
    
    if (xtest_label_hat == xtest_label) {
      num_of_corrects <- num_of_corrects + 1
    }
  }

  acc_array <- c(acc_array, num_of_corrects / fold_size * 100)
}

print(paste(mean(acc_array), " +/- ", sd(acc_array)))

df <- data.frame(seq(1, 10, 1), round(acc_array, 2))
colnames(df) <- c("Fold", "Acurácia (%)")
ft <- flextable(df)
ft <- align(ft, align = "center", part = "all")



