rm(list = ls())
if (length(dev.list())) {
  dev.off()
}

library("kernlab")
library("mlbench")
library("flextable")

data("BreastCancer")

xyall <- data.matrix(na.omit(BreastCancer))
xall <- xyall[,-c(1, 11)]
yall <- (xyall[,11] - 1.5) * 2 

ic1 <- which(yall == -1)
ic2 <- which(yall == 1)

xc1 <- xall[ic1, ]
xc2 <- xall[ic2, ]

m1 <- colMeans(xc1)
m2 <- colMeans(xc2)

mall <- colMeans(xall)

fscore <- (abs(m1 - mall) + abs(m2 - mall)) / (apply(xc1, 2, sd) + apply(xc2, 2, sd))

# chama SVM com 2, 3, 6 (com o id é 3, 4, 7)
# e depois com todas as variaveis

xall_reduced <- xall[, c(2, 3, 6)]
all_data <- cbind(xall, yall)

N <- dim(all_data)[1]
n <- dim(all_data)[2] - 1

n_folds <- 5
fold_size <- floor(N / n_folds)
h <- 0.1
C <- 15

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
  svm_train <- ksvm(X_train, Y_train, type='C-bsvc', kenel='rbfdot',
                    kpar=list(sigma=h), C=C)
  
  yhat <- predict(svm_train, X_test, type="response")
  
  # compara com a saida de teste e conta os corretos
  for (i in 1:fold_size) {
    if (yhat[i] == Y_test[i]) {
      num_of_corrects <- num_of_corrects + 1
    } 
  }
  
  acc_array <- c(acc_array, num_of_corrects / fold_size * 100)
}

print(paste("Completo = ", mean(acc_array), " +/- ", sd(acc_array)))

df <- data.frame(seq(1, n_folds, 1), round(acc_array, 2))
colnames(df) <- c("Fold", "Acurácia (%)")
ft <- flextable(df)
ft <- align(ft, align = "center", part = "all")



