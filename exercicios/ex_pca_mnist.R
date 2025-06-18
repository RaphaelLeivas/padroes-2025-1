rm(list = ls())
if (length(dev.list())) {
    dev.off()
}

show_image <- function(x) {
  digit_matrix <- matrix(x, nrow = 28, byrow = TRUE)
  
  # Plot
  image(1:28, 1:28, apply(digit_matrix, 2, rev), col = gray.colors(256))
}

library(dslabs)
library("kernlab")
library("flextable")

mnist <- read_mnist()

images <- mnist$train$images[1:1000, ]
labels <- mnist$train$labels[1:1000]

# escolhe uma classe para classificar contra as demais
# 0 é classe 1, outros é classe -1
C1_LABEL <- 1
C2_LABEL <- -1

# Gerando os rótulos
y <- c()
y_colors <- c()
for (i in 1:nrow(images)) {
  if (labels[i] == 0) {
    y <- c(y, C1_LABEL)
  } else {
    y <- c(y, C2_LABEL)
  }
}

# # faz o PCA
# X <- as.matrix(faces)
meanx<-colMeans(images)
Xs<- images - t(replicate(dim(images)[1],meanx))
S<-cov(Xs)
eigS<-eigen(S)
projX<-Xs %*% eigS$vectors

plot(eigS$values[1:20],type='b',xlab='',ylab='Autovalor',col='red', lwd = 2, main = "Autovalores")

# salva no CSV para nao precisar ficar calculando toda hora
# csv_df <- data.frame(projX)
# path <- "C:\\dev\\padroes-2025-1\\exercicios\\projX_mnist.csv"
# 
# write.csv(csv_df, file=path, row.names = FALSE, quote = FALSE)

# path <- "C:\\dev\\padroes-2025-1\\exercicios\\projX_mnist.csv"
# projX <- read.csv(path)
eig_to_use <- 784
n <- eig_to_use
N <- nrow(projX)

all_data <- cbind(projX[, 1:eig_to_use], y)

acc_array <- c()

n_folds <- 10
fold_size <- floor(N / n_folds)

for (fold in 1:n_folds) {
  # embaralha a matriz dos dados de entrada - remove bias de coleta
  all_data <- all_data[sample.int(nrow(all_data)), ]
  
  start_index <- fold_size * (fold - 1) + 1
  end_index <- start_index + fold_size
  
  if (end_index > N) {
    end_index <- end_index - 1
  }
  
  data_for_test <- all_data[start_index:end_index, ]
  X_test <- data_for_test[, 1:n]
  Y_test <- data_for_test[, n+1]
  
  data_for_train <- all_data[-(start_index:end_index), ]
  X_train <- data_for_train[, 1:n]
  Y_train <- data_for_train[, n+1]
  
  h <- 0.1
  C <- 15
  
  svm_train <- ksvm(X_train, Y_train, type='C-bsvc', kernel='rbfdot',
                    kpar=list(sigma=h), C=C)
  
  yhat <- predict(svm_train, X_test, type="response")
  
  num_of_corrects <- 0
  
  for (i in 1:length(Y_test)) {
    if (yhat[i] == Y_test[i]) {
      num_of_corrects <- num_of_corrects + 1
    }
  }
  
  print(num_of_corrects / length(Y_test) * 100)
  
  acc_array <- c(acc_array, num_of_corrects / length(Y_test) * 100)
}

print(paste(mean(acc_array), " +/- ", sd(acc_array)))

df <- data.frame(seq(1, 10, 1), round(acc_array, 2))
colnames(df) <- c("Execução", "Acurácia (%)")
ft <- flextable(df)
ft <- align(ft, align = "center", part = "all")
