rm(list = ls())

require(RnavGraphImageData)
library("kernlab")
library("flextable")

NUMBER_OF_FACES = 400 # original 400
NUMBER_OF_PIXELS = 4096 # original 2^12 = 4096

data(faces)
faces <- t(faces[1:NUMBER_OF_PIXELS, 1:NUMBER_OF_FACES])

MostraImagem <- function(x) {
    rotate <- function(x) t(apply(x, 2, rev))
    img <- matrix(x, nrow = 64)
    cor <- rev(gray(50:1 / 50))
    image(rotate(img), col = cor)
}

# Nomeando os atributos
nomeColunas <- NULL
for (i in 1:ncol(faces))
{
    nomeColunas <- c(nomeColunas, paste("a", as.character(i), sep = "."))
}
colnames(faces) <- nomeColunas
rownames(faces) <- NULL

# escolhe uma classe para classificar contra as demais
C1_LABEL <- 1
C2_LABEL <- -1

# Gerando os rótulos
y <- c()
y_colors <- c()
for (i in 1:nrow(faces)) {
  if (i <= 10) { # primeiro rosto vai ate 10
    y <- c(y, C1_LABEL)
    y_colors <- c(y_colors, "red")
  } else {
    y <- c(y, C2_LABEL)
    y_colors <- c(y_colors, "blue")
  }
}

# faz o PCA
# X <- as.matrix(faces)
meanx<-colMeans(X)
Xs<- X - t(replicate(dim(X)[1],meanx))
S<-cov(Xs)
eigS<-eigen(S)
projX<-Xs %*% eigS$vectors

plot(eigS$values[1:20],type='b',xlab='',ylab='Autovalor',col='red', lwd = 2, main = "Autovalores")


# salva no CSV para nao precisar ficar calculando toda hora
# csv_df <- data.frame(projX)
# path <- "C:\\dev\\padroes-2025-1\\exercicios\\projX_faces.csv"
# path_eigen <- "C:\\dev\\padroes-2025-1\\exercicios\\eigen_faces.csv"

# write.csv(csv_df, file=path, row.names = FALSE, quote = FALSE)
# write.csv(data.frame(eigS), file = path_eigen, row.names = FALSE, quote = FALSE)


path <- "C:\\dev\\padroes-2025-1\\exercicios\\projX_faces.csv"
projX <- read.csv(path)
eig_to_use <- 20

all_data <- cbind(projX, y)

acc_array <- c()

experiments <- 10

k <- 5 # 50%
# k <- 7 # 70%
# k <- 9 # 90%

for (exp in 1:experiments) {
  # embaralha a matriz dos dados de entrada - remove bias de coleta
  all_data <- all_data[sample.int(nrow(all_data)), ]
  
  c1_indexes <- which(all_data[, ncol(all_data)] == C1_LABEL)
  c2_indexes <- which(all_data[, ncol(all_data)] == C2_LABEL)
  
  # metade dos indices de c1 vai para train, a outra metade para vai teste
  # mesma coisa para o c2
  train_indexes <- c(c1_indexes[1:k],
                     c2_indexes[1:(k * 390 / 10)])
  
  test_indexes <- c(c1_indexes[(k+1):length(c1_indexes)],
                    c2_indexes[(k * 390 / 10 + 1):length(c2_indexes)])
  
  X_train <- as.matrix(all_data[train_indexes, 1:eig_to_use])
  Y_train <- all_data[train_indexes, ncol(all_data)]
  
  X_test <- as.matrix(all_data[test_indexes, 1:eig_to_use])
  Y_test <- all_data[test_indexes, ncol(all_data)]
  
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




