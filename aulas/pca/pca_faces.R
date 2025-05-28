rm(list = ls())
require(RnavGraphImageData)

data(faces)
faces <- t(faces)

MostraImagem <- function(x) {
    rotate <- function(x) t(apply(x, 2, rev))
    img <- matrix(x, nrow = 64)
    cor <- rev(gray(50:1 / 50))
    image(rotate(img), col = cor)
}

# Gerando os rótulos
y <- NULL
for (i in 1:nrow(faces))
{
    y <- c(y, ((i - 1) %/% 10) + 1)
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
TARGET_LABEL <- 1


# faz o PCA
X <- as.matrix(faces)
meanx<-colMeans(X)
Xs<- X - t(replicate(dim(X)[1],meanx))
S<-cov(Xs)
eigS<-eigen(S)
projX<-Xs %*% eigS$vectors




