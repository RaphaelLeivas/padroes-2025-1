rm(list = ls())
if (length(dev.list())) {
    dev.off()
}

library("plot3D")
# library("rgl")

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

set.seed(203)

# aberturas das gaussianas: quanto maior s, mais ela espalha
s1<-0.25 # desvio da gaussiana
s2<-0.5

nc<-100 # numero de pontos de cada classe
n<-2

xc1 <- matrix(rnorm(nc), ncol = n) * s1 + 2
xc2 <- matrix(rnorm(nc), ncol = n) * s2 + 4

# calcula medio e desvio dos dados de entrada
# media para cada coluna, coluna 1 e 2
# calcula a covarianca com cov(xc1)
# mesma coisa pro xc2

m1 <- matrix(c(mean(xc1[,1]), mean(xc1[,2])), ncol = 1, nrow = n) # matriz das medias da classe 1
k1 <- cov(xc1)

m2 <- matrix(c(mean(xc2[,1]), mean(xc2[,2])), ncol = 1, nrow = n) # matriz das medias da classe 2
k2 <- cov(xc2)

# calculou para 1 e 2, faz o grid
# para cada par da varredura, estima a vero e faz a classificacao

# matriz de entrada
X <- rbind(xc1, xc2)

# rotulos de entrada
yc1 <- rep(1, nc)
yc2 <- rep(-1, nc)
Y <- c(yc1, yc2)

plot(
  NULL,
  main = "Bayes n var",
  xlab = "x1",
  ylab = "x2",
  ylim = c(0, 6),
  xlim = c(0, 6)
)

points(xc1, col="red", lwd=2)
points(xc2, col="blue", lwd=2)

# gera o grid

seqi <- seq(0, 6, 0.1)
seqj <- seq(0, 6, 0.1)
M1 <- matrix(1, nrow =  length(seqi), ncol = length(seqj))
M2 <- matrix(1, nrow =  length(seqi), ncol = length(seqj))
M3 <- matrix(1, nrow =  length(seqi), ncol = length(seqj))

ci <- 0

for (i in seqi) {
  ci <- ci + 1
  cj <- 0
  
  for (j in seqj) {
    cj <- cj +1
    x <- matrix(c(i, j), byrow = T, ncol = 1)
    # estima pdfnvar para a verossimilhanca
    M1[ci, cj] <- pdfnvar(x, m1, k1, n)
    M2[ci, cj] <- pdfnvar(x, m2, k2, n)
    M3[ci, cj] <- 1*(M2[ci, cj] >= M1[ci, cj]) # retorna 0 ou 1
    
    # n de amostras de cada classe e igual, ao calcular a posteriori pode ignorar as a prioris
    #  o jeito geral calcula as duas e ve qual é maior para tomar a decisao de qual é maior
  }
}

# ver na documentacao como mexe na funcao ribbon, deixa mais bonito
ribbon3D(seqi, seqj, M3)


# muda desvio padrao e ve o que acontece com a separacao

# ----- PROX EXERCICO 

# breastcancer com bayes, problema real com bayes
# rsnns para separar em treinamento e teste, se quiser
# quando chamamos a pdfnvar no loop, estamos centralizando a normal (a pdfnvar) 
# no centro de cada amostra. muitas vezes isso funciona bem, quando os dados
# estao bem separados no espaço. 
# se nao funcionar, na validação cruzada vai ter acuracia baixa, ai muda a estrategia


# estrategia geral - testa varios classificadores, do mais simples (knn e perceptron)
# ate os nao lineares mais complexos. problema nao linear vai ter
# acuracia ruim com perceptron, ai pega mais complexos

