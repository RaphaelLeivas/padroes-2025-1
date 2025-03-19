rm(list = ls())
if (length(dev.list())) {
    dev.off()
}

minkowski_distance <- function(p1, p2, p) {
  # p1 =  ponto 1, p2 = ponto 2
  # p = 2 é distancia euclidiana
  
  n <- ncol(p1) # dimensao
  print(p1)
  partial_sum <- 0
  for (i in 1:n) {
    partial_sum <- partial_sum + (abs(p1[i] - p2[i]))^p
  }
  
  return (partial_sum^(1/p))
}

myknn <- function(Xarg, Yarg, xt, k) {
  N <- dim(Xarg)[1] # numero de pontos (numero de linhas de X)
  n <- dim(Xarg)[2] # numero de variaveis (dimensao do esp de entrada) (n de colunas de X)
  # primeira linha é a distancia, segunda linha é o rotulo
  dist_matrix <- matrix(NA, nrow = N, ncol = 2)
  
  for (i in 1:N) {
    # calcula a distancia do xt ate o ponto atual da classe
    current_point <- matrix(X[i,], ncol = 2)
    
    # ambos pontos devem estar na forma de matriz - cuidado
    dist_to_point <- minkowski_distance(current_point, xt, p = 2)
    label_of_point <- Y[i]
    
    dist_matrix[i,] <- c(dist_to_point, label_of_point)
  }
  
  print(dist_matrix)
  
  # agora é fazer o sort da matriz. ai eu consigo pegar os k mais proximos
  # junto com os seus labels
}

# aberturas das gaussianas: quanto maior s, mais ela espalha
s1<-0.3
s2<-0.3

nc<-100 # numero de pontos de cada classe

k_list <- c(3)

for (k in k_list) {
  # centro dessa gaussiana esta na posicao (2,2)
  xc1<-matrix(rnorm( nc*2 ) , ncol=2)*s1 + t (matrix( c ( 2 , 2 ) ,nrow=2,ncol=nc ) )
  # centro dessa outra gaussiana esta na posicao (4,4)
  xc2<-matrix(rnorm( nc*2 ) , ncol=2)*s2 + t (matrix( c ( 4 , 4 ) ,nrow=2,ncol=nc ) )
  
  plot(
    NULL,
    main = paste("KNN: k = ", k),
    xlab = "x1",
    ylab = "x2",
    ylim = c(0, 6),
    xlim = c(0, 6)
  )
  
  points(xc1, col="red")
  points(xc2, col="blue")
  
  # matriz de entrada
  X <- rbind(xc1, xc2)
  
  # rotulos de entrada
  yc1 <- rep(1, nc)
  yc2 <- rep(-1, nc)
  Y <- c(yc1, yc2)
  
  
  # dados gerados. 
  
  # calcula o knn
  myknn(X, Y, matrix(c(2,2), ncol = 2), k)
  
  #gera o agora ca
}




