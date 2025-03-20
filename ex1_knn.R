rm(list = ls())
if (length(dev.list())) {
    dev.off()
}

library("plot3D")

set.seed(203)

minkowski_distance <- function(p1, p2, p) {
  # p1 =  ponto 1, p2 = ponto 2
  # p = 2 é distancia euclidiana
  
  # garante que sao da forma de matriz
  if (!is.matrix(p1) || !is.matrix(p2)) {
    stop("minkowski_distance argumento não e matricial")
  }
    
  n <- ncol(p1) # dimensao

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
  
  # agora é fazer o sort da matriz. ai eu consigo pegar os k mais proximos
  # junto com os seus labels
  ordered_distances <- dist_matrix[order(dist_matrix[,1]),]
  
  # calcula o sign nos k primeiros mais proximos
  final_label <- sign(sum(ordered_distances[1:k, n]))
  
  return (final_label)
}

# aberturas das gaussianas: quanto maior s, mais ela espalha
s1<-0.75
s2<-0.75

nc<-100 # numero de pontos de cada classe

k_list <- c(3, 5, 9, 17)

par(mfrow=c(2,2), cex.lab=2, cex.axis=2, cex.main=2, mai = c(0.4, 0.4, 0.4, 0.4))

# gera os dados - fora do loop, para que sejam os mesmo dados para todos os k
# centro dessa gaussiana esta na posicao (2,2)
xc1<-matrix(rnorm( nc*2 ) , ncol=2)*s1 + t (matrix( c ( 2 , 2 ) ,nrow=2,ncol=nc ) )
# centro dessa outra gaussiana esta na posicao (4,4)
xc2<-matrix(rnorm( nc*2 ) , ncol=2)*s2 + t (matrix( c ( 4 , 4 ) ,nrow=2,ncol=nc ) )

# matriz de entrada
X <- rbind(xc1, xc2)

# rotulos de entrada
yc1 <- rep(1, nc)
yc2 <- rep(-1, nc)
Y <- c(yc1, yc2)

# gera o grid
x1grid <- seq(0, 6, 0.1)
x2grid <- seq(0, 6, 0.1)
grid_matrix <- matrix(NA, nrow = length(x1grid), ncol = length(x2grid))

for (k in k_list) {
  plot(
    NULL,
    main = paste("KNN: k = ", k),
    xlab = "x1",
    ylab = "x2",
    ylim = c(0, 6),
    xlim = c(0, 6)
  )
  
  points(xc1, col="red", lwd=2)
  points(xc2, col="blue", lwd=2)
  
  for (i in 1:length(x1grid)) {
    for (j in 1:length(x2grid)) {
      current_point_grid <- matrix(c(x1grid[i], x2grid[j]), ncol = 2)
      
      # calcula o knn
      grid_matrix[i, j] = myknn(X, Y, current_point_grid, k)
    }
  }
  
  contour2D(grid_matrix, x1grid, x2grid, levels=0, xlim=c(0,6), ylim=c(0,6), 
            add = T, col = "green", lwd = 2)
}




