rm(list = ls())
if (length(dev.list())) {
  dev.off()
}

library("plot3D")
source("C:\\dev\\padroes-2025-1\\utils\\trainperceptron.R")

set.seed(203)

# aberturas das gaussianas: quanto maior s, mais ela espalha
s1 <- 0.6
s2 <- 0.6

nc <- 100 # numero de pontos de cada classe
k_list <- c(10)

# par(mfrow=c(2,2), cex.lab=2, cex.axis=2, cex.main=2, mai = c(0.4, 0.4, 0.4, 0.4))

# gera os dados - fora do loop, para que sejam os mesmo dados para todos os k
# centro dessa gaussiana esta na posicao (2,2)
xc1 <- matrix(rnorm(nc * 2) , ncol = 2) * s1 + t (matrix(c (2 , 2) , nrow =
                                                           2, ncol = nc))
# centro dessa outra gaussiana esta na posicao (4,4)
xc2 <- matrix(rnorm(nc * 2) , ncol = 2) * s2 + t (matrix(c (4 , 4) , nrow =
                                                           2, ncol = nc))

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

# treina o perceptron
eta <- 0.1
tol <- 0.01
maxepocas <- 1000
retlist <- trainperceptron(X, Y, eta, tol, maxepocas, 1)
w <- retlist[[1]] # w na posicao n+1 é o theta

for (k in k_list) {
  plot(
    NULL,
    main = paste("Perceptron: s = ", s1),
    xlab = "x1",
    ylab = "x2",
    ylim = c(0, 6),
    xlim = c(0, 6)
  )
  
  points(xc1, col = "red", lwd = 2)
  points(xc2, col = "blue", lwd = 2)
  
  for (i in 1:length(x1grid)) {
    for (j in 1:length(x2grid)) {
      current_point_grid <- matrix(c(x1grid[i], x2grid[j]), ncol = 2)
      
      # calcula a saida do perceptron
      grid_matrix[i, j] = yperceptron(current_point_grid, w, 1)
    }
  }
  
  contour2D(
    grid_matrix,
    x1grid,
    x2grid,
    levels = 0,
    xlim = c(0, 6),
    ylim = c(0, 6),
    add = T,
    col = "green",
    lwd = 2
  )
}
