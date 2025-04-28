rm(list = ls())
if (length(dev.list())) {
  dev.off()
}

library("plot3D")

set.seed(203)

# K é o raio da gaussiana (K = hI)
# x e o ponto atual - ponto de teste
# m: centro das amostras - vai ser os pontos das classes mesmo
# n: dimensao de x
# vetores sao todos colunas, cuidado
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
  
  return (partial_sum^(1 / p))
}

myknn <- function(X, Y, xt, k, h) {
  N <- dim(X)[1] # numero de pontos (numero de linhas de X)
  n <- dim(X)[2] # numero de variaveis (dimensao do esp de entrada) (n de colunas de X)
  K <- h * diag(n) # h * matriz identidade
  
  # matriz que salva a distancia de xt ate cada amostra
  # coluna 1 a n: amostra
  # coluna 2: label
  # coluna 3: distancia
  dist_mat_xt <- matrix(NA, nrow = N, ncol = n + 2)
  
  for (i in 1:N) {
    # calcula a distancia do xt ate o ponto atual da classe que estou iterando
    current_point <- matrix(X[i, ], ncol = n)
    
    # ambos pontos devem estar na forma de matriz - cuidado
    dist_to_point <- minkowski_distance(current_point, xt, p = 2)
    label_of_point <- Y[i]
    
    # salva tudo na matriz dist_mat_xt
    dist_mat_xt[i, 1:n] <- current_point
    dist_mat_xt[i, n+1] <- label_of_point
    dist_mat_xt[i, n+2] <- dist_to_point
  }
  
  # agora é fazer o sort da matriz com base na distancia, que esta em n+2
  ordered_dist_mat_xt <- dist_mat_xt[order(dist_mat_xt[, n+2]), ]
  
  sumk <- 0
  
  for (i in 1:k) {
    current_point <- ordered_dist_mat_xt[i, 1:n]
    current_point_label <- ordered_dist_mat_xt[i, n+1]
    
    sumk <- sumk + current_point_label * pdfnvar(xt, current_point, K, n)
  }
  return (sign(sumk))
}

calculate_q1q2 <- function(X, Y, h) {
  N <- dim(X)[1] # numero de pontos (numero de linhas de X)
  n <- dim(X)[2] # numero de variaveis (dimensao do esp de entrada) (n de colunas de X)
  K <- h * diag(n) # h * matriz identidade
  
  q1q2_mat <- matrix(NA, nrow = N, ncol = 2)
  
  for (i in 1:N) {
    xt <- X[i,]
    xt_label <- Y[i]
    q1_sum <- 0 # todos que sao da mesma classe que ela
    q2_sum <- 0 # # todos que sao de classe diferente que ela
    
    for (j in 1:N) {
      current_point <- X[j,]
      current_point_label <- Y[j]
      
      if (current_point_label == 1) {
        q1_sum <- q1_sum + pdfnvar(xt, current_point, K, n)
      } else {
        q2_sum <- q2_sum + pdfnvar(xt, current_point, K, n)
      }
    }
    
    q1q2_mat[i, 1] <- q1_sum
    q1q2_mat[i, 2] <- q2_sum
  }
  
  return (q1q2_mat)
}

# aberturas das gaussianas: quanto maior s, mais ela espalha
s1 <- 0.7
s2 <- 0.7

nc <- 100 # numero de pontos de cada classe

k_list <- c(3, 9, 17)
h_list <- c(0.01, 0.05, 0.2, 0.5)

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

par(mfrow=c(2,2), cex.lab=2, cex.axis=2, cex.main=2, mai = c(0.4, 0.4, 0.4, 0.4))

for (h in h_list) {
  q1q2_mat <- calculate_q1q2(X, Y, h)
  
  plot(
    NULL,
    main = paste("Q1 x Q2 - ", "h = ", h),
    xlab = "Q1",
    ylab = "Q2",
    ylim = c(0, max(q1q2_mat)),
    xlim = c(0, max(q1q2_mat)),
    cex.main = 2,
    cex.axis = 2,
    cex.lab = 2
  )
  
  points(q1q2_mat[1:nc,], col = "red", lwd = 3)
  points(q1q2_mat[(nc+1):(2*nc),], col = "blue", lwd = 3)
  lines(seq(0, 100, 0.1), seq(0, 100, 0.1), col = "blue", lwd = 2)
}

par(mfrow=c(2,2), cex.lab=2, cex.axis=2, cex.main=2, mai = c(0.4, 0.4, 0.4, 0.4))

for (k in k_list) {
  for (h in h_list) {
    plot(
      NULL,
      main = paste("KNN: k = ", k, "h = ", h),
      xlab = "x1",
      ylab = "x2",
      ylim = c(0, 6),
      xlim = c(0, 6),
      cex.main = 2,
      cex.axis = 2,
      cex.lab = 2
    )
    
    points(xc1, col = "red", lwd = 2)
    points(xc2, col = "blue", lwd = 2)
    
    for (i in 1:length(x1grid)) {
      for (j in 1:length(x2grid)) {
        current_point_grid <- matrix(c(x1grid[i], x2grid[j]), ncol = 2)
        
        # calcula o knn
        grid_matrix[i, j] = myknn(X, Y, current_point_grid, k, h)
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
}
