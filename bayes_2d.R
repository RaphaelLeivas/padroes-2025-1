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

# puxa a pdfnvar

n <- 2 # dimensao
s1 <- 1
s2 <- 1
ro <- 0.8 # coef de correlacao linear

m1 <- matrix(c(3,3), byrow = T, ncol = n)
K1 <- matrix(c(s1^2, ro*s1*s2, 
               ro*s2*s1, s2^2), byrow = T, ncol = n) # matriz covarianca

seqi <- seq(0, 6, 0.1)
seqj <- seq(0, 6, 0.1)
M1 <- matrix(1, nrow =  length(seqi), ncol = length(seqj))

ci <- 0

for (i in seqi) {
  ci <- ci + 1
  cj <- 0
  
  for (j in seqj) {
    cj <- cj +1
    x <- matrix(c(i, j), byrow = T, ncol = 1)
    M1[ci, cj] <- pdfnvar(x, m1, K1, n)
  }
}

contour(seqi, seqj, M1)

# persp3d(seqi, seqj, M1, col="red")

# se tivesse duas classes, seria duas superficies contour
# M1[ci, cj] <- pdfnvar(xt, m1, K1, n)
# M2[ci, cj] <- pdfnvar(xt, m2, K2, n)
# M3[ci, cj] <- 1 * (M2[ci, cj] >= M1 [ci, cj]) # quem plota é o M3





