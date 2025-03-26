rm(list=ls())
dev.off()

library(ISLR)

Auto = ISLR::Auto

x <- c(3, 4, 5)
y <- c(4,5,6)
z <- x + y

mat <- matrix(rbind(x, y), nrow = 2, ncol = 3, byrow = TRUE)

seq(from = 0, to = 1, by = 0.1)

# estudo da função contour, que retorna as curvas de nivel
# de z f(x, y) no plano xy
# seguindo https://r-charts.com/correlation/contour-plot/ 

N <- 10

xgrid <- -N:(N - 1) # grid primeira dimensao
ygrid <- -N:(N - 1) # grid segunda dimensao
z <- matrix(nrow = 2 * N, ncol = 2 * N)

# preenche matrix z para cada ponto do grid
for (i in 1:(2 * N)) {
  for (j in 1:(2 * N)) {
    z[i, j] = sqrt(xgrid[i]^2 + ygrid[j]^2)
  }
}


filled.contour(xgrid, ygrid, z, nlevels = 10, lwd = 2, lty = 1,
        col = hcl.colors(10, "Spectral"), plot.axes = {
          axis(1)
          axis(2)
          contour(xgrid, ygrid, z, nlevels = 10, add = TRUE)
        })

contour(xgrid, ygrid, z, nlevels = 10, lwd = 2, lty = 1, labcex = 0.75,
        drawlabels = TRUE, col = hcl.colors(10, "Spectral"))
