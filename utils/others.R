rm(list = ls())
if (length(dev.list())) {
  dev.off()
}

A <- matrix(c(
  2, 0, 1, 3,
  0, 4, 1, 2,
  1, 1, 0, 0,
  3, 2, 0, 0
), byrow = T, ncol = 4, nrow = 4)
b <- c(6, 12, 5, 12)

x <- solve(A) %*% b
print(x)