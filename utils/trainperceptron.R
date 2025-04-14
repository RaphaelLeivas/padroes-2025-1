# nomenclatura: época é um loop do while()

trainperceptron <- function(
    xin,
    yd,
    eta, # passo de aprendizagem 
    tol, # tolerancia de treinamento
    maxepocas, # maximo de loops principais
    par # flag true ou false para acrescentar uma coluna adicional à matriz Z - parametro theta
) {
    # extrai informações das entradas
    dimxin <- dim(xin)
    N <- dimxin[1]
    n <- dimxin[2]

    if (par == 1) {
        wt <- as.matrix(runif(n + 1) - 0.5)
        xin <- cbind(1, xin)
    } else {
        wt <- as.matrix(runif(n) - 0.5) # seta um w aleatorio para começar o treinamento
    }

    nepocas <- 0
    eepoca <- tol + 1
    evec <- matrix(nrow = 1, ncol = maxepocas)

    # so sai do loop se o erro do loop atual for menor que a tolerancia dada
    # ou se ultrapssar o numero maximo de loops
    while ((nepocas < maxepocas) && (eepoca > tol)) {
        ei2 <- 0
        xseq <- sample(N)
        for (i in 1:N) {
            irand <- xseq[i]
            # yhati <- 1.0 * ((xin[irand, ] %*% wt) >= 0)
            yhati <- if ((xin[irand, ] %*% wt) >= 0) 1 else 0 # nossa saida Y é 0 ou 1
            ei <- yd[irand] - yhati
            dw <- c(eta) * c(ei) * xin[irand, ] # original: dw <- eta * ei * xin[irand, ]
            wt <- wt + dw
            ei2 <- ei2 + ei * ei
        }

        nepocas <- nepocas + 1
        evec[nepocas] <- ei2 / N
        eepoca <- evec[nepocas]
    }

    retlist <- list(wt, evec[1:nepocas])
    return(retlist)
    # retorna pametros parendidos e os erros de cada loop durante o processo
}

yperceptron <- function(x, w, par) {
  if (par == 1) {
    x <- cbind(1, x)
  }
  
  u <- x %*% w
  
  if (u >= 0) {
    return (1)
  } else {
    return (0)
  }
}