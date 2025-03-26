# nomenclatura: época é um loop do while()
# realimentação e aprendizado de w ocorre em: wt <- wt + dw, onde dw <- c(eta) * c(ei) * xin[irand, ]
# mdoelo teorico para realimentação: w(t+1) = w(t) + eta * ei * xi

trainadaline <- function(
    xin, # matriz X com todas as entradas da rede
    yd, # matriz Y com todas as saidas desejadas para cada observação i na matriz X
    eta, # passo de ajuste
    tol, # tolerancia de treinamento (condição para parar o treinamento, que ja esta bom)
    maxepocas, # maximo de loops permitidos no treinamento
    par # flag true ou false para acrescentar uma coluna adicional só com 1 à matriz X
) {
    # extrai informações das entradas
    dimxin <- dim(xin)
    N <- dimxin[1]
    n <- dimxin[2]

    if (par == 1) {
        wt <- as.matrix(runif(n + 1) - 0.5)
        xin <- cbind(1, xin)
    } else {
        wt <- as.matrix(runif(n) - 0.5)
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
            yhati <- xin[irand, ] %*% wt
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
    # retorna pametros aprendidos e os erros de cada loop durante o processo
    # os parametros retornados são coeficientes de uma equação de um plano
    # w[n] * x[n] + w[n-1] * x[n-1] + ... + w[1] = 0 -> atenção!
}
