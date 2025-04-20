# inlist: lista de amostras de cada cluster. exemplo: se temos 4 clsuters cada um com 100 amostras, é uma
# lsita de 4 items, cada item uma matriz de 100 x n (n é a dimensão, 100 é o numero de amostras do cluster)
# x: ponto atual que quero saber
# retorna: probabilidade do ponto x pertencer à mistura de gaussianas dada
mymix<-function(x,inlist)

{

  pdfnvar<-function(x,m,K,n) ((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m) %*% (solve(K)) %*% (x-m))))





  ng<-length(inlist)

  klist<-list()

  mlist<-list()

  pglist<-list()

  nglist<-list()

  n<-dim(inlist[[1]])[2]



  for (i in (1:ng))

  {

    klist[[i]]<-cov(inlist[[i]])

    mlist[[i]]<-colMeans(inlist[[i]])

    nglist[[i]]<-dim(inlist[[i]])[1]

  }



  N<-0

  for (i in (1:ng))

  {

    N<-N+nglist[[i]]

  }



  for (i in (1:ng))

  {

    pglist[[i]]<-nglist[[i]]/N

  }



  Px<-0

  for (i in (1:ng))

  {

    Px<-Px+pglist[[i]]*pdfnvar(x,mlist[[i]],klist[[i]],n)

  }



  return(Px)

}