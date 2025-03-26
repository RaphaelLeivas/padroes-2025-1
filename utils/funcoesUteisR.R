gera_gaussianas_2classes_2D <- function(N,M,SD){
  # N = número de amostras
  # M = vetor com o ponto médio da distribuição. Ex: c(2,2) Para centrar no ponto 2,2
  # SD = dispersão das amostras (a mesma para as duas dimensões)
  
  xc1<-matrix(rnorm(N),ncol=2)*SD + (matrix(M,ncol=2,nrow=N/2, byrow = TRUE))
  xc2<-matrix(rnorm(N),ncol=2)*SD + (matrix(M,ncol=2,nrow=N/2, byrow = TRUE))
  
  y1 = array(1,c(N,1))
  y2 = y1*(-1)
  
  X = rbind(xc1,xc2)
  Y = rbind(y1,y2)
  
  retlist<-list(X,Y)
  
  return(retlist) 
}

sech2<-function(u)
{
  return(((2/(exp(u)+exp(-u)))*(2/(exp(u)+exp(-u)))))  
}

getRandomNumber <- function(min = 0, max = 1) {
  return (runif(1, min = min, max = max)[1]) - 0.5
}

###########################################
#plotando as superfícies de contorno
# plot(xc1[,1],xc1[,2],col="red",xlim=c(0,6),ylim=c(0,6))
# par(new=T)
# contour2D(M1,seqi,seqj,levels=0,xlim=c(0,6),ylim=c(0,6))


#############################################

#gerando um grid
# seqi<-seq(0.06,6,0.06)
# seqj<-seq(0.06,6,0.06)
# M1 <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 
# ci<-0
# for (i in seqi){
#   ci<-ci+1
#   cj<-0
#   for(j in seqj)
#   {
#     cj<-cj+1
    
#     M1[ci,cj]<- ALGUM_CALCULO_COM(matrix(c(i,j),nrow=2))
    
#   }
# }