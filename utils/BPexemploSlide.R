rm(list = ls())
# dev.off()

source("C:\\dev\\rna-2023-2\\utils\\funcoesUteisR.R")

set.seed(209)

# geração dos dados
s1<-0.3
s2<-0.3
nc<-20
xc1<-matrix(rnorm(nc*2),ncol=2)*s1 + t(matrix((c(2,2)),ncol=nc,nrow=2))
xc2<-matrix(rnorm(nc*2),ncol=2)*s2 + t(matrix((c(4,4)),ncol=nc,nrow=2))
plot(xc1[,1],xc1[,2],col = 'red', xlim = c(0,6),ylim = c(0,6),xlab = 'x_1',ylab='x_2')
par(new=T)
plot(xc2[,1],xc2[,2],col = 'blue', xlim = c(0,6),ylim = c(0,6),xlab = '',ylab='')

# dados de entrada e saida esperada
X= rbind(xc1,xc2)
y = rbind(as.matrix(rep(-1, nc), ncol = nc), as.matrix(rep(1, nc), ncol = nc))

# hiperparametros (argumentos) do treinamento
maxepocas<-4000
tol<-0.01
eepoca<-tol+1
nepocas<-1
eta<-0.1

# inicializa todos os pesos
z10 = 1
z12 = 1
z11 = 1
z21 = 1
z22 = 1
z20 = 1
w30 = 1
w31 = 1
w32 = 1

evec<-matrix(nrow=1,ncol=maxepocas) 
while ((nepocas < maxepocas) && (eepoca>tol))
{
  ei2<-0
  N <- dim(x_train)[1]
  xseq<-sample(length(y))       
  for (i in 1:length(y))
  {
    irand<-xseq[i]   
    x1<-X[irand,1]
    x2<-X[irand,2]
    
    h1 = tanh(x1*z11 + x2*z12 + z10)
    h2 = tanh(x1*z21 + x2*z22 + z20)
    
    yhat = tanh(h1*w31 + h2*w32 + w30)
    
    e = y[irand] - yhat
    
    de3 = e * (sech2(h1*w31 + h2*w32 + w30))
    
    dw30 = eta* de3 * 1
    dw31 = eta* de3 * h1
    dw32 = eta* de3 * h2
    
    de1 = de3 * w31 * (sech2(x1*z11 + x2*z12 + z10))
    dz10 = eta* de1 * 1
    dz11 = eta* de1 * x1
    dz12 = eta* de1 * x2
    
    de2 = de3 * w32 * (sech2(x1*z21 + x2*z22 + z20))
    dz20 = eta* de2 * 1
    dz21 = eta* de2 * x1
    dz22 = eta* de2 * x2
    
    w30 = w30 + dw30
    w31 = w31 + dw31
    w32 = w32 + dw32
    z10 = z10 + dz10
    z11 = z11 + dz11
    z12 = z12 + dz12
    z20 = z20 + dz20
    z21 = z21 + dz21
    z22 = z22 + dz22
    
    ei2<-ei2+(e*e)/2         
  }
  
  nepocas<-nepocas+1       
  evec[nepocas]<-ei2 / N
  
  eepoca<-evec[nepocas]    
}  

# concluido o aprendizado, cria mais dados de teste
xc1_t<-matrix(rnorm(nc*2),ncol=2)*s1 + t(matrix((c(2,2)),ncol=nc,nrow=2))
xc2_t<-matrix(rnorm(nc*2),ncol=2)*s2 + t(matrix((c(4,4)),ncol=nc,nrow=2))
x_t= rbind(xc1_t,xc2_t)
y = rbind(as.matrix(rep(-1, nc), ncol = nc), as.matrix(rep(1, nc), ncol = nc))

# calcula a saida da rede para esses dados de teste x_t
xt_length <- dim(x_t)[1]
correct <- 0
yhat_list <- c()

for (i in 1:xt_length) {
  x1<-x_t[i,1]
  x2<-x_t[i,2]
  
  h1 = tanh(x1*z11 + x2*z12 + z10)
  h2 = tanh(x1*z21 + x2*z22 + z20)
  
  yhat = tanh(h1*w31 + h2*w32 + w30)
  yhat_list <- append(yhat_list, yhat)
  
  if (sign(yhat) == sign(y[i])) {
    correct <- correct + 1
  }
}

print(correct / xt_length)




