#Exemplo em R - Base de dados Iris





rm(list=ls())



data(iris)

X<-as.matrix(iris[,(1:4)])







meanx<-colMeans(X)

Xs<- X - t(replicate(dim(X)[1],meanx))

S<-cov(Xs)

eigS<-eigen(S)

plot(c(1,2,3,4),eigS$values,type='b',xlab='Eixo PCA',ylab='Autovalor')


par(col.lab='blue')

plot(eigS$values,type='b',xlab='Eixo PCA',ylab='Autovalor',col='red')



projX<-Xs %*% eigS$vectors

plot(projX[,1],projX[,2],type='p',xlim=c(-4,4),ylim=c(-2,2),xlab='PCA1',ylab='PCA2')

par(new=TRUE)

plot(projX[(1:50),1],projX[(1:50),2],type='p',xlim=c(-4,4),ylim=c(-2,2),col='red',xlab='PCA1',ylab='PCA2')

par(new=TRUE)

plot(projX[(51:100),1],projX[(51:100),2],type='p',xlim=c(-4,4),ylim=c(-2,2),col='blue',xlab='PCA1',ylab='PCA2')

# Reduzindo para os dois primeiros eixos

par(col.lab='blue')


#melhores eixos originais

dataPCA <- data.frame(projX)

dataPCA <- cbind(dataPCA,iris$Species)

plot(dataPCA)

plot(iris)

plot(Xs[,1],Xs[,3],type='p',xlim=c(min(Xs[,1]),max(Xs[,1])),ylim=c(min(Xs[,3]),max(Xs[,3])),xlab='feature 1',ylab='feature 2')

par(new=TRUE)

plot(Xs[(1:50),1],Xs[(1:50),3],type='p',xlim=c(min(Xs[,1]),max(Xs[,1])),ylim=c(min(Xs[,3]),max(Xs[,3])),col='red',xlab='',ylab='')

par(new=TRUE)

plot(Xs[(51:100),1],Xs[(51:100),3],type='p',xlim=c(min(Xs[,1]),max(Xs[,1])),ylim=c(min(Xs[,3]),max(Xs[,3])),col='blue',xlab='',ylab='')


#Melhores eixos com PCA

plot(projX[,1],projX[,2],type='p',xlim=c(-4,4),ylim=c(-2,2),xlab='PCA1',ylab='PCA2')

par(new=TRUE)

plot(projX[(1:50),1],projX[(1:50),2],type='p',xlim=c(-4,4),ylim=c(-2,2),col='red',xlab='PCA1',ylab='PCA2')

par(new=TRUE)

plot(projX[(51:100),1],projX[(51:100),2],type='p',xlim=c(-4,4),ylim=c(-2,2),col='blue',xlab='PCA1',ylab='PCA2')

text(-3,-1.6,'Classe 1',col = 'red')

text(0.5,-0.8,'Classe 2',col = 'blue')

text(3.5,0.2,'Classe 3',col = 'black')