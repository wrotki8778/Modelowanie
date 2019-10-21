dt<-function(x){pgamma(x,4)}
f<-function(x){dgamma(x,4)}
skala=4
x = rgamma(100,skala)
x = round(x,1)
#### Wykres czêstoœci (dla rozk³adu dyskretnego) ####
y=runif(2000)
y=floor(10*y)-3
par(mfrow=c(1,2))
plot(table(y)/length(y),ylab="Czêstoœæ", 
     main='Wykres czêstoœci')
lines(seq(-3,6,by=1),rep(0.1,10),type='p', col='red')
plot(table(y),ylab="Licznoœæ", 
     main='Wykres licznoœci')
#### Rysowanie histogramu####
T=seq(0, max(x)+1,by=1)
T_2=seq(0,max(x)+1,by=0.07)
sup=1.5*max(dgamma(T_2,skala))
hist(x,main="Histogram",ylab=expression(paste(f[N],'(x)')), 
     breaks=T,prob=TRUE, col='gray70', ylim=c(0,sup))
lines(T_2,dgamma(T_2,skala), col='red')
#### i dystrybuanty empirycznej/teoretycznej####
t=seq(0, 1.2*max(x),by=0.1)
plot(ecdf(x),main="Dystrybuanta empiryczna", 
     ylab=expression(paste(F[N],'(x)')),
     xlim = c(0,max(x)), xaxt='n',cex=1)
axis(1,knots(ecdf(x)),cex.axis=0.8)
abline(h = (0:10)/10, lty = 2, col = "gray70")
abline(h = 0, v = 0)
axis(2,(0:5)/5)
lines(t,dt(t),col='red')
#### Dystrybuanta a dystrybuanta odwrotna####
skala_1=4
skala_2=3
dt_beta<-function(x){pbeta(x,skala_1,skala_2)}
dodw_beta<-function(x){qbeta(x,skala_1,skala_2)}
t=seq(0,1,by=0.01)
par(mfrow=c(1,1))
plot(t,dodw_beta(t),main="Dystrybuanta odwrotna", 
     ylab=expression(paste(F^{-1},'(x)')),
    col='blue', type='l')
abline(h = 0, v = 0)
lines(t,t,col='green')
lines(t,dt_beta(t),col='red')
legend('bottomright',c(expression(paste(F^{-1},(x))),expression(F(x)))
      ,lwd=2,col=c('blue','red'))
#### Generowanie rozk³adów dyskretnych ####
len=10000
x_dys=runif(len)
w_dys=rep(0,len)
pr=c(0)
tmp=0
n=100
p=0.3
k=0
while(tmp<=max(x_dys)){
  tmp=tmp+choose(n,k)*p^k*(1-p)^(n-k)
  pr=c(pr,tmp)
  k=k+1
}
for(i in 1:len){
  w_dys[i]=length(pr[pr<x_dys[i]])-1
}
hist(w_dys,prob=TRUE)
#### Generowanie próby losowej za pomoc¹ dystrybuanty odwrotnej/q... ####
mu=1
sigma=2
jedn=runif(50)
z=qnorm(jedn,mu,sigma)
T=seq(min(z)-1, max(z)+1,by=1)
T_2=seq(min(z)-1, max(z)+1,by=0.07)
hist(z,main="Histogram",ylab=expression(paste(f[N],'(x)')), 
     breaks=T,prob=TRUE, col='gray70')
lines(T_2,dnorm(T_2,mu,sigma), col='red')
t=seq(min(z)-1, max(z)+1,by=0.1)
plot(ecdf(z),main="Dystrybuanta empiryczna", 
     ylab=expression(paste(F[N],'(x)')),
     cex=1)
abline(h = (0:10)/10, lty = 2, col = "gray70")
abline(h = 0, v = mu)
axis(2,(0:5)/5)
lines(t,pnorm(t,mu,sigma),col='red')
#### i test statystyczny na zgodnoœæ rozk³adu ####
z=rnorm(10,0.5,0.1)
shapiro.test(z)
ks.test(z,'pnorm',0.6,0.15)
T=seq(0,1,by=0.03)
plot(ecdf(z),xlim=c(0,1))
lines(T,pnorm(T,0.6,0.15),type='l')
