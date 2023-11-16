## ----
## Estudo Inicial da distribuição
## Beta sob reparametrização
## ----

## Definida sobre a média (mu) e a dispersão (phi)
#a = (1-mu)*phi
#b = mu*phi

## Escrevendo as densidades do modelo
## Igual os autores

mus = c(0.20,0.25,0.50,0.75,0.90)
phis = c(5,100)

dbetarep = function(x,mu,phi){
  den = dbeta(x=x,shape1=(1-mu)*phi,shape2=mu*phi)
  return(den)
}

suporte = seq(0,1,length=1000)

## Variando o mu e phi igual a 5.

plot(suporte,dbetarep(x=suporte,mu=mus[1],phi=phis[1]), 
     type='l', xlab = "x", ylab = "Densidade", lwd=2,
     col='purple', ylim = c(0,10))

lines(suporte,dbetarep(x=suporte,mu=mus[2],phi=phis[1]), 
     type='l', lwd=2,col='red')

lines(suporte,dbetarep(x=suporte,mu=mus[3],phi=phis[1]), 
      type='l', lwd=2,col='green3')

lines(suporte,dbetarep(x=suporte,mu=mus[4],phi=phis[1]), 
      type='l', lwd=2,col='steelblue')

lines(suporte,dbetarep(x=suporte,mu=mus[5],phi=phis[1]), 
      type='l', lwd=2,col='deeppink3')

## Variando o mu e phi igual a 100.


plot(suporte,dbetarep(x=suporte,mu=mus[1],phi=phis[2]), 
     type='l', xlab = "x", ylab = "Densidade", lwd=2,
     col='purple', ylim = c(0,10))

lines(suporte,dbetarep(x=suporte,mu=mus[2],phi=phis[2]), 
      type='l', lwd=2,col='red')

lines(suporte,dbetarep(x=suporte,mu=mus[3],phi=phis[2]), 
      type='l', lwd=2,col='green3')

lines(suporte,dbetarep(x=suporte,mu=mus[4],phi=phis[2]), 
      type='l', lwd=2,col='steelblue')

lines(suporte,dbetarep(x=suporte,mu=mus[5],phi=phis[2]), 
      type='l', lwd=2,col='deeppink3')


### ----
### Algoritmo gerador de dados
### na regressão beta.
### ----

set.seed(1234)
eta = c(1, -0.2, 0.4)

n = 200
x = rnorm(n, 2, 2)
z = rnorm(n, 2, 2)

mu = binomial(link = logit)$linkinv(eta[1] + eta[2]*x + eta[3]*z)
phi = 100 ## Disperso, distribuição centralizada

y <- rbeta(n, mu * phi, (1 - mu) * phi)
dat <- data.frame(cbind(y, x, z))

hist(dat$y)

### ----
### Estimação do algoritmo iterativo para
### a estimação de Máxima Verossimilhança dos
### betas na Regressão Beta
### ----

library(betareg)

mbeta = betareg(y~x+z,data=dat)

summary(mbeta) 






