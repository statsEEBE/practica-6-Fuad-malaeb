
x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)
#apartado a caso 1 confianza al 90%
n<- length(x)
mu= 16
sigma <- sqrt(25)
xbar <- mean(x)
xbar
z005<- qnorm(0.95)
c(xbar-z005*sigma/sqrt(n), xbar+z005*sigma/sqrt(16))

#install.packages("BSDA")
library(BSDA)
z.test(x,sigma.x=sigma, conf.level=0.9)


mu0= 500
zc<- qnorm(0.9)
zobs<- (xbar-mu0)/(sigma/sqrt(n))
zobs
#como zobs es superior a la zc en la cola superior, rechazo la hipotesis nula
pvalor =1-pnorm(zobs)
pvalor

z.test(x,sigma.x=sigma, conf.level=0.9, alternative="greater",mu=mu0)


#apartado b
n<-(qnorm(0.975)*sigma)^2
n
#ya qeu la formula del error es qnorm(0.975)*sigma/sqrt(n)=1

#apartado c
#t.test se usa cuando no conocemos la varianza/sigma, y z.test se usa cuando lo conocemos
t.test(x,conf.level=0.99)
nn<-16
c(xbar-qt(0.995,nn-1)*sd(x)/sqrt(nn),xbar+qt(0.995,nn-1)*sd(x)/sqrt(nn))

##testeemos:
#H0: mu<=500
#H1:mu>500 (cola superior)

t.test(x,conf.level = 0.99, alternative="greater",mu= 500)
#como alfa es mayor que pvalor, aceptamos la hipotesis de H0
tobs <-(xbar-mu0)/(sd(x)/sqrt(nn))
tobs

tc = qt(0.99,nn-1)
tc 

#apartado d

install.packages("EnvStats")

 library(EnvStats)
varTest(x,conf.level=0.95 )
c(sqrt(20.99068),sqrt(92.14106))

c((nn-1)*var(x)/qchisq(0.025,nn-1)(nn-1)*var(x)/qchisq(0.025,nn-1))
#me falta poner en el qchisq(0.025,nn-1)

#apartado e
varTest(x,conf.level=0.95,alternative="two.sided",sigma.squared=25)

#como el pvalor es mucho mas grande que alfa, aceptamos la hipotesis H0

chiobs<-(nn-1)*var(x)/25
chiobs
