rm(list=ls(all=TRUE))

library(moments)
library(knitr)
library(ggplot2)
library(dplyr)
library(xtable)
library(gamlss)

source('~/EOLLN.05.R')
source('~/EOLLN.25.R')
source('~/EOLLN.50.R')
source('~/EOLLN.75.R')
source('~/EOLLN.95.R')

data("rent")
head(rent)

y <- rent$R
x1 <- rent$Fl

medias <- mean(y)
desvios <- sd(y)
min <- min(y)
max <- max(y)
assimetria <-skewness(y)
mediana <- median(y)
kurtosi <- kurtosis(y)
cv <- 100*desvios/medias
descritiva <- cbind(Trat=medias,Mean=medias,s.d.=desvios,Median=mediana,Min.=min,Max=max, Skewness=assimetria, VC=cv,Kurtosis=kurtosi)
kable(descritiva, digits = 4)
xtable(descritiva)

##Regression constant sigma 

fit1.50 <- gamlss(y~x1,family = NO,n.cyc=200,c.crit=0.01)
fit2.50 <- gamlss(y~x1,family = EOLLN.50,n.cyc=2000, nu.fix = T, nu.start =1,c.crit=0.01)
fit3.50 <- gamlss(y~x1,family = EOLLN.50,n.cyc=2000, tau.fix = T, tau.start =1,c.crit=0.01)
fit4.50 <- gamlss(y~x1,family = EOLLN.50,n.cyc=2000,c.crit=0.01)


fit1.05 <- gamlss(y~x1,family = EOLLN.05,n.cyc=200,c.crit=0.01,nu.fix = T, nu.start =1,tau.fix = T, tau.start =1)
fit2.05 <- gamlss(y~x1,family = EOLLN.05,n.cyc=2000, nu.fix = T, nu.start =1,c.crit=0.01)
fit3.05 <- gamlss(y~x1,family = EOLLN.05,n.cyc=2000, tau.fix = T, tau.start =1,c.crit=0.01)
fit4.05 <- gamlss(y~x1,family = EOLLN.05,n.cyc=2000,c.crit=0.01)


fit1.75 <- gamlss(y~x1,family = EOLLN.75,n.cyc=200,c.crit=0.01,nu.fix = T, nu.start =1,tau.fix = T, tau.start =1)
fit2.75 <- gamlss(y~x1,family = EOLLN.75,n.cyc=2000, nu.fix = T, nu.start =1,c.crit=0.01)
fit3.75 <- gamlss(y~x1,family = EOLLN.75,n.cyc=2000, tau.fix = T, tau.start =1,c.crit=0.01)
fit4.75 <- gamlss(y~x1,family = EOLLN.75,n.cyc=2000,c.crit=0.01)


fit1.95 <- gamlss(y~x1,family = EOLLN.95,n.cyc=200,c.crit=0.01,nu.fix = T, nu.start =1,tau.fix = T, tau.start =1)
fit2.95 <- gamlss(y~x1,family = EOLLN.95,n.cyc=2000, nu.fix = T, nu.start =1,c.crit=0.01)
fit3.95 <- gamlss(y~x1,family = EOLLN.95,n.cyc=2000, tau.fix = T, tau.start =1,c.crit=0.01)
fit4.95 <- gamlss(y~x1,family = EOLLN.95,n.cyc=2000,c.crit=0.01)


fit1.25 <- gamlss(y~x1,family = EOLLN.25,n.cyc=200,c.crit=0.01,nu.fix = T, nu.start =1,tau.fix = T, tau.start =1)
fit2.25 <- gamlss(y~x1,family = EOLLN.25,n.cyc=2000, nu.fix = T, nu.start =1,c.crit=0.01)
fit3.25 <- gamlss(y~x1,family = EOLLN.25,n.cyc=2000, tau.fix = T, tau.start =1,c.crit=0.01)
fit4.25 <- gamlss(y~x1,family = EOLLN.25,n.cyc=2000,c.crit=0.01)


##AIC
aic_1 <- rbind(c(AIC(fit4.05),AIC(fit4.25),AIC(fit4.50),AIC(fit4.75),AIC(fit4.95)),
               c(AIC(fit3.05),AIC(fit3.25),AIC(fit3.50),AIC(fit3.75),AIC(fit3.95)),
               c(AIC(fit2.05),AIC(fit2.25),AIC(fit2.50),AIC(fit2.75),AIC(fit2.95)),
               c(AIC(fit1.05),AIC(fit1.25),AIC(fit1.50),AIC(fit1.75),AIC(fit1.95)))

aic_1

##Regression Modeling sigma

fit1.50 <- gamlss(y~x1,sigma.formula=~x1,family = NO,n.cyc=200,c.crit=0.001)
fit2.50 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.50,n.cyc=2000, nu.fix = T, nu.start =1)
fit3.50 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.50,n.cyc=2000, tau.fix = T, tau.start =1)
fit4.50 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.50,n.cyc=2000)

fit1.05 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.05,n.cyc=200,c.crit=0.001,nu.fix = T, nu.start =1,tau.fix = T, tau.start =1)
fit2.05 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.05,n.cyc=2000, nu.fix = T, nu.start =1)
fit3.05 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.05,n.cyc=2000, tau.fix = T, tau.start =1)
fit4.05 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.05,n.cyc=2000)

fit1.75 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.75,n.cyc=200,c.crit=0.001,nu.fix = T, nu.start =1,tau.fix = T, tau.start =1)
fit2.75 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.75,n.cyc=2000, nu.fix = T, nu.start =1)
fit3.75 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.75,n.cyc=2000, tau.fix = T, tau.start =1)
fit4.75 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.75,n.cyc=2000)


fit1.95 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.95,n.cyc=200,c.crit=0.001,nu.fix = T, nu.start =1,tau.fix = T, tau.start =1)
fit2.95 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.95,n.cyc=2000, nu.fix = T, nu.start =1)
fit3.95 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.95,n.cyc=2000, tau.fix = T, tau.start =1)
fit4.95 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.95,n.cyc=2000)


fit1.25 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.25,n.cyc=200,c.crit=0.001,nu.fix = T, nu.start =1,tau.fix = T, tau.start =1)
fit2.25 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.25,n.cyc=2000, nu.fix = T, nu.start =1)
fit3.25 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.25,n.cyc=2000, tau.fix = T, tau.start =1)
fit4.25 <- gamlss(y~x1,sigma.formula=~x1,family = EOLLN.25,n.cyc=2000)


##AIC
aic_2 <- rbind(c(AIC(fit4.05),AIC(fit4.25),AIC(fit4.50),AIC(fit4.75),AIC(fit4.95)),
               c(AIC(fit3.05),AIC(fit3.25),AIC(fit3.50),AIC(fit3.75),AIC(fit3.95)),
               c(AIC(fit2.05),AIC(fit2.25),AIC(fit2.50),AIC(fit2.75),AIC(fit2.95)),
               c(AIC(fit1.05),AIC(fit1.25),AIC(fit1.50),AIC(fit1.75),AIC(fit1.95)))
aic_2


##Fts

summary(fit2.05, typr='qr')
summary(fit2.25, typr='qr')
summary(fit2.50, typr='qr')
summary(fit2.75, typr='qr')
summary(fit2.95, typr='qr')

fit2.05$mu.coefficients
fit2.25$mu.coefficients
fit2.50$mu.coefficients
fit2.75$mu.coefficients
fit2.95$mu.coefficients

plot(rent$R~rent$Fl,pch=20)
curve(166.73934+2.57084*x,add=T,lwd=2,col='red')
curve(212.491150+5.449678*x,add=T,lwd=2,col='blue')
curve(253.953151+7.715996*x,add=T,lwd=2,col='green')
curve(314.60868+10.11855*x,add=T,lwd=2,col='orange')
curve(456.87911+13.60944*x,add=T,lwd=2,col='6')


##Residuals 

#EOLLN
qqnorm(fit4.05$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit4.25$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit4.50$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit4.75$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit4.95$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)


#OLLN
qqnorm(fit3.05$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit3.25$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit3.50$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit3.75$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit3.95$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)


#EXP-Normal
qqnorm(fit2.05$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit2.25$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit2.50$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit2.75$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit2.95$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)

#Normal
qqnorm(fit1.05$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit1.25$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit1.50$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit1.75$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)
qqnorm(fit1.95$residuals, pch=20,ylim=c(-4,4), xlim=c(-4,4))
abline(0,1,col='red', lwd=2)


