rm(list=ls(all=TRUE))

library(ggplot2)
library(dplyr)
library(readxl)
library(RColorBrewer)
library(moments)
library(xtable)


source('~/EOLLN.05.R')
source('~/EOLLN.25.R')
source('~/EOLLN.50.R')
source('~/EOLLN.75.R')
source('~/EOLLN.95.R')
source('~/EOLLN.99.R')
source('~/EOLLN.01.R')


##Dataset

data3 <- read_excel("data3.xlsx")
y <- data3$y
x1 <- data3$x1

## Descriptive analysis

means <- aggregate(y~x1, FUN=mean)
desv <- aggregate(y~x1, FUN=sd)
min_ <- aggregate(y~x1, FUN=min)
max_ <- aggregate(y~x1, FUN=max)
assim <- aggregate(y~x1, FUN=skewness)
median_ <- aggregate(y~x1, FUN=median)
kurtosi <-  aggregate(y~x1, FUN=kurtosis)
cv <- 100*desv[,2]/means[,2]
descriptive <- cbind(Trat=means[,1],Mean=means[,2],Median=median_[,2] ,s.d.=desv[,2], Min.=min_[,2],Max=max_[,2],Skewness=assim[,2], VC=cv,Kurtosis=kurtosi[,2])

descriptive

## Marginal analysis tau=0.50

fit1 <- gamlss(y~1,family = EOLLN.50,n.cyc=5000,c.crit=0.01,nu.fix = T, nu.start =1,tau.fix = T, tau.start =1)
fit3olln <- gamlss(y~1,family = EOLLN.50,n.cyc=5000,c.crit=0.01,tau.fix = T, tau.start =1)
fit2en <- gamlss(y~1,family = EOLLN.50,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1)
fit4eolln <- gamlss(y~1,family = EOLLN.50,n.cyc=5000,c.crit=0.01)

fit1 <- fit1
fit3 <- fit3olln
fit2 <- fit2en
fit4 <- fit4eolln

summary(fit1,type = 'qr')
summary(fit2,type = 'qr')
summary(fit3,type = 'qr')
summary(fit4,type = 'qr')


#AIC

aic1 <- c(AIC(fit4),'',AIC(fit3),'',AIC(fit2),'',AIC(fit1),'')
bic1 <- c(BIC(fit4),'',BIC(fit3),'',BIC(fit2),'',BIC(fit1),'')
gd1 <- c(fit4$G.deviance,'',fit3$G.deviance,'',fit2$G.deviance,'',fit1$G.deviance,'')


tab1 <-  data.frame(aic1,bic1,gd1)
tab1


##Lr testes

LR.test(fit3,fit4)   
LR.test(fit2,fit4) 
LR.test(fit1,fit4) 


##Regression with covariates in mu

fitr1 <- gamlss(y~x1,data=data3,family = EOLLN.50,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1,tau.fix = T, tau.start =1)
fitr2.en <- gamlss(y~x1,data=data3,family = EOLLN.50,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1)
fitr3.olln <- gamlss(y~x1,data=data3,family = EOLLN.50,n.cyc=5000,c.crit=0.01,tau.fix = T, tau.start =1)
fitr4.eolln <- gamlss(y~x1,data=data3,family = EOLLN.50,n.cyc=5000,c.crit=0.01)

mu1.25 <- gamlss(y~x1,data=data3,family = EOLLN.25,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1,tau.fix = T, tau.start =1)
mu2.en.25 <- gamlss(y~x1,data=data3,family = EOLLN.25,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1)
mu3.olln.25 <- gamlss(y~x1,data=data3,family = EOLLN.25,n.cyc=5000,c.crit=0.01,tau.fix = T, tau.start =1)
mu4.eolln.25 <- gamlss(y~x1,data=data3,family = EOLLN.25,n.cyc=5000,c.crit=0.01)

mu1.75 <- gamlss(y~x1,data=data3,family = EOLLN.75,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1,tau.fix = T, tau.start =1)
mu2.en.75 <- gamlss(y~x1,data=data3,family = EOLLN.75,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1)
mu3.olln.75 <- gamlss(y~x1,data=data3,family = EOLLN.75,n.cyc=5000,c.crit=0.01,tau.fix = T, tau.start =1)
mu4.eolln.75 <- gamlss(y~x1,data=data3,family = EOLLN.75,n.cyc=5000,c.crit=0.01)

mu1.95 <- gamlss(y~x1,data=data3,family = EOLLN.95,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1,tau.fix = T, tau.start =1)
mu2.en.95 <- gamlss(y~x1,data=data3,family = EOLLN.95,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1)
mu3.olln.95 <- gamlss(y~x1,data=data3,family = EOLLN.95,n.cyc=5000,c.crit=0.01,tau.fix = T, tau.start =1)
mu4.eolln.95 <- gamlss(y~x1,data=data3,family = EOLLN.95,n.cyc=5000,c.crit=0.01)

mu1.05 <- gamlss(y~x1,data=data3,family = EOLLN.05,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1,tau.fix = T, tau.start =1)
mu2.en.05 <- gamlss(y~x1,data=data3,family = EOLLN.05,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1)
mu3.olln.05 <- gamlss(y~x1,data=data3,family = EOLLN.05,n.cyc=5000,c.crit=0.01,tau.fix = T, tau.start =1)
mu4.eolln.05 <- gamlss(y~x1,data=data3,family = EOLLN.05,n.cyc=5000,c.crit=0.01)


##Regression with covariates in mu and sigma

fitr1.2 <- gamlss(y~x1,sigma.formula = ~x1, data=data3,family = EOLLN.50,n.cyc=5000,c.crit=0.01,tau.fix = T, tau.start =1,nu.fix = T, nu.start =1)
fitr2.en2 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.50,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1)
fitr3.olln2 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.50,n.cyc=5000,c.crit=0.01, tau.fix = T, tau.start =1)
fitr4.eolln2 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.50,n.cyc=5000,c.crit=0.01)

s1.25 <- gamlss(y~x1,sigma.formula = ~x1, data=data3,family = EOLLN.25,n.cyc=5000,c.crit=0.01,tau.fix = T, tau.start =1,nu.fix = T, nu.start =1)
s2.25 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.25,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1)
s3.25 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.25,n.cyc=5000,c.crit=0.01, tau.fix = T, tau.start =1)
s4.25 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.25,n.cyc=5000,c.crit=0.01)

s1.75 <- gamlss(y~x1,sigma.formula = ~x1, data=data3,family = EOLLN.75,n.cyc=5000,c.crit=0.01,tau.fix = T, tau.start =1,nu.fix = T, nu.start =1)
s2.75 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.75,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1)
s3.75 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.75,n.cyc=5000,c.crit=0.01, tau.fix = T, tau.start =1)
s4.75 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.75,n.cyc=5000,c.crit=0.01)

s1.95 <- gamlss(y~x1,sigma.formula = ~x1, data=data3,family = EOLLN.95,n.cyc=5000,c.crit=0.01,tau.fix = T, tau.start =1,nu.fix = T, nu.start =1)
s2.95 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.95,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1)
s3.95 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.95,n.cyc=5000,c.crit=0.01, tau.fix = T, tau.start =1)
s4.95 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.95,n.cyc=5000,c.crit=0.01)

s1.05 <- gamlss(y~x1,sigma.formula = ~x1, data=data3,family = EOLLN.05,n.cyc=5000,c.crit=0.01,tau.fix = T, tau.start =1,nu.fix = T, nu.start =1)
s2.05 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.05,n.cyc=5000,c.crit=0.01, nu.fix = T, nu.start =1)
s3.05 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.05,n.cyc=5000,c.crit=0.01, tau.fix = T, tau.start =1)
s4.05 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.05,n.cyc=5000,c.crit=0.01)

s4.01 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.01,n.cyc=5000,c.crit=0.01)
s4.99 <- gamlss(y~x1,sigma.formula = ~x1,data=data3,family = EOLLN.99,n.cyc=5000,c.crit=0.01)



## AIC
aic_1 <- rbind(c(AIC(mu4.eolln.05),AIC(mu4.eolln.25),AIC(fitr4.eolln),AIC(mu4.eolln.75),AIC(mu4.eolln.95)),
               c(AIC(mu3.olln.05),AIC(mu3.olln.25),AIC(fitr3.olln),AIC(mu3.olln.75),AIC(mu3.olln.95)),
               c(AIC(mu2.en.05),AIC(mu2.en.25),AIC(fitr2.en),AIC(mu2.en.75),AIC(mu2.en.95)),
               c(AIC(mu1.05),AIC(mu1.25),AIC(fitr1),AIC(mu1.75),AIC(mu1.95)))

aic_2 <- rbind(c(AIC(s4.05),AIC(s4.25),AIC(fitr4.eolln2),AIC(s4.75),AIC(s4.95)),
               c(AIC(s3.05),AIC(s3.25),AIC(fitr3.olln2),AIC(s3.75),AIC(s3.95)),
               c(AIC(s2.05),AIC(s2.25),AIC(fitr2.en2),AIC(s2.75),AIC(s2.95)),
               c(AIC(s1.05),AIC(s1.25),AIC(fitr1.2),AIC(s1.75),AIC(s1.95)))

rownames(aic_1) <- c('EOLLN','OLLN','Exp-N','Normal')
rownames(aic_2) <- c('EOLLN','OLLN','Exp-N','Normal')
colnames(aic_1) <- c('0.05','0.25','0.50','0.75','0.95')
colnames(aic_2) <- c('0.05','0.25','0.50','0.75','0.95')
table1 <- rbind(aic_1,aic_2)
table1

## Selected model
summary(s4.05, type='qr')
summary(s4.25, type='qr')
summary(fitr4.eolln2, type='qr')
summary(s4.75, type='qr')
summary(s4.95, type='qr')


##Residuals
qqnorm(fitr4.eolln2$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s4.05$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s4.25$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s4.75$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s4.95$residuals, pch=20)
abline(0,1,col='red', lwd=2)

qqnorm(fitr3.olln2$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s3.05$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s3.25$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s3.75$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s3.95$residuals, pch=20)
abline(0,1,col='red', lwd=2)

qqnorm(fitr2.en2$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s2.05$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s2.25$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s2.75$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s2.95$residuals, pch=20)
abline(0,1,col='red', lwd=2)

qqnorm(fitr1.2$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s1.05$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s1.25$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s1.75$residuals, pch=20)
abline(0,1,col='red', lwd=2)
qqnorm(s1.95$residuals, pch=20)
abline(0,1,col='red', lwd=2)


