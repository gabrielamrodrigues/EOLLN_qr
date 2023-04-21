rm(list=ls(all=TRUE))


## Gamlss models
source('~/EOLLN.05.R')
source('~/EOLLN.25.R')
source('~/EOLLN.50.R')
source('~/EOLLN.75.R')
source('~/EOLLN.95.R')


library(readxl)
library(moments)
library(knitr)
library(ggplot2)
library(dplyr)
library(xtable)

## Dataset

data1 <-  read_excel("data1.xlsx")

data1$Treatment[data1$Treatment == 'AIR'] <- '1 AIR'
data1$Treatment[data1$Treatment == 'LW'] <- '2 LW'
data1$Treatment[data1$Treatment == 'HW'] <- '3 HW'

treat <- as.factor(data1$Treatment)
y <- as.numeric(data1$`Residence time (%)`)


## Descriptive analysis

means <- aggregate(y~treat, FUN=mean)
desv <- aggregate(y~treat, FUN=sd)
min_ <- aggregate(y~treat, FUN=min)
max_ <- aggregate(y~treat, FUN=max)
assim <- aggregate(y~treat, FUN=skewness)
median_ <- aggregate(y~treat, FUN=median)
kurtosi <-  aggregate(y~treat, FUN=kurtosis)
cv <- 100*desv[,2]/means[,2]
descriptive  <- cbind(Trat=means[,1],Mean=means[,2],Median=median_[,2] ,s.d.=desv[,2], Min.=min_[,2],Max=max_[,2],Skewness=assim[,2], VC=cv,Kurtosis=kurtosi[,2])
descriptive 

p2 <- ggplot(data1, aes(x=treat, y=y)) +
  geom_boxplot(fill='#A4A4A4', color="black",outlier.shape=8,
               outlier.size=4)+
  stat_summary(fun.y=mean, geom="point", size=2, color="black", fill="black")+
  labs(title="",x="Watering regime", y = "Residence time (%)")
p2


##Marginal analysis

fit1 <- gamlss(y~1,family = EOLLN.50,n.cyc=2000, nu.fix = T, nu.start =1,tau.fix = T,tau.start = 1)
fit3 <- gamlss(y~1,family = EOLLN.50,n.cyc=2000, tau.fix = T, tau.start =1)
fit4 <- gamlss(y~1,family = EOLLN.50,n.cyc=2000)
fit2 <- gamlss(y~1,family = EOLLN.50,n.cyc=2000, c.crit=0.09,nu.fix = T, nu.start =1, tau.start = 1)

##AIC

aic1 <- c(AIC(fit4),'',AIC(fit3),'',AIC(fit2),'',AIC(fit1),'')
bic1 <- c(BIC(fit4),'',BIC(fit3),'',BIC(fit2),'',BIC(fit1),'')
gd1 <- c(fit4$G.deviance,'',fit3$G.deviance,'',fit2$G.deviance,'',fit1$G.deviance,'')

tab1 <-  data.frame(aic1,bic1,gd1);tab1

##Summarys
summary(fit1, type='qr')
summary(fit2, type='qr')
summary(fit3, type='qr')
summary(fit4, type='qr')

##LR test
LR.test(fit3,fit4) 
LR.test(fit2,fit4) 
LR.test(fit1,fit4) 


## Regression constant sigma 

fit1 <- gamlss(y~treat,family = EOLLN.50,n.cyc=2000,c.crit=0.01, nu.fix = T, nu.start =1,tau.fix = T,tau.start = 1)
fit2 <- gamlss(y~treat,family = EOLLN.50,n.cyc=2000,c.crit=0.03, nu.fix = T, nu.start =1)
fit3 <- gamlss(y~treat,family = EOLLN.50,n.cyc=2000,c.crit=0.01, tau.fix = T, tau.start =1)
fit4 <- gamlss(y~treat,family = EOLLN.50,n.cyc=2000,c.crit=0.01)

## Regression Modeling sigma

fit1.2 <- gamlss(y~treat,sigma.formula=~treat,family = EOLLN.50,n.cyc=2000,c.crit=0.01, nu.fix = T, nu.start =1,tau.fix = T,tau.start = 1)
fit2.2 <- gamlss(y~treat,sigma.formula=~treat,family = EOLLN.50,n.cyc=2000,c.crit=0.02, nu.fix = T, nu.start =1)
fit3.2 <- gamlss(y~treat,sigma.formula=~treat,family = EOLLN.50,n.cyc=2000,c.crit=0.02, tau.fix = T, tau.start =1)
fit4.2 <- gamlss(y~treat,sigma.formula=~treat,family = EOLLN.50,n.cyc=2000,c.crit=0.01)


##LR test

LR.test(fit3.2,fit4.2) 
LR.test(fit2.2,fit4.2) 
LR.test(fit1.2,fit4.2) 

##AIC

tab2 <- rbind(c(AIC(fit4),AIC(fit3),AIC(fit2),AIC(fit1)),
              c(BIC(fit4),BIC(fit3),BIC(fit2),BIC(fit1)),
              c(fit4$G.deviance,fit3$G.deviance,fit2$G.deviance,fit1$G.deviance),
              c(AIC(fit4.2),AIC(fit3.2),AIC(fit2.2),AIC(fit1.2)),
              c(BIC(fit4.2),BIC(fit3.2),BIC(fit2.2),BIC(fit1.2)),
              c(fit4.2$G.deviance,fit3.2$G.deviance,fit2.2$G.deviance,fit1.2$G.deviance))
colnames(tab2) <- c('EOLLN','OLLN','Exp-N','Normal')
tab2

##Summarys
summary(fit4.2,type='qr')
summary(fit3.2,type='qr')
summary(fit2.2, type='qr')
summary(fit1.2,type='qr')


##Residuals 
qqnorm(fit4.2$residuals, pch=20,ylim=c(-3,3))
abline(0,1,col='red', lwd=2)
qqnorm(fit3.2$residuals, pch=20,ylim=c(-3,3))
abline(0,1,col='red', lwd=2)
qqnorm(fit2.2$residuals, pch=20,ylim=c(-3,3))
abline(0,1,col='red', lwd=2)
qqnorm(fit1.2$residuals, pch=20,ylim=c(-3,3))
abline(0,1,col='red', lwd=2)

