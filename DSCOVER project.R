#---SEOUL---
install.packages("readr")
library(readr)
seoul=read.csv(file("C:/Users/Administrator/Desktop/datafile/seoul.csv"),header=T)
setwd("C:/Users/Administrator/Desktop/datafile")
seoul=read_csv('seoul.csv')
head(seoul)
result=lm(overall~traffic+program+food,seoul)
summary(result)

install.packages('glmnet')
library(glmnet)

x=as.matrix(seoul[,-7])
head(x)
y=seoul[,7]
head(y)
y<-as.numeric(unlist(y))
cv.out=cv.glmnet(x,y,alpha=1,nfolds=10)
opt.lam=cv.out$lambda.min
fit=glmnet(x,y,alpha=1,lambda=opt.lam); fit$beta
yhat=predict(fit,newx=x); mean((y-yhat)^2)


#---Busan---
setwd("C:/Users/Administrator/Desktop/datafile")
busan=read_csv('busan.csv')
head(busan)
result=lm(overall~heritage+traffic+program+food+shopping+congestion,busan)
summary(result)

x=as.matrix(busan[,-7])
head(x)
y=busan[,7]
head(y)
y<-as.numeric(unlist(y))
cv.out=cv.glmnet(x,y,alpha=1,nfolds=10)
opt.lam=cv.out$lambda.min
fit=glmnet(x,y,alpha=1,lambda=opt.lam); fit$beta

#---Daegu---
setwd("C:/Users/Administrator/Desktop/datafile")
daegu=read_csv('daegu.csv')
head(daegu)
result=lm(overall~heritage+traffic+program+food+shopping+congestion,daegu)
summary(result)

x=as.matrix(daegu[,-7])
head(x)
y=daegu[,7]
head(y)
y<-as.numeric(unlist(y))
cv.out=cv.glmnet(x,y,alpha=1,nfolds=10)
opt.lam=cv.out$lambda.min
fit=glmnet(x,y,alpha=1,lambda=opt.lam); fit$beta


#---Incheon---
setwd("C:/Users/Administrator/Desktop/datafile")
incheon=read_csv('incheon.csv')
head(incheon)
result=lm(overall~heritage+traffic+program+food+shopping+congestion,incheon)
summary(result)

x=as.matrix(incheon[,-7])
y=incheon[,7]
y<-as.numeric(unlist(y))
cv.out=cv.glmnet(x,y,alpha=1,nfolds=10)
opt.lam=cv.out$lambda.min
fit=glmnet(x,y,alpha=1,lambda=opt.lam); fit$beta


#---Chungbuk---
setwd("C:/Users/Administrator/Desktop/datafile")
chungbuk=read_csv('chungbuk.csv')
head(chungbuk)
result=lm(overall~heritage+traffic+program+food+shopping+congestion,chungbuk)
summary(result)

x=as.matrix(chungbuk[,-7])
y=chungbuk[,7]
y<-as.numeric(unlist(y))
cv.out=cv.glmnet(x,y,alpha=1,nfolds=10)
opt.lam=cv.out$lambda.min
fit=glmnet(x,y,alpha=1,lambda=opt.lam); fit$beta


#---Chungnam---
setwd("C:/Users/Administrator/Desktop/datafile")
chungnam=read_csv('chungnam.csv')
head(chungnam)
result=lm(overall~heritage+traffic+program+food+shopping+congestion,chungnam)
summary(result)

x=as.matrix(chungnam[,-7])
y=chungnam[,7]
y<-as.numeric(unlist(y))
cv.out=cv.glmnet(x,y,alpha=1,nfolds=10)
opt.lam=cv.out$lambda.min
fit=glmnet(x,y,alpha=1,lambda=opt.lam); fit$beta


#---Daejoen---
setwd("C:/Users/Administrator/Desktop/datafile")
daejeon=read_csv('daejeon.csv')
head(daejeon)
result=lm(overall~heritage+traffic+program+food+shopping+congestion,daejeon)
summary(result)

x=as.matrix(daejeon[,-7])
y=daejeon[,7]
y<-as.numeric(unlist(y))
cv.out=cv.glmnet(x,y,alpha=1,nfolds=10)
opt.lam=cv.out$lambda.min
fit=glmnet(x,y,alpha=1,lambda=opt.lam); fit$beta


#---Gangwon---
setwd("C:/Users/Administrator/Desktop/datafile")
gangwon=read_csv('gangwon.csv')
head(gangwon)
result=lm(overall~heritage+traffic+program+food+shopping+congestion,gangwon)
summary(result)

x=as.matrix(gangwon[,-7])
y=gangwon[,7]
y<-as.numeric(unlist(y))
cv.out=cv.glmnet(x,y,alpha=1,nfolds=10)
opt.lam=cv.out$lambda.min
fit=glmnet(x,y,alpha=1,lambda=opt.lam); fit$beta


#---Gyeonggi---
setwd("C:/Users/Administrator/Desktop/datafile")
gyeonggi=read_csv('gyeonggi.csv')
head(gyeonggi)
result=lm(overall~heritage+traffic+program+food+shopping+congestion,gyeonggi)
summary(result)

x=as.matrix(gyeonggi[,-7])
y=gyeonggi[,7]
y<-as.numeric(unlist(y))
cv.out=cv.glmnet(x,y,alpha=1,nfolds=10)
opt.lam=cv.out$lambda.min
fit=glmnet(x,y,alpha=1,lambda=opt.lam); fit$beta


#---Jeonbuk---
setwd("C:/Users/Administrator/Desktop/datafile")
jeonbuk=read_csv('jeonbuk.csv')
head(jeonbuk)
result=lm(overall~heritage+traffic+program+food+shopping+congestion,jeonbuk)
summary(result)

x=as.matrix(jeonbuk[,-7])
y=jeonbuk[,7]
y<-as.numeric(unlist(y))
cv.out=cv.glmnet(x,y,alpha=1,nfolds=10)
opt.lam=cv.out$lambda.min
fit=glmnet(x,y,alpha=1,lambda=opt.lam); fit$beta


#---Jeonnam---
setwd("C:/Users/Administrator/Desktop/datafile")
jeonnam=read_csv('jeonnam.csv')
head(jeonnam)
result=lm(overall~heritage+traffic+program+food+shopping+congestion,jeonnam)
summary(result)

x=as.matrix(jeonnam[,-7])
y=jeonnam[,7]
y<-as.numeric(unlist(y))
cv.out=cv.glmnet(x,y,alpha=1,nfolds=10)
opt.lam=cv.out$lambda.min
fit=glmnet(x,y,alpha=1,lambda=opt.lam); fit$beta


#---Kyungbuk---
setwd("C:/Users/Administrator/Desktop/datafile")
kyungbuk=read_csv('kyungbuk.csv')
head(kyungbuk)
result=lm(overall~heritage+traffic+program+food+shopping+congestion,kyungbuk)
summary(result)

x=as.matrix(kyungbuk[,-7])
y=kyungbuk[,7]
y<-as.numeric(unlist(y))
cv.out=cv.glmnet(x,y,alpha=1,nfolds=10)
opt.lam=cv.out$lambda.min
fit=glmnet(x,y,alpha=1,lambda=opt.lam); fit$beta

#---Kyungnam---
setwd("C:/Users/Administrator/Desktop/datafile")
kyungnam=read_csv('kyungnam.csv')
head(kyungnam)
result=lm(overall~heritage+traffic+program+food+shopping+congestion,kyungnam)
summary(result)

x=as.matrix(kyungnam[,-7])
y=kyungnam[,7]
y<-as.numeric(unlist(y))
cv.out=cv.glmnet(x,y,alpha=1,nfolds=10)
opt.lam=cv.out$lambda.min
fit=glmnet(x,y,alpha=1,lambda=opt.lam); fit$beta


#---Jeju---
setwd("C:/Users/Administrator/Desktop/datafile")
jeju=read_csv('jeju.csv')
head(jeju)
result=lm(overall~heritage+traffic+program+food+shopping+congestion,jeju)
summary(result)

x=as.matrix(jeju[,-7])
y=jeju[,7]
y<-as.numeric(unlist(y))
cv.out=cv.glmnet(x,y,alpha=1,nfolds=10)
opt.lam=cv.out$lambda.min
fit=glmnet(x,y,alpha=1,lambda=opt.lam); fit$beta




