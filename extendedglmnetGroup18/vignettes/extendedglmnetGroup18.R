## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(extendedglmnetGroup18)

## ----warning=FALSE,message=FALSE----------------------------------------------
data("airquality")
dat=airquality[,c(1,2,3,4)]
colSums(is.na(dat)) # The number of NA values for each column
dim(na.omit(dat)) # The number of rows and columns in the data

y=dat$Ozone
X=dat[,-1]

## ----warning=FALSE,message=FALSE,error=TRUE-----------------------------------
set.seed(123)
linear=extended_glm(y,X,model='linear',prob=0.75)
set.seed(123)
ridge=extended_glm(y,X,model='ridge',prob=0.75)
set.seed(123)
lasso=extended_glm(y,X,model='lasso',prob=0.75)
set.seed(123)
random_lasso=extended_glm(y,X,model='random_lasso',prob=0.75,nfold=5)

result=as.vector(c(linear,ridge,lasso,random_lasso))
names(result)=paste0('RMSE','.',c('linear','ridge','lasso','random_lasso'));result

## ----warning=FALSE,message=FALSE----------------------------------------------
data("ChickWeight")
dat=ChickWeight
colSums(is.na(dat)) # No NA values
dim(na.omit(dat)) # The number of rows and columns in the data

y=dat$weight
x1=dat$Time
x2=(dat$Time+rnorm(length(dat$Time)))^2
x3=2*dat$Time+rexp(length(dat$Time))
X=cbind(x1=x1,x2=x2,x3=x3)

## ----warning=FALSE,message=FALSE,error=TRUE-----------------------------------
set.seed(123)
linear=extended_glm(y,X,model='linear',prob=1)
set.seed(123)
ridge=extended_glm(y,X,model='ridge',prob=1)
set.seed(123)
lasso=extended_glm(y,X,model='lasso',prob=1)
set.seed(123)
random_lasso=extended_glm(y,X,model='random_lasso',prob=1,nfold=10)

result=as.vector(c(linear,ridge,lasso,random_lasso))
names(result)=paste0('RMSE','.',c('linear','ridge','lasso','random_lasso'));result

## ----warning=FALSE,message=FALSE----------------------------------------------
Crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=TRUE)
colSums(is.na(Crabs)) # No missing values
y=Crabs$y
X=Crabs[,c(4,5,7)]
X$spine=as.factor(X$spine)

## ----warning=FALSE,message=FALSE,error=TRUE-----------------------------------
set.seed(123)
extended_glm(y,X,model='linear',prob=0.8)
set.seed(123)
extended_glm(y,X,model='ridge',prob=0.8)
set.seed(123)
extended_glm(y,X,model='lasso',prob=0.8)
set.seed(123)
extended_glm(y,X,model='random_lasso',prob=0.8,nfold=3,len=2)

## ----warning=FALSE,message=FALSE----------------------------------------------
y=rnorm(10)
X=data.frame(x1=rexp(10),x2=as.factor(sample(1:2,10,replace=T)))

y_b=sample(c(0,1),10,replace=T)
X_b=data.frame(x1=rexp(10),x2=as.factor(sample(1:2,10,replace=T)))

## ----warning=FALSE,message=FALSE,error=TRUE-----------------------------------
set.seed(1)
extended_glm(y,X,model='linear',prob=0.1)

extended_glm(y_b,X_b,model='linear',prob=0.1)
set.seed(1)

## ----warning=FALSE,message=FALSE----------------------------------------------
dat=ChickWeight
y=dat$weight
X=dat[,-1]
dim(model.matrix(~.,data=dat)[,-c(1,2)]) # There are 53 predictors and 578 obs

## ----warning=FALSE,message=FALSE,error=TRUE-----------------------------------
set.seed(123)
extended_glm(y,X,model='ridge',prob=0.05)
set.seed(123)
extended_glm(y,X,model='lasso',prob=0.05)
set.seed(123)
extended_glm(y,X,model='random_lasso',prob=0.05,nfold=10,len=3) # Error
extended_glm(y,X,model='random_lasso',prob=0.5,nfold=5,len=3) # Increase probability/ reduce the fold to 5

