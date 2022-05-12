#' Extended version of glmnet
#'
#' Take as input a response variable y (binary or continuous) and a set of candidate predictors/independent variables X and fit the model the user specified even in situations where the number of predictors is large for some models;
#' linear, logistic, ridge, lasso, or random lasso regression.Then calculate the model performance on the test data or the entire input data, giving different metrics based on the type of response.
#' For continuous case, it can be assumed to be normally distributed.
#'
#' @param y a (non-empty) numeric vector or factor with two levels with some missing value (NA).
#' @param X a (non-empty) data frame or matrix where each column is a variable/predictor with some missing value (NA).For more information, see details section below.
#' @param model a character string specifying the type of model to fit, must be one of "linear" (default), "ridge", "lasso", or "random_lasso".
#' @param prob a numeric value of the proportion of data (0-1) to use for training, 0.7 (default).
#' @param len a numeric value of the number of tuning parameter combinations for random lasso regression, 3 (default).For more information, see details section below.
#' @param nfold a numeric value of the number of folds for cross validation for random lasso regression, 5 (default).For more information, see Details section below.
#'
#' @details
#' In the algorithm of random lasso, q1 candidate variables are randomly selected in each bootstrap sample to calculate the importance measure of each predictor, and then again q2 candidate variables are randomly selected in each bootstrap sample,
#' where q1 and q2 are two tuning parameters that can be chosen as large as p. The function chooses q1 and q2 using cross-validation based on the number of possible q1 and q2 values, which the value len.
#' It selects equally distanced values of length len for q1 and q2,including 2,and p. See the reference section for more details on how random lasso works.
#' Furthermore, the function standardizes the numeric vectors, continuous y variable and numeric predictors. Then when calculating the root mean square error it unscales the predicted values using sample mean, and sample standard deviation of the input of y.
#'
#' @return For continuous y, it returns the root mean square error of the test data and a plotting of an actual against predicted y response while for a binary response, a list of accuracy score and confusion matrix of the test data.
#'
#' @examples
#' # Fitting a linear, lasso and ridge regression with data splitting,
#' # 75% for training data and 25% for testing data
#'
#' X=matrix(rnorm(100),ncol=2)
#' y=rexp(50)
#'
#' extended_glm(y,X,model='linear',prob=0.75)
#' extended_glm(y,X,model='lasso',prob=0.75)
#' extended_glm(y,X,model='ridge',prob=0.75)
#'
#' # Fitting a logistic, lasso and ridge regression with no data splitting
#'
#' X_b=data.frame(x1=sample(LETTERS[1:2],50,replace=TRUE),x2=rnorm(50))
#' y_b=sample(c(0,1),50,replace=TRUE)
#'
#' extended_glm(y_b,X_b,model='linear',prob=1)
#' extended_glm(y_b,X_b,model='lasso',prob=1)
#' extended_glm(y_b,X_b,model='ridge',prob=1)
#'
#' # Fitting a random lasso regression
#' # 70% for training data and 30% for testing data
#' # Perform 3-fold cross validation to get the parameters
#' # Considers 2 possible values of q1 and q2
#'
#' X=matrix(rnorm(400),ncol=4)
#' y=rexp(100)
#'
#' X_b=data.frame(x1=sample(LETTERS[1:2],100,replace=TRUE),x2=rnorm(100),x3=runif(100))
#' y_b=sample(c(0,1),100,replace=TRUE)
#'
#' extended_glm(y,X,model='random_lasso',prob=0.7,nfold=3,len=2)
#' extended_glm(y_b,X_b,model='random_lasso',prob=0.7,nfold=3,len=2)
#'
#' @references
#' Wang, S., Nan, B., Rosset, S., &amp; Zhu, J. (2011). Random lasso. The Annals of Applied Statistics, 5(1).
#' https://doi.org/10.1214/10-aoas377
#'
#' @import glmnet
#' @import stats
#' @import graphics
#' @export

extended_glm <- function(y,X,model=c("linear","ridge","lasso","random_lasso"),prob=0.7,len=3,nfold=5){
  data=na.omit(data.frame(y,X)) # delete NA values
  ########### Data Pre-Processing ############
  if(length(unique(data[,1]))!=2 && !is.numeric(data[,1])){
    stop("y is not a binary or continuous variable")
  }
  if(length(unique(data[,1]))==1|(length(unique(data[,1]))<=length(data[,1])/3 & length(unique(data[,1]))>2)){
    warning("y might be a categorical variable treated as a continuous variable")
  }
  if(is.vector(X)){
    stop("There should be at least 2 predictors")
  }
  ########### Binary Classification ############
  if(length(unique(data[,1]))==2){
    data$y==as.factor(data$y)

    if(model=="linear"){
      if(round(prob*nrow(data))==nrow(data)){
        y_train=data[,1]
        x_train=data[,-1]
        y_test=y_train
        x_test=x_train
      }else{
        y=data[,1]
        X=data[,-1]
        index=sample(1:nrow(data), round(prob*nrow(data)))
        y_train=y[index]
        x_train=X[index,]
        y_test=y[-index]
        x_test=X[-index,]
      }

      if(nrow(x_train)<ncol(x_train)){
        stop("Number of observation < Number of variables")
      }else{
        subdat=cbind(y=y_train,x_train)
        fit.log=glm(y~.,data=subdat,family='binomial')
        prob=as.vector(predict(fit.log,x_test,type='response'))
        predict =ifelse(prob> 0.5, 1, 0)
        accuracy=mean(predict==y_test)
        return(list(Accuracy=accuracy,confusionMatrix=table(predict,y_test)))
      }
    }else if(model=='ridge'){
      data_dum=model.matrix(~.,data=data)[,-1]
      if(round(prob*nrow(data_dum))==nrow(data_dum)){
        y_train=data_dum[,1]
        x_train=data_dum[,-1]
        y_test=y_train
        x_test=x_train

      }else{
        y=data_dum[,1]
        X=data_dum[,-1]
        index=sample(1:nrow(data), round(prob*nrow(data)))
        y_train=y[index]
        x_train=X[index,]
        y_test=y[-index]
        x_test=X[-index,]

      }

      cv.ridge=cv.glmnet(x_train,y_train,family='binomial',alpha=0)
      fit.ridge=glmnet(x_train,y_train,family='binomial',alpha=0,lambda=cv.ridge$lambda.min)
      prob=as.vector(predict(fit.ridge,x_test,type='response'))
      predict =ifelse(prob> 0.5, 1, 0)
      accuracy=mean(predict==y_test)
      return(list(Accuracy=accuracy,confusionMatrix=table(predict,y_test)))

    }else if(model=='lasso'){
      data_dum=model.matrix(~.,data=data)[,-1]
      if(round(prob*nrow(data_dum))==nrow(data_dum)){
        y_train=data_dum[,1]
        x_train=data_dum[,-1]
        y_test=y_train
        x_test=x_train

      }else{
        y=data_dum[,1]
        X=data_dum[,-1]
        index=sample(1:nrow(data), round(prob*nrow(data)))
        y_train=y[index]
        x_train=X[index,]
        y_test=y[-index]
        x_test=X[-index,]
      }
      cv.lasso=cv.glmnet(x_train,y_train,family='binomial',alpha=1)
      fit.lasso=glmnet(x_train,y_train,family='binomial',alpha=1,lambda=cv.lasso$lambda.min)
      prob=as.vector(predict(fit.lasso,x_test,type='response'))
      predict =ifelse(prob> 0.5, 1, 0)
      accuracy=mean(predict==y_test)
      return(list(Accuracy=accuracy,confusionMatrix=table(predict,y_test)))

    }else if(model=='random_lasso'){
      data$y=as.factor(data$y)
      cat=sapply(data,function(x) !is.numeric(x))
      data[,!cat]=data.frame(scale(data[,!cat]))
      data_dum=model.matrix(~.,data=data)[,-1]

      if(round(prob*nrow(data_dum))==nrow(data_dum)){
        y_train=data_dum[,1]
        x_train=data_dum[,-1]
        y_test=y_train
        x_test=x_train
        if(nrow(x_train)<=10*nfold){
          stop("The size of each fold is small to perform cross validation on regularization")
        }

      }else{
        index=sample(1:nrow(data_dum), round(prob*nrow(data_dum)))
        y=data_dum[,1]
        X=data_dum[,-1]
        y_train=y[index]
        x_train=X[index,]
        y_test=y[-index]
        x_test=X[-index,]
        if(nrow(x_train)<=10*nfold){
          stop("The size of each fold is small to perform cross validation on regularization")
        }

      }

      B=500 # bootstrap
      n_train=nrow(x_train)
      p=ncol(x_train)
      best.q1=NA
      best.q2=NA
      best.acc=0
      if (p<=4){len=p-1}
      ########### K-cross validation ############
      q1_list=round(seq(2,p,length.out=len))
      q2_list=round(seq(2,p,length.out=len))
      for(qq1 in 1:length(q1_list)){
        q1=q1_list[qq1]
        for(qq2 in 1:length(q2_list)){
          q2=q2_list[qq2]
          ind_shuffle=sample(1:n_train)
          train.data=cbind(y_train,x_train)[ind_shuffle,]
          ind=round(seq(1,n_train,length.out=nfold+1))
          v.acc=rep(NA,nfold)
          for (i in 1:nfold){
            val=ind[i]:ind[i+1]
            test_x=train.data[val,-1]
            test_y=train.data[val,1]
            train_x=train.data[-val,-1]
            train_y=train.data[-val,1]
            n_cross=nrow(train_x)
            beta_est=matrix(0,B,p)
            for(b in 1:B){
              id=sample(1:n_cross,n_cross,replace=T)
              boot.index=train_x[id,]
              rcol=sample(1:p,q1)
              y_train_sub=train_y[id]
              x_train_sub=train_x[id,rcol]

              cv.lasso=cv.glmnet(x_train_sub,y_train_sub,family='binomial',alpha=1,type.measure='deviance')
              fit.lasso=glmnet(x_train_sub,y_train_sub,family='binomial',alpha=1,lambda=cv.lasso$lambda.min,intercept=FALSE)
              beta_est[b,rcol]=as.vector(coef(fit.lasso,s=cv.lasso$lambda.min))[-1]

            }
            imp=abs(colSums(beta_est)/B)
            p_imp=imp/sum(imp)
            beta_est=matrix(0,B,p)
            for(b in 1:B){
              id=sample(1:n_cross,n_cross,replace=T)
              boot.index=train_x[id,]
              rcol=sample(1:p,q2,prob=p_imp)
              y_train_sub=train_y[id]
              x_train_sub=train_x[id,rcol]

              cv.lasso=cv.glmnet(x_train_sub,y_train_sub,family='binomial',alpha=1,type.measure='deviance')
              fit.lasso=glmnet(x_train_sub,y_train_sub,family='binomial',alpha=1,lambda=cv.lasso$lambda.min,intercept=FALSE)
              beta_est[b,rcol]=as.vector(coef(fit.lasso,s=cv.lasso$lambda.min))[-1]
            }
            beta_est=colSums(beta_est)/B
            e=exp(as.vector(test_x%*%beta_est))
            my.fitted=e/(e+1)

            predicted <- ifelse(my.fitted > 0.5, 1, 0)
            n.acc=mean(predicted==test_y)
            v.acc[i]=n.acc
          }
          if(all(is.na(v.acc))){
            v.acc=rep(best.acc,length(nfold))
          }
          if (mean(v.acc,na.rm=TRUE)>best.acc){
            best.acc=mean(v.acc)
            best.q1=q1
            best.q2=q2
          }
        }
      }
      ############# Fitting #################
      q1=best.q1
      q2=best.q2
      beta_est=matrix(0,B,p)
      for(b in 1:B){
        id=sample(1:n_train,n_train,replace=T)
        boot.index=x_train[id,]
        rcol=sample(1:p,q1)
        y_train_sub=y_train[id]
        x_train_sub=x_train[id,rcol]
        cv.lasso=cv.glmnet(x_train_sub,y_train_sub,family='binomial',alpha=1,type.measure='deviance')
        fit.lasso=glmnet(x_train_sub,y_train_sub,family='binomial',alpha=1,lambda=cv.lasso$lambda.min,intercept=FALSE)
        beta_est[b,rcol]=as.vector(coef(fit.lasso,s=cv.lasso$lambda.min))[-1]
      }

      imp=abs(colSums(beta_est)/B)
      p_imp=imp/sum(imp)
      beta_est=matrix(0,B,p) # Select (proportional to importance)
      for(b in 1:B){
        id=sample(1:n_train,n_train,replace=T)
        boot.index=x_train[id,]
        rcol=sample(1:p,q2,prob=p_imp)
        y_train_sub=y_train[id]
        x_train_sub=x_train[id,rcol]

        cv.lasso=cv.glmnet(x_train_sub,y_train_sub,family='binomial',alpha=1,type.measure='deviance')
        fit.lasso=glmnet(x_train_sub,y_train_sub,family='binomial',alpha=1,lambda=cv.lasso$lambda.min,intercept=FALSE)
        beta_est[b,rcol]=as.vector(coef(fit.lasso,s=cv.lasso$lambda.min))[-1]
      }
      beta_est=colSums(beta_est)/B
      e=exp(as.vector(x_test%*%beta_est))
      my.fitted=e/(e+1)
      predicted <- ifelse(my.fitted > 0.5, 1, 0)
      accuracy=mean(y_test==predicted)
      return(list(Accuracy=accuracy,confusionMatrix=table(predicted,y_test)))
      return(accuracy)
    }
  }else{ ########### Continuous ############
    if(model=="linear"){
      if((round(prob*nrow(data))==nrow(data))){
        y_train=data[,1]
        x_train=data[,-1]
        y_test=y_train
        x_test=x_train
      }else{
        y=data[,1]
        X=data[,-1]
        index=sample(1:nrow(data), round(prob*nrow(data)))
        y_train=y[index]
        x_train=X[index,]
        y_test=y[-index]
        x_test=X[-index,]

      }
      if(nrow(x_train)<ncol(x_train)){
        stop("Number of observation < Number of variables")
      }else{
        subdat=data.frame(cbind(y=y_train,x_train))
        fit.lm=lm(y~.,data=subdat)
        pred=predict(fit.lm,x_test)
        rmse=sqrt(sum((pred-y_test)^2))
        plot(pred,y_test,xlab="Predicted value",ylab="Actual Value")
        abline(0,1,col='red',lwd=1.5)
        return(c(Root.mean.square=rmse))
      }
    }else if(model=='ridge'){
      if((round(prob*nrow(data))==nrow(data))){
        y_train=data[,1]
        x_train=model.matrix(~.,data=data[,-1])[,-1]
        y_test=y_train
        x_test=x_train

      }else{
        y=data[,1]
        X=model.matrix(~.,data=data[,-1])[,-1]
        index=sample(1:nrow(data), round(prob*nrow(data)))
        y_train=y[index]
        x_train=X[index,]
        y_test=y[-index]
        x_test=X[-index,]

      }


      cv.ridge=cv.glmnet(x_train,y_train,family='gaussian',alpha=0)
      fit.ridge=glmnet(x_train,y_train,family='gaussian',alpha=0,lambda=cv.ridge$lambda.min,intercept=FALSE)

      pred=predict(fit.ridge,x_test)
      rmse=sqrt(sum((pred-y_test)^2))

      plot(pred,y_test,xlab="Predicted value",ylab="Actual Value")
      abline(0,1,col='red',lwd=1.5)
      return(c(Root.mean.square=rmse))
    }else if(model=='lasso'){
      if((round(prob*nrow(data))==nrow(data))){
        y_train=data[,1]
        x_train=model.matrix(~.,data=data[,-1])[,-1]
        y_test=y_train
        x_test=x_train

      }else{
        y=data[,1]
        X=model.matrix(~.,data=data[,-1])[,-1]
        index=sample(1:nrow(data), round(prob*nrow(data)))
        y_train=y[index]
        x_train=X[index,]
        y_test=y[-index]
        x_test=X[-index,]

      }
      cv.lasso=cv.glmnet(x_train,y_train,family='gaussian',alpha=1)
      fit.lasso=glmnet(x_train,y_train,family='gaussian',alpha=1,lambda=cv.lasso$lambda.min)
      pred=predict(fit.lasso,x_test)
      rmse=sqrt(sum((pred-y_test)^2))
      plot(pred,y_test,xlab="Predicted value",ylab="Actual Value")
      abline(0,1,col='red',lwd=1.5)
      return(c(Root.mean.square=rmse))
    }else if(model=='random_lasso'){
      mean=mean(data[,1])
      sd=sd(data[,1])
      cat=sapply(data,function(x) !is.numeric(x))
      data[,!cat]=data.frame(scale(data[,!cat]))
      if((round(prob*nrow(data))==nrow(data))){
        y_train=data[,1]
        x_train=model.matrix(~.,data=data[,-1])[,-1]
        y_test=y_train
        x_test=x_train
        if(nrow(x_train)<=10*nfold){
          stop("The size of each fold is small to perform cross validation on regularization")
        }
      }else{
        y=data[,1]
        X=model.matrix(~.,data=data[,-1])[,-1]
        index=sample(1:nrow(data), round(prob*nrow(data)))
        y_train=y[index]
        x_train=X[index,]
        y_test=y[-index]
        x_test=X[-index,]
        if(nrow(x_train)<=10*nfold){
          stop("The size of each fold is small to perform cross validation on regularization")
        }

      }
      B=500 # bootstrap
      n_train=nrow(x_train)
      p=ncol(x_train)
      best.q1=NA
      best.q2=NA
      best.rmse=.Machine$double.xmax
      if (p<=4){
        len=p-1}
      ########### K-cross validation ############
      q1_list=round(seq(2,p,length.out=len))
      q2_list=round(seq(2,p,length.out=len))
      for(qq1 in 1:length(q1_list)){
        q1=q1_list[qq1]
        for(qq2 in 1:length(q2_list)){
          q2=q2_list[qq2]
          ind_shuffle=sample(1:n_train)
          train.data=cbind(y_train,x_train)[ind_shuffle,] # check
          ind=round(seq(1,n_train,length.out=nfold+1))
          v.rmse=rep(NA,nfold)
          for (i in 1:nfold){
            val=ind[i]:ind[i+1]
            test_x=train.data[val,-1]
            test_y=train.data[val,1]
            train_x=train.data[-val,-1]
            train_y=train.data[-val,1]
            n_cross=nrow(train_x)
            beta_est=matrix(0,B,p)
            for(b in 1:B){
              id=sample(1:n_cross,n_cross,replace=T)
              boot.index=train_x[id,]
              rcol=sample(1:p,q1)
              y_train_sub=train_y[id]
              x_train_sub=train_x[id,rcol]

              cv.lasso=cv.glmnet(x_train_sub,y_train_sub,family='gaussian',alpha=1)
              fit.lasso=glmnet(x_train_sub,y_train_sub,family='gaussian',alpha=1,lambda=cv.lasso$lambda.min,intercept=FALSE)
              beta_est[b,rcol]=as.vector(coef(fit.lasso,s=cv.lasso$lambda.min))[-1]
            }

            imp=abs(colSums(beta_est)/B)
            p_imp=imp/sum(imp)
            beta_est=matrix(0,B,p)

            for(b in 1:B){
              id=sample(1:n_cross,n_cross,replace=T)
              boot.index=train_x[id,]
              rcol=sample(1:p,q2,prob=p_imp)
              y_train_sub=train_y[id]
              x_train_sub=train_x[id,rcol]

              cv.lasso=cv.glmnet(x_train_sub,y_train_sub,family='gaussian',alpha=1)
              fit.lasso=glmnet(x_train_sub,y_train_sub,family='gaussian',alpha=1,lambda=cv.lasso$lambda.min,intercept=FALSE)
              beta_est[b,rcol]=as.vector(coef(fit.lasso,s=cv.lasso$lambda.min))[-1]
            }
            beta_est=colSums(beta_est)/B
            my.fitted=test_x%*%beta_est
            n.rmse=sqrt(sum((my.fitted-test_y)^2))
            v.rmse[i]=n.rmse
          }
          if(all(is.na(v.rmse))){
            v.rmse=rep(best.rmse,length(nfold))
          }
          if (mean(v.rmse,na.rm=TRUE)<best.rmse){
            best.rmse=mean(v.rmse)
            best.q1=q1
            best.q2=q2
          }
        }
      }
      ############# Fitting #################
      q1=best.q1
      q2=best.q2
      beta_est=matrix(0,B,p)
      for(b in 1:B){
        id=sample(1:n_train,n_train,replace=T)
        boot.index=x_train[id,]
        rcol=sample(1:p,q1)
        y_train_sub=y_train[id]
        x_train_sub=x_train[id,rcol]
        cv.lasso=cv.glmnet(x_train_sub,y_train_sub,family='gaussian',alpha=1)

        fit.lasso=glmnet(x_train_sub,y_train_sub,family='gaussian',alpha=1,lambda=cv.lasso$lambda.min,intercept=FALSE)
        beta_est[b,rcol]=as.vector(coef(fit.lasso,s=cv.lasso$lambda.min))[-1]
      }
      imp=abs(colSums(beta_est)/B)
      p_imp=imp/sum(imp)
      beta_est=matrix(0,B,p) # Select (proportional to importance)
      for(b in 1:B){
        id=sample(1:n_train,n_train,replace=T)
        boot.index=x_train[id,]
        rcol=sample(1:p,q2,prob=p_imp)
        y_train_sub=y_train[id]
        x_train_sub=x_train[id,rcol]
        cv.lasso=cv.glmnet(x_train_sub,y_train_sub,family='gaussian',alpha=1)
        fit.lasso=glmnet(x_train_sub,y_train_sub,family='gaussian',alpha=1,lambda=cv.lasso$lambda.min,intercept=FALSE)
        beta_est[b,rcol]=as.vector(coef(fit.lasso,s=cv.lasso$lambda.min))[-1]
      }
      beta_est=colSums(beta_est)/B
      predict=as.vector(x_test%*%beta_est)*sd+mean
      y=y_test*sd+mean
      rmse=sqrt(sum((predict-y)^2))
      plot(predict,y,xlab="Predicted value",ylab="Actual Value")
      abline(0,1,col='red',lwd=1.5)
      return(RMSE=rmse)
    }
  }
}
