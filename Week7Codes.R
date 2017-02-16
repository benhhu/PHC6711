#INSTALL AND LOAD PACKAGES
install.packages("cvAUC")
install.packages("tree")
install.packages("ROCR")
library(ROCR)
library(cvAUC)
library(tree)
library(scales)


#############################
#      Part One ROC         # 
#############################
data(admissions)
head(admissions)
attach(admissions)

#Example 1
#Classifier 1 based on logistic regression

reg<-glm(Y~.,family=binomial,data=admissions)

S<-predict(reg,type="response")

pred.log<-S

#function to generate FP and TP
roc.curve<-function(s,print=FALSE){
  Ps<-(S>s)*1
  FP<-sum((Ps==1)*(Y==0))/sum(Y==0)
  TP<-sum((Ps==1)*(Y==1))/sum(Y==1)
    if(print==TRUE){
    print(table(Observed=Y,Predicted=Ps))
    }
  vect<-c(FP,TP)
  names(vect)<-c("FPR","TPR")
  return(vect)
}

#set threshold=0.5
threshold <-0.5
roc.curve(threshold,print=TRUE)

#Plot ROC curve
ROC.curve<-Vectorize(roc.curve)

M.ROC<-ROC.curve(seq(0,1,by=.001))
plot(M.ROC[1,],M.ROC[2,],lwd=2,type="l")

#Classifier 2 based on Classification Tree
ctr <- tree(Y~.,data=admissions)
plot(ctr)
text(ctr)
S<-predict(ctr)
pred.tree<-S

M.ROC.tree<-ROC.curve(seq(0,1,by=.001))

plot(M.ROC[1,],M.ROC[2,],type="l")
lines(M.ROC.tree[1,],M.ROC.tree[2,],type="l",col="grey",lwd=2)

#############################################
#Example 2. Use ROCR package to calculate AUC

T<-admissions$Y
pred.admissions.log<-prediction(pred.log,T)
pred.admissions.tree<-prediction(pred.tree,T)
perf.admissions.log<-performance(pred.admissions.log,"tpr","fpr")
perf.admissions.tree<-performance(pred.admissions.tree,"tpr","fpr")
(auc.log<-round(unlist(performance(pred.admissions.log,"auc")@y.values),3))
(auc.tree<-round(unlist(performance(pred.admissions.tree,"auc")@y.values),3))

#Plot ROC curve
plot(perf.admissions.log)
plot(perf.admissions.tree,col="gray",add=TRUE)
#lines(as.numeric(unlist(perf.admissions.tree@x.values)),as.numeric(unlist(perf.admissions.tree@y.values)),col="gray")
abline(a=0,b=1,xlim=c(0,1),ylim=c(0,1),lty=2)              
legend("bottomright",legend=c(paste("Logistic, AUC=",auc.log),paste("Tree, AUC=",auc.tree)),col=c("black","gray"),lty=c("solid","solid"))



#############################
#     Part Two CV ROC       # 
#############################

#Example 1. The built-in hiv dataset (10-fold CV)
data(ROCR.hiv)
head(ROCR.hiv)
attach(ROCR.hiv)
pred<-prediction(ROCR.hiv$hiv.svm$predictions,ROCR.hiv$hiv.svm$labels)
perf<-performance(pred,"tpr","fpr")
plot(perf,avg="vertical",spread.estimate="stderror")

#calculate cost
cost<-performance(pred,"cost",cost.fp=0.2,cost.fn=0.5)
cost2<-performance(pred,"cost",cost.fp=1,cost.fn=1000)

par(mfrow=c(1,2))
plot(cost,lty=3,col="gray")
plot(cost,avg="vertical",lwd=3,add=TRUE)

plot(cost2,lty=3,col="gray")
plot(cost2,avg="vertical",lwd=3,add=TRUE)

par(mfrow=c(1,1))

auc<-performance(pred,"auc")

#Package cvAUC
out<-cvAUC(hiv.svm$predictions,hiv.svm$labels)
plot(out$perf,lty=3,main="10-fold CV AUC")
plot(out$perf,col="red",lwd=3,avg="vertical",add=TRUE)
plot(out$perf,col=alpha("blue",0.5),lwd=3,avg="threshold",add=TRUE)
abline(a=0,b=1,xlim=c(0,1),ylim=c(0,1),lty=2) 

ci.cvAUC(hiv.svm$predictions, hiv.svm$labels, confidence=0.95)

##############################
#Example 2. Generate 10-fold CV ROC and AUC for the admissions dataset

iid_example <- function(data, V=10,method){
  require(cvAUC)
  #Create CV folds (stratify by outcome)
  .cvFolds <- function(Y, V){ 
    Y0 <- split(sample(which(Y==0)), rep(1:V, length=length(which(Y==0))))
    Y1 <- split(sample(which(Y==1)), rep(1:V, length=length(which(Y==1))))
    folds <- vector("list", length=V)
    for (v in seq(V)) {folds[[v]] <- c(Y0[[v]], Y1[[v]])}
    return(folds)
  }
  #GLM-based classifier
  .doFit1 <- function(v, folds, data){ 
    fit <- glm(Y~., data=data[-folds[[v]],], family=binomial)
    pred <- predict(fit, newdata=data[folds[[v]],], type="response")
    return(pred)
  }
  #Tree-based classifier
  .doFit2 <- function(v, folds, data){ 
    fit <- tree(Y~., data=data[-folds[[v]],])
    pred <- predict(fit, newdata=data[folds[[v]],])
    return(pred)
  }
  #Create folds
  folds <- .cvFolds(Y=data$Y, V=V) 
  #CV train/predict
  predictions1 <- unlist(sapply(seq(V), .doFit1, folds=folds, data=data)) 
  predictions2 <- unlist(sapply(seq(V), .doFit2, folds=folds, data=data)) 
  #Re-order pred values
  predictions1[unlist(folds)] <- predictions1 
  predictions2[unlist(folds)] <- predictions2 
  #Get CV AUC and confidence interval
  out1 <- ci.cvAUC(predictions=predictions1, labels=data$Y, folds=folds, confidence=0.95)
  out2 <- ci.cvAUC(predictions=predictions2, labels=data$Y, folds=folds, confidence=0.95)
  
  if (method=="glm"){
    return(out1)
  }
  if (method=="tree"){
    return(out2)
  }
}
# Load data
data(admissions)
# Get performance
set.seed(1)
(out1<-iid_example(data=admissions, V=10,method="glm"))
(out2<-iid_example(data=admissions, V=10,method="tree"))






