install.packages("nhanesA")

library(nhanesA)
library(sqldf)
library(plyr)
library(survey)
library(gtools)

#Demographics 
DEMO<-nhanes("DEMO")
DEMO_B<-nhanes("DEMO_B")
DEMO_C<-nhanes("DEMO_C")
DEMO_D<-nhanes("DEMO_D")
DEMO_E<-nhanes("DEMO_E")
DEMO_F<-nhanes("DEMO_F")
DEMO_G<-nhanes("DEMO_G")
DEMO_H<-nhanes("DEMO_H")

#Blood Lead
LAB06<-nhanes("LAB06")
L06_B<-nhanes("L06_B")
L06BMT_C<-nhanes("L06BMT_C")
PBCD_D<-nhanes("PBCD_D")
PBCD_E<-nhanes("PBCD_E")
PBCD_F<-nhanes("PBCD_F")
PBCD_G<-nhanes("PBCD_G")
PBCD_H<-nhanes("PBCD_H")

#Urinary Lead
LAB06HM<-nhanes("LAB06HM")
L06HM_B<-nhanes("L06HM_B")
L06HM_C<-nhanes("L06HM_C")
UHM_D<-nhanes("UHM_D")
UHM_E<-nhanes("UHM_E")
UHM_F<-nhanes("UHM_F")
UHM_G<-nhanes("UHM_G")
UM_H<-nhanes("UM_H")

###############
#Data cleaning#
###############

#Demographics
DEMO[]<- lapply(DEMO, as.numeric)
q<-'SELECT SEQN,RIAGENDR,RIDAGEYR,RIDRETH1,DMDEDUC3,DMDEDUC2,DMDMARTL,INDFMPIR,WTMEC2YR,SDMVPSU,SDMVSTRA
    FROM DEMO
;'
DEMO<-sqldf(q)

DEMO_B[]<- lapply(DEMO_B, as.numeric)
q<-'SELECT SEQN,RIAGENDR,RIDAGEYR,RIDRETH1,DMDEDUC3,DMDEDUC2,DMDMARTL,INDFMPIR,WTMEC2YR,SDMVPSU,SDMVSTRA
    FROM DEMO_B
;'
DEMO_B<-sqldf(q)

DEMO_C[]<- lapply(DEMO_C, as.numeric)
q<-'SELECT SEQN,RIAGENDR,RIDAGEYR,RIDRETH1,DMDEDUC3,DMDEDUC2,DMDMARTL,INDFMPIR,WTMEC2YR,SDMVPSU,SDMVSTRA
FROM DEMO_C
;'
DEMO_C<-sqldf(q)

DEMO_D[]<- lapply(DEMO_D, as.numeric)
q<-'SELECT SEQN,RIAGENDR,RIDAGEYR,RIDRETH1,DMDEDUC3,DMDEDUC2,DMDMARTL,INDFMPIR,WTMEC2YR,SDMVPSU,SDMVSTRA
FROM DEMO_D
;'
DEMO_D<-sqldf(q)

DEMO_E[]<- lapply(DEMO_E, as.numeric)
q<-'SELECT SEQN,RIAGENDR,RIDAGEYR,RIDRETH1,DMDEDUC3,DMDEDUC2,DMDMARTL,INDFMPIR,WTMEC2YR,SDMVPSU,SDMVSTRA
FROM DEMO_E
;'
DEMO_E<-sqldf(q)

DEMO_F[]<- lapply(DEMO_F, as.numeric)
q<-'SELECT SEQN,RIAGENDR,RIDAGEYR,RIDRETH1,DMDEDUC3,DMDEDUC2,DMDMARTL,INDFMPIR,WTMEC2YR,SDMVPSU,SDMVSTRA
FROM DEMO_F
;'
DEMO_F<-sqldf(q)

DEMO_G[]<- lapply(DEMO_G, as.numeric)
q<-'SELECT SEQN,RIAGENDR,RIDAGEYR,RIDRETH1,DMDEDUC3,DMDEDUC2,DMDMARTL,INDFMPIR,WTMEC2YR,SDMVPSU,SDMVSTRA
FROM DEMO_G
;'
DEMO_G<-sqldf(q)

DEMO_H[]<- lapply(DEMO_H, as.numeric)
q<-'SELECT SEQN,RIAGENDR,RIDAGEYR,RIDRETH1,DMDEDUC3,DMDEDUC2,DMDMARTL,INDFMPIR,WTMEC2YR,SDMVPSU,SDMVSTRA
FROM DEMO_H
;'
DEMO_H<-sqldf(q)

DEMO$CYCLE<-1
DEMO_B$CYCLE<-2
DEMO_C$CYCLE<-3
DEMO_D$CYCLE<-4
DEMO_E$CYCLE<-5
DEMO_F$CYCLE<-6
DEMO_G$CYCLE<-7
DEMO_H$CYCLE<-8

demo<-rbind(DEMO,DEMO_B,DEMO_C,DEMO_D,DEMO_E,DEMO_F,DEMO_G,DEMO_H)

#Blood Lead
LAB06[]<- lapply(LAB06, as.numeric)
q<-'SELECT SEQN,LBXBPB
FROM LAB06
;'
LAB06<-sqldf(q)

L06_B[]<- lapply(L06_B, as.numeric)
q<-'SELECT SEQN,LBXBPB
FROM L06_B
;'
L06_B<-sqldf(q)

L06BMT_C[]<- lapply(L06BMT_C, as.numeric)
q<-'SELECT SEQN,LBXBPB
FROM L06BMT_C
;'
L06BMT_C<-sqldf(q)

PBCD_D[]<- lapply(PBCD_D, as.numeric)
q<-'SELECT SEQN,LBXBPB
FROM PBCD_D
;'
PBCD_D<-sqldf(q)

PBCD_E[]<- lapply(PBCD_E, as.numeric)
q<-'SELECT SEQN,LBXBPB
FROM PBCD_E
;'
PBCD_E<-sqldf(q)

PBCD_F[]<- lapply(PBCD_F, as.numeric)
q<-'SELECT SEQN,LBXBPB
FROM PBCD_F
;'
PBCD_F<-sqldf(q)

PBCD_G[]<- lapply(PBCD_G, as.numeric)
q<-'SELECT SEQN,LBXBPB
FROM PBCD_G
;'
PBCD_G<-sqldf(q)

PBCD_H[]<- lapply(PBCD_H, as.numeric)
q<-'SELECT SEQN,LBXBPB
FROM PBCD_H
;'
PBCD_H<-sqldf(q)

bl<-rbind(LAB06,L06_B,L06BMT_C,PBCD_D,PBCD_E,PBCD_F,PBCD_G,PBCD_H)

#Urinary Lead
LAB06HM[]<- lapply(LAB06HM, as.numeric)
q<-'SELECT SEQN,URXUPB,URXUCR,WTSHM4YR AS WEIGHTUHM
FROM LAB06HM
;'
LAB06HM<-sqldf(q)

L06HM_B[]<- lapply(L06HM_B, as.numeric)
q<-'SELECT SEQN,URXUPB,URXUCR,WTSHM4YR AS WEIGHTUHM
FROM L06HM_B
;'
L06HM_B<-sqldf(q)

L06HM_C[]<- lapply(L06HM_C, as.numeric)
q<-'SELECT SEQN,URXUPB,URXUCR,WTSA2YR AS WEIGHTUHM
FROM L06HM_C
;'
L06HM_C<-sqldf(q)

UHM_D[]<- lapply(UHM_D, as.numeric)
q<-'SELECT SEQN,URXUPB,URXUCR,WTSA2YR AS WEIGHTUHM
FROM UHM_D
;'
UHM_D<-sqldf(q)
UHM_E[]<- lapply(UHM_E, as.numeric)
q<-'SELECT SEQN,URXUPB,URXUCR,WTSA2YR AS WEIGHTUHM
FROM UHM_E
;'
UHM_E<-sqldf(q)
UHM_F[]<- lapply(UHM_F, as.numeric)
q<-'SELECT SEQN,URXUPB,URXUCR,WTSA2YR AS WEIGHTUHM
FROM UHM_F
;'
UHM_F<-sqldf(q)
UHM_G[]<- lapply(UHM_G, as.numeric)
q<-'SELECT SEQN,URXUPB,URXUCR,WTSA2YR AS WEIGHTUHM
FROM UHM_G
;'
UHM_G<-sqldf(q)
UM_H[]<- lapply(UM_H, as.numeric)
q<-'SELECT SEQN,URXUPB,URXUCR,WTSA2YR AS WEIGHTUHM
FROM UM_H
;'
UM_H<-sqldf(q)

ul<-rbind(LAB06HM,L06HM_B,L06HM_C,UHM_D,UHM_E,UHM_F,UHM_G,UM_H)

#Merge together
q<-'SELECT *
FROM demo
LEFT OUTER JOIN bl ON demo.SEQN=bl.SEQN
LEFT OUTER JOIN ul ON demo.SEQN=ul.SEQN
;'
dat<-sqldf(q)

#Recoding
#AGE
dat$ageg[dat$RIDAGEYR>=20 & dat$RIDAGEYR<=29]<-1
dat$ageg[dat$RIDAGEYR>=30 & dat$RIDAGEYR<=39]<-2
dat$ageg[dat$RIDAGEYR>=40 & dat$RIDAGEYR<=49]<-3
dat$ageg[dat$RIDAGEYR>=50 & dat$RIDAGEYR<=59]<-4
dat$ageg[dat$RIDAGEYR>=60 & dat$RIDAGEYR<=69]<-5
dat$ageg[dat$RIDAGEYR>=70 & dat$RIDAGEYR<=79]<-6
dat$ageg[dat$RIDAGEYR>=80]<-7
#RACE, 1=white, 2=black, 3=others
dat$race[dat$RIDRETH1==3]<-1
dat$race[dat$RIDRETH1==4]<-2
dat$race[dat$RIDRETH1 %in% c(1,2,5)]<-3
#EDUCATION, 1=<hs, 2=hs, 3=>hs
dat$DMDEDUC2[dat$DMDEDUC2 %in% c(7,9)]<-NA
dat$edu[dat$DMDEDUC2 %in% c(1,2)]<-1
dat$edu[dat$DMDEDUC2 %in% c(3)]<-2
dat$edu[dat$DMDEDUC2 %in% c(4,5)]<-3
#MARITAL, 1=yes, 0=no
dat$DMDMARTL[dat$DMDMARTL %in% c(77,99)]<-NA
dat$marry[dat$DMDMARTL %in% c(1)]<-1
dat$marry[dat$DMDMARTL %in% c(2,3,4,5,6)]<-0
#PIR, 1=<1, 2=1-2, 3=>=2
dat$pir[dat$INDFMPIR>=0 & dat$INDFMPIR<1]<-1
dat$pir[dat$INDFMPIR>=1 & dat$INDFMPIR<2]<-2
dat$pir[dat$INDFMPIR>=2]<-3
#Weight
dat$weight[dat$CYCLE %in% c(1,2)]<-2*dat$WEIGHTUHM[dat$CYCLE %in% c(1,2)]/8
dat$weight[dat$CYCLE %in% c(3,4,5,6,7,8)]<-dat$WEIGHTUHM[dat$CYCLE %in% c(3,4,5,6,7,8)]/8

#Exclusion
#dat2<-dat[(!is.na(dat$LBXBPB))&(!is.na(dat$URXUPB))&(!is.na(dat$URXUCR))&(!is.na(dat$weight))&(!is.na(dat$DMDEDUC2))&(!is.na(dat$DMDMARTL))&(!is.na(dat$INDFMPIR)),]

dat2<-dat[(!is.na(dat$LBXBPB))&(!is.na(dat$URXUPB))&(!is.na(dat$URXUCR))&(!is.na(dat$weight)),]

#Filter on age: 18-26 years old
#dat2<-dat2[(dat2$RIDAGEYR>=18)&(dat2$RIDAGEYR<=26),]

############
# Analyses #
############

#log blood lead
dat2$logbl<-log(dat2$LBXBPB)
#creatinine adjusted urinary lead (ng/mg)
dat2$cul<-dat2$URXUPB/(0.01*dat2$URXUCR)
#log creatinine adjusted urinary lead
dat2$logcul<-log(dat2$cul)

#survey design
design <- svydesign(id = ~ SDMVPSU, strata = ~ SDMVSTRA, nest = TRUE, weight = ~ weight, data = dat2)

#Correlation
cor.test(dat2$logbl,dat2$logcul,method=c("pearson"))
cor.test(dat2$logbl,dat2$logcul,method=c("spearman"))

model1<-svyglm(logbl ~ logcul, design = design)
(R<-sqrt(1-model1$deviance/model1$null.deviance))

#Scatterplot
plot(dat2$logbl,dat2$logcul,xlab="Log-transformed Blood Lead", ylab="Log-transformed Creatinine-adjusted Urinary Lead")
lines(lowess(dat2$logbl,dat2$logcul), col="blue") 

#quantile blood lead
dat2$qbl<-as.integer(cut(dat2$LBXBPB, quantile(dat2$LBXBPB, probs=c(0,0.75,1)), include.lowest=TRUE))-1
#quantile creatinine adjusted urinary lead
dat2$qcul<-as.integer(cut(dat2$cul, quantile(dat2$cul, probs=c(0,0.75,1)), include.lowest=TRUE))-1

sum(dat2$qbl==dat2$qcul)/length(dat2$SEQN)

table(dat2$qbl,dat2$qcul)

########################
# Predictive Modelling #
########################
library(dummies)
library(xgboost)
library(pROC)

#Make dummies
dat2[,((ncol(dat2)+1):(ncol(dat2)+2))]<-data.frame(dummy(dat2$RIAGENDR))
dat2[,((ncol(dat2)+1):(ncol(dat2)+5))]<-data.frame(dummy(dat2$RIDRETH1))
#dat2[,((ncol(dat2)+1):(ncol(dat2)+5))]<-data.frame(dummy(dat2$DMDEDUC2))
#dat2[,((ncol(dat2)+1):(ncol(dat2)+6))]<-data.frame(dummy(dat2$DMDMARTL))


#Training and testing split
## 80% of the sample size
smp_size <- floor(0.80 * nrow(dat2))
## set the seed
set.seed(123)
train_ind <- sample(seq_len(nrow(dat2)), size = smp_size)

train <- dat2[train_ind, ]
test <- dat2[-train_ind, ]

###############
#Continuous bpb
X_train<-as.matrix(train[,c(3,16,17,30:36)])
X_test<-as.matrix(test[,c(3,16,17,30:36)])
#X_train<-as.matrix(train[,c(3,8,16,17,30:47)])
#X_test<-as.matrix(test[,c(3,8,16,17,30:47)])
y_train<-train$LBXBPB
y_test<-test$LBXBPB

####
param <- list("objective" = "reg:linear",
              "eval_metric" = "rmse",
              "max_depth" = 5, 
              "eta"=0.04,
              "colsample_bytree"=1,
              "min_child_weight"=1,
              "subsample"=0.8)
cv.nround<-50000
cv.nfold<-4

bst.cv<-xgb.cv(param=param,data=X_train,label=y_train,nfold=cv.nfold,nrounds=cv.nround)

nround<-207
bst<-xgboost(param=param,data=X_train,label=y_train,nrounds=nround)

####
y_pred<-predict(bst,X_test)
#RMSE: 0.88
error<-y_pred-y_test
sqrt(mean(error^2))
#R2
summary(lm(y_pred~y_test))

###############
#75th percentile bpb
X_train<-as.matrix(train[,c(3,16,17,30:36)])
X_test<-as.matrix(test[,c(3,16,17,30:36)])
#X_train<-as.matrix(train[,c(3,8,16,17,30:47)])
#X_test<-as.matrix(test[,c(3,8,16,17,30:47)])
y_train<-train$qbl
y_test<-test$qbl


####
param <- list("objective" = "binary:logistic",
              "eval_metric" = "auc",
              "max_depth" = 5, 
              "eta"=0.04,
              "colsample_bytree"=1,
              "min_child_weight"=1,
              "subsample"=0.8,
              "set.seed"=123)
cv.nround<-50000
cv.nfold<-4

bst.cv<-xgb.cv(param=param,data=X_train,label=y_train,nfold=cv.nfold,nrounds=cv.nround)

nround<-176
bst<-xgboost(param=param,data=X_train,label=y_train,nrounds=nround)
#xgb.save(bst,"xgboost_656693_2.model")
#bst<-xgb.load("xgboost_663611.model")

names<-dimnames(X_train)[[2]]
importance_matrix<-xgb.importance(names,model=bst)
#save(importance_matrix,file="importance_matrix.Rda") 
xgb.plot.importance(importance_matrix)

####
y_pred<-predict(bst,X_test)
#AUC: 0.9238
auc(y_test,y_pred)
#Accuracy: 0.8661417
sum((as.numeric(cut(y_pred,c(0,0.5,1)))-1)==y_test)/length(y_test)


#############
############
####
param <- list("objective" = "binary:logistic",
              "eval_metric" = "auc",
              "max_depth" = 4, 
              "eta"=0.005,
              "colsample_bytree"=0.8,
              "min_child_weight"=1,
              "subsample"=0.8,
              "nthread"=4,
              "set.seed"=123)
cv.nround<-50000
cv.nfold<-4

bst.cv<-xgb.cv(param=param,data=X_train,label=y_train,nfold=cv.nfold,nrounds=cv.nround)

nround<-2264
bst<-xgboost(param=param,data=X_train,label=y_train,nrounds=nround)

####
y_pred<-predict(bst,X_test)
#AUC: 0.9239
auc(y_test,y_pred)
#Accuracy: 0.8673083
sum((as.numeric(cut(y_pred,c(0,0.5,1)))-1)==y_test)/length(y_test)











