library(nhanesA)
library(sqldf)
library(plyr)
library(survey)  
library(gtools)

#Get Relevant Tables
#Demographics
DEMO_D<-nhanes("DEMO_D")
DEMO_E<-nhanes("DEMO_E")
DEMO_F<-nhanes("DEMO_F")
DEMO_G<-nhanes("DEMO_G")
DEMO_H<-nhanes("DEMO_H")

#Urinary Lead
UHM_D<-nhanes("UHM_D")
UHM_E<-nhanes("UHM_E")
UHM_F<-nhanes("UHM_F")
UHM_G<-nhanes("UHM_G")
UM_H<-nhanes("UM_H")

#Drug Use
DUQ_D<-nhanes("DUQ_D")
DUQ_E<-nhanes("DUQ_E")
DUQ_F<-nhanes("DUQ_F")
DUQ_G<-nhanes("DUQ_G")
DUQ_H<-nhanes("DUQ_H")

#Alcohol Use
ALQ_D<-nhanes("ALQ_D")
ALQ_E<-nhanes("ALQ_E")
ALQ_F<-nhanes("ALQ_F")
ALQ_G<-nhanes("ALQ_G")
ALQ_H<-nhanes("ALQ_H")

#Sexual Behavior
SXQ_D<-nhanes("SXQ_D")
SXQ_E<-nhanes("SXQ_E")
SXQ_F<-nhanes("SXQ_F")
SXQ_G<-nhanes("SXQ_G")
SXQ_H<-nhanes("SXQ_H")

#HSV-2
HSV_D<-nhanes("HSV_D")
HSV_E<-nhanes("HSV_E")
HSV_F<-nhanes("HSV_F")
HSV_G<-nhanes("HSV_G")
HSV_H<-nhanes("HSV_H")

###############
#Data cleaning#
###############

#Demographics
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

demo<-rbind(DEMO_D,DEMO_E,DEMO_F,DEMO_G,DEMO_H)

#Urinary Lead
UHM_D[]<- lapply(UHM_D, as.numeric)
q<-'SELECT SEQN,URXUPB,URXUCR,WTSA2YR
FROM UHM_D
;'
UHM_D<-sqldf(q)
UHM_E[]<- lapply(UHM_E, as.numeric)
q<-'SELECT SEQN,URXUPB,URXUCR,WTSA2YR
FROM UHM_E
;'
UHM_E<-sqldf(q)
UHM_F[]<- lapply(UHM_F, as.numeric)
q<-'SELECT SEQN,URXUPB,URXUCR,WTSA2YR
FROM UHM_F
;'
UHM_F<-sqldf(q)
UHM_G[]<- lapply(UHM_G, as.numeric)
q<-'SELECT SEQN,URXUPB,URXUCR,WTSA2YR
FROM UHM_G
;'
UHM_G<-sqldf(q)
UM_H[]<- lapply(UM_H, as.numeric)
q<-'SELECT SEQN,URXUPB,URXUCR,WTSA2YR
FROM UM_H
;'
UM_H<-sqldf(q)

uhm<-rbind(UHM_D,UHM_E,UHM_F,UHM_G,UM_H)


#Alcohol Use
ALQ_D[]<- lapply(ALQ_D, as.numeric)
q<-'SELECT SEQN,ALQ101,ALQ110,ALQ140Q,ALQ150
FROM ALQ_D
;'
ALQ_D<-sqldf(q)
ALQ_E[]<- lapply(ALQ_E, as.numeric)
q<-'SELECT SEQN,ALQ101,ALQ110,ALQ140Q,ALQ150
FROM ALQ_E
;'
ALQ_E<-sqldf(q)
ALQ_F[]<- lapply(ALQ_F, as.numeric)
q<-'SELECT SEQN,ALQ101,ALQ110,ALQ140Q,ALQ150
FROM ALQ_F
;'
ALQ_F<-sqldf(q)
ALQ_G[]<- lapply(ALQ_G, as.numeric)
q<-'SELECT SEQN,ALQ101,ALQ110,ALQ141Q AS ALQ140Q,ALQ151 AS ALQ150
FROM ALQ_G
;'
ALQ_G<-sqldf(q)
ALQ_H[]<- lapply(ALQ_H, as.numeric)
q<-'SELECT SEQN,ALQ101,ALQ110,ALQ141Q AS ALQ140Q,ALQ151 AS ALQ150
FROM ALQ_H
;'
ALQ_H<-sqldf(q)

alq<-rbind(ALQ_D,ALQ_E,ALQ_F,ALQ_G,ALQ_H)

#Drug Use
DUQ_D[]<- lapply(DUQ_D, as.numeric)
q<-'SELECT SEQN,DUQ200,DUQ240,DUQ250,DUQ290,DUQ330,DUQ370
FROM DUQ_D
;'
DUQ_D<-sqldf(q)
DUQ_E[]<- lapply(DUQ_E, as.numeric)
q<-'SELECT SEQN,DUQ200,DUQ240,DUQ250,DUQ290,DUQ330,DUQ370
FROM DUQ_E
;'
DUQ_E<-sqldf(q)
DUQ_F[]<- lapply(DUQ_F, as.numeric)
q<-'SELECT SEQN,DUQ200,DUQ240,DUQ250,DUQ290,DUQ330,DUQ370
FROM DUQ_F
;'
DUQ_F<-sqldf(q)
DUQ_G[]<- lapply(DUQ_G, as.numeric)
q<-'SELECT SEQN,DUQ200,DUQ240,DUQ250,DUQ290,DUQ330,DUQ370
FROM DUQ_G
;'
DUQ_G<-sqldf(q)
DUQ_H[]<- lapply(DUQ_H, as.numeric)
q<-'SELECT SEQN,DUQ200,DUQ240,DUQ250,DUQ290,DUQ330,DUQ370
FROM DUQ_H
;'
DUQ_H<-sqldf(q)

duq<-rbind(DUQ_D,DUQ_E,DUQ_F,DUQ_G,DUQ_H)

#Sexual Behavior
SXQ_D[]<- lapply(SXQ_D, as.numeric)
q<-'SELECT SEQN,SXQ450,SXQ490,SXQ510,SXQ550,SXQ590,SXQ600,SXQ260,SXQ265,SXQ270,SXQ272,SXQ101,SXQ130,SXD171,SXQ410,SXQ021
FROM SXQ_D
;'
SXQ_D<-sqldf(q)
SXQ_E[]<- lapply(SXQ_E, as.numeric)
q<-'SELECT SEQN,SXQ450,SXQ490,SXQ510,SXQ550,SXQ590,SXQ600,SXQ260,SXQ265,SXQ270,SXQ272,SXQ101,SXQ130,SXD171,SXQ410,SXQ021
FROM SXQ_E
;'
SXQ_E<-sqldf(q)
SXQ_F[]<- lapply(SXQ_F, as.numeric)
q<-'SELECT SEQN,SXD450 AS SXQ450,SXQ490,SXD510 AS SXQ510,SXQ550,SXQ590,SXQ600,SXQ260,SXQ265,SXQ270,SXQ272,SXD101 AS SXQ101,SXQ130,SXD171,SXQ410,SXD021 AS SXQ021
FROM SXQ_F
;'
SXQ_F<-sqldf(q)
SXQ_G[]<- lapply(SXQ_G, as.numeric)
q<-'SELECT SEQN,SXD450 AS SXQ450,SXQ490,SXD510 AS SXQ510,SXQ550,SXQ590,SXQ600,SXQ260,SXQ265,SXQ270,SXQ272,SXD101 AS SXQ101,SXQ130,SXD171,SXQ410,SXD021 AS SXQ021
FROM SXQ_G
;'
SXQ_G<-sqldf(q)
SXQ_H[]<- lapply(SXQ_H, as.numeric)
q<-'SELECT SEQN,SXD450 AS SXQ450,SXQ490,SXD510 AS SXQ510,SXQ550,SXQ590,SXQ600,SXQ260,SXQ265,SXQ270,SXQ272,SXD101 AS SXQ101,SXQ130,SXD171,SXQ410,SXD021 AS SXQ021
FROM SXQ_H
;'
SXQ_H<-sqldf(q)

sxq<-rbind(SXQ_D,SXQ_E,SXQ_F,SXQ_G,SXQ_H)

#HSV-2
HSV_D[]<- lapply(HSV_D, as.numeric)
q<-'SELECT SEQN,LBXHE2
FROM HSV_D
;'
HSV_D<-sqldf(q)
HSV_E[]<- lapply(HSV_E, as.numeric)
q<-'SELECT SEQN,LBXHE2
FROM HSV_E
;'
HSV_E<-sqldf(q)
HSV_F[]<- lapply(HSV_F, as.numeric)
q<-'SELECT SEQN,LBXHE2
FROM HSV_F
;'
HSV_F<-sqldf(q)
HSV_G[]<- lapply(HSV_G, as.numeric)
q<-'SELECT SEQN,LBXHE2
FROM HSV_G
;'
HSV_G<-sqldf(q)
HSV_H[]<- lapply(HSV_H, as.numeric)
q<-'SELECT SEQN,LBXHE2
FROM HSV_H
;'
HSV_H<-sqldf(q)

hsv<-rbind(HSV_D,HSV_E,HSV_F,HSV_G,HSV_H)

#Merge together
q<-'SELECT *
FROM demo
LEFT OUTER JOIN uhm ON demo.SEQN=uhm.SEQN
LEFT OUTER JOIN alq ON demo.SEQN=alq.SEQN
LEFT OUTER JOIN duq ON demo.SEQN=duq.SEQN
LEFT OUTER JOIN sxq ON demo.SEQN=sxq.SEQN
LEFT OUTER JOIN hsv ON demo.SEQN=hsv.SEQN
;'
dat<-sqldf(q)

#Recoding
#Demographics
#AGE
dat$ageg[dat$RIDAGEYR>=20 & dat$RIDAGEYR<=29]<-1
dat$ageg[dat$RIDAGEYR>=30 & dat$RIDAGEYR<=39]<-2
dat$ageg[dat$RIDAGEYR>=40 & dat$RIDAGEYR<=49]<-3
dat$ageg[dat$RIDAGEYR>=50 & dat$RIDAGEYR<=59]<-4
#RACE, 1=white, 2=black, 3=others
dat$race[dat$RIDRETH1==3]<-1
dat$race[dat$RIDRETH1==4]<-2
dat$race[dat$RIDRETH1 %in% c(1,2,5)]<-3
#EDUCATION, 1=<hs, 2=hs, 3=>hs
dat$edu[dat$DMDEDUC2 %in% c(1,2)]<-1
dat$edu[dat$DMDEDUC2 %in% c(3)]<-2
dat$edu[dat$DMDEDUC2 %in% c(4,5)]<-3
#MARITAL, 1=yes, 0=no
dat$marry[dat$DMDMARTL %in% c(1)]<-1
dat$marry[dat$DMDMARTL %in% c(2,3,4,5,6)]<-0
#PIR, 1=<1, 2=1-2, 3=>=2
dat$pir[dat$INDFMPIR>=0 & dat$INDFMPIR<1]<-1
dat$pir[dat$INDFMPIR>=1 & dat$INDFMPIR<2]<-2
dat$pir[dat$INDFMPIR>=2]<-3

#Urinary Lead
#creatinine adjusted urinary lead (ng/mg)
dat$cul<-dat$URXUPB/(0.01*dat$URXUCR)
#log creatinine adjusted urinary lead
dat$logcul<-log(dat$cul)
#quartile creatinine adjusted urinary lead
dat$qcul<-as.integer(cut(dat$cul, quantile(dat$cul, probs=c(0,0.25,0.5,0.75,1), na.rm=TRUE), include.lowest=TRUE))

#Alcohol Use
#Any binge drinking in the past 12 months
dat$anybinge[dat$ALQ101==2]<-0
dat$anybinge[(dat$ALQ101==1)&(dat$ALQ140Q==0)]<-0
dat$anybinge[(dat$ALQ101==1)&(dat$ALQ140Q>0)&(dat$ALQ140Q<=365)]<-1
#Frequent binge drinking in the past 12 months (>=12 episodes)
dat$frebinge[dat$anybinge==0]<-0
dat$frebinge[(dat$anybinge==1)&(dat$ALQ140Q<12)]<-0
dat$frebinge[(dat$anybinge==1)&(dat$ALQ140Q>=12)]<-1
#Lifetime frequent binge drinking
dat$ltfrebinge[(dat$ALQ101==2)&(dat$ALQ110==2)]<-0
dat$ltfrebinge[((dat$ALQ101==1)|(dat$ALQ110==1))&(dat$ALQ150==2)]<-0
dat$ltfrebinge[((dat$ALQ101==1)|(dat$ALQ110==1))&(dat$ALQ150==1)]<-1

#Drug Use
#Marijuana
dat$marijuana[dat$DUQ200==1]<-1
dat$marijuana[dat$DUQ200==2]<-0
#Cocaine
dat$cocaine[dat$DUQ240==2]<-0
dat$cocaine[(dat$DUQ240==1)&(dat$DUQ250==2)]<-0
dat$cocaine[(dat$DUQ240==1)&(dat$DUQ250==1)]<-1
#Heroin
dat$heroin[dat$DUQ240==2]<-0
dat$heroin[(dat$DUQ240==1)&(dat$DUQ290==2)]<-0
dat$heroin[(dat$DUQ240==1)&(dat$DUQ290==1)]<-1
#Methamphetamine
dat$metha[dat$DUQ240==2]<-0
dat$metha[(dat$DUQ240==1)&(dat$DUQ330==2)]<-0
dat$metha[(dat$DUQ240==1)&(dat$DUQ330==1)]<-1
#Injection Drug Use
dat$injection[dat$DUQ370==2]<-0
dat$injection[dat$DUQ370==1]<-1
#Previous Drug Use
dat$druguse[(dat$marijuana==0)&(dat$DUQ240==2)&(dat$injection==0)]<-0
dat$druguse[(dat$marijuana==1)|(dat$DUQ240==1)|(dat$injection==1)]<-1

#Sexual Behavior
#Multiple Partners
dat$SXQ450[dat$SXQ450 %in% c(77777,99999)]<-NA
dat$SXQ490[dat$SXQ490 %in% c(77777,99999)]<-NA
dat$SXQ510[dat$SXQ510 %in% c(77777,99999)]<-NA
dat$SXQ550[dat$SXQ550 %in% c(77777,99999)]<-NA
dat$SXQ600[dat$SXQ600 %in% c(77777,99999)]<-NA
dat$SXQ590[dat$SXQ590 %in% c(77777,99999)]<-NA

dat$SXQ450[(dat$SXQ101==0)|(dat$SXQ021==2)]<-0
dat$SXQ490[(dat$SXQ130==0)|(dat$SXQ021==2)]<-0
dat$SXQ510[(dat$SXD171==0)|(dat$SXQ021==2)]<-0
dat$SXQ550[(dat$SXQ410==0)|(dat$SXQ021==2)]<-0

dat$numpartner[(dat$RIAGENDR==1)&(!is.na(dat$SXQ510))&(!is.na(dat$SXQ550))]<-dat$SXQ510[(dat$RIAGENDR==1)&(!is.na(dat$SXQ510))&(!is.na(dat$SXQ550))]+dat$SXQ550[(dat$RIAGENDR==1)&(!is.na(dat$SXQ510))&(!is.na(dat$SXQ550))]
dat$numpartner[(dat$RIAGENDR==1)&(!is.na(dat$SXQ510))&(is.na(dat$SXQ550))]<-dat$SXQ510[(dat$RIAGENDR==1)&(!is.na(dat$SXQ510))&(is.na(dat$SXQ550))]
dat$numpartner[(dat$RIAGENDR==1)&(is.na(dat$SXQ510))&(!is.na(dat$SXQ550))]<-dat$SXQ550[(dat$RIAGENDR==1)&(is.na(dat$SXQ510))&(!is.na(dat$SXQ550))]

dat$numpartner[(dat$RIAGENDR==2)&(!is.na(dat$SXQ450))&(!is.na(dat$SXQ490))]<-dat$SXQ450[(dat$RIAGENDR==2)&(!is.na(dat$SXQ450))&(!is.na(dat$SXQ490))]+dat$SXQ490[(dat$RIAGENDR==2)&(!is.na(dat$SXQ450))&(!is.na(dat$SXQ490))]
dat$numpartner[(dat$RIAGENDR==2)&(!is.na(dat$SXQ450))&(is.na(dat$SXQ490))]<-dat$SXQ450[(dat$RIAGENDR==2)&(!is.na(dat$SXQ450))&(is.na(dat$SXQ490))]
dat$numpartner[(dat$RIAGENDR==2)&(is.na(dat$SXQ450))&(!is.na(dat$SXQ490))]<-dat$SXQ490[(dat$RIAGENDR==2)&(is.na(dat$SXQ450))&(!is.na(dat$SXQ490))]

dat$mp[dat$numpartner>1]<-1
dat$mp[dat$numpartner %in% c(0,1)]<-0
#Sex Partners Five Years Younger
dat$spyoung[dat$SXQ600>=1]<-1
dat$spyoung[(dat$SXQ600==0)|(dat$numpartner==0)]<-0
#Sex Partners Five Years Older
dat$spold[dat$SXQ590>=1]<-1
dat$spold[(dat$SXQ590==0)|(dat$numpartner==0)]<-0
#Self-reported STI
dat$sti[(dat$SXQ260==1)|(dat$SXQ265==1)|(dat$SXQ270==1)|(dat$SXQ272==1)]<-1
dat$sti[(dat$SXQ260==2)&(dat$SXQ265==2)&(dat$SXQ270==2)&(dat$SXQ272==2)]<-0

#HSV-2
dat$hsv2[dat$LBXHE2==1]<-1
dat$hsv2[dat$LBXHE2==2]<-0

#Weight
dat$weight<-dat$WTSA2YR/5

##########
#Analysis#
##########
#Exclusion
dat2<-dat[(!is.na(dat$cul))&(!is.na(dat$ageg))&(!is.na(dat$weight)),]
#survey design
design <- svydesign(id = ~ SDMVPSU, strata = ~ SDMVSTRA, nest = TRUE, weight = ~ weight, data = dat2)

#Alcohol Use
model1<-svyglm(anybinge ~ factor(qcul)+factor(ageg)+factor(race)+factor(RIAGENDR)+factor(edu)+factor(pir)+factor(marry), design = design, family=quasibinomial())
cbind(exp(coef(model1)),exp(confint(model1)))

model2<-svyglm(frebinge ~ factor(qcul)+factor(ageg)+factor(race)+factor(RIAGENDR)+factor(edu)+factor(pir)+factor(marry), design = design, family=quasibinomial())
cbind(exp(coef(model2)),exp(confint(model2)))

model3<-svyglm(ltfrebinge ~ factor(qcul)+factor(ageg)+factor(race)+factor(RIAGENDR)+factor(edu)+factor(pir)+factor(marry), design = design, family=quasibinomial())
cbind(exp(coef(model3)),exp(confint(model3)))

#Drug Use
model4<-svyglm(druguse ~ factor(qcul)+factor(ageg)+factor(race)+factor(RIAGENDR)+factor(edu)+factor(pir)+factor(marry), design = design, family=quasibinomial())
cbind(exp(coef(model4)),exp(confint(model4)))

model5<-svyglm(injection ~ factor(qcul)+factor(ageg)+factor(race)+factor(RIAGENDR)+factor(edu)+factor(pir)+factor(marry), design = design, family=quasibinomial())
cbind(exp(coef(model5)),exp(confint(model5)))

model6<-svyglm(marijuana ~ factor(qcul)+factor(ageg)+factor(race)+factor(RIAGENDR)+factor(edu)+factor(pir)+factor(marry), design = design, family=quasibinomial())
cbind(exp(coef(model6)),exp(confint(model6)))

model7<-svyglm(cocaine ~ factor(qcul)+factor(ageg)+factor(race)+factor(RIAGENDR)+factor(edu)+factor(pir)+factor(marry), design = design, family=quasibinomial())
cbind(exp(coef(model7)),exp(confint(model7)))

model8<-svyglm(heroin ~ factor(qcul)+factor(ageg)+factor(race)+factor(RIAGENDR)+factor(edu)+factor(pir)+factor(marry), design = design, family=quasibinomial())
cbind(exp(coef(model8)),exp(confint(model8)))

model9<-svyglm(metha ~ factor(qcul)+factor(ageg)+factor(race)+factor(RIAGENDR)+factor(edu)+factor(pir)+factor(marry), design = design, family=quasibinomial())
cbind(exp(coef(model9)),exp(confint(model9)))

#Sexual Behavior
model10<-svyglm(mp ~ factor(qcul)+factor(ageg)+factor(race)+factor(RIAGENDR)+factor(edu)+factor(pir)+factor(marry), design = design, family=quasibinomial())
cbind(exp(coef(model10)),exp(confint(model10)))

model11<-svyglm(spyoung ~ factor(qcul)+factor(ageg)+factor(race)+factor(RIAGENDR)+factor(edu)+factor(pir)+factor(marry), design = design, family=quasibinomial())
cbind(exp(coef(model11)),exp(confint(model11)))

model12<-svyglm(spold ~ factor(qcul)+factor(ageg)+factor(race)+factor(RIAGENDR)+factor(edu)+factor(pir)+factor(marry), design = design, family=quasibinomial())
cbind(exp(coef(model12)),exp(confint(model12)))

model13<-svyglm(sti ~ factor(qcul)+factor(ageg)+factor(race)+factor(RIAGENDR)+factor(edu)+factor(pir)+factor(marry), design = design, family=quasibinomial())
cbind(exp(coef(model13)),exp(confint(model13)))

model14<-svyglm(hsv2 ~ factor(qcul)+factor(ageg)+factor(race)+factor(RIAGENDR)+factor(edu)+factor(pir)+factor(marry), design = design, family=quasibinomial())
cbind(exp(coef(model14)),exp(confint(model14)))