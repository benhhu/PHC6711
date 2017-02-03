install.packages("psych")
install.packages("irr")
library(psych)
library(irr)

data(diagnoses)
dat1<-diagnoses[,1:2]
head(dat1)

data(anxiety)
dat2<-anxiety[,1:2]
head(dat2)

# Kappa and weighted Kappa

#Inter-Rater
cohen.kappa(dat1)
cohen.kappa(dat2)

#data matrix taken from Cohen
cohen <- matrix(c(
  0.44, 0.07, 0.09,
  0.05, 0.20, 0.05,
  0.01, 0.03, 0.06),ncol=3,byrow=TRUE)

#cohen.weights  weight differences
cohen.weights <- matrix(c(
  0,1,3,
  1,0,6,
  3,6,0),ncol=3)

cohen.kappa(cohen,cohen.weights,n.obs=200)

# ICC
ICC(dat2)
#ICC1 - one-way random (R1)
#ICC2 - two-way random (R3)
#ICC3 - two-way fixed (R2)
#ICC1k,ICC2k,ICC3k: reliability is calculated by taking an average of the k raters measurements (R4)

