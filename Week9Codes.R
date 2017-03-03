#install.packages("bkmr")
#install.packages("ggplot2")

library(bkmr)
library(ggplot2)

setwd("//file.phhp.ufl.edu/home/huihu/files/Teaching/PHC6711/Codes/PHC6711/dat/")

#import data
dat1 <- read.csv("dat1.csv", header = TRUE) 

#set up the data
y <- dat1$Y
expos <- scale(dat1[, paste0("X", 1:7)])
covar <- matrix(dat1$Z, ncol = 1) 

#fit model
set.seed(1000) 
fitkm <- kmbayes(y = y, Z = expos, X = covar, iter = 500, verbose = FALSE, varsel = TRUE, control.params = list(r.jump=0.3,lambda.jump=20)) #r.jump specifies the SD of the proposal distriution for r_m, lambda.jump specifies the SD of the proposal distribution for lambda

#check convergence
TracePlot(fit = fitkm, par = "beta")

#estimated posterior inclusion probabilities (when varsel=True)
ExtractPIPs(fitkm)

#exposure-response function
#univaraite
pred.resp.univar <- PredictorResponseUnivar(fit = fitkm, q.fixed=0.5)  #univariate relationship between each exposure and the outcome, where all of the other exposures are fixed to a particular percentile (50th percentile by default)
ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + geom_smooth(stat = "identity") + facet_wrap(~ variable) + ylab("h(z)")

#bivariate
pred.resp.bivar <- PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
ggplot(pred.resp.bivar, aes(z1, z2, fill = est)) + geom_raster() + facet_grid(variable2 ~ variable1) + scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF")) + xlab("expos1") + ylab("expos2") + ggtitle("h(expos1, expos2)")

#sometimes it's hard to see what's going on in these contour plots. Another approach is to investigate univariate relationship by fixing the seond predictor at various quantiles
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, both_pairs = FALSE, Z = expos, qs = c(0.25, 0.5, 0.75))
ggplot(pred.resp.bivar.levels, aes(z1, est)) + geom_smooth(aes(col = quantile), stat = "identity") + facet_grid(variable2 ~ variable1) + ggtitle("h(expos1 | quantiles of expos2)") + xlab("expos1")

#summary statistics 

# compute the overall effect of the predictors by comparing the value of h when all of predictors are at a particular percentile as compared to when all of them are at their 50th percentile
risks.overall.approx <- OverallRiskSummaries(fit = fitkm, y = y, Z = expos, X = covar, qs = seq(0.25, 0.75, by = 0.05), q.fixed = 0.5)
risks.overall.approx
ggplot(risks.overall.approx, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + geom_pointrange()

# summarize the contribution of an individual predictors to the response by comparing the value of h when the exposure is at the 75th percentile to when it is at the 25th percentile, fixing the other predictors to a particular percentile
risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = y, Z = expos, X = covar, qs.diff = c(0.25, 0.75), q.fixed = c(0.25, 0.50, 0.75))
risks.singvar
ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd,  col = q.fixed)) + geom_pointrange(position = position_dodge(width = 0.75)) + coord_flip()

# evaluate interaction: compare the single-predictor health risks when all of the other predictors in Z are fixed to 75th percentile to when all of the other predictors in Z are fixed to their 25th percentile
risks.int <- SingVarIntSummaries(fit = fitkm, y = y, Z = expos, X = covar,qs.diff = c(0.25, 0.75), qs.fixed = c(0.25, 0.75))
risks.int


