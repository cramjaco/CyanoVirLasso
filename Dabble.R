source("Bring_In_Data.R")

library(glmnet)

## Only take overlapping rows

fit <- glmnet(y = VirMat, x = CyanoMat, family = "mgaussian")
plot(fit)
coef(fit, s = 0.1)

cvfit <- cv.glmnet(y = VirMat, x = CyanoMat, family = "mgaussian") # takes about a minute with one core
plot(cvfit)


cvfit$lambda.min
cvfit$lambda.1se

c1se <- coef(cvfit, s = "lambda.1se")

c1se$pur_c

length(c1se$pur_c@x)
length(c1se)
length(colnames(CyanoEnvMat)) # longer than the number of things in each variable, or some reason
length(colnames(VirMat))

assocMtx <- matrix(nrow = dim(as.matrix(c1se$pur_c))[1], ncol = length(c1se), dimnames = list(rownames(c1se$pur_c), names(c1se)))

for(iter in 1:length(c1se)){
  assocMtx[,iter] <- as.vector(c1se[[iter]])
}

assocMtx !=0
# Uh oh. All viruses are predicted by the same thing.
# Maybe I've misunderstood how this works.
# True for both reads and proportions.
