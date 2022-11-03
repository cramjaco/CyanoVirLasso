xy_common_lasoo <- function(Y, X, folds = 10){
  folds_vec = sample(rep(1:folds, length = nrow(X)), replace = FALSE)
  OptimalLambdaSet <- opt_lam_fn(Y, X, folds_vec)
  print(OptimalLambdaSet)
  OptimalLambda <- OptimalLambdaSet$minimum
  ymtx_lasoo(Y, X, OptimalLambda)
}

opt_lam_fn <- function(Y, X, folds_vec){
  optimize(function(x){
  ymtx_jcv(Y, X, lam = 10^x, folds_vec = folds_vec)
  }, interval = c(-3, 3))
}

opt1 <- opt_lam_fn(VirMat, CyanoEnvMat, folds_overvec)
opt1

ymtx_jcv(VirMat, CyanoEnvMat, lam = 10^opt1$minimum, return.list = TRUE)


## Range of lambda

folds_overvec <- sample(rep(1:10, length = nrow(CyanoMat)), replace = FALSE)
Lambda_Values <- 10^seq(from = -2, to = 0, by = 0.1)
OutVec <- vector(mode = "list", length = length(Lambda_Values))
for(iter in 1:length(Lambda_Values)){
   out <- ymtx_jcv(VirMat, CyanoEnvMat, lam = Lambda_Values[iter], folds_vec = folds_overvec, return.list = TRUE)
   print(c(log10(Lambda_Values[iter]), out))
   OutVec[[iter]] <- out
}

rmsTib <- tibble(lambda = Lambda_Values, rmse = map_dbl(OutVec, ~.[[1]]), sd =  map_dbl(OutVec, ~.[[2]]))

rmsTib %>% ggplot(aes(x = log10(lambda), y = rmse, ymin = rmse - sd, ymax = rmse + sd)) + geom_point() + geom_errorbar()

optList <- ymtx_jcv(VirMat, CyanoEnvMat, lam = 10^opt1$minimum, return.list = TRUE)

targetRmse <- optList$rmse_mean + optList$rmse_se

jfun <- function(x){
  (ymtx_jcv(VirMat, CyanoEnvMat, lam = 10^x, folds_vec = folds_overvec) - targetRmse)^2
}

testSeq <- seq(from = opt1$minimum,to = 2, by = 0.1)
jout = map_dbl(testSeq, jfun)
plot(testSeq, jout)

testSeq <- seq(from = 1,to = 5, by = 0.5)
jout = map_dbl(testSeq, jfun)
plot(testSeq, jout)

# optimize(function(x){
#   (ymtx_jcv(VirMat, CyanoEnvMat, lam = 10^x, folds_vec = folds_overvec) - targetRmse)^2
# }, interval = c(opt1$minimum, 5))
# # this is wrong

plus1se <- optimize(jfun, interval = c(opt1$minimum, 5))  

10^plus1se$minimum # this is my new lambda

# this looks promising + 1se may get me there.
# now I just need to make it happen

## here are the steps
# Get the mean and 1se of the optimum
# using the optimize function find the function who has rmse closest that value in the interval between opt1$minimum and infinity




# Lambda_Test_Df <- tibble(lambda = Lambda_Values)
# Lambda_Test_Df <- Lambda_Test_Df %>%
#   mutate(score = purrr::map(lambda, ~ymtx_jcv(VirMat, CyanoEnvMat, ., folds_overvec)))
