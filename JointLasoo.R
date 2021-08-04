# first I want to be able to run lasoo with a specified lambda with a matrix of common y variables
# then I want to be able to predict values and calculate mse
# then I want to cross validate the optimal lambda

# VirMatSm <- VirMat[1:20, 1:5]
# CyanoMatSm <- CyanoMat[1:10, 1:5]

X = CyanoMat
Y = VirMat

library(glmnet)

ymtx_lasoo <- function(Y, X, lam){
  CoefMtx <- matrix(nrow = ncol(X), ncol = ncol(Y), dimnames = list(colnames(X), colnames(Y)))
  nY = ncol(Y)
  for (iter in 1:nY){
    fit_loc <- glmnet(y = Y[,iter], x = X, family = "gaussian", lower.limits = 0)
    coef_loc00 <- as.matrix(coef(fit_loc, s = lam))#[2:ncol(X),]
    coef_loc01 <- coef_loc00[2:(ncol(X) + 1),]
    CoefMtx[,iter] <- coef_loc01
  }
  CoefMtx
}

test <- ymtx_lasoo(VirMat, CyanoMat, 0)
test

ymtx_jcv <- function(Y, X, folds = 10, lam = 0.1){
  if(nrow(X) != nrow(Y)){stop("X and Y matrices must have the same number of rows")}
  folds_vec = sample(rep(1:10, length = nrow(X)), replace = FALSE)
  rmse_folds_vec <- vector(length = folds)
  for(loc_fold in 1:folds){
    # split the matrices
    X_test = X[folds_vec == loc_fold,]
    Y_test = Y[folds_vec == loc_fold,]
    X_train = X[folds_vec != loc_fold,]
    Y_train = Y[folds_vec != loc_fold,]
    
    # run the lasoo
    nY = ncol(Y_train)
    rmse_fold <- vector(length = nY)
    for(iter in 1:nY){
      fit_loc <- glmnet(y = Y_train[,iter], x = X_train)
      y_pred <- predict.glmnet(fit_loc, X_test, s = lam)
      rmse_loc <- sum((y_pred - Y_test[,iter])^2)
      rmse_fold[iter] <- rmse_loc
    }
    rmse_allY_fold <- mean(rmse_fold)
    rmse_folds_vec[loc_fold] <- rmse_allY_fold
  }
  rmse_overall <- mean(rmse_folds_vec)
  rmse_overall
}

ymtx_jcv(VirMat, CyanoMat, lam = 0.01)

try_lambdas = seq(from = 0, to = .1, by = 0.001)

pt0 <- proc.time()
try_rmse <- map_dbl(try_lambdas, ~ymtx_jcv(VirMat, CyanoMat, lam = .))
pt1 <- proc.time()
pt1 - pt0

plot(try_lambdas, try_rmse)
