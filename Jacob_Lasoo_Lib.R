library(glmnet)

pass <- function(x){x}

folds_overvec <- sample(rep(1:10, length = nrow(CyanoMat)), replace = FALSE)
folds_overvec2 <- sample(rep(1:10, length = nrow(CyanoMat)), replace = FALSE)

## ll = lower.limits
ymtx_lasoo <- function(Y, X, lam, ll = 0){
  CoefMtx <- matrix(nrow = ncol(X), ncol = ncol(Y), dimnames = list(colnames(X), colnames(Y)))
  nY = ncol(Y)
  for (iter in 1:nY){
      fit_loc <- glmnet(y = Y[,iter], x = X, family = "gaussian", lower.limits = ll)
    coef_loc00 <- as.matrix(coef(fit_loc, s = lam))#[2:ncol(X),]
    coef_loc01 <- coef_loc00[2:(ncol(X) + 1),]
    CoefMtx[,iter] <- coef_loc01
  }
  CoefMtx
}

ymtx_jcv <- function(Y, X, folds = 10, lam = 0.1, ll = 0, folds_vec = folds_overvec, return.list = FALSE){
  if(nrow(X) != nrow(Y)){stop("X and Y matrices must have the same number of rows")}
  #folds_vec = sample(rep(1:10, length = nrow(X)), replace = FALSE)
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
      fit_loc <- glmnet(y = Y_train[,iter], x = X_train, family = "gaussian", lower.limits = 0)
      y_pred <- predict.glmnet(fit_loc, X_test, s = lam)
      rmse_loc <- sum((y_pred - Y_test[,iter])^2)
      rmse_fold[iter] <- rmse_loc
    }
    rmse_allY_fold <- mean(rmse_fold)
    rmse_folds_vec[loc_fold] <- rmse_allY_fold
  }
  rmse_overall <- mean(rmse_folds_vec)
  rmse_se <- sd(rmse_folds_vec)
  if(return.list){return(list(rmse_mean = rmse_overall, rmse_se = rmse_se, rmse_all = rmse_folds_vec))}
  return(rmse_overall)
}

opt_lam_fn <- function(Y, X, folds_vec, ll = 0){
  optimize(function(x){
  ymtx_jcv(X, Y, lam = 10^x, folds_vec = folds_vec, ll = ll)
  }, interval = c(-3, 3))
}

xy_common_lasoo <- function(Y, X, folds = 10, ll = 0){
  folds_vec = sample(rep(1:folds, length = nrow(X)), replace = FALSE)
  OptimalLambdaSet <- opt_lam_fn(Y, X, folds_vec, ll = ll)
  print(OptimalLambdaSet)
  OptimalLambda <- OptimalLambdaSet$minimum
  ymtx_lasoo(Y, X, OptimalLambda, ll = ll)
}


make_nodes_table <- function(VirusNames, CyanoNames, EnvNames){
  
  VirNodes <- data.frame(Node = VirusNames, Type = "Virus")
  CyanoNodes <- data.frame(Node = CyanoNames, Type = "Cyano")
  EnvNodes <- data.frame(Node = EnvNames, Type = "Env")
  
  Nodes <- bind_rows(VirNodes, CyanoNodes, EnvNodes) %>%
    mutate(shape = case_when(Type == "Virus" ~ "vrectangle", Type == "Cyano" ~ "circle", Type == "Env" ~ "square")) %>%
    mutate(color = case_when(Type == "Virus" ~ "white", Type == "Cyano" ~ "green", Type == "Env" ~ "blue")) %>%
    mutate(label = str_remove(Node, "ocean_region_"),
           label = str_remove(label, "depth_regime_"),
           label = str_remove(label, "oxygen_category_"),
           label = str_remove(label, "_reads")
    )
  
  Nodes
}

make_edges_table <- function(CoefMtx){
  CoefDf <- as_tibble(CoefMtx, rownames = "Cyano")
  Edges <- CoefDf %>% pivot_longer(cols = -Cyano, names_to = "Vir", values_to = "Coef") %>%
    filter(Coef !=0) %>%
    mutate(color = if_else(Coef>0, "black", "red")) %>%
    mutate(lty = if_else(Coef > 0, 1, 2)) %>%
    mutate(width = sqrt(abs(Coef)) * 5) %>%
    pass
  Edges
}

only_strong_cyano_edges <- function(Edges){
  Edges2 <- Edges %>% #[starts_with(Edges$Cyano, "[:upper:]"),] %>%
    filter(str_starts(Cyano, "[:upper:]")) %>%
    mutate(width = sqrt(abs(Coef)) * 2) %>%
    pass
  Edges2
}

# only_strong_env_edges <- function(Edges){
#   Edges2 <- Edges[!str_detect(Edges$Cyano, "reads"),] %>%
#     #mutate(width = sqrt(abs(Coef)) * 20) %>%
#     pass
#   Edges2
# }

only_cyano_nodes <- function(Nodes){
  
  Nodes2 <- Nodes %>%
    filter(Type != "Env")
  
  Nodes2
}

no_cyano_nodes <- function(Nodes){
  
  Nodes2 <- Nodes %>%
    filter(Type != "Cyano")
  
  Nodes2
}
