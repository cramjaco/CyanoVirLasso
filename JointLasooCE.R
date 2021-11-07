# first I want to be able to run lasoo with a specified lambda with a matrix of common y variables
# then I want to be able to predict values and calculate mse
# then I want to cross validate the optimal lambda

# VirMatSm <- VirMat[1:20, 1:5]
# CyanoMatSm <- CyanoMat[1:10, 1:5]

source("Bring_In_Data.R")
source("Jacob_Lasoo_Lib.R")


X = CyanoEnvMat
Y = VirMat



test <- ymtx_lasoo(VirMat, CyanoEnvMat, 0)
test


ymtx_jcv(VirMat, CyanoEnvMat, lam = 0.01)

try_lambdas = seq(from = 0, to = .1, by = 0.001)

# pt0 <- proc.time()
# try_rmse <- map_dbl(try_lambdas, ~ymtx_jcv(VirMat, CyanoEnvMat, lam = .))
# pt1 <- proc.time()
# pt1 - pt0

# pt0 <- proc.time()
# try_rmse2 <- map_dbl(try_lambdas, ~ymtx_jcv(VirMat, CyanoMat, lam = ., folds_vec = folds_overvec2))
# pt1 <- proc.time()
# pt1 - pt0

#plot(try_lambdas, try_rmse)
#points(try_lambdas, try_rmse2, pch = 2)


opt_lam <- optimize(function(x){
  ymtx_jcv(VirMat, CyanoEnvMat, lam = x, folds_vec = folds_overvec)
  }, interval = c(0, 0.1))
opt_lam2 <- optimize(function(x){
  ymtx_jcv(VirMat, CyanoEnvMat, lam = x, folds_vec = folds_overvec2)
  }, interval = c(0, 0.1))

# some variabiltiy between opt_lam and opt_lam2, but I'll live
#lambda ~ 0.03

CoefMtx <- ymtx_lasoo(VirMat, CyanoEnvMat, opt_lam$minimum)

## Show network

SignMtx <- sign(CoefMtx)

library(igraph)

CoefDf <- as_tibble(CoefMtx, rownames = "Cyano")
SignDf <- as_tibble(SignMtx, rownames = "Cyano")
Edges <- CoefDf %>% pivot_longer(cols = -Cyano, names_to = "Vir", values_to = "Coef") %>%
  filter(Coef !=0) %>%
  mutate(color = if_else(Coef>0, "black", "red")) %>%
  mutate(lty = if_else(Coef > 0, 1, 2)) %>%
  mutate(width = sqrt(abs(Coef)) * 5) %>%
  pass

VirNodes <- data.frame(Node = colnames(VirMat), Type = "Virus")
CyanoNodes <- data.frame(Node = colnames(CyanoMat), Type = "Cyano")
EnvNodes <- data.frame(Node = colnames(CyanoEnvMat)[!colnames(CyanoEnvMat) %in% colnames(CyanoMat)], Type = "Env")


Nodes <- bind_rows(VirNodes, CyanoNodes, EnvNodes) %>%
  mutate(shape = case_when(Type == "Virus" ~ "vrectangle", Type == "Cyano" ~ "circle", Type == "Env" ~ "square")) %>%
  mutate(color = case_when(Type == "Virus" ~ "white", Type == "Cyano" ~ "green", Type == "Env" ~ "blue")) %>%
  mutate(label = str_remove(Node, "ocean_region_"),
         label = str_remove(Node, "depth_regime_"),
         label = str_remove(Node, "oxygen_category_"),
         )

CVGraph <- graph_from_data_frame(d = Edges, directed = FALSE, vertices = Nodes)

plot(CVGraph, layout = layout_nicely(CVGraph), vertex.shape = V(CVGraph)$shape, vertex.size = 15)

Edges2 <- Edges[str_detect(Edges$Cyano, "reads"),] %>%
  mutate(width = sqrt(abs(Coef)) * 20)
Nodes2 <- Nodes %>%
  filter(Type != "Env")

CVGraph2 <- graph_from_data_frame(d = Edges2, directed = FALSE, vertices = Nodes2)

plot(CVGraph2, vertex.size = 15)
