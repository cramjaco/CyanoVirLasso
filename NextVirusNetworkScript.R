source("Bring_In_Data.R")
source("Jacob_Lasoo_Lib.R")
library(igraph)

# Debug

# Run calculation
CoefMtx <- xy_common_lasoo(Y = VirMat, X = CyanoEnvMat)


## Processing


EnvNames <- colnames(EnvTransformed)[-1]
CyanoNames <- colnames(CyanoTransformed)[-1]

Edges <- CoefMtx %>% make_edges_table() %>% only_strong_cyano_edges()
Nodes <- make_nodes_table(colnames(VirMat),CyanoNames, EnvNames) %>% only_cyano_nodes()

## Plotting

CVGraph2 <- graph_from_data_frame(d = Edges, directed = FALSE, vertices = Nodes)

plot(CVGraph2, vertex.size = 15)

tkplot(CVGraph2, vertex.size = 15)

## Enviromental Network

EdgesEnv <- CoefMtx %>% make_edges_table() #%>% only_strong_env_edges()
NodesEnv <- make_nodes_table(colnames(VirMat), colnames(CyanoMat), EnvNames) #%>% no_cyano_nodes()

CVGraphEnv <- graph_from_data_frame(d = EdgesEnv, directed = FALSE, vertices = NodesEnv)

plot(CVGraphEnv, vertex.size = 15)

## Explore Lambda
folds_overvec <- sample(rep(1:10, length = nrow(CyanoMat)), replace = FALSE)
Lambda_Values <- 10^seq(from = -2, to = 0, by = 0.1)
OutVec <- rep(NA, length(Lambda_Values))
for(iter in 1:length(Lambda_Values)){
   out <- ymtx_jcv(VirMat, CyanoEnvMat, lam = Lambda_Values[iter], folds_vec = folds_overvec)
   print(c(log10(Lambda_Values[iter]), out))
   OutVec[iter] <- out
}

Lambda_Test_Df <- tibble(lambda = Lambda_Values)
Lambda_Test_Df <- Lambda_Test_Df %>%
  mutate(score = purrr::map(lambda, ~ymtx_jcv(VirMat, CyanoEnvMat, ., folds_overvec)))

## Set higher thresholds

Test_Lambda <- .14

# Run calculation
CoefMtx <- ymtx_lasoo(Y = VirMat, X = CyanoEnvMat, Test_Lambda)


## Processing


EnvNames <- colnames(EnvTransformed)[-1]
CyanoNames <- colnames(CyanoTransformed)[-1]

Edges <- CoefMtx %>% make_edges_table() %>% only_strong_cyano_edges()
Nodes <- make_nodes_table(colnames(VirMat),CyanoNames, EnvNames) %>% only_cyano_nodes()

## Plotting

CVGraph_Higher <- graph_from_data_frame(d = Edges, directed = FALSE, vertices = Nodes)

plot(CVGraph_Higher)
tkplot(CVGraph_Higher, vertex.size = 15)
tkCoords <- tkplot.getcoords(2, norm = FALSE)
plot(CVGraph_Higher, vertex.size = 15, layout = tkCoords)

svg("CyanoVirLambda0.2.svg")
plot(CVGraph_Higher, vertex.size = 15, layout = tkCoords)
dev.off()

## Something very strange is happening

save.image("x08Nov2021.Rdata")
