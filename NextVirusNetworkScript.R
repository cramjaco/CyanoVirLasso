source("Bring_In_Data.R")
source("Jacob_Lasoo_Lib.R")

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

EdgesEnv <- CoefMtx %>% make_edges_table() %>% only_strong_env_edges()
NodesEnv <- make_nodes_table(colnames(VirMat), colnames(CyanoMat), EnvNames) %>% no_cyano_nodes()

CVGraphEnv <- graph_from_data_frame(d = EdgesEnv, directed = FALSE, vertices = NodesEnv)

plot(CVGraphEnv, vertex.size = 15)

