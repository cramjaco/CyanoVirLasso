source(here::here("Bring_In_Data.R"))
source(here::here("Jacob_Lasoo_Lib.R"))
library(igraph)

## Functions
delete_isolated <- function(G){
  Isolated = which(degree(G) == 0)
  G2 = delete.vertices(G, Isolated)
}

# Debug

# Run calculation
CoefMtx <- xy_common_lasoo(Y = VirMat, X = CyanoEnvMat, ll = 0) # breaks if there are columns with only one zero in CyanoEnvMat
# why did this stop working?
# And after I ran the hacking script, it magically started working again?
# In any case, suboxic and mesopelagic have only one non-zero in them and thats why they like to fail. I have cut them out above

## Processing


EnvNames <- colnames(EnvTransformed)[-1]
CyanoNames <- colnames(CyanoTransformed)[-1]

Edges <- CoefMtx %>% make_edges_table() %>% only_strong_cyano_edges()
Nodes <- make_nodes_table(colnames(VirMat),CyanoNames, EnvNames) %>% only_cyano_nodes()

## Plotting



CVGraph2 <- graph_from_data_frame(d = Edges, directed = FALSE, vertices = Nodes)

CVGraph2_Connected <- delete_isolated(CVGraph2)

plot(CVGraph2_Connected, vertex.size = 15)

tkp1 <- tkplot(CVGraph2_Connected, vertex.size = 15)
tkCoords1 <- tkplot.getcoords(tkp1, norm = FALSE)
plot(CVGraph2_Connected)

## Enviromental Network

EdgesEnv <- CoefMtx %>% make_edges_table() #%>% only_strong_env_edges()
#NodesEnv <- make_nodes_table(colnames(VirMat), colnames(CyanoMat), EnvNames) #%>% no_cyano_nodes()
# Hardcoded devide, which I don't love
NodesEnv <- make_nodes_table(colnames(VirMat), colnames(CyanoEnvMat)[c(1:8)], colnames(CyanoEnvMat)[-c(1:8)])

CVGraphEnv <- graph_from_data_frame(d = EdgesEnv, directed = FALSE)
# not working for now
CVGraphEnv <- graph_from_data_frame(d = EdgesEnv, directed = FALSE, vertices = NodesEnv) %>%
  delete_isolated()

plot(CVGraphEnv, vertex.size = 15)
tkp <- tkplot(CVGraphEnv, vertex.size = 15)

tkCoords <- tkplot.getcoords(tkp, norm = FALSE)
plot(CVGraphEnv, vertex.size = 15, layout = tkCoords)

## With negative values

# Run calculation
CoefMtx_Neg <- xy_common_lasoo(Y = VirMat, X = CyanoEnvMat, ll = -Inf)

Edges_Neg <- CoefMtx_Neg %>% make_edges_table() %>% only_strong_cyano_edges()


CVGraph2_Neg <- graph_from_data_frame(d = Edges_Neg, directed = FALSE, vertices = Nodes)

CVGraph2_Neg_Connected <- delete_isolated(CVGraph2_Neg)

plot(CVGraph2_Neg_Connected, vertex.size = 15)

tkp2 <- tkplot(CVGraph2_Neg_Connected, vertex.size = 15)
tkCoords2 <- tkplot.getcoords(tkp2, norm = FALSE)
plot(CVGraph2_Connected)
