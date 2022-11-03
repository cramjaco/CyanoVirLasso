source("Bring_In_Data.R")

library(glmnet)


CoefMtx <- matrix(nrow = ncol(CyanoMat), ncol = ncol(VirMat), dimnames = list(colnames(CyanoMat), colnames(VirMat)))
LambdaThing <- vector(length = ncol(VirMat))

for(iter in 1:dim(VirMat)[2]){
  cvfit_loc <- cv.glmnet(y = VirMat[,iter], x = CyanoMat, family = "gaussian", lower.limits = 0)
  c1se_loc <- as.matrix(coef(cvfit_loc, s = "lambda.1se"))
  CoefMtx[,iter] <- c1se_loc[2:length(c1se_loc),]
  LambdaThing[iter] <- cvfit_loc$lambda.1se
}

SignMtx <- sign(CoefMtx)

library(igraph)

SignDf <- as_tibble(SignMtx, rownames = "Cyano")
Edges <- SignDf %>% pivot_longer(cols = -Cyano, names_to = "Vir", values_to = "Sign") %>%
  filter(Sign !=0) %>%
  mutate(color = if_else(Sign>0, "black", "red")) %>%
  mutate(lty = if_else(Sign > 0, 1, 2))

#EdgeList <- Edges %>% filter(Sign !=0) %>% select(-Sign) %>% as.matrix()

#CVGraph <- graph_from_edgelist(EdgeList, directed = FALSE)
#CVGraph <- graph_from_data_frame(Edges, directed = FALSE)

#plot(CVGraph, layout = layout_nicely(CVGraph))

VirNodes <- data.frame(Node = colnames(VirMat), Type = "Virus")
CyanoNodes <- data.frame(Node = colnames(CyanoMat), Type = "Cyano")
Nodes <- bind_rows(VirNodes, CyanoNodes) %>%
  mutate(shape = if_else(Type == "Virus", "vrectangle", "circle")) %>%
  mutate(color = if_else(Type == "Virus", "white", "green")) %>%
  mutate(label = str_remove(Node, "_reads"))
CVGraph <- graph_from_data_frame(d = Edges, directed = FALSE, vertices = Nodes)

plot(CVGraph, layout = layout_nicely(CVGraph), vertex.shape = V(CVGraph)$shape, vertex.size = 15)

# CVPositives <- graph_from_data_frame(d = Edges %>% filter(Sign > 0), directed = FALSE, vertices = Nodes)
# 
# plot(CVPositives)
# 
# CVNegatives <- graph_from_data_frame(d = Edges %>% filter(Sign < 0), directed = FALSE, vertices = Nodes)
# 
# plot(CVNegatives)
