source("Bring_In_Data.R")

library(glmnet)


CoefMtx <- matrix(nrow = ncol(CyanoEnvMat), ncol = ncol(VirMat), dimnames = list(colnames(CyanoEnvMat), colnames(VirMat)))
LambdaThing <- vector(length = ncol(VirMat))

for(iter in 1:dim(VirMat)[2]){
  cvfit_loc <- cv.glmnet(y = VirMat[,iter], x = CyanoEnvMat, family = "gaussian", lower.limits = 0)
  c1se_loc <- as.matrix(coef(cvfit_loc, s = "lambda.1se"))
  CoefMtx[,iter] <- c1se_loc[2:length(c1se_loc),]
  LambdaThing[iter] <- cvfit_loc$lambda.1se
}

SignMtx <- sign(CoefMtx)

SignDf <- as_tibble(SignMtx, rownames = "Cyano")

Edges <- SignDf %>% pivot_longer(cols = -Cyano, names_to = "Vir", values_to = "Sign") %>%
  filter(Sign !=0) %>%
  mutate(color = if_else(Sign>0, "black", "red")) %>%
  mutate(lty = if_else(Sign > 0, 1, 2))

Edges2 <- Edges[str_detect(Edges$Cyano, "reads"),]


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
Nodes

Nodes2 <- Nodes %>%
  filter(Type != "Env")

CVGraph <- graph_from_data_frame(d = Edges, directed = FALSE, vertices = Nodes)

plot(CVGraph, vertex_size = 10)

CVGraph2 <- graph_from_data_frame(d = Edges2, directed = FALSE, vertices = Nodes2)

plot(CVGraph2, vertex_size = 10)

