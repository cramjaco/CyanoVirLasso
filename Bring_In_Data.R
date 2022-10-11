library(tidyverse)
library(readxl)
library(janitor)
library(fastDummies)
library(here)

pass <- function(x){x}

CyanoEcotypes00 <- read_excel(here("data", "September22", "ITS_ecotypes_cut_Jacob.xlsx"))
CyanoEcotypes01 <- CyanoEcotypes00 %>% select(Name, `HLI reads`:`Total Pro proportion`, -`...17`)
EnvData00 <- read_excel(here("data", "September23", "Metadata_cut_cyano2.xlsx"))
EnvData01 <- EnvData00 %>% select(-c(`latitude N`:Year))
VirData00 <- read_excel(here("data", "September22", "Myocyanophage_ratio_spreadsheet2_cut-CAF.xlsx"))
VirData01 <- VirData00 %>% select(-`Depth(m)`)

CyanoProportions <- CyanoEcotypes01 %>%
  select(Name, ends_with("Proportion"), -`Total Pro proportion`) %>% as.data.frame()

## clr transform a data frame and return a data frame. First col must be names
jacob_clr <- function(tib){
  df <- as.data.frame(tib)
  rownames(df) <- df[,1]
  df <- df[,-1]
  mtx <- as.matrix(df)
  clrmtx <- clr(mtx)
  clrdf <- as.data.frame(clrmtx)
  clrdf <- rownames_to_column(clrdf, var = "Name")
  clrtib <- as_tibble(clrdf)
  clrtib
}

# rownames(CyanoProportions) <- CyanoProportions[,1]
# CyanoProportions <- CyanoProportions[,-1]
# CyanoPropMtx <- as.matrix(CyanoProportions)
# CyanoPropClr <- clr(CyanoPropMtx)

fix_geo_station <- function(tib) {
  tib%>%
  mutate(Name = str_replace(Name, "GA03 St", "GA03_St")) %>% # Make the names the same
  mutate(Name = str_replace(Name,  "GeoSt", "GA03_St")) %>%
  mutate(Name = str_replace(Name,  "GA03_st", "GA03_St")) %>%
  pass
}
  

CyanoTransformed <- CyanoProportions %>%
  #jacob_clr() %>%
  rename_with(~str_remove(., " proportion$")) %>%
  fix_geo_station() %>%
  pass()
  

logJp <- function(x, adj = 0.0001){
  log(x + adj)
}

EnvTransformed <- EnvData01 %>%
  mutate(across(c(PO4, Nitrate, Nitrite, `Oxygen umol/kg`), logJp)) %>%
  clean_names() %>%
  rename(Name = "name") %>%
  fix_geo_station() %>%
  pass()

VirData02 <- VirData01 %>%
  #clean_names() %>%
  fix_geo_station() %>%
  pass

VirTransformed <- VirData02 %>%
  #mutate(across(c(-Name), logJp)) %>%
  mutate(across(c(-Name), sqrt)) %>%
 # Make the names the same
  pass

CyanoEnv01 <- left_join(CyanoTransformed, EnvTransformed, by = "Name") %>%
  #clean_names() %>%
  #mutate(Name = str_replace(Name, "GA03_St", "GeoSt")) %>% # Make the names the same
  filter(Name %in% VirData02$Name)

# Join everything so their rows are the same
VirCyanoEnv <- left_join(VirTransformed, CyanoEnv01, by = "Name")

Vir <- VirCyanoEnv[,names(VirTransformed)]
CyanoEnv <- VirCyanoEnv[,names(CyanoEnv01)] %>% select(Name, !ends_with("reads"), -contains("total_pro"))
Cyano <- CyanoEnv

# If variance is too low in CyanoEnvMat, then glassoo doesn't work. This tends to happen for some ocean regions
# |depth_regime|oxygen_category

CyanoEnvDummy <- CyanoEnv %>% 
  as.data.frame() %>%
  column_to_rownames("Name") %>%
  dummy_cols(remove_selected_columns = TRUE, remove_most_frequent_dummy = TRUE) %>%
  identity()

CyanoEnvMat <- CyanoEnvDummy %>%
  as.matrix()

# which categories have one or fewer entries
NonZeros <- CyanoEnvMat %>% apply(2, function(x){sum(x !=0)})
UseThese <- NonZeros >= 2

CyanoEnvMat <- CyanoEnvMat[,UseThese]

rownames(CyanoEnvMat) <- CyanoEnv$Name

VirMat <- Vir %>% column_to_rownames("Name") %>% as.matrix() %>% scale()
CyanoMat <- CyanoEnvMat #%>% column_to_rownames("Name") %>% as.matrix()  %>% scale()





#rm(CyanoEcotypes00, CyanoEcotypes01, EnvData00, EnvData01, VirData00, VirData01, VirData02, CyanoEnv01)

