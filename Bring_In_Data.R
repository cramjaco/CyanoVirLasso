library(tidyverse)
library(readxl)
library(janitor)
library(fastDummies)

CyanoEcotypes00 <- read_excel("data/ITS_ecotypes_cut.xlsx")
CyanoEcotypes01 <- CyanoEcotypes00 %>% select(Name, `HLI reads`:`Total Pro proportion`, -`...17`)
EnvData00 <- read_excel("data/Metadata_cyano.xlsx")
EnvData01 <- EnvData00 %>% select(-c(latitude:Year))
VirData00 <- read_excel("data/Myocyanophage_ratio_spreadsheet_cut.xlsx")
VirData01 <- VirData00 %>% select(-`Depth(m)`)

VirData02 <- VirData01 %>%
  clean_names() %>%
  mutate(name = str_replace(name, "GA03_St", "GeoSt")) # Make the names the same

CyanoEnv01 <- left_join(CyanoEcotypes01, EnvData01, by = "Name") %>%
  clean_names() %>%
  mutate(name = str_replace(name, "GA03_St", "GeoSt")) %>% # Make the names the same
  filter(name %in% VirData02$name)

# Join everything so their rows are the same
VirCyanoEnv <- left_join(VirData02, CyanoEnv01, by = "name")

Vir <- VirCyanoEnv[,names(VirData02)]
CyanoEnv <- VirCyanoEnv[,names(CyanoEnv01)] %>% select(name, !ends_with("proportion"))
Cyano <- CyanoEnv %>% select(name, ends_with("reads"))

## Make matrices



CyanoEnvMat <- CyanoEnv %>% 
  column_to_rownames("name") %>%
  dummy_cols(remove_selected_columns = TRUE, remove_most_frequent_dummy = TRUE) %>%
  as.matrix()
VirMat <- Vir %>% column_to_rownames("name") %>% as.matrix() %>% sqrt() %>% scale()
CyanoMat <- Cyano %>% column_to_rownames("name") %>% as.matrix() %>% sqrt() %>% scale()


rm(CyanoEcotypes00, CyanoEcotypes01, EnvData00, EnvData01, VirData00, VirData01, VirData02, CyanoEnv01)
