#Create panel with all plots, qualitative version

#Load plots
bees = readRDS("Data/RData/Bee_PCA.rds")
coleoptera = readRDS("Data/RData/Coleoptera_PCA.rds")
lepidoptera = readRDS("Data/RData/Lepidoptera_PCA.rds")
non_bee_hymenoptera = readRDS("Data/RData/Non_bee_hymenoptera_PCA.rds")
non_syrphid_diptera = readRDS("Data/RData/Non_syrphid_diptera_PCA.rds")
syrphids = readRDS("Data/RData/Syrphids_PCA.rds")

#Make plot
library(patchwork)

bees + coleoptera + lepidoptera + 
non_bee_hymenoptera + non_syrphid_diptera + syrphids
