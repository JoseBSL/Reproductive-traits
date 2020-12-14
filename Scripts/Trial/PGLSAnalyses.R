# ------------------------------------------------------------------------------------------------------------ #
# - FILE NAME:   PGLSAnalyses.R         
# - DATE:        23/10/2017
# - AUTHOR: Pol Capdevila Lanzaco (pol.capdevila@zoo.ox.ac.uk)
# - DESCRIPTION: This code is to perfomr PGLS analyses to see the correlations between the life history traits 
#                explored in our study. 
#                The data required are:
#                - ImputedData.RData: file containing two dataframes with the imputed data for 807 species 
#                                     and 6 traits. 
#                                     imputed.terrestrial: contains a list of the imputated life histories  
#                                     for the terrestrial species
#                                     imputed.aquatic: contains a list of the imputated life histories  
#                                     for the terrestrial species
#                - LifeHistoryData.csv: file containing a dataframe with 807 species and 7 columns where
#                                       sp: Species name
#                                       GenT: Generation time
#                                       H: Rate of senescence
#                                       La: Age at maturity
#                                       GrowSSD: Development
#                                       RepSSD: Reproductive output
#                                       S: Degree of iteroparity
#                                       R: whether the species is Aquatic or Terrestrial
#                                       Kingdom: the taxonomic kingdom
#                                       Sessi: sessile/mobile classification
#                - Tree.tre: phylogenetic tree 807 species that should match the ones in the dataframe
#
#
# ------------------------------------------------------------------------------------------------------------ #

rm(list=ls()) # This cleans up the memory in R

# Libraries

library(ape)
library(phytools)
library(reshape)
library(caper)

# Load and clean the data---------------------------------------------------------------------------------------

# Define working directories (this will depend on where you store the data and code)

ResultsPath <- "your/working/directory"
DataPath <- "your/working/directory"

#Set the working directory

setwd(DataPath)

#Load the imputed data 

load(paste0(ResultsPath, "ImputedData.RData"))

# Load the csv containing the life history data

dem <- read.csv("LifeHistoryData.csv")

# We also separate aquatic and terrestrial species like  

ter <- subset(dem, R=="Terrestrial")
aq <- subset(dem, R=="Aquatic")

# Load the tree

tree <- read.tree("Tree.tre")

# Correct names of the tree 

tree$tip.label <- gsub("_", " ", tree$tip.label)

# Check if the tree is rooted

is.rooted(tree)

# If it is binary

is.binary(tree)

# Make the node labels uniques

tree<- makeNodeLabel(tree)

# PGLS analyses -------------------------------------------------------------------------------------------------

# To run the PGLS analyses we will correlate all the life history traits with each other
# to do that we will run a for loop

# We define the life history trait names that we will use for the pPCA

lifeHistory<- c("GenT", "H", "La","GrowSSD", "RepSSD", "S")

# We create an empy lists where we will store the results

phyloSignS <- list() # For the phylogenetic signal analyses

labels=c("Slope","SD","t","P", 
         "R", "SD2","t2","P2",
         "Pagel","CI","R2","Padjusted", 
         "P2adjusted")
saveTable=matrix(NA,28,length(labels))
colnames(saveTable)=labels
rownames(saveTable)=c(1:28)
Tables <-  list()

# Now we will run a loop for each of the imputations 

for(i in 1:length(imputed.terrestrial)){
  
  # Subset aquatic and terrestrial 
  
  ter[lifeHistory] <- as.data.frame(imputed.terrestrial[[i]])
  aq[lifeHistory] <- as.data.frame(imputed.aquatic[[i]])
  
  # Bind them
  
  dem2 <- rbind(ter, aq)
  
  # Create the comparative data
  
  rownames(dem2) <- dem2$sp# to match with the tree species name 
  comp_data <- comparative.data(phy = tree, 
                                data =dem2,
                                names.col = sp, 
                                vcv = TRUE)
  
  # Estimate the phylogenetic signal
  
  psign <-apply(comp_data$data[lifeHistory], 
                2,  
                function(x) phylosig(comp_data$phy, x, 
                                     method="lambda", test=TRUE, 
                                     nsim=999)$lambda)
  sign <-apply(comp_data$data[lifeHistory], 
               2,  
               function(x) phylosig(comp_data$phy, x, 
                                    method="lambda", 
                                    test=TRUE, nsim=999)$P)
  
  # Shape it correctly
  
  PhyloSign <- cbind(melt(as.data.frame(psign)), melt(as.data.frame(sign)))
  PhyloSign$Trait <- colnames(comp_data$data[lifeHistory])
  
  # Store for the first set of imputed values
  
  phyloSignS [[i]] <- PhyloSign
  
  #PGLS assuming Brownian motion model of evolution 
  
  count=0
  for (expl in 1:5) {
    tryCatch({for (resp in (expl+1):6) {
      count=count+1
      explanatory=as.numeric(as.matrix(comp_data$data[lifeHistory[expl]]))
      response=as.numeric(as.matrix(comp_data$data[lifeHistory[resp]]))
      mod=pgls(explanatory ~ response+R, comp_data, lambda='ML')
      saveTable[count,c(1:11)]=c(summary(mod)$coeff[2,],
                                 summary(mod)$coeff[3,],
                                 mod$param[2],
                                 mod$param.CI$lambda$ci,summary(mod)$r.squared)
      rownames(saveTable)[count]=paste(lifeHistory[expl],"~",lifeHistory[resp])
      print(paste("PGSL model: ",lifeHistory[expl]," ~ ",lifeHistory[resp],sep=""))
    }}, error=function(e){})
  }
  
  #Correction for family-wise error
  saveTable[,12]<- p.adjust(saveTable[,4], method = "bonferroni", n = length(saveTable[,4]))
  saveTable[,13]<- p.adjust(saveTable[,8], method = "bonferroni", n = length(saveTable[,8]))
  saveTable[,"Padjusted"]=p.adjust(saveTable[,"P"],"BH")
  
  #Save the data
  
  Tables[[i]] <- saveTable
}

# Re-order the data 

for(i in 1:length(phyloSignS)){
  colnames(phyloSignS[[i]])[c(2,4)] <- c("Lambda", "P")
  phyloSignS[[i]] <- subset(phyloSignS[[i]], select=c("Trait","Lambda", "P"))
}

# Save the results

setwd(ResultsPath)
write.csv(phyloSignS, file="PhyloSignal.csv") # Phylogenetic signal
write.csv(Tables, file = "PGLsList.csv", dec=".", sep=",") # PGLS raw correlations

# Simplify the PGLS results
PGLS <- apply(simplify2array(Tables), 1:2, mean, na.rm=T) # Calculate mean values
PGLSsd <- apply(simplify2array(Tables), 1:2, plotrix::std.error, na.rm=T) # Calculate the standard error

# Save simplified results

write.csv(PGLS, file = "PGLsMean.csv", dec=".", sep=",")
write.csv(PGLSsd, file = "PGLsSE.csv", dec=".", sep=",")

