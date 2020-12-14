# ------------------------------------------------------------------------------------------------------------ #
# - FILE NAME:   PhylogeneticPCA.R         
# - DATE:        23/10/2017
# - AUTHOR: Pol Capdevila Lanzaco (pol.capdevila@zoo.ox.ac.uk)
# - DESCRIPTION: This code is to estimate the phylogenetically corrected Principal Component Analysis of our set
#                of life history traits.            
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

# Phylogenetically corrected PCA --------------------------------------------------------------------------------

# Now we run the phylogenetic analysis for each of the imputed values 

# First we create empty lists where we will store the data

PCAscree <- PCAloadings <- PCAscore <- PCAEigen <- lambda <-  list()

# We define the life history trait names that we will use for the pPCA

lifeHistory<- c("GenT", "H", "La","GrowSSD", "RepSSD", "S")

# We will do a loop to run the analyses for each of the imputed values 

for(i in 1:length(data.imp1$imputations)){
  
  #Here we introduce the imputed values within the data.frame of aquatic and terrestrial species

    ter[lifeHistory] <- as.data.frame(imputed.terrestrial[[i]])
    aq[lifeHistory] <- as.data.frame(imputed.aquatic[[i]])
  
  #Then we bind the two dataframes
  
  dem2 <- rbind(ter, aq)
  
  #We then join the numeric data that we will use for the pPCA

  Y <- dem2[lifeHistory]
  
  # We need to give rownames as the species to do the pPCA 
  
  row.names(Y) <- dem2$sp
  
  # We scale the data
  
  Y <- scale(Y)
  
  #Run the phylogenetic pca
  
  result <- phyl.pca(tree,
                     Y,
                     method="lambda",
                     mode="cov")
  
  # The following inverts the order in which the PCA 1 axes (longevity)
  # will be displayed, so long-lived species are on the right
  
  if(result$L["GenT","PC1"]<0){
    result$S[,"PC1"]=-result$S[,"PC1"]
    result$L[,"PC1"]=-result$L[,"PC1"]
  }
  
  # Same as above but displaying highly reproductive species at the top
  
  if(result$L["S","PC2"]<0){
    result$S[,"PC2"]=-result$S[,"PC2"]
    result$L[,"PC2"]=-result$L[,"PC2"] }
  
  #percent variance explained by each eigenvector
  
  percVar = diag(result$Eval)/sum(result$Eval)*100
  names(percVar)=paste("PCA",1:length(percVar))
  
  # Store the results
  
  PCAscree[[i]] <- percVar
  PCAscore[[i]] <- result$S
  PCAloadings[[i]] <- result$L
  PCAEigen[[i]] <- result$Eval
  lambda[[i]] <- result$lambda
  
  # Keep track of the loop
  
  print(i)
}

# Save the results -----------------------------------------------------------------------------------------

# Set the working directory for the results

setwd(ResultsPath)

# Scree values

PCAscreeM <- apply(simplify2array(PCAscree), 1, mean)
PCAscreesd <- apply(simplify2array(PCAscree), 1, plotrix::std.error)
PCAscreMean<- cbind(PCAscreeM, PCAscreesd)

write.csv(PCAscreMean, file = "Phylo PCA scree.csv")

# Scree plot

pdf(paste("Phylo PCA_Scree - ", phyloMethod,".pdf",sep=""),w=6,h=6)
mp=barplot(PCAscreMean[,1],ylab = "% explained variation",
           col=c("black","red","green","orange","yellow","light blue","purple","pink","maroon","blue2","blue"),
           ylim=c(0,100),axisnames=FALSE)
text(mp,par("usr")[3],
     labels = names(PCAscreMean[,1]), 
     srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
dev.off()

# Now for the scores

PCAscoreM <- apply(simplify2array(PCAscore), 1:2, mean)

write.csv(PCAscoreM, file = "Phylo PCA scores.csv")

# The loadings

PCALM <- apply(simplify2array(PCAloadings), 1:2, mean) 
PCALst <- apply(simplify2array(PCAloadings), 
                1:2, 
                plotrix::std.error) 

write.csv(PCALM, file = "Phylo PCA mean loadings.csv")
write.csv(PCALst, file = "Phylo PCA SE loadings.csv")

# Now the eigenvalues

PCAEigM <- apply(simplify2array(PCAEigen), 1:2, mean) 
PCAEigst <- apply(simplify2array(PCAEigen), 
                  1:2, plotrix::std.error) 
write.csv(PCAEigM, file = "Phylo PCA mean eigenvalues.csv")
write.csv(PCAEigst, file = "Phylo PCA SE eigenvalues.csv")

# Saving Pagel's lambda 

write.csv(c(mean(unlist(lambda)),
            plotrix::std.error(unlist(lambda))), 
          file = "Phylo PCA lambda.csv")

