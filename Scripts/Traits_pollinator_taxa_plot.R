#GRAPHICAL REPRESENTATION OF TAXONOMIC GROUP BY TRAIT

#LOAD LIBRARIES
#library
library(readxl)
library(dplyr)
library(reshape2)
library(ggplot2)

#-----------------------------------------------------
#WORKFLOW
#1)READ DATA 
#2)MAKE NA'S EQUAL 
#3)SUBSET BY REAL INTERACTION (GREATER THAN 1) 
#4)SELECT SELF, SELF IN AND PARTIALLY FOR NOW
#5)SUBSET BY MAIN POLL ORDERS
#6)PLOT IT
#-----------------------------------------------------

#-----------------------------------------------------
#1)READ DATA 
#-----------------------------------------------------


#LOAD DATA
#Load data long format
Long_data <- read.csv("Data/Data_processing/Long_format_metawebs_poll_taxa_added.csv")
#Read data with traits
data <- read_excel("Data/Data_raw/Trait_data_final.xlsx")
data <- as.data.frame(data)

#FILTER TRAIT DATA
data_filtered <- subset(data, Info_level=="flower"|Info_level=="capitulum"|Info_level=="inflorescence"|Info_level=="NA")
#RENAME COLUMN BEFORE MERGING
colnames(data_filtered)[1] <- "Plant_species"
data_filtered$Species <- as.factor(data_filtered$Species)
data_filtered <- as.data.frame(data_filtered, stringsAsFactors = TRUE)

#NOW I MERGE BOTH  (THE LIST OF SPECIES AND THE TRAIT DATA)
all <- merge(Long_data, data_filtered,  by="Plant_species", all = T )
#DATA READY


#-----------------------------------------------------
#2)MAKE NA'S EQUAL 
#-----------------------------------------------------

#Making all NA'S the same NA type 
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
all[] <- lapply(all, make.true.NA)


#-----------------------------------------------------
#3)SUBSET BY REAL INTERACTION (GREATER THAN 1) 
#-----------------------------------------------------

#SELECT INTERACTION THAT OCCURED
#Subset by real interactions
i <- subset(all, Interaction>0)
#Now we have the number of poll. species recorded for each plat species 

#-----------------------------------------------------
#4)SELECT SELF, SELF IN AND PARTIALLY FOR NOW
#-----------------------------------------------------

i_s <- subset(i, Compatibility=="self_compatible"|Compatibility=="self_incompatible"|Compatibility=="partially_self_incompatible")


#-----------------------------------------------------
#5)SUBSET BY MAIN POLLINATOR ORDERS
#-----------------------------------------------------
str(i_s$Pollinator_order)
i_s$Pollinator_order <- as.character(i_s$Pollinator_order)


i_s_t <- subset(i_s, Pollinator_order=="Hymenoptera"|Pollinator_order=="Lepidoptera"|Pollinator_order=="Coleoptera"|Pollinator_order=="Diptera")


#-----------------------------------------------------
#6)PLOT IT
#-----------------------------------------------------

ggplot(i_s_t) +
  geom_bar(aes(x = Compatibility, fill = Pollinator_order)) +
  facet_wrap(~ Id) 


#Maybe I have tu subset by pollinator too (4 main orders)

library(bipartite)
par(xpd=T)
plotweb(motten1982)
visweb(motten1982)
plotPAC(PAC(motten1982), outby=0.9)

mod <- computeModules(motten1982)
plotModuleWeb(mod)


plotweb(motten1982, 
        
        #Set the drawing method. 
        method="cca", 
        
        #Set the color of the row nodes
        col.low="lightblue",
        
        #Set the color of the column nodes
        col.high="pink", 
        
        #Set the link color
        col.interaction="grey90", 
        
        #Set the rotation of node labels
        text.rot="90", 
        
        #Set the size of node labels
        labsize=1)
dev.off()


levels(as.factor(all$Id))


t <- subset(all, Id=="1_metaweb_carpobrotus_Bartomeus_2008.csv")
t_sub <- t[,c(4,3,26)]
#Remove NA's
t_sub <- t_sub[!is.na(t_sub), ]

#convert to matrix
trial  <- as.matrix(xtabs(Interaction~., t_sub))

plotweb(trial, method="cca", text.rot="90",
        labsize=4.5,col.low="green4", col.high=c("gold","blue","red"),
        col.interaction="skyblue3", bor.col.interaction ="black" )

#

t <- subset(all, Id=="1_metaweb_carpobrotus_Bartomeus_2008.csv")
t_sub <- t[,c(6,4,26)]
#Remove NA's
t_sub <- t_sub[!is.na(t_sub), ]

#convert to matrix
trial  <- as.matrix(xtabs(Interaction~., t_sub))

plotweb(trial, method="cca", text.rot="90",
        labsize=0.9,col.low="green4", col.high=c("gold","blue","red"),
        col.interaction="skyblue3", bor.col.interaction ="black" )

#




# CODE FROM: https://www.data-to-viz.com/graph/sankey.html

# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)
# Package
library(networkD3)

# I need a long format
data_long <- data %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname) %>%
  filter(value > 0)
colnames(data_long) <- c("source", "target", "value")
data_long$target <- paste(data_long$target, " ", sep="")

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long$IDsource=match(data_long$source, nodes$name)-1 
data_long$IDtarget=match(data_long$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)


#COPY CODE OF THE MATRIX, THIS COULD SOLVE MY PROBLEM
t <- subset(all, Id=="1_metaweb_carpobrotus_Bartomeus_2008.csv")
t_sub <- t[,c(6,4,26)]
#Remove NA's
t_sub <- t_sub[complete.cases(t_sub), ]
str(t_sub)
#convert to matrix
library(reshape2)
matrix <- acast(t_sub, Pollinator_order ~ Compatibility , value.var='Interaction', 
                fun.aggregate=sum, margins=F)


matrix_d <- as.data.frame(matrix)


    # I need a long format
data_long <- matrix_d %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname) %>%
  filter(value > 0)
colnames(data_long) <- c("source", "target", "value")
data_long$target <- paste(data_long$target, " ", sep="")

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long$IDsource=match(data_long$source, nodes$name)-1 
data_long$IDtarget=match(data_long$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)





str(data_long)






#subset 1 network 
t <- subset(all, Id=="1_metaweb_carpobrotus_Bartomeus_2008.csv")
t_sub <- t[,c(4,3,26)]
str(t_sub)
#Remove NA's CHECK
t_sub <- t_sub[complete.cases(t_sub), ]

#Select values of intreraction greater than 0
#t_sub <- t_sub[t_sub$Interaction>0,]

#levels data frame 
nodes <- data.frame("name"=unique(c(levels(as.factor(t_sub$Pollinator_species)),levels(as.factor(t_sub$Compatibility)))))
str(t_sub)

#Try to make the frequency column for each set of nodes
#Fisr pollinators
library(plyr)
IDsource <- count(t_sub, 'Pollinator_species')
IDtarget <- count(t_sub, 'Compatibility')


y_sub <- merge(t_sub, IDsource,by= "Pollinator_species")
colnames(y_sub)[4] <- "IDsource"

final <- merge(y_sub, IDtarget,by= "Compatibility")
colnames(final)[5] <- "IDtarget"
str(final)

sankeyNetwork(Links = final, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "Interaction", NodeID = "name", 
              sinksRight=FALSE,  nodeWidth=40, fontSize=13, nodePadding=20)



