#Example of analysis with 16 networks of Bartomeus Unpublished (Beefun project)


#load library
library(readxl)
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(reshape2)


#First read all networks
#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_processing/Bartomeus_BeeFun") 

#Workflow found on stackoverflow to read all the files in a list
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, read.csv)
#Add "id" to the list to the long format data
data_id_list <- lapply(seq_along(my_data), 
                       function(x) cbind(my_data[[x]], unique.id=my_files[x]))

#Now I have to convert to long format all these networks
#For loop to melt each data frame and merge
i <- NULL
data <- NULL

for (i in data_id_list){
  i <- melt(i)
  data <- rbind(data, i)
}

#Colnames
colnames(data) <- c("Plant_species", "Net_ID", "Pollinator_species", "Interaction")
#Add space and remove dot (Poll. column)
data$Pollinator_species=gsub("\\."," ",data$Pollinator_species)

#Read trait data
#Read data with traits
setwd("~/R_Projects/Reproductive traits") 

t_data <- read_excel("Data/Data_raw/Trait_data_final.xlsx")
t_data <- as.data.frame(t_data)
colnames(t_data)[1] <- "Plant_species"

#Merge data 
all <- merge(data, t_data,  by="Plant_species", all = T )


#Making all NA'S the same NA type 
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
all[] <- lapply(all, make.true.NA)

#Subset just for Bartomeus data
bart <- subset(all, Unique_id=="bartomeus_2015.csv")

###############################
###############################
#PLOT DATA#####################
###############################
###############################

#select columns of interest
bart_1 <- bart[,c(2,3,4,22)]
#Remove NA's
str(bart_1)
bart_1 <- bart_1[complete.cases(bart_1), ]



#convert to matrix
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






