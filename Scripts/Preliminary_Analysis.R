#Example of analysis with 16 networks of Bartomeus Unpublished (Beefun project)


#load library
library(readxl)
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(reshape2)
library(stringr)
library(networkD3)
library(magrittr)
library(pagedown)
library(manipulateWidget)
library(shiny)
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
all <- merge(data, t_data,  by="Plant_species", all = T)


#Making all NA'S the same NA type 
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
all[] <- lapply(all, make.true.NA)

#Subset just for Bartomeus data
bart <- subset(all, Unique_id=="bartomeus_2015.csv")

#Let´s add the poll family and order. I do not have it in 

poll <- read.csv("Data/Data_processing/poll_spp_names_corrected.csv", row.names = 1)

#Check if it was read well
head(poll)
str(poll)
poll$genus_old <- as.character(poll$genus_old)
#All correct

#Now merge with bart dataframe
#But before I have to split the poll species names in two

bart$genus_old <- word(bart$Pollinator_species)
head(bart)

bart$Compatibility[bart$Compatibility=="self_compatible"] <- "Self compatible"
bart$Compatibility[bart$Compatibility=="self_incompatible"] <- "Self incompatible"
bart$Compatibility[bart$Compatibility=="partially_self_compatible"] <- "Partially self compatible"

#Now merge by genus
b <- merge(bart, poll, by="genus_old", all = F)
#data ready

###############################
###############################
#PLOT DATA#####################
###############################
###############################

#select columns of interest
b_1 <- b[,c(3,4,5,23,78,79)]
#Remove NA's
#select complete cases
b_1 <- b_1[complete.cases(b_1), ]
b_1$order <- as.character(b_1$order)
b_2 <- subset(b_1, order=="Hymenoptera"| order=="Diptera"| order=="Lepidoptera"| order=="Coleoptera")
#Remove .csv from id
b_2$Net_ID <- gsub("\\..*","",b_2$Net_ID)
#Removing first id for plotting
b_2$Net_ID <-gsub("29_Bartomeus_", "", b_2$Net_ID)
#Removing first last word for plotting
b_2$Net_ID <-gsub("_unp", "", b_2$Net_ID)
#Removing underscore #OLD, CHANGES ABOVE
#b_2$Compatibility <- gsub("_", " ", b_2$Compatibility)


list_bee_fun <- split(b_2, b_2$Net_ID)
#saveRDS(list_bee_fun, "Data/RData/list_bee_fun.RData")
#list_bee_fun <- readRDS("Data/RData/list_bee_fun.RData")
#Now find a way to convert to matrix and plot all

list_bee_fun_1 <- lapply(list_bee_fun, function(x) acast(x, order ~ Compatibility , value.var='Interaction', 
                                            fun.aggregate=sum, margins=F))

#TRY WITH A SAPPLY PROBABLY I CAN AVOID THE FOOR LOOP AND KEEP THE LIST STRUCTURE


i<-NULL
for (i in names(list_bee_fun_1)){
 
matrix_d <- as.data.frame(list_bee_fun_1[[i]])


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

setwd("~/R_Projects/Reproductive traits/Images/Sankey") 

# Make the Network
sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)%>%
saveNetwork( file = paste0(i, ".html")) #ADD SPECIFIC FOLDER

}

library(colortools)

sequential("#4682B4")
sequential("orange")
sequential("grey")


######
#Code to generate the list for plotting
#####
a <- list()
b <- list()
i<-NULL
for (i in names(list_bee_fun_1)){
  
matrix_d <- as.data.frame(list_bee_fun_1[[i]])
  
  
# I need a long format
data_long <- matrix_d %>%
rownames_to_column %>%
gather(key = 'key', value = 'value', -rowname) %>%
filter(value > 0)
colnames(data_long) <- c("source", "target", "value")
data_long$target <- paste(data_long$target, " ", sep="")
  
  
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())
str(nodes)  
nodes$group
#colour nodes
nodes$group[nodes$name=="Diptera"]<- "a"
nodes$group[nodes$name=="Lepidoptera"]<- "b"
nodes$group[nodes$name=="Hymenoptera"]<- "c"
nodes$group[nodes$name=="Coleoptera"]<- "d"
nodes$group[nodes$name=="Self compatible "]<- "e"
nodes$group[nodes$name=="Partially self compatible "]<- "f"
nodes$group[nodes$name=="Self incompatible "]<- "g"

#Colour links by group
data_long$group[data_long$source=="Diptera"]<- "a_1"
data_long$group[data_long$source=="Lepidoptera"]<- "b_1"
data_long$group[data_long$source=="Hymenoptera"]<- "c_1"
data_long$group[data_long$source=="Coleoptera"]<- "d_1"
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long$IDsource=match(data_long$source, nodes$name)-1 
data_long$IDtarget=match(data_long$target, nodes$name)-1

#Great code to see how to customize colours 
#https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html
my_color <- 'd3.scaleOrdinal() .domain(["a","a_1", "b", "b_1","c","c_1", "d","d_1", "e", "f", "g"]) .range([ "#0062B4FF", "#386CB0",  "orange","orange", "seagreen","darkseagreen","#B60A1C","indianred", "#8074a8", "#c5bfbe", "#c6c1f0"])'

#Setwd for printing files#not doing it anymore here but I do below
setwd("~/R_Projects/Reproductive traits/Images/Sankey") 

data_long <- data_long[order(data_long$source),]
data_long$source<- as.factor(data_long$source)

# Make the Network
a[[i]] <- sankeyNetwork(Links = data_long, Nodes = nodes,
                Source = "IDsource", Target = "IDtarget",
                Value = "value", NodeID = "name", 
                sinksRight=FALSE, colourScale=my_color, LinkGroup="group",NodeGroup="group",nodeWidth=15, fontSize=9, nodePadding=30)

title<- tags$div(i,style = "font-size:14px")

b[[i]] <- combineWidgets(a[[i]], title = title)

}


#Select all files from the folder
#filenames <- list.files( pattern="*.html", full.names=TRUE)
#Try to loop it
#str(filenames)
#for(i in 1:16){
#
#chrome_print(filenames[i], options = list(pageRanges="1-1"))
#
#}
#chrome_print("index.html", options = list(pageRanges="1-1"))
#w <- combineWidgets(list=a) #%>%saveNetwork("Bee_fun.html")
require(shiny)

#Add general title
title<- tags$div("Bartomeus 2015 unpublished (1/4)", style = "font-size:20px")
#with tilte/look loop to understand
c_1 <- list(b[[1]],b[[2]],b[[3]],b[[4]])
combineWidgets(list=c_1, ncol=2,nrow=2,title=title)%>%saveNetwork("Bee_fun_1.html")
chrome_print("Bee_fun_1.html", options = list(pageRanges="1", landscape=T,scale=1.1))

#Add general title
title<- tags$div("Bartomeus 2015 unpublished (2/4)", style = "font-size:20px")
#with tilte/look loop to understand
c_1 <- list(b[[5]],b[[6]],b[[7]],b[[8]])
combineWidgets(list=c_1, ncol=2,nrow=2,title=title)%>%saveNetwork("Bee_fun_2.html")
chrome_print("Bee_fun_2.html", options = list(pageRanges="1", landscape=T,scale=1.1))

#Add general title
title<- tags$div("Bartomeus 2015 unpublished (3/4)", style = "font-size:20px")
#with tilte/look loop to understand
c_1 <- list(b[[9]],b[[10]],b[[11]],b[[12]])
combineWidgets(list=c_1, ncol=2,nrow=2,title=title)%>%saveNetwork("Bee_fun_3.html")
chrome_print("Bee_fun_3.html", options = list(pageRanges="1", landscape=T,scale=1.1))

#Add general title
title<- tags$div("Bartomeus 2015 unpublished (4/4)", style = "font-size:20px")
#with tilte/look loop to understand
c_1 <- list(b[[13]],b[[14]],b[[15]],b[[16]])
combineWidgets(list=c_1, ncol=2,nrow=2,title=title)%>%saveNetwork("Bee_fun_4.html")
chrome_print("Bee_fun_4.html", options = list(pageRanges="1", landscape=T,scale=1.1))
