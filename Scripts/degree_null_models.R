#load libraries
library(plyr)
########################################################################################################################################################
#1) LOAD NETWORK DATA
########################################################################################################################################################
#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_networks_quantitative") 

temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, function(i){read.csv(i, check.names=FALSE)})
#Add "id" to the list to the long format data
data_id_list <- lapply(seq_along(my_data), 
                       function(x) cbind(my_data[[x]], unique.id=my_files[x]))


#For loop to melt each data frame and merge
i <- NULL
df.long <- NULL

for (i in data_id_list){
  i <- melt(i)
  df.long <- rbind(df.long, i)
}

#Renaming columns
colnames(df.long) <- c("Plant", "Network", "Pollinator", "Int") 
head(df.long)

#cast long format datafr$ame
df <- dcast(df.long, Network + Pollinator ~ Plant, value.var = "Int")
df[is.na(df)] <- 0
df[,1]=as.factor(df[,1])

#create empty list
sp.links <- c()
sp.links <- list(sp.links)

#run loop over each site
for (j in levels(df[, 1])){
  web <- subset(df, Network == j)#iterate over networks
  web <- web[,c(-1,-2)]
  web = web[,colSums(web) > 0]#remove species with no links at each site
  
  #Null model II from Bascompte et al. (2003). Creates random networks by probabilistically fixing row and column marginal totals. The expected number of links is same as observed number. Rodriguez-Girona and Santamaria (2006) showed that this null model has the best compromise between Type I and Type II errors
  #Run this code once to create the null model function
  null.model.II <- function(web){
    web <- as.matrix(web > 0) + 0
    # calculate the probability based on row marginals. Creates matrix same size as web, with row sums divided by number of columns (to get probability of a 1 in each cell of each row), repeated across all columns for each row.
    row.probs <- matrix(rowSums(web)/ncol(web),nrow(web),ncol(web))
    # calculate the probability based on column marginals (again, repeated for whole column). Transpose used instead of byrow=T
    col.probs <- t(matrix(colSums(web)/nrow(web),ncol(web),nrow(web)))
    # calculate the element by element mean of this probabilities
    mat.probs <- (row.probs + col.probs) / 2.0
    # generate a random matrix with 1s proportional to the above probabilities. rbinom(n, size, prob) n is number of observations, size is number of trials, prob is prob of success in each trial
    mat.null <- matrix(rbinom(nrow(web)*ncol(web),1,as.vector(mat.probs)),nrow(web),ncol(web))  
    # return that matrix in all its glory
    return(mat.null)
  }
  
  #Begin permutation test (two tailed)
  reps <- 99 #set number of permutations
  
  nulls<-vector("list",reps)  # Create a list with spaces for each output matrix
  for (i in 1:reps) {
    nulls[[i]]<-null.model.II(web)
  }
  
  #call any individual matrix from that list using nulls[[x]], where x is the number of the matrix you want to call
  null.links <- data.frame(matrix(, nrow=reps, ncol(web)))
  for (i in 1:reps) {
    null.links[i, ] <- t(colSums(nulls[[i]]))
  }
  
  weblink <- t(colSums(as.matrix(web > 0) + 0))
  colnames(null.links) <- colnames(weblink)
  links <- rbind(null.links,weblink)#Add observed connectance into distribution
  sd <- apply(null.links, 2, sd)#calculate standard deviation
  sp.links[[j]] <- abs(weblink - colMeans(null.links)/sd)#print results into list
  
}

#convert list to dataframe and melt
sp.links.df <- rbind.fill(lapply(sp.links, as.data.frame))
sp.links.df$Network <- levels(df.long$Network)
sp.links.long <- melt(sp.links.df, "Network", variable.name = "Plant", value.name = "value", na.rm = TRUE)
