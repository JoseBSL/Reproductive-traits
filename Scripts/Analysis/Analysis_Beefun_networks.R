#Load library

library(bipartite)
library(plyr)
library(data.table)
library(tidyverse) 
library(lubridate)
library(readxl)

#####################################
###
####CODE TO CALCULATE NETWORK METRICS
###
#####################################


#First read all networks
#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_processing/Bartomeus_BeeFun") 

#Workflow found on stackoverflow to read all the files in a list
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i],stringsAsFactors = FALSE)))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, function(i){read.csv(i, row.names = 1)})

names(my_data) <- my_files

data_id_list <- lapply(seq_along(my_data), 
                       function(x) cbind(my_data[[x]], unique.id=my_files[x]))
#Load trait data

setwd("~/R_Projects/Reproductive traits") 

t_data <- read_excel("Data/Data_raw/Trait_data_final.xlsx")
t_data <- as.data.frame(t_data)
colnames(t_data)[1] <- "Plant_species"
# Function to sum the humber of visits per row
#It converts NA'S to 0's
#
str(my_data[[1]])
rs_NA <- function(x){
z <- as.data.frame(x)
z[is.na(z)] <- 0
z <- as.data.frame(rowSums(z))
z <- setDT(z, keep.rownames = TRUE)[]
colnames(z) <- c("Species","Visits")
z[is.na(z)] <- 0
z[z$Species=="NA NA"]<-NA
z <- z[complete.cases(z$Species),]
z <- z[apply(z!=0, 1, all),]

return(z)
}

rs_NA(my_data[[2]])

#Function to prepare data for bipartite analysis
clean <- function(x){
  w <- x
row.names.remove <- "NA NA"
w <- w[!(row.names(w) %in% row.names.remove), ]
w[is.na(w)] <- 0
w <- as.data.frame(w, stringsAsFactors = TRUE)
w[, c(1:ncol(w))] <- sapply(w[, c(1:ncol(w))], as.numeric)

return(w)
}

B =clean(my_data[[2]])
z <- as.data.frame(rowSums(my_data[[2]]))

#it works


#Function to calculate all metrics and bind on dataframe
met <- function(x){
visits <- rs_NA(x)
#degree
degree <- specieslevel(clean(x), index="degree", level="lower")
#normalise degree
n_degree <- specieslevel(clean(x), index="normalised degree", level="lower")
#specialization
d <- specieslevel(clean(x), index="d", level="lower")
#combine metrics in a unique data frame
metrics <- cbind(visits, degree, n_degree, d)
return(metrics)
}

met(my_data[[1]])

#loop to create a list of dataframes with the 16 networks
#and the network metrics for each plant species
i <- NULL
data <- NULL
metrics_list <- list()
for (i in names(my_data)){
  metrics_list[[i]] <- met(my_data[[i]])
}

#Now I have to merge the lsit of dataframe metrics with the traits
#First subset by traits of interest (just compatibility at the moment)

compat <- t_data[,c(1,14:19)]
colnames(compat)[1] <- "Species"
metrics_list[[1]]

#Loop to create the list with all the metrics and traits--> MERGE
i <- NULL
all_list <- list()
for (i in names(metrics_list)){
  all_list[[i]] <- merge(metrics_list[[i]], compat, by="Species")
  
}

#ADD Id to all trhe dataframes as column
#this will help to convert everything to unique dataframe easily
#Add "id" to the list to the long format data
all_list <- lapply(seq_along(all_list), 
                       function(x) cbind(all_list[[x]], unique.id=my_files[x]))
#Now merge all the data frames 
all_df <- bind_rows(all_list, .id = "unique.id")


#####
####
##MODEL TIME
####
####


#Here we try to understand the differences in metrics between the diff comp systems
#Before modelling remove NA'S they are not treated as such
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
all_df$Compatibility <- make.true.NA(all_df$Compatibility)
all_df <- all_df[complete.cases(all_df),]


#VISUALIZA DATA
#Visits
hist(all_df$Visits)
#Degree
hist(all_df$degree)
#Normalise degree
hist(all_df$normalised.degree)
#specialization d'
hist(all_df$d)
#d seems to dont fit that well within a Poisson distribution

library(lme4)
library('fitdistrplus')
library(AER)
library(DHARMa)
library(MASS)
model_1 <- glmer(Visits~Compatibility+(1|unique.id),data=all_df,
                     family=poisson)
#Checking residuals
plot(model_1)
plot(residuals(model_1, type = "pearson") ~ predict(model_1, type = "link"))
plot(residuals(model_1, type = "pearson") ~ as.numeric(all_df$Visits))

summary(model_1)

#Checking ddata distribution 
#Our data does not foloow a normal distribution
plot(fitdist(all_df$Visits,"norm"))
#Our data seems to follow a Poisson dist
plot(fitdist(all_df$Visits,"pois"))


# OR using Ben Bolker's function
overdisp_fun <- function(model_1) {
## number of variance parameters in an n-by-n variance-covariance matrix
       vpars <- function(m) {
        nrow(m) * (nrow(m) + 1)/2
    }
       model.df <- sum(sapply(VarCorr(model_1), vpars)) + length(fixef(model_1))
       rdf <- nrow(model.frame(model_1)) - model.df
       rp <- residuals(model_1, type = "pearson")
       Pearson.chisq <- sum(rp^2)
       prat <- Pearson.chisq/rdf
       pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
       c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
   }

overdisp_fun(model_1)
#Seems that our residuals are overdispersed 
#Alternative way to check dispersion from dharma package
sim_fmp <- simulateResiduals(model_1, refit=T) 
testOverdispersion(sim_fmp)
plotSimulatedResiduals(sim_fmp)
#Also shows that my data is overdispersed

#Now we have an alternative family for our distribution
#Quasipoison and negative binomial are next candidates

model_2<-glmmPQL(Visits ~ Compatibility ,
            random = ~ 1 | unique.id,
            family = quasipoisson(link='log'), 
            data = all_df)

summary(model_2)
plot(model_2)
plot(residuals(model_2, type = "pearson") ~ predict(model_2, type = "link"))
plot(residuals(model_2, type = "pearson") ~ as.numeric(all_df$Visits))


predict(model_2, newdata = all_df(treat = gl(3, 1, 3, c("Low", "Medium","High"))), 
type = "response", level = 0)
plot(model_2)

library(ggplot2)
colnames(all_df) <- make.unique(names(all_df))


ggplot(all_df, aes(x = Visits)) + geom_histogram() + facet_wrap(~Compatibility)
ggplot(all_df, aes(y = Compatibility, x = Visits)) + geom_point() + facet_wrap(~unique.id.1)


plot(model_2)
#Checking overdispersion
#FROM: http://www.flutterbys.com.au/stats/tut/tut11.7a.html
data.hqp.resid <- sum(resid(model_2, type = "pearson")^2)
str(model_2)
rdf <- length(model_2$data$Visits) - length(fixef(model_2)) - 1
rdf
1 - pchisq(data.hqp.resid, rdf)
 data.hqp.resid/(nrow(all_df) - length(fixef(model_2)) - 1)
#P>0.05 data is not overdispersed





model_3 <- glmer.nb(Visits ~ Compatibility + (1|unique.id), data = all_df, verbose = TRUE)
summary(model_3)
plot(model_3)





plot(ranef(model_1))

hist(all_df$Visits)


dispersiontest(model_1,trafo=1)


glm(Visits~Compatibility+(1|unique.id),data=all_df,
    family=poisson)

library('fitdistrplus')
plot(fitdist(all_df$Visits,"pois"))


library(MASS)
library(vcd)
data(quine) 
fit <- goodfit(all_df$Visits, type="poisson", method="ML") 
summary(fit) 
rootogram(fit)
plot(fit)


## Simulated data examples:
dummy <- rnbinom(200, size = 1.5, prob = 0.8)
gf <- goodfit(dummy, type = "nbinomial", method = "MinChisq")
summary(gf)
plot(gf)



install.packages("DHARMa")

library(DHARMa)
testDispersion(model_1)
# get residuals
simulationOutput <- simulateResiduals(fittedModel = model_1, n = 250)
residuals(simulationOutput)
plot(simulationOutput)
plotQQunif(simulationOutput) # left plot in plot.DHARMa()

plotResiduals(simulationOutput) # right plot in plot.DHARMa()


plotResiduals(simulationOutput, all_df$Visits)
hist(simulationOutput)
testResiduals(simulationOutput)
testQuantiles(simulationOutput)
par(mfrow = c(1,2))
plotResiduals(simulationOutput, model_1$Compatibility)
plotResiduals(simulationOutput, model_1$Visits)
testUniformity(simulationOutput = simulationOutput)



#Now time to play with the models
library(lme4)
library(car)
library(emmeans)
library(multcompView)
library(multcomp)

#Analyse data and plot it
junk.glmer = lm(Visits ~ Compatibility, data =  all_list[[16]])
junk.glmer = lm(d ~ Compatibility, data =  all_list[[16]])
summary(junk.glmer)
lsm = lsmeans(junk.glmer, "Compatibility")
pairs(lsm)
CLD <- CLD(lsm,alpha=0.05,adjust="tukey")
CLD <- cld(lsm,alpha=0.05,adjust="tukey")
lsmeans(junk.glmer,
        pairwise ~ Compatibility,
        adjust="tukey")

par(mfrow=c(2,2))
ggplot(CLD,aes(x= Compatibility,y= lsmean,
               label = .group)) +theme_minimal()+
  geom_point(shape  = 15,size   = 4) +ggtitle("A) Comparison of ratios as recipient")+
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Compatibility")+
  geom_text(nudge_x = c(0, 0, 0,0), nudge_y = c(0.8, 0.8, 0.8,0.8),color   = c("yellow","red","green","orange"))


a <- all_list[[16]]
str(a)
a<-a[,c(2,11)]
a[a$Compatibility=="NA"] <- NA
a <- a[complete.cases(a)]
a$Visits <- as.factor(a$Visits)
a$Compatibility <- as.factor(a$Compatibility)
a<- as.data.frame(a)
str(a)
library(rstanarm)
stan_model <- stan_lm(Visits ~ Compatibility, data = a, prior=NULL)
summary(stan_model)
em_color_simple <- emmeans(stan_model, ~Compatibility)
pairs(em_color_simple) # simple effects for color


