##################################################
########
######
####
##
# BRMS model
##
###
####
#####
#################################################


#####################################
###
#### MODELS METRICS ~ SELFING LEVEL
###
#####################################


library("performance")
library(brms)
library(ape)
library(tidybayes)
library(ggplot2)

#load data
all_df <- readRDS("Data/RData/network_metrics.RData")
colnames(all_df)[17]<-"Id"
all_df <- as.data.frame(all_df) 
all_df$Visits <- as.integer(all_df$Visits)

#Convert all NA'S to same type of NA's
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
all_df$Autonomous_selfing_level <- make.true.NA(all_df$Autonomous_selfing_level)
all_df <- all_df[complete.cases(all_df$Autonomous_selfing_level),]
colnames(all_df) <- make.unique(names(all_df))
all_df$Id <- as.factor(all_df$Id)
all_df$Autonomous_selfing_level <- as.factor(all_df$Autonomous_selfing_level)


#Load phylogenetic tree
tree <- read.nexus("Data/Data_phylogeny/bee_fun_tree.nexus")
A <- ape::vcv.phylo(tree)
#Convert tree distance from 0 to 1
A <- A/max(A)
rownames(A) <- gsub("\\*", "", rownames(A))
colnames(A) <- gsub("\\*", "", colnames(A))
#call column phylo
colnames(all_df)[13] <- "phylo"
#make it equal the species names to the tree names with underscore
all_df$phylo <- gsub(" ","_", all_df$phylo)


#Checking data
hist(all_df$Visits)
ggplot()+geom_point(data = all_df, aes(y = Visits, x = Autonomous_selfing_level))+geom_smooth()

all_df <- all_df %>%mutate(Autonomous_selfing_level = fct_relevel(Autonomous_selfing_level, levels=c("high", "medium", "low", "none")))
all_df$Autonomous_selfing_level <- as.factor(all_df$Autonomous_selfing_level)



#####################################
###
#### MODEL 1 VISITS ~ SELFING
###
#####################################



#do not know yet how to set priors on this
prior1 <- c(
  prior(normal(0, 10), class = Intercept),
  prior_string("normal(0, 10)", class="b"),
  prior(cauchy(0, 10), class = sigma)
)

bmod1 <- brm(
  Visits ~ Autonomous_selfing_level + (1|gr(phylo, cov = A)),
  data = all_df, family = negbinomial(), data2 = list(A = A),
  sample_prior = TRUE, warmup = 2000, iter = 5000,save_all_pars=T,
  control = list(adapt_delta = 0.99))

bmod1 <- add_criterion(bmod1, "loo", moment_match = TRUE)


pp_check(bmod1)
pp_check(bmod1, type='stat', stat='mean')
pp_check(bmod1, type='intervals')
pp_check(bmod1, nsamples = 1e2) + theme_bw(base_size = 20)

stanplot(bmod1)


marginal_effects(bmod1)
plot(conditional_effects(bmod1), points = TRUE) 

posterior_summary(bmod1, pars = c("^b_", "^sd_", "sigma"), probs = c(0.025, 0.975) )



#We have compare this model with and without phylogeny and 
bmod1_1 <- brm(
  Visits ~ Autonomous_selfing_level,
  data = all_df, family = negbinomial(),
  sample_prior = TRUE, warmup = 2000, iter = 5000,
  save_all_pars=T, control = list(adapt_delta = 0.99))

bmod1_1 <- add_criterion(bmod1_1, "loo", moment_match = TRUE)


#compare model fit
loo_compare(bmod1, bmod1_1, criterion = "loo")
#The phylogenetic relatedness as random factor improves the model

hyp <- "sd_phylo__Intercept^2 / (sd_phylo__Intercept^2 + shape^2) = 0"
(hyp <- hypothesis(bmod1, hyp, class = NULL))


pp_check(bmod1, resp = "visits")

