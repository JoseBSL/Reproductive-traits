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
  Visits ~ Autonomous_selfing_level + (1|phylo),
  data = all_df, family = negbinomial(), cov = list("phylo" = A),
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




bmod2 <- brm(
  Visits ~ Autonomous_selfing_level,
  data = all_df, family = negbinomial(),
  sample_prior = TRUE, warmup = 2000, iter = 5000,
  save_all_pars=T, control = list(adapt_delta = 0.99))

bmod2 <- add_criterion(bmod2, "loo", moment_match = TRUE)



#compare model fit
loo_compare(bmod1, bmod2, criterion = "loo")

pp_check(bmod1, resp = "visits")














mean(all_df$Visits)














library(tidyverse)
bmod1 %>%
  plot(
    combo = c("hist", "trace"), widths = c(1, 1.5),
    theme = theme_bw(base_size = 10)
  )

#The chain seems well mixed, consider many different values

#Cheking number of cores
#getOption("mc.cores", 1)


# "cov_ranef =" es donde metemos el árbol
ab <-brm(Visits ~ Autonomous_selfing_level + (1|Species), data = all_df,
           cores=4,
           family = "gaussian",sample_prior = TRUE)
check_model(ab)

file.edit("~/.Renviron")
plot(ab)
plot(conditional_effects(m7b3), points = TRUE)


bayes_R2(m7b3)

plot(fit_ir3)



all_df %>%
  add_residual_draws(m7b3) %>%
  ggplot(aes(x = .row, y = .residual)) +
  stat_pointinterval()






#Y esto simplemente para la representación gráfica, que pienso te puede ayudar
# marginal_effects() es una función increíblemente buena para trabajar con brms, te saca ya los
# predict y puedes hacer lo que quieras como en este ejemplo
plot(Success.test.as.numeric ~ Brain.weight, data = dat3b, 
     main="Success related to \nbrain size (a)", xlab="Absolute brain size", 
     cex.lab= 1.3 ,ylab = "Success learning test", las = 1, col = c("red","darkgreen", "blue", "black")[Family])
legend(x=4, y=0.4, legend = levels(Success8trials.ITf$Family),
       col = c("red","darkgreen", "blue", "black"), pch=19, cex=0.85,ncol = 1)
box(which = "plot", lty = "solid")
fit <- marginal_effects(m7b3)
fits<-as.data.frame(fit$Brain.weight)
fits$Brain.weight
polygon(c((fits$Brain.weight), rev((fits$Brain.weight))), c(fits$upper__, rev(fits$lower__)),
        col = "Gray95", border = NA)

lines((fits$Brain.weight), fits$estimate__, lwd=2)
lines((fits$Brain.weight), fits$lower__, col = "purple")
lines((fits$Brain.weight), fits$upper__, col = "purple")
points(Success.test.as.numeric ~ (Brain.weight), data = dat3b,col = c("red","darkgreen", "blue", "black")[Family])


