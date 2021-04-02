########################################################################################################################################################
# SCRIPT FOR ANALYSIS 1 VISITS/SPECIALIZATION ~ PRINCIPAL COMPONENTS
########################################################################################################################################################
#Load libraries
library(brms) #modelling
library(cmdstanr) #modelling
library(cowplot) #panel of plots
library(ggplot2) #plotting
# Theme for publication
theme_ms <- function(base_size=12, base_family="Helvetica") {
  (theme_bw(base_size = base_size, base_family = base_family)+
     theme(text=element_text(color="black"),
           axis.title=element_text( size = rel(1.3)),
           axis.text=element_text(size = rel(1.5), color = "black"),
           legend.title=element_text(face="bold"),
           legend.text=element_text(),
           legend.background=element_rect(fill="transparent"),
           legend.key.size = unit(0.4, 'lines'),
           panel.border=element_rect(color="black",size=1),
           panel.grid.minor.x =element_blank(),
           panel.grid.minor.y= element_blank(),
           panel.grid.major= element_blank()
     ))
}
########################################################################################################################################################
#LOAD DATA
########################################################################################################################################################
data_analysis_2 <- readRDS("Data/RData/data_analysis_2.rds") 
colnames(data_analysis_2)[4] <- "Id"
########################################################################################################################################################
#MODEL VISITS ~ PRINCIPAL COMPONENTS
########################################################################################################################################################
Visits_PCs <- brm((Visits-1) ~ PC1 + PC2 + PC3 + (1|Id),
                  data = data_analysis_2, family  = zero_inflated_negbinomial(), cores = 4,chains = 4, 
                  sample_prior = TRUE, warmup = 500, iter = 2000,
                  control = list(adapt_delta = 0.99)) 


#looks ok
pp_check(Visits_PCs) +xlim(-50,200)+ylim(0,0.1)

#Explore conditional and marginal Bayesian r2
bayes_R2(Visits_PCs)
loo_R2(Visits_PCs)

#Explore each of the PC's
conditional_effects(Visits_PCs, effects = "PC1",points=T)
conditional_effects(Visits_PCs, effects = "PC2",points=T)
conditional_effects(Visits_PCs, effects = "PC3",points=T)

#Save output
setwd("~/Dropbox/PhD/R/Chapter_2") #DROPBOX, files too large for github
saveRDS(Visits_PCs, "results_analysis_2_Visits.rds") 
########################################################################################################################################################
#Plot nicely
########################################################################################################################################################
Visits_PCs <- readRDS("results_analysis_2_Visits.rds") #load model
setwd("~/R_Projects/Reproductive Traits") #setwd to load data
data_analysis_2 <- readRDS("Data/RData/data_analysis_2.rds") #load data 

ce_1 <- conditional_effects(Visits_PCs, effects = "PC1",points=T) 
colnames(ce_1[[1]])[3] <- "Interaction"

Visits_PC1 <- ggplot(ce_1[[1]], aes(x = PC1, y = (estimate__+1))) + geom_point(data = data_analysis_2,aes(x = PC1, y = Visits),
  size = 1.5, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(data_analysis_2$Visits, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Number of visits")

ce_2 <- conditional_effects(Visits_PCs, effects = "PC2",points=T) 
colnames(ce_2[[1]])[3] <- "Interaction"

Visits_PC2 <- ggplot(ce_2[[1]], aes(x = PC2, y = (estimate__+1))) + geom_point(data = data_analysis_2,aes(x = PC2, y = Visits),
  size = 1.5, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(data_analysis_2$Visits, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Number of visits")

ce_3 <- conditional_effects(Visits_PCs, effects = "PC3",points=T) 
colnames(ce_3[[1]])[3] <- "Interaction"

Visits_PC3 <- ggplot(ce_3[[1]], aes(x = PC3, y = (estimate__+1))) + geom_point(data = data_analysis_2,aes(x = PC3, y = Visits),
  size = 1.5, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(data_analysis_2$Visits, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Number of visits")

cowplot::plot_grid(Visits_PC1,Visits_PC2,Visits_PC3)
########################################################################################################################################################
#MODEL SPECIALIZATION ~  PRINCIPAL COMPONENT
########################################################################################################################################################

Specialization_PCs <- brm(d ~ PC1 + PC2 + PC3 + (1|Id),
                          data = data_analysis_2, family  = skew_normal(), cores = 4,chains = 4, 
                          sample_prior = TRUE, warmup = 500, iter = 2000,
                          control = list(adapt_delta = 0.99)) 

#its kind of bimodal but overall not terrible fit
pp_check(Specialization_PCs) #fitting a mix distribution may be a best solution than a skew_normal|I tried but the model did not converge
#When checking the dist
hist(data_analysis_2$d)
#looks gaussian but has a second peak on the max values


#Explore conditional and marginal Bayesian r2
bayes_R2(Specialization_PCs)
loo_R2(Specialization_PCs)

#Explore each of the PC's
conditional_effects(Specialization_PCs, effects = "PC1",points=T)
conditional_effects(Specialization_PCs, effects = "PC2",points=T)
conditional_effects(Specialization_PCs, effects = "PC3",points=T)

#Save output
setwd("~/Dropbox/PhD/R/Chapter_2") #DROPBOX, files too large for github
saveRDS(Specialization_PCs, "results_analysis_2_Specialization.rds") 
########################################################################################################################################################
#Plot nicely
########################################################################################################################################################
Specialization_PCs <- readRDS("results_analysis_2_Specialization.rds") #load model
setwd("~/R_Projects/Reproductive Traits") #setwd to load data
data_analysis_2 <- readRDS("Data/RData/data_analysis_2.rds") #load data 

ce_1 <- conditional_effects(Specialization_PCs, effects = "PC1",points=T) 
colnames(ce_1[[1]])[3] <- "Interaction"

Specialization_PC1 <- ggplot(ce_1[[1]], aes(x = PC1, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = PC1, y = d),
  size = 1.5, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(data_analysis_2$d, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Specialization (d')")

ce_2 <- conditional_effects(Specialization_PCs, effects = "PC2",points=T) 
colnames(ce_2[[1]])[3] <- "Interaction"

Specialization_PC2 <- ggplot(ce_2[[1]], aes(x = PC2, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = PC2, y = d),
  size = 1.5, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(data_analysis_2$d, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Specialization (d')")

ce_3 <- conditional_effects(Specialization_PCs, effects = "PC3",points=T) 
colnames(ce_3[[1]])[3] <- "Interaction"

Specialization_PC3 <- ggplot(ce_3[[1]], aes(x = PC3, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = PC3, y = d),
  size = 1.5, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(data_analysis_2$d, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Specialization (d')")

cowplot::plot_grid(Specialization_PC1, Specialization_PC2, Specialization_PC3)
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################



