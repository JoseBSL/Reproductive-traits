########################################################################################################################################################
#Analysis of how visitation and specialization changes with the two principal components
########################################################################################################################################################

#Load libraries
library(brms)
library(cmdstanr)
library(cowplot) #panel of plots

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

########################################################################################################################################################################
#Set working directory
setwd("~/R_Projects/Reproductive Traits")
########################################################################################################################################################################
#LOAD DATA
data <- read.csv("Data/Csv/metric_analysis_data_3rd_question.csv", row.names = 1) # data with specialization and integer of visits
head(data) #Check data
#Now we obtain the loadings from the global PCA which is likely to define better in the trait space the species
phyl_pca_2 <- readRDS("Data/RData/phyl_pca_forest.rds")
########################################################################################################################################################################
#PROCESS DATA BEFORE MODELLING
pca_loadings_2 <- data.frame(phyl_pca_2$S)
#prepare cols for merging
rownames(pca_loadings_2) <- gsub("_", " ", rownames(pca_loadings_2)) #remove underscores from species names
#set same colnames for merging
pca_loadings_2 <- setDT(pca_loadings_2, keep.rownames = TRUE)[]
#same colname
colnames(pca_loadings_2)[1] <- "Species"
#check data for merge
head(data)
#merge columns
data_analysis2 <- merge(data, pca_loadings_2, by="Species")
########################################################################################################################################################################
#RUN ANALYSIS VISITS ~ PRINCIPAL COMPONENTS 1 AND 2 FROM THE PPCA WITH ALL SPECIES
#VISITS PC1
Visits_PC1 <- brm((Visits-1) ~ PC1 + (1|Id),
                  data = data_analysis2, family  = zero_inflated_negbinomial(), cores = 4,chains = 4, 
                  sample_prior = TRUE, warmup = 500, iter = 2000,
                  control = list(adapt_delta = 0.99)) 

#looks ok
pp_check(Visits_PC1) +xlim(-50,200)+ylim(0,0.1)

bayes_R2(Visits_PC1)
loo_R2(Visits_PC1)

conditional_effects(Visits_PC1, effects = "PC1",points=T)


ce_1 <- conditional_effects(Visits_PC1, effects = "PC1",points=T) 
colnames(ce_1[[1]])[3] <- "Interaction"

Visits_PC1 <- ggplot(ce_1[[1]], aes(x = PC1, y = (estimate__+1))) + geom_point(data = data_analysis2,aes(x = PC1, y = Visits),
       size = 1.5, alpha=0.9) + geom_line(colour="darkblue",size=1.2) + ylim(0,quantile(data_analysis2$Visits, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Number of visits")


#VISITS PC2
Visits_PC2 <- brm((Visits-1) ~ PC2 + (1|Id),
                  data = data_analysis2, family  = zero_inflated_negbinomial(), cores = 4,chains = 4, 
                  sample_prior = TRUE, warmup = 500, iter = 2000,
                  control = list(adapt_delta = 0.99)) 



bayes_R2(Visits_PC2)
loo_R2(Visits_PC2)

#looks ok
pp_check(Visits_PC2) +xlim(-50,200)+ylim(0,0.1)

conditional_effects(Visits_PC2, effects = "PC2",points=T)

ce_2 <- conditional_effects(Visits_PC2, effects = "PC2",points=T) 
colnames(ce_2[[1]])[2] <- "Interaction"

Visits_PC2 <- ggplot(ce_2[[1]], aes(x = PC2, y = (estimate__+1))) + geom_point(data = data_analysis2,aes(x = PC2, y = Visits),
             size = 1.5, alpha=0.9) + geom_line(colour="darkgreen",size=1.2) + ylim(0,quantile(data_analysis2$Visits, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="darkgreen") + ylab("Number of visits")

########################################################################################################################################################################
#RUN ANALYSIS VISITS ~ PRINCIPAL COMPONENTS 1 AND 2 FROM THE PPCA WITH ALL SPECIES
#Specilization PC1
Specialization_PC1 <- brm(d ~ PC1 + (1|Id),
                          data = data_analysis2, family  = skew_normal(), cores = 4,chains = 4, 
                          sample_prior = TRUE, warmup = 500, iter = 2000,
                          control = list(adapt_delta = 0.99)) 

#looks ok
pp_check(Specialization_PC1) 


conditional_effects(Specialization_PC1, effects = "PC1",points=T)


spe_1 <- conditional_effects(Specialization_PC1, effects = "PC1",points=T) 

Specialization_PC1 <- ggplot(spe_1[[1]], aes(x = PC1, y = (estimate__))) + geom_point(data = data_analysis2,aes(x = PC1, y = d),
                 size = 1.5, alpha=0.9) + geom_line(colour="darkblue",size=1.2)  +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkblue") + ylab("Specilization (d)")


#Specilization PC2
Specialization_PC2 <- brm(d ~ PC2 + (1|Id),
                          data = data_analysis2, family  = skew_normal(), cores = 4,chains = 4, 
                          sample_prior = TRUE, warmup = 500, iter = 2000,
                          control = list(adapt_delta = 0.99)) 

#looks ok
pp_check(Specialization_PC2) 


conditional_effects(Specialization_PC2, effects = "PC2",points=T)


spe_2 <- conditional_effects(Specialization_PC2, effects = "PC2",points=T) 

Specialization_PC2 <- ggplot(spe_2[[1]], aes(x = PC2, y = (estimate__))) + geom_point(data = data_analysis2,aes(x = PC2, y = d),
     size = 1.5, alpha=0.9,colour="grey") + geom_line(colour="darkgreen",size=1.2)  +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="darkgreen") + ylab("Specilization (d)")


plot_grid(Visits_PC1,Visits_PC2,Specialization_PC1,Specialization_PC2)

########################################################################################################################################################################
#Save model outputs to plot in markdown file
#Save in dropbox they are too large for git

setwd("~/Dropbox/PhD/R") #DROPBOX, files too large for github
saveRDS(Visits_PC1, "Visits_PC1.RDS")
saveRDS(Visits_PC2, "Visits_PC2.RDS")
saveRDS(Specialization_PC1, "Specialization_PC1.RDS")
saveRDS(Specialization_PC2, "Specialization_PC2.RDS")

