########################################################################################################################################################
# SCRIPT FOR ANALYSIS 1 VISITS/SPECIALIZATION ~ PRINCIPAL COMPONENTS
########################################################################################################################################################
#Load libraries
library(brms) #modelling
library(cmdstanr) #modelling
library(cowplot) #panel of plots
library(ggplot2) #plotting
library(patchwork)

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
colnames(data_analysis_2)[11] <- "Id"

data_analysis_2$System <- data_analysis_2$Id

data_analysis_2$System[grepl("peralta_2006", data_analysis_2$System)] <- "peralta_2006"
data_analysis_2$System[grepl("small_1976", data_analysis_2$System)] <- "small_1976"
data_analysis_2$System[grepl("arroyo_correa_new_zealand", data_analysis_2$System)] <- "arroyo_correa_2019"
data_analysis_2$System[grepl("fang_2008", data_analysis_2$System)] <- "fang_2008"
data_analysis_2$System[grepl("kaiser-bunbury_2017", data_analysis_2$System)] <- "kaiser-bunbury_2017"
data_analysis_2$System[grepl("inouye_1988", data_analysis_2$System)] <- "inouye_1988"
data_analysis_2$System[grepl("kaiser-bunbury_2010", data_analysis_2$System)] <- "kaiser-bunbury_2010"
data_analysis_2$System[grepl("kaiser-bunbury_2011", data_analysis_2$System)] <- "kaiser-bunbury_2011"
data_analysis_2$System[grepl("burkle_usa_2013", data_analysis_2$System)] <- "burkle_2013"
data_analysis_2$System[grepl("dicks_2002", data_analysis_2$System)] <- "dicks_2002"
data_analysis_2$System[grepl("dupont_2009", data_analysis_2$System)] <- "dupont_2009"
data_analysis_2$System[grepl("bartomeus_spain_2008_medca", data_analysis_2$System)] <- "bartomeus_2008"
data_analysis_2$System[grepl("bartomeus_spain_2008_batca", data_analysis_2$System)] <- "bartomeus_2008"
data_analysis_2$System[grepl("bartomeus_spain_2008_selop", data_analysis_2$System)] <- "bartomeus_2008"
data_analysis_2$System[grepl("bartomeus_spain_2008_miqop", data_analysis_2$System)] <- "bartomeus_2008"
data_analysis_2$System[grepl("bartomeus_spain_2008_fraop", data_analysis_2$System)] <- "bartomeus_2008"
data_analysis_2$System[grepl("lundgren_2005", data_analysis_2$System)] <- "lundgren_2005"
data_analysis_2$System[grepl("olesen_2002_mauritius", data_analysis_2$System)] <- "olesen_2002_mauritius"
data_analysis_2$System[grepl("olesen_2002_azores", data_analysis_2$System)] <- "olesen_2002_azores"
data_analysis_2$System[grepl("bartomeus_spain_2008", data_analysis_2$System)] <- "bartomeus_spain_2008"
data_analysis_2$System[grepl("bundgaard_2003_denmark", data_analysis_2$System)] <- "bundgaard_2003"
data_analysis_2$System[grepl("elberling_sweeden_1999", data_analysis_2$System)] <- "elberling_1999"

#Save data for plotting
setwd("~/Dropbox/PhD/R/Chapter_2") #save with rest of the files in dropbox
saveRDS(data_analysis_2, "data_analysis_2.rds") 


########################################################################################################################################################
#MODEL VISITS ~ PRINCIPAL COMPONENTS
########################################################################################################################################################
Visits_PCs <- brm((Visits-1) ~ PC1 + PC2 + PC3  +(1|System/Id),
                  data = data_analysis_2, family  = zero_inflated_negbinomial(), cores = 4,chains = 4, 
                  sample_prior = TRUE, warmup = 1000, iter = 3000,
                  control = list(adapt_delta = 0.99)) 

#looks ok
pp_check(Visits_PCs) +xlim(-50,200)+ylim(0,0.1)
#summary
summary(Visits_PCs)

performance::r2(Visits_PCs)

loo_R2(Visits_PCs)


#Save output
setwd("~/Dropbox/PhD/R/Chapter_2") #DROPBOX, files too large for github
saveRDS(Visits_PCs, "results_analysis_2_Visits.rds") 
#read
Visits_PCs <- readRDS("results_analysis_2_Visits.rds") #load model

ce_1 <- conditional_effects(Visits_PCs, effects = "PC1",points=T) 
colnames(ce_1[[1]])[3] <- "Interaction"

Visits_PC1 <- ggplot(ce_1[[1]], aes(x = -PC1, y = (estimate__+1))) + geom_point(data = data_analysis_2,aes(x = -PC1, y = Visits),
  size = 1.25, alpha=0.65, colour="goldenrod3") + geom_line(colour="black",size=1) + ylim(0,quantile(data_analysis_2$Visits, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="black") + ylab("Sum of visits") + xlab("PC1")

ce_2 <- conditional_effects(Visits_PCs, effects = "PC2",points=T) 
colnames(ce_2[[1]])[3] <- "Interaction"

Visits_PC2 <- ggplot(ce_2[[1]], aes(x = -PC2, y = (estimate__+1))) + geom_point(data = data_analysis_2,aes(x = -PC2, y = Visits),
  size = 1.25, alpha=0.65, colour="goldenrod3") + geom_line(colour="black",size=1) + ylim(0,quantile(data_analysis_2$Visits, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="black") + ylab("Sum of visits") + xlab("PC2")

ce_3 <- conditional_effects(Visits_PCs, effects = "PC3",points=T) 
colnames(ce_3[[1]])[3] <- "Interaction"

Visits_PC3 <- ggplot(ce_3[[1]], aes(x = -PC3, y = (estimate__+1))) + geom_point(data = data_analysis_2,aes(x = -PC3, y = Visits),
  size = 1.25, alpha=0.65, colour="goldenrod3") + geom_line(colour="black",size=1) + ylim(0,quantile(data_analysis_2$Visits, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), linetype=2, alpha=0.1,fill="black") + ylab("Sum of visits") + xlab("PC3")

cowplot::plot_grid(Visits_PC1,Visits_PC2,Visits_PC3, ncol=3)
########################################################################################################################################################
# MODEL SPECIALIZATION ~ PRINCIPAL COMPONENTS
########################################################################################################################################################
data_analysis_2$d <- ifelse(data_analysis_2$d > 1, 1, data_analysis_2$d)

Specialization_PCs <- brm(d ~ PC1 + PC2 + PC3 + (1|System/Id),
                          data = data_analysis_2, family  = zero_one_inflated_beta(), cores = 4,chains = 4, 
                          sample_prior = TRUE, warmup = 1000, iter = 3000,
                          control = list(adapt_delta = 0.99)) 


summary(Specialization_PCs)

performance::r2(Specialization_PCs)


#Save output
setwd("~/Dropbox/PhD/R/Chapter_2") #DROPBOX, files too large for github
saveRDS(Specialization_PCs, "results_analysis_2_Specialization_PCs.rds") 
#read
Specialization_PCs <- readRDS("results_analysis_2_Specialization_PCs.rds") #load model

#DO PPCHECK HERE

ce_1 <- conditional_effects(Specialization_PCs, effects = "PC1",points=T) 
colnames(ce_1[[1]])[3] <- "Interaction"

Specialization_PC1 <- ggplot(ce_1[[1]], aes(x = -PC1, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC1, y = d),
  size = 1.25, alpha=0.65, colour="darkorange3") + geom_line(colour="black",size=1) + ylim(0,quantile(data_analysis_2$d, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="black") + ylab("Specialization (d')") + xlab("PC1")


ce_2 <- conditional_effects(Specialization_PCs, effects = "PC2",points=T) 
colnames(ce_2[[1]])[3] <- "Interaction"

Specialization_PC2 <- ggplot(ce_2[[1]], aes(x = -PC2, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC2, y = d),
  size = 1.25, alpha=0.65, colour="darkorange3") + geom_line(colour="black",size=1) + ylim(0,quantile(data_analysis_2$d, 0.95)) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="black") + ylab("Specialization (d')") + xlab("PC2")

ce_3 <- conditional_effects(Specialization_PCs, effects = "PC3",points=T) 
colnames(ce_3[[1]])[3] <- "Interaction"

Specialization_PC3 <- ggplot(ce_3[[1]], aes(x = -PC3, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC3, y = d),
 size = 1.25, alpha=0.65, colour="darkorange3") + ylim(0,quantile(data_analysis_2$d, 0.95)) +theme_ms()+geom_line(colour="black",size=1)+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.1,fill="black") + ylab("Specialization (d')") + xlab("PC3")

cowplot::plot_grid(Specialization_PC1, Specialization_PC2, Specialization_PC3)

cowplot::plot_grid(Visits_PC1,Visits_PC2,Visits_PC3,Specialization_PC1, Specialization_PC2, Specialization_PC3, ncol=3)


########################################################################################################################################################
# MODEL NORMALIZED DEGREE ~ PRINCIPAL COMPONENTS
########################################################################################################################################################

normalised.degree_PCs <- brm(normalised.degree ~ PC1 + PC2 + PC3 + (1|System/Id),
                             data = data_analysis_2, family  = weibull(), cores = 4,chains = 4, 
                             sample_prior = TRUE, warmup = 1000, iter = 3000,
                             control = list(adapt_delta = 0.99)) 


#I have increase the number of iterations for this model because was not converging

performance::r2(normalised.degree_PCs)


pp_check(normalised.degree_PCs) +xlim(-1,1.5)

conditional_effects(normalised.degree_PCs, effects = "PC1",points=T) 
conditional_effects(normalised.degree_PCs, effects = "PC2",points=T) 
conditional_effects(normalised.degree_PCs, effects = "PC3",points=T) 


#Save output
setwd("~/Dropbox/PhD/R/Chapter_2") #DROPBOX, files too large for github
saveRDS(normalised.degree_PCs, "results_analysis_2_normalised.degree_PCs.rds") 
#read
normalised.degree_PCs <- readRDS("results_analysis_2_normalised.degree_PCs.rds") #load model

ce_1 <- conditional_effects(normalised.degree_PCs, effects = "PC1",points=T) 

normalised.degree_PC1 <- ggplot(ce_1[[1]], aes(x = -PC1, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC1, y = normalised.degree),
  size = 1.25, alpha=0.65, colour="aquamarine4") + geom_line(colour="black",size=1)  +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.25,fill="black") + ylab("Normalized degree") + xlab("PC1")

ce_2 <- conditional_effects(normalised.degree_PCs, effects = "PC2",points=T) 

normalised.degree_PC2 <- ggplot(ce_2[[1]], aes(x = -PC2, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC2, y = normalised.degree),
  size = 1.25, alpha=0.65,colour="aquamarine4") + geom_line(colour="black",size=1) +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.25,fill="black") + ylab("Normalized degree") + xlab("PC2")


ce_3 <- conditional_effects(normalised.degree_PCs, effects = "PC3",points=T) 

normalised.degree_PC3 <- ggplot(ce_3[[1]], aes(x = -PC3, y = (estimate__))) + geom_point(data = data_analysis_2,aes(x = -PC3, y = normalised.degree),
  size = 1.25, alpha=0.65,colour="aquamarine4") + geom_line(colour="black",size=1)  +theme_ms()+
  geom_ribbon(aes(ymin=(lower__), ymax=(upper__)), linetype=2, alpha=0.25,fill="black") + ylab("Normalized degree") + xlab("PC3")

normalised.degree_PC1 + normalised.degree_PC2 + normalised.degree_PC3


########################################################################################################################################################
#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL#PLOTALL
########################################################################################################################################################

Visits_PC1 + Visits_PC2 + Visits_PC3 + normalised.degree_PC1 + normalised.degree_PC2 + normalised.degree_PC3 +
  Specialization_PC1 + Specialization_PC2+ Specialization_PC3  + plot_layout(ncol = 3)
  

