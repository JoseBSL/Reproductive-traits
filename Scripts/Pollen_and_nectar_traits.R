########################################################################################################################################################
#Script to analyse data of pollen and nectar||Visits~Floral rewards
########################################################################################################################################################
#LOAD LIBRARIES
library(ggplot2)
library(brms)
library(ape) #for phylogenetic distance
library(rtrees) #for phylogenetic distancelibrary(MASS)
library(dplyr) #data processing
library(data.table)
library(tidyverse)
library(readxl)

#Set publication theme
# Theme for publication
theme_ms <- function(base_size=12, base_family="Helvetica") {
  (theme_bw(base_size = base_size, base_family = base_family)+
     theme(text=element_text(color="black"),
           axis.title=element_text( size = rel(1.1)),
           axis.text=element_text(size = rel(1), color = "black"),
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
#1) READ TRAIT DATA
########################################################################################################################################################
setwd("~/R_Projects/Reproductive Traits")
#load data
trait_data <- read_excel("Data/Trait_data_raw/Trait_data_final.xlsx",na = "NA")
########################################################################################################################################################
#2) A) CLEAN DATA AND B) RENAME LEVELS FOR IMPUTATION 
########################################################################################################################################################
#####
#A) CLEAN DATA
#####
#select just filled rows
trait_data_1 <- trait_data[1:1712,] #It may be a more elegant way but this does the job, 1712 rows of data

str(trait_data_1)
#filter data, select species with flower level info and capitulum
trait_filtered <- filter(trait_data_1, Info_level == "flower" |  Info_level == "capitulum")
levels(as.factor(trait_filtered$Info_level)) #checking levels
#remove NA's and duplicated species|Some spp are repeated once the spp names are standardize with taxize
trait_filtered$Species_all[trait_filtered$Species_all=="NA"]<-NA 
trait_filtered_1 <- trait_filtered[!is.na(trait_filtered$Species_all),]
trait_filtered_2 <- trait_filtered_1[!duplicated(trait_filtered_1$Species_all),]
str(trait_filtered_2)

#Select columns to work with
t <- trait_filtered_2[c("Species_geonet","Order_all","Family_all","Genus_all","Species_all",
                        "Nectar_presence_absence","Nectar_ul","Nectar_mg","Nectar_concentration","Pollen_ovule_ratio","Pollen_per_flower")]


hist(log(t$Pollen_per_flower))
colnames(t)[1] <- "Plant_species"

#Now read visitation data
long_d <- read.csv("Data/Csv/long_format_quantitative_networks.csv", row.names = 1) #quantitative network data|weighted by frequency of visits per plant species

#Select data with interaction greater than 0
long_d_1 <- long_d[long_d$Interaction>0,]

#Remove other orders/guilds that are not these ones
long_d_2 <- long_d_1[!is.na(long_d_1$guild),] #I do it by guild because just these 6 guilds are named in this column

#check levels
levels(factor(long_d_2$guild)) #9 DIFFERENT GUILDS|After I'll select the main fucntional poll. groups for analysis
#MERGE DATA
data <- merge(long_d_2, t, by="Plant_species")

#remove species that are not until species level
#ALL THE SPECIES WITH SP. ARE DELETD
data$Species_all <- gsub("M_PL_.*","",data$Species_all) #subsitute partial string for nothing
data$Species_all <- gsub(" $","", data$Species_all, perl=T) #remove trailing spaces
data$Species_all <-  sub('sp.1$', 'sp.', data$Species_all)#PREPARE ALL SP TO SP.
data$Species_all <- sub('sp.2$', 'sp.', data$Species_all)#PREPARE ALL SP TO SP.
data$Species_all <- sub('sp$', 'sp.', data$Species_all) #PREPARE ALL SP TO SP.
data$Species_all <- sub("sp.$", "DELETE", data$Species_all) #change all sp. for DELETE
data <- data[- grep("DELETE",  data$Species_all),] #remove species with "DELETE"
#CHECK LEVELS
levels(factor(t$Species_all))

#Make these NA's as NA
data$Species_all[data$Species_all=="NA"] <- NA
data$Family_all[data$Family_all=="NA"] <- NA
data$Genus_all[data$Genus_all=="NA"] <- NA
#remove NA's
data <- data[!is.na(data$Family_all),]
data <- data[!is.na(data$Species_all),]
data <- data[!is.na(data$Genus_all),]

#Prepare species, genus anD_5 family for calculating phylogenetic distance
data$Family_all  <- as.character(data$Family_all)
data$Genus_all   <- as.character(data$Genus_all)
data$Species_all <- as.character(data$Species_all)

#prepare dataframe to calculate tree
phylo_5 <- as.data.frame(cbind(data$Family_all, data$Genus_all, data$Species_all))
colnames(phylo_5) <-  c("family", "genus", "species")

#Select unique cases
phylo_5_1 <- phylo_5[!duplicated(phylo_5$species),]
phylo_5_2 <- tibble(phylo_5_1)
str(phylo_5_2)

phylo_5_3 <- get_tree(sp_list = phylo_5_2, tree = tree_plant_otl, taxon = "plant")

#Convert phylogenetic tree into matrix
A_5 <- vcv.phylo(phylo_5_3)
#Standardize to max value 1
A_5 <- A_5/max(A_5)
#Unify column names; remove underscore and remove asterik
rownames(A_5) <- gsub("\\*", "", rownames(A_5))
colnames(A_5) <- gsub("\\*", "", colnames(A_5))
colnames(A_5) <- gsub("_", " ", colnames(A_5))
rownames(A_5) <- gsub("_", " ", rownames(A_5))

#Add phylo column to dataset
data$phylo
data$phylo <- data$Species_all
str(data)

#Create column of system for nested random effects

data$System <- data$Id


data$System[grepl("peralta_2006", data$System)] <- "peralta_2006"
data$System[grepl("small_1976", data$System)] <- "small_1976"
data$System[grepl("arroyo_correa_new_zealand", data$System)] <- "arroyo_correa_2019"
data$System[grepl("fang_2008", data$System)] <- "fang_2008"
data$System[grepl("kaiser-bunbury_2017", data$System)] <- "kaiser-bunbury_2017"
data$System[grepl("inouye_1988", data$System)] <- "inouye_1988"
data$System[grepl("kaiser-bunbury_2010", data$System)] <- "kaiser-bunbury_2010"
data$System[grepl("kaiser-bunbury_2011", data$System)] <- "kaiser-bunbury_2011"
data$System[grepl("burkle_usa_2013", data$System)] <- "burkle_2013"
data$System[grepl("dicks_2002", data$System)] <- "dicks_2002"
data$System[grepl("dupont_2009", data$System)] <- "dupont_2009"
data$System[grepl("bartomeus_spain_2008_medca", data$System)] <- "bartomeus_2008"
data$System[grepl("bartomeus_spain_2008_batca", data$System)] <- "bartomeus_2008"
data$System[grepl("bartomeus_spain_2008_selop", data$System)] <- "bartomeus_2008"
data$System[grepl("bartomeus_spain_2008_miqop", data$System)] <- "bartomeus_2008"
data$System[grepl("bartomeus_spain_2008_fraop", data$System)] <- "bartomeus_2008"
data$System[grepl("lundgren_2005", data$System)] <- "lundgren_2005"
data$System[grepl("olesen_2002_mauritius", data$System)] <- "olesen_2002_mauritius"
data$System[grepl("olesen_2002_azores", data$System)] <- "olesen_2002_azores"
data$System[grepl("bartomeus_spain_2008", data$System)] <- "bartomeus_spain_2008"
data$System[grepl("bundgaard_2003_denmark", data$System)] <- "bundgaard_2003"
data$System[grepl("elberling_sweeden_1999", data$System)] <- "elberling_1999"

#Save data to other dataframe so we keep the original for plotting
data_1 <- data

#Convert to logarithmic scale (fixed effect)
data_1$Pollen_per_flower <- log10(data_1$Pollen_per_flower + 1)
data_1$Pollen_ovule_ratio <- log10(data_1$Pollen_ovule_ratio + 1)
data_1$Nectar_ul <- log10(data_1$Nectar_ul +1)
data_1$Nectar_mg <- as.numeric(data_1$Nectar_mg)
data_1$Nectar_mg <- log10(data_1$Nectar_mg + 1)

data$
hist(data_1$Nectar_mg)
hist(as.numeric(data$Nectar_mg))

################################################################################################################################################################
#POLLEN ALL
################################################################################################################################################################
m_pollen <- brm((Interaction-1) ~ Pollen_per_flower +(1|System/Id) + (1|gr(phylo, cov = A)),
                                        data = data_1, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                                        sample_prior = TRUE, warmup = 500, iter = 2000,
                                        control = list(adapt_delta = 0.99))

performance::r2(m_pollen)

plot_pollen <- conditional_effects(m_pollen)

plot(plot_pollen, points=TRUE)


saveRDS(m_pollen, "model_pollen_all.rds")
#POLLEN
pollen_plot_all <- ggplot(plot_pollen[[1]], aes(Pollen_per_flower, estimate__+1)) + geom_line() + geom_point(data = data_1,aes(x = Pollen_per_flower, y = Interaction),
  size = 1, alpha=0.5) +  ylim(0,quantile(data$Interaction, 0.95)) + xlab("Pollen grains per flower (log scale)") +theme_ms() + ylab("Visits") +
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), alpha=0.15) + scale_x_continuous(breaks = c(1,2,3,4,5,6),labels = c(10^1,10^2,10^3,10^4,10^5,10^6))

################################################################################################################################################################
#POLLEN OVULE RATIO ALL
################################################################################################################################################################
m_pollen_ovule <- brm((Interaction-1) ~ Pollen_ovule_ratio +(1|System/Id) + (1|gr(phylo, cov = A)),
                      data = data_1, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                      sample_prior = TRUE, warmup = 500, iter = 2000,
                      control = list(adapt_delta = 0.99))

plot_pollen_ovule <- conditional_effects(m_pollen_ovule)
plot(plot_pollen_ovule, points=TRUE)

#POLLEN OVULE RATIO
pollen_ovule_ratio_plot_all <- ggplot(plot_pollen_ovule[[1]], aes(Pollen_ovule_ratio, estimate__+1)) + geom_line() + geom_point(data = data_1,aes(x = Pollen_ovule_ratio, y = Interaction),
  size = 1, alpha=0.5) +  ylim(0,quantile(data$Interaction, 0.95)) + xlab("Pollen-ovule ratio (log scale)") +theme_ms() + ylab("Visits") +
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), alpha=0.15) + scale_x_continuous(breaks = c(1,2,3,4,5),labels = c(10^1,10^2,10^3,10^4,10^5))

################################################################################################################################################################
#NECTAR MICROLITRES ALL
################################################################################################################################################################
m_nectar_ul <- brm((Interaction-1) ~ Nectar_ul +(1|System/Id) + (1|gr(phylo, cov = A)),
                data = data_1, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                sample_prior = TRUE, warmup = 500, iter = 2000,
                control = list(adapt_delta = 0.99))


plot_nectar <- conditional_effects(m_nectar_ul)
plot(plot_nectar, points=TRUE)

nectar_ul_plot_all <- ggplot(plot_nectar[[1]], aes(Nectar_ul, estimate__+1)) + geom_line() + geom_point(data = data_1,aes(x = Nectar_ul, y = Interaction),
  size = 1, alpha=0.5) +  ylim(0,quantile(data$Interaction, 0.95)) + xlab("Microlitres of nectar (log scale)") +theme_ms() + ylab("Visits") +
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), alpha=0.15) + scale_x_continuous(breaks = c(0,1,2),labels = c(10^0-1,10^1,10^2))

################################################################################################################################################################
#NECTAR MG ALL
################################################################################################################################################################
m_nectar_mg <- brm((Interaction-1) ~ Nectar_mg +(1|System/Id) + (1|gr(phylo, cov = A)),
                   data = data_1, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                   sample_prior = TRUE, warmup = 500, iter = 2000,
                   control = list(adapt_delta = 0.99))

plot_nectar_mg <- conditional_effects(m_nectar_mg)
plot(plot_nectar_mg, points=TRUE)

nectar_mg_plot_all <-  ggplot(plot_nectar_mg[[1]], aes(Nectar_mg, estimate__+1)) + geom_line() + geom_point(data = data_1,aes(x = Nectar_mg, y = Interaction),
  size = 1, alpha=0.5) +  ylim(0,quantile(data$Interaction, 0.95)) + xlab("Miligrams of nectar sugar (log scale)") +theme_ms() + ylab("Visits") +
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), alpha=0.15) + scale_x_continuous(breaks = c(0,0.3,0.6),labels = c(0,2,4))
################################################################################################################################################################
#NECTAR COCENTARTION ALL
################################################################################################################################################################
m_nectar_con <- brm((Interaction-1) ~ Nectar_concentration +(1|System/Id) + (1|gr(phylo, cov = A)),
                   data = data, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                   sample_prior = TRUE, warmup = 500, iter = 2000,
                   control = list(adapt_delta = 0.99))

plot_nectar_con <- conditional_effects(m_nectar_con)
plot(plot_nectar_con, points=TRUE)

nectar_con_plot_all <- ggplot(plot_nectar_con[[1]], aes(Nectar_concentration, estimate__+1)) + geom_line() + geom_point(data = data,aes(x = Nectar_concentration, y = Interaction),
  size = 1, alpha=0.5) +  ylim(0,quantile(data$Interaction, 0.95)) + xlab("Nectar concentration") +theme_ms() + ylab("Visits") +
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), alpha=0.15)

################################################################################################################################################################
################################################################################################################################################################
###### NOW FOR BEES#########
################################################################################################################################################################
################################################################################################################################################################

################################################################################################################################################################
#POLLEN BEES
################################################################################################################################################################
data_bees <- subset(data, guild=="Bee")

data_bees$Pollen_per_flower <- log(data_bees$Pollen_per_flower)

m_pollen_bees <- brm((Interaction-1) ~ Pollen_per_flower +(1|System/Id) + (1|gr(phylo, cov = A)),
                data = data_bees, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                sample_prior = TRUE, warmup = 500, iter = 3000,
                control = list(adapt_delta = 0.99))


plot_pollen_bees <- conditional_effects(m_pollen_bees)

plot(plot_pollen_bees, points=TRUE)

pollen_bees <- ggplot(plot_pollen_bees[[1]], aes(Pollen_per_flower, estimate__)) + geom_line() + geom_point(data = data_bees,aes(x = Pollen_per_flower, y = Interaction),
      size = 0.75, alpha=0.5) +  ylim(0,quantile(data_bees$Interaction, 0.95)) + xlab("log(Pollen per flower)")

################################################################################################################################################################
#NECTAR UL BEES
################################################################################################################################################################
hist(data_bees$Nectar_ul)

data_bees$Nectar_ul <- log(data_bees$Nectar_ul +1)

m_nectar_ul_bees <- brm((Interaction-1) ~ Nectar_ul +(1|System/Id) + (1|gr(phylo, cov = A)),
                   data = data_bees, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                   sample_prior = TRUE, warmup = 500, iter = 2000,
                   control = list(adapt_delta = 0.99))


plot_nectar_bees <- conditional_effects(m_nectar_ul_bees)

plot(plot_nectar_bees, points=TRUE)

nectar_ul <- ggplot(plot_nectar_bees[[1]], aes(Nectar_ul, estimate__)) + geom_line() + geom_point(data = data_bees,aes(x = Nectar_ul, y = Interaction),
  size = 0.75, alpha=0.5) +  ylim(0,quantile(data_bees$Interaction, 0.95)) + xlab("log(Nectar ul)")

hist(data$Nectar_mg)

################################################################################################################################################################
#NECTAR MG BEES
################################################################################################################################################################
data_bees$Nectar_m <- as.numeric(data_bees$Nectar_m)
data_bees$Nectar_mg <- log(data_bees$Nectar_m + 1)

m_nectar_mg_bees <- brm((Interaction-1) ~ Nectar_mg +(1|System/Id) + (1|gr(phylo, cov = A)),
                   data = data_bees, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                   sample_prior = TRUE, warmup = 500, iter = 2000,
                   control = list(adapt_delta = 0.99))

plot_nectar_mg_bees <- conditional_effects(m_nectar_mg_bees)
plot(plot_nectar_mg_bees, points=TRUE)

nectar_mg <- ggplot(plot_nectar_mg_bees[[1]], aes(Nectar_mg, estimate__)) + geom_line() + geom_point(data = data_bees,aes(x = Nectar_mg, y = Interaction),
     size = 0.75, alpha=0.5) +  ylim(0,quantile(data_bees$Interaction, 0.95)) + xlab("log(Nectar mg)")
################################################################################################################################################################
#NECTARCONCENTRATION BEES
################################################################################################################################################################
m_nectar_con_bees <- brm((Interaction-1) ~ Nectar_concentration +(1|System/Id) + (1|gr(phylo, cov = A)),
                    data = data_bees, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                    sample_prior = TRUE, warmup = 500, iter = 2000,
                    control = list(adapt_delta = 0.99))

plot_nectar_con_bees <- conditional_effects(m_nectar_con_bees)
plot(plot_nectar_con_bees, points=TRUE)

nectar_con <- ggplot(plot_nectar_con_bees[[1]], aes(Nectar_concentration, estimate__)) + geom_line() + geom_point(data = data_bees,aes(x = Nectar_concentration, y = Interaction),
size = 0.75, alpha=0.5) +  ylim(0,quantile(data_bees$Interaction, 0.95)) + xlab("Nectar concentration")
################################################################################################################################################################
#POLLEN OVULE RATIO BEES
################################################################################################################################################################
data_bees$Pollen_ovule_ratio <- log(data_bees$Pollen_ovule_ratio + 1)

m_pollen_ovule_bees <- brm((Interaction-1) ~ Pollen_ovule_ratio +(1|System/Id) + (1|gr(phylo, cov = A)),
                      data = data_bees, family  = zero_inflated_negbinomial(),data2 = list(A = A_5), cores = 4,chains = 4, 
                      sample_prior = TRUE, warmup = 500, iter = 2000,
                      control = list(adapt_delta = 0.99))

plot_pollen_ovule_bees <- conditional_effects(m_pollen_ovule_bees)
plot(plot_pollen_ovule_bees, points=TRUE)

polle_ovule <- ggplot(plot_pollen_ovule_bees[[1]], aes(Pollen_ovule_ratio, estimate__)) + geom_line() + geom_point(data = data_bees,aes(x = Pollen_ovule_ratio, y = Interaction),
size = 0.75, alpha=0.5) +  ylim(0,quantile(data_bees$Interaction, 0.95)) + xlab("log(Pollen:ovule ratio)")


################################################################################################################################################################
#SAVE MODELS
################################################################################################################################################################


cowplot::plot_grid(pollen_bees,polle_ovule,nectar_ul,nectar_mg,nectar_con )

setwd("~/Dropbox/PhD/R/Chapter_2")
#BEE MODELS
saveRDS(m_pollen_ovule_bees, "model_pollen_ovule_bees.rds")
saveRDS(m_nectar_con_bees, "model_nectar_con_bees.rds")
saveRDS(m_nectar_mg_bees, "model_nectar_mg_bees.rds")
saveRDS(m_nectar_ul_bees, "model_nectar_ul_bees.rds")
saveRDS(m_pollen_bees, "model_pollen_bees.rds")

#ALL MODELS
saveRDS(m_pollen_ovule, "model_pollen_ovule_all.rds")
saveRDS(m_nectar_ul, "model_nectar_con_all.rds")
saveRDS(m_nectar_mg, "model_nectar_mg_all.rds")
saveRDS(m_nectar_con, "model_nectar_ul_all.rds")
saveRDS(m_pollen, "model_pollen_all.rds")

#Save data
saveRDS(data, "model_all.rds")
saveRDS(data_bees, "model_bees.rds")

#Read data

#BEE MODELS
model_pollen_ovule_bees <- readRDS("model_pollen_ovule_bees.rds")
model_nectar_con_bees <- readRDS("model_nectar_con_bees.rds")
model_nectar_mg_bees <- readRDS("model_nectar_mg_bees.rds")
model_nectar_ul_bees <- readRDS("model_nectar_ul_bees.rds")
model_pollen_bees <- readRDS("model_pollen_bees.rds")

#ALL MODELS
m_pollen_ovule <- readRDS("model_pollen_ovule_all.rds")
m_nectar_con <- readRDS("model_nectar_con_all.rds")
model_nectar_mg_all <- readRDS("model_nectar_mg_all.rds")
m_nectar_con <- readRDS("model_nectar_ul_all.rds")
m_pollen <- readRDS("model_pollen_all.rds")

#Save data
data <- readRDS("model_all.rds")
model_bees <- readRDS("model_bees.rds")

 


library(scales)

#The zero inflated negative binomial requieres zeros
#We have substracted previously 1 unit and we add it now in the gg


#POLLEN
ggplot(plot_pollen[[1]], aes(Pollen_per_flower, estimate__+1)) + geom_line() + geom_point(data = data_1,aes(x = Pollen_per_flower, y = Interaction),
size = 1, alpha=0.5) +  ylim(0,quantile(data$Interaction, 0.95)) + xlab("Pollen grains per flower (log scale)") +theme_ms() + ylab("Visits") +
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), alpha=0.15) + scale_x_continuous(breaks = c(1,2,3,4,5,6),labels = c(10^1,10^2,10^3,10^4,10^5,10^6))
  
#POLLEN OVULE RATIO
ggplot(plot_pollen_ovule[[1]], aes(Pollen_ovule_ratio, estimate__+1)) + geom_line() + geom_point(data = data_1,aes(x = Pollen_ovule_ratio, y = Interaction),
  size = 1, alpha=0.5) +  ylim(0,quantile(data$Interaction, 0.95)) + xlab("Pollen grains per flower (log scale)") +theme_ms() + ylab("Visits") +
  geom_ribbon(aes(ymin=(lower__+1), ymax=(upper__+1)), alpha=0.15) + scale_x_continuous(breaks = c(2,4,6,8,10,12),labels = c(10^2,10^4,10^6,10^8,10^10,10^12))

library(patchwork)
nectar_mg_plot_all

pollen_plot_all + pollen_ovule_ratio_plot_all + nectar_ul_plot_all  + nectar_con_plot_all 
