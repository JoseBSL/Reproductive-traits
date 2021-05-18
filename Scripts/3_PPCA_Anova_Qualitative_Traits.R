########################################################################################################################################################
#CORRELATION BETWEEN QUALITATIVE VARIABLES AND PC's
########################################################################################################################################################
#NOTES
#I did not include the qualitative variables when I prepare the data for the PPCA
#So I have to recalculate the ata here with the qualitative variables
########################################################################################################################################################
#LOAD LIBRARIES
library(emmeans)
library(ggplot2)
theme_ms <- function(base_size=10, base_family="Helvetica") {
  (theme_bw(base_size = base_size, base_family = base_family)+
     theme(text=element_text(color="black"),
           axis.title=element_text( size = rel(0.8)),
           axis.text=element_text(size = rel(0.8), color = "black"),
           panel.border=element_rect(color="black",size=1.2),
           panel.grid.minor.x =element_blank(),
           panel.grid.minor.y= element_blank(),
           panel.grid.major= element_blank(),
           legend.position = "none",
           plot.title = element_text(face = "bold",size = rel(1.3)),
           plot.subtitle = element_text(face = "bold",size = rel(1.3))
     ))
}

########################################################################################################################################################
#LOAD DATA
phyl_pca_forest <- readRDS("Data/RData/phyl_pca_forest.rds")
dat <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv", row.names = "X")
########################################################################################################################################################
#remove not found species, cannot do PCA with unequal numbers of rows
cols.num <- c("Family_all","Genus_all","Species_all")
dat[cols.num] <- sapply(dat[cols.num],as.character)
dat$Species_all <- gsub("Species_all_", "", dat$Species_all)
########################################################################################################################################################
#3) REMOVE OUTLIERS, OUT OF 2.5-97.5 RANGE WHICH HELPS IMPUTATION PROCESS. SEE ARTICLE FOR REF.
########################################################################################################################################################
dat_cleaning <- dat[,]
#CHECK LEVELS
str(dat)

dat_cleaning_1 <- dat_cleaning %>%
  filter(between(Flowers_per_plant, quantile(Flowers_per_plant, 0.025), quantile(Flowers_per_plant, 0.975)))

dat_cleaning_2 <- dat_cleaning_1 %>%
  filter(between(Corolla_diameter_mean, quantile(Corolla_diameter_mean, 0.025), quantile(Corolla_diameter_mean, 0.975)))

dat_cleaning_3 <- dat_cleaning_2 %>%
  filter(between(Style_length, quantile(Style_length, 0.025), quantile(Style_length, 0.975)))

dat_cleaning_4 <- dat_cleaning_3 %>%
  filter(between(Ovule_number, quantile(Ovule_number, 0.025), quantile(Ovule_number, 0.975)))

dat_cleaning_5 <- dat_cleaning_4 %>%
  filter(between(Plant_height_mean_m, quantile(Plant_height_mean_m, 0.025), quantile(Plant_height_mean_m, 0.975)))


final_d <- dat_cleaning_5[,]

pca_data <- as.data.frame(phyl_pca_forest$S)

d <- cbind(final_d, pca_data)

#Check dist of the pcs's
hist(d$PC1)
hist(d$PC2)
hist(d$PC3)


#Prepare qualitative variables
d$Compatibility_system[d$Compatibility_system=="dioecious"] <- "Unisexual flowers"
d$Compatibility_system[d$Compatibility_system=="monoecious"] <- "Unisexual flowers"
d$Compatibility_system[d$Compatibility_system=="partially_self_compatible"] <- "Partially self compatible"
d$Compatibility_system[d$Compatibility_system=="self_compatible"] <- "Self compatible"
d$Compatibility_system[d$Compatibility_system=="self_incompatible"] <- "Self incompatible"
d$Compatibility_system <- factor(d$Compatibility_system, levels = c("Self compatible", "Partially self compatible", "Self incompatible", "Unisexual flowers"))

d$life_form[d$life_form=="herb"] <- "Herb"
d$life_form[d$life_form=="shrub"] <- "Shrub"
d$life_form[d$life_form=="tree"] <- "Tree"
d$life_form[d$life_form=="vine"] <- "Shrub"

d$Flower_symmetry[d$Flower_symmetry=="actinomorphic"] <- "Actinomorphic"
d$Flower_symmetry[d$Flower_symmetry=="zygomorphic"] <- "Zygomorphic"

d$Flower_symmetry[d$Flower_symmetry=="Funnelform"] <- "Campanulate"
d$Flower_symmetry[d$Flower_symmetry=="Spike"] <- "Brush"


d$Flower_morphology[d$Flower_morphology=="Funnelform"] <- "Campanulate"
d$Flower_morphology[d$Flower_morphology=="Spike"] <- "Brush"

#The other qualitative are ok

#All principal components are normally distributed
#Convert to factor before running model
d[sapply(d, is.character)] <- lapply(d[sapply(d, is.character)], as.factor)

#Run model
model1 <- lm(PC1 ~ Breeding_system + Compatibility_system + lifespan + life_form + Flower_morphology + Flower_symmetry + Nectar_presence_absence, d)
model2 <- lm(PC2 ~ Breeding_system + Compatibility_system + lifespan + life_form + Flower_morphology + Flower_symmetry + Nectar_presence_absence, d)

#Check summary
summary(model1)
#Run Anova to test for associations
anova(model1)
#Check summary
summary(model2)
#Run Anova to test for associations
anova(model2)
########################################################################################################################################################
#1) BREEDING SYSTEM
########################################################################################################################################################
model_breeding_1 <- emmeans(model1, "Breeding_system")
breeding_1 <- as.data.frame(model_breeding_1[1:3])
pairs(model_breeding_1)
breed1 <- multcomp::cld(model_breeding_1, alpha = 0.10, Letters = LETTERS)


breed_1 <- ggplot(breed1,aes(x= Breeding_system,y= emmean,
               label = .group)) +theme_minimal()+
  geom_point(data = breed1, aes(x = Breeding_system, y = emmean, color = Breeding_system),size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL,  color = Breeding_system),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("")+ theme(legend.position = "none")+ theme_ms()+
  scale_color_manual(values=c("forestgreen","orange", "black"))+ labs(title = "PC1",subtitle = "Breeding system")+
  theme(plot.title = element_text(hjust=0.5))+
  geom_text(nudge_x = c(0), nudge_y = c(0.3),color   = "black")


model_breeding_2 <- emmeans(model2, "Breeding_system")
breeding_2 <- as.data.frame(model_breeding_2[1:3])
pairs(model_breeding_2)
breed2 <- multcomp::cld(model_breeding_2, alpha = 0.10, Letters = LETTERS)

breed_2 <- ggplot(breed2,aes(x= Breeding_system,y= emmean,
             label = .group)) +theme_minimal()+
  geom_point(data = breed2, aes(x = Breeding_system, y = emmean, color = Breeding_system),size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL,  color = Breeding_system),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("")+ theme(legend.position = "none")+ theme_ms()+
  scale_color_manual(values=c("forestgreen","orange", "black")) + labs(title = "PC2",subtitle = "Breeding system")+
  theme(plot.title = element_text(hjust=0.5))+
  geom_text(nudge_x = c(0), nudge_y = c(0.3),color   = "black")


########################################################################################################################################################
#2) COMPATIBILITY
########################################################################################################################################################
model_com <- emmeans(model1, "Compatibility_system")
pairs(model_com)

str(model_com)
d <- as.data.frame(model_com[1:4])
comp1 <- multcomp::cld(model_com, alpha = 0.10, Letters = LETTERS)

comp_1 <- ggplot(comp1,aes(x= Compatibility_system,y= emmean,
  label = .group)) +theme_minimal()+
  geom_point(data = comp1, aes(x = Compatibility_system, y = emmean, color = Compatibility_system),size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL,  color = Compatibility_system),
  width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("")+ theme(legend.position = "none")+ theme_ms()+
  scale_color_manual(values=c("cyan4", "deepskyblue2","orange","gray7")) +  labs(title =  "Compatibility system")+
  geom_text(nudge_x = c(0), nudge_y = c(0.3),color   = "black")

model_com <- emmeans(model2, "Compatibility_system")
pairs(model_com)
str(model_com)
d <- as.data.frame(model_com[1:4])
comp2 <- multcomp::cld(model_com, alpha = 0.10, Letters = LETTERS)

comp_2 <- ggplot(comp2,aes(x= Compatibility_system,y= emmean,
  label = .group)) +theme_minimal()+
  geom_point(data = comp2, aes(x = Compatibility_system, y = emmean, color = Compatibility_system),size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL,  color = Compatibility_system),
  width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("")+ theme(legend.position = "none")+ theme_ms()+
  scale_color_manual(values=c("cyan4", "deepskyblue2","orange","gray7")) +  labs(title =  "Compatibility system")+
  geom_text(nudge_x = c(0), nudge_y = c(0.3),color   = "black")



########################################################################################################################################################
#3) LIFESPAN
########################################################################################################################################################
model_lif_1 <- emmeans(model1, "lifespan")
pairs(model_lif_1)
lif_1 <- as.data.frame(model_lif_1[1:2])
lif_1 <- multcomp::cld(model_lif_1, alpha = 0.10, Letters = LETTERS)

lifespan_1 <- ggplot(lif_1,aes(x= lifespan,y= emmean,
             label = .group)) +theme_minimal()+
  geom_point(data = lif_1, aes(x = lifespan, y = emmean, color = lifespan),size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL,  color = lifespan),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("")+ theme(legend.position = "none")+ theme_ms()+
  scale_color_manual(values=c("sienna4","forestgreen")) +  labs(title =  "Lifespan")+
  geom_text(nudge_x = c(0), nudge_y = c(0.3),color   = "black")



model_lif_2 <- emmeans(model2, "lifespan")
pairs(model_lif_2)
lif_2 <- as.data.frame(model_lif_2[1:2])
lif_2 <- multcomp::cld(model_lif_2, alpha = 0.10, Letters = LETTERS)

lifespan_2 <- ggplot(lif_2,aes(x= lifespan,y= emmean,
                 label = .group)) +theme_minimal()+
  geom_point(data = lif_2, aes(x = lifespan, y = emmean, color = lifespan),size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL,  color = lifespan),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("")+ theme(legend.position = "none")+ theme_ms()+
  scale_color_manual(values=c("sienna4","forestgreen"))+  labs(title =  "Lifespan")+
  geom_text(nudge_x = c(0), nudge_y = c(0.3),color   = "black")



########################################################################################################################################################
# 4) LIFE FORM
########################################################################################################################################################

model_lif_1 <- emmeans(model1, "life_form")
pairs(model_lif_1)
lif_1 <- as.data.frame(model_lif_1[1:3])
lif_1 <- multcomp::cld(model_lif_1, alpha = 0.10, Letters = LETTERS)

lifeform_1 <- ggplot(lif_1,aes(x= life_form,y= emmean,
                 label = .group)) +theme_minimal()+
  geom_point(data = lif_1, aes(x = life_form, y = emmean, color = life_form),size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL,  color = life_form),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("")+ theme(legend.position = "none")+ theme_ms()+
  scale_color_manual(values=c("forestgreen", "darkorange2","gray7"))+ labs(title  = "Life form")+
  geom_text(nudge_x = c(0, 0), nudge_y = c(0.4, 0.4, 0.4),color   = "black")


model_lif_2 <- emmeans(model2, "life_form")
pairs(model_lif_2)
lif_2 <- as.data.frame(model_lif_2[1:3])
lif_2 <- multcomp::cld(model_lif_2, alpha = 0.10, Letters = LETTERS)

lifeform_2 <- ggplot(lif_2,aes(x= life_form,y= emmean,
                 label = .group)) +theme_minimal()+
  geom_point(data = lif_2, aes(x = life_form, y = emmean, color = life_form),size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL,  color = life_form),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("")+ theme(legend.position = "none")+ theme_ms()+
  scale_color_manual(values=c("forestgreen", "darkorange2","gray7")) + labs(title =  "Life form")+
  geom_text(nudge_x = c(0, 0), nudge_y = c(0.3, 0.3, 0.3),color   = "black")


########################################################################################################################################################
# 5) FLOWER MORPHOLOGY
########################################################################################################################################################
model_flo_1 <- emmeans(model1, "Flower_morphology")
pairs(model_flo_1)
flo_1 <- as.data.frame(model_flo_1[1:6])
flo_1 <- multcomp::cld(model_flo_1, alpha = 0.10, Letters = LETTERS)

shape_1 <- ggplot(flo_1,aes(x= Flower_morphology,y= emmean,
                 label = .group)) +theme_minimal()+
  geom_point(data = flo_1, aes(x = Flower_morphology, y = emmean, color = Flower_morphology),size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL,  color = Flower_morphology),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("")+ theme(legend.position = "none")+ theme_ms()+
  scale_color_manual(values=c("blue","cyan4", "red","forestgreen","purple","gold"))+labs(title  = "Flower shape")+
  geom_text(nudge_x = c(0, 0), nudge_y = c(0.5, 0.5),color   = "black")



model_flo_2 <- emmeans(model2, "Flower_morphology")
pairs(model_flo_2)
flo_2 <- as.data.frame(model_flo_2[1:6])
flo_2 <- multcomp::cld(model_flo_2, alpha = 0.10, Letters = LETTERS)

shape_2 <- ggplot(flo_2,aes(x= Flower_morphology,y= emmean,
                 label = .group)) +theme_minimal()+
  geom_point(data = flo_2, aes(x = Flower_morphology, y = emmean, color = Flower_morphology),size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL,  color = Flower_morphology),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("")+ theme(legend.position = "none")+ theme_ms()+
  scale_color_manual(values=c("blue","cyan4", "red","forestgreen","purple","gold"))+labs(title  = "Flower shape")+
   geom_text(nudge_x = c(0, 0), nudge_y = c(0.4, 0.4),color   = "black")


########################################################################################################################################################
#6) FLOWER SYMMETRY
########################################################################################################################################################
model_flo_sym_1 <- emmeans(model1, "Flower_symmetry")
pairs(model_flo_sym_1)
flo_sym_1 <- as.data.frame(model_flo_sym_1[1:2])
flo_sym_1 <- multcomp::cld(model_flo_sym_1, alpha = 0.10, Letters = LETTERS)

symmetry_1 <-ggplot(flo_sym_1,aes(x= Flower_symmetry,y= emmean,
                 label = .group)) +theme_minimal()+
  geom_point(data = flo_sym_1, aes(x = Flower_symmetry, y = emmean, color = Flower_symmetry),size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL,  color = Flower_symmetry),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("")+ theme(legend.position = "none")+ theme_ms()+
  scale_color_manual(values=c("sienna2","purple"))+labs(title =  "Flower symmetry")+ geom_text(nudge_x = c(0, 0), nudge_y = c(0.3, 0.3),color   = "black")



model_flo_sym_2 <- emmeans(model2, "Flower_symmetry")
pairs(model_flo_sym_2)
flo_sym_2 <- as.data.frame(model_flo_sym_2[1:2])
flo_sym_2 <- multcomp::cld(model_flo_sym_2, alpha = 0.10, Letters = LETTERS)

symmetry_2 <- ggplot(flo_sym_2,aes(x= Flower_symmetry,y= emmean,
                     label = .group)) +theme_minimal()+
  geom_point(data = flo_sym_2, aes(x = Flower_symmetry, y = emmean, color = Flower_symmetry),size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL,  color = Flower_symmetry),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("")+ theme(legend.position = "none")+ theme_ms()+
  scale_color_manual(values=c("sienna2","purple"))+labs(title = "Flower symmetry")+ geom_text(nudge_x = c(0, 0), nudge_y = c(0.2, 0.2),color   = "black")


########################################################################################################################################################
#7) NECTAR
########################################################################################################################################################
model_nec <- emmeans(model1, "Nectar_presence_absence")
pairs(model_nec)

model_nec_1 <- emmeans(model1, "Nectar_presence_absence")
pairs(model_nec_1)
nec_1 <- as.data.frame(model_nec_1[1:2])
nec_1 <- multcomp::cld(model_nec_1, alpha = 0.10, Letters = LETTERS)

nectar_1 <- ggplot(nec_1,aes(x= Nectar_presence_absence,y= emmean,
                     label = .group)) +theme_minimal()+
  geom_point(data = nec_1, aes(x = Nectar_presence_absence, y = emmean, color = Nectar_presence_absence),size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL,  color = Nectar_presence_absence),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("")+ theme(legend.position = "none")+ theme_ms()+
  scale_color_manual(values=c("black","grey"))+labs(title = "Nectar")+ geom_text(nudge_x = c(0, 0), nudge_y = c(0.2, 0.2),color   = "black")



model_nec_2 <- emmeans(model2, "Nectar_presence_absence")
pairs(model_nec_2)
nec_2 <- as.data.frame(model_nec_2[1:2])
nec_2 <- multcomp::cld(model_nec_2, alpha = 0.10, Letters = LETTERS)

library(ggpubr)

nectar_2 <-ggplot(nec_2,aes(x= Nectar_presence_absence,y= emmean,
                     label = .group)) +theme_minimal()+
  geom_point(data = nec_2, aes(x = Nectar_presence_absence, y = emmean, color = Nectar_presence_absence),size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL,  color = Nectar_presence_absence),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("")+ theme(legend.position = "none")+ theme_ms()+
  scale_color_manual(values=c("black","grey"))+labs(title = "Nectar")+ geom_text(nudge_x = c(0, 0), nudge_y = c(0.2, 0.2),color   = "black")


library(patchwork)


breed_1 + breed_2+ comp_1+ comp_2+ lifespan_1+ lifespan_2+ lifeform_1+ lifeform_2+ shape_1+ shape_2+
symmetry_1+ symmetry_2+ nectar_1+ nectar_2 + plot_layout(ncol = 2)+ plot_annotation(tag_levels = c('A', ')'))


######################################################################################################################################################


#Save model 1 and 2 so I can read them in markdown

saveRDS(model1, "Data/RData/model1_tukey_categories.rds")
saveRDS(model2, "Data/RData/model2_tukey_categories.rds")

model1 <- readRDS("Data/RData/model1_tukey_categories.rds")
model2 <- readRDS("Data/RData/model2_tukey_categories.rds")


library(dplyr)
library(kableExtra)
#CREATE TABLE WITH OUTPUT OF ANOVA MODEL

#PC1
am1 <- anova(model1) # save anova output

am1 <- as.data.frame(am1) #convert to dataframe

am2 <- am1[c(1:7),c(2,4,5)] #select columns of interest

am3 <- am2 %>% mutate(across(is.numeric, ~ round(., 2))) #select just two decimals

rownames(am3) <- c("Breeding system","Compatibility system", "Lifespan", "Life form", "Flower shape", "Flower symmetry", "Nectar provision") #rownames

am4 <- tibble::rownames_to_column(am3, "Functional traits")

am4$PC <- c(rep("PC1",7))


#NOW PC2
#PC1
an1 <- anova(model1) # save anova output

an1 <- as.data.frame(an1) #convert to dataframe

an2 <- an1[c(1:7),c(2,4,5)] #select columns of interest

an3 <- an2 %>% mutate(across(is.numeric, ~ round(., 2))) #select just two decimals

rownames(an3) <- c("Breeding system","Compatibility system", "Lifespan", "Life form", "Flower shape", "Flower symmetry", "Nectar provision") #rownames

an4 <- tibble::rownames_to_column(an3, "Functional traits")

an4$PC <- c(rep("PC2",7))


#Merge

d <- rbind(am4,an4)


#Plot table
#Add latex format in markdown

kable(d, longtable = T, booktabs = T,linesep = "\\addlinespace", escape=FALSE) %>%
  kable_styling(latex_options = c("repeat_header","striped"), font_size = 12, full_width=F,position = "center") 

