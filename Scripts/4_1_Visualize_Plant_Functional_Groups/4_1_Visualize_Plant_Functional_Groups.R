########################################################################################################################################################
#SCRIPT TO EXPLORE THE TRAIT DATA OF THE DIFFERENT CLUSTERS

#1)LOAD DATA 

#2)PHYLOGENETIC DISTANCE OF THE SPECIES -Add them as covariable-

#3)SAVE MODEL

#4)PLOT OUTPUT NICELY
########################################################################################################################################################
library(dplyr)
library(ggplot2)
library(viridis)
<<<<<<< HEAD
library(grid)
library(cowplot)
library("gridExtra")
=======
library(tidyr)
library(ggpubr)
>>>>>>> afbdf8ef850e818754516a1d17cb40ccfd186aa0
########################################################################################################################################################
#1) READ DATA
########################################################################################################################################################
#read unscaled trait data in order to visualize better the clusters
d <- read.csv("Data/Csv/all_species_imputed_trait_data.csv")
#read trait data with clusters
#I'll select the column of clusters and include it on the non standardize trait data
hclust_d_5 <- read.csv("Data/Csv/imputed_trait_data_hclust_5_clusters.csv") #5 clusters
#No add the cluster columns (it has the same order)
d$Clusters <- as.factor(hclust_d_5$Clusters)

#select columns of interest
t <- d[c("Breeding_system","IMPUTED_Compatibility","Autonomous_selfing_level",
                        "Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant", "Flowers_per_inflorescence",
                        "Floral_unit_width", "Corolla_diameter_mean", "Corolla_length_mean", "STYLE_IMPUTED", "OVULES_IMPUTED", "life_form", "lifespan",
                        "IMPUTED_plant_height_mean_m", "Clusters")]


#Exploratory summary of the data
summ_clusters <- t  %>% group_by(Clusters) %>% do(the_summary = summary(.))
summ_clusters$the_summary


#I start visualizing the qualitative variables

########################################################################################################################################################
#Breeding system
df <- t  %>%
  group_by(Breeding_system, Clusters) %>%
  summarise(counts = n()) 

df$Breeding_system <- factor(df$Breeding_system, levels=c("Dioecious","Hermaphrodite", "Monoecious"))

#This is an option to add the missing levels 
df <- complete(df, Breeding_system, Clusters, fill = list(counts=0))

#option1
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Breeding_system),
    stat = "identity", position = position_stack()) +scale_fill_viridis(discrete = T)+theme_classic()

#option2
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Breeding_system),
    stat = "identity", position = position_dodge(0.7),
    width = 0.7
<<<<<<< HEAD
  )+scale_fill_viridis(discrete = T,drop=FALSE)+theme_classic()

#option3
diamonds.frac <- dplyr::sample_frac(diamonds, 1/5)
ggplot(t, aes(Clusters, Breeding_system)) +
  geom_jitter(aes(color = Breeding_system), size = 0.4)+
  ggpubr::color_palette("jco")+
  ggpubr::theme_pubclean()

=======
  )+scale_fill_viridis(discrete = T,drop=FALSE)+theme_pubr(legend = "right")+
  scale_y_continuous(expand = c(0,0.5))


colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pie(rep(1, 8), col = colorBlindBlack8)
>>>>>>> afbdf8ef850e818754516a1d17cb40ccfd186aa0

########################################################################################################################################################
#Compatibility
df <- t  %>%
  group_by(IMPUTED_Compatibility, Clusters) %>%
  summarise(counts = n()) 

#option1
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = IMPUTED_Compatibility),
    stat = "identity", position = position_stack()) +scale_fill_viridis(discrete = T)+theme_classic()

#option2
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = IMPUTED_Compatibility),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  )+scale_fill_viridis(discrete = T)+theme_classic()
########################################################################################################################################################
#Autonomous_selfing_level

str(t$Breeding_system)

df <- t  %>%
  group_by(Autonomous_selfing_level, Clusters) %>%
  summarise(counts = n()) 

#option1
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Autonomous_selfing_level),
    stat = "identity", position = position_stack()) +scale_fill_viridis(discrete = T)+theme_classic()

#option2
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Autonomous_selfing_level),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  )+scale_fill_viridis(discrete = T,drop=FALSE)+theme_classic() + scale_x_discrete(drop=FALSE)
########################################################################################################################################################
#Flower_morphology
df <- t  %>%
  group_by(Flower_morphology, Clusters) %>%
  summarise(counts = n()) 

#option1
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Flower_morphology),
    stat = "identity", position = position_stack()) +scale_fill_viridis(discrete = T)+theme_classic()

#option2
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Flower_morphology),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  )+scale_fill_viridis(discrete = T)+theme_classic()
########################################################################################################################################################
#Flower_symmetry
df <- t  %>%
  group_by(Flower_symmetry, Clusters) %>%
  summarise(counts = n()) 

#option1
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Flower_symmetry),
    stat = "identity", position = position_stack()) +scale_fill_viridis(discrete = T)+theme_classic()

#option2
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = Flower_symmetry),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  )+scale_fill_viridis(discrete = T)+theme_classic()



########################################################################################################################################################
#lifespan
df <- t  %>%
  group_by(lifespan, Clusters) %>%
  summarise(counts = n()) 

#option1
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = lifespan),
    stat = "identity", position = position_stack()) +scale_fill_viridis(discrete = T)+theme_classic()

#option2
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = lifespan),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  )+scale_fill_viridis(discrete = T)+theme_classic()


########################################################################################################################################################
#life_form
df <- t  %>%
  group_by(life_form, Clusters) %>%
  summarise(counts = n()) 

#option1
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = life_form),
    stat = "identity", position = position_stack()) +scale_fill_viridis(discrete = T)+theme_classic()

#option2
ggplot(df, aes(x = Clusters, y = counts)) +
  geom_bar(
    aes( fill = life_form),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  )+scale_fill_viridis(discrete = T)+theme_classic()










########################################################################################################################################################
#Now plot the quantitative data
########################################################################################################################################################


#Autonomous_selfing_level_fruit_set
Selfing <- ggplot(t, aes(Clusters, Autonomous_selfing_level_fruit_set)) +
  geom_sina(aes(color = Clusters), size = 0.7)+
  scale_color_manual(values =  c("#00AFBB", "#E69F00", "#FC4E07","#000000", "#009E73"))+theme_classic()+
  ylab("Autonomous fruit production")+theme(legend.position = "none")

#Floral_unit_width
ggplot(t, aes(Clusters, Floral_unit_width)) +
  geom_sina(aes(color = Clusters), size = 0.7)+
  scale_color_manual(values =  c("#00AFBB", "#E69F00", "#FC4E07","#000000", "#009E73"))+theme_classic()+ylim(0,400)+
  ylab("Floral unit width")+theme(legend.position = "none")


#Corolla_diameter_mean
Corolla_diameter <- ggplot(t, aes(Clusters, Corolla_diameter_mean)) +
  geom_sina(aes(color = Clusters), size = 0.7)+
  scale_color_manual(values =  c("#00AFBB", "#E69F00", "#FC4E07","#000000", "#009E73"))+theme_classic()+ylim(0,200)+
  ylab("Corolla diameter")+theme(legend.position = "none")

#STYLE_IMPUTED
Style_length <- ggplot(t, aes(Clusters, STYLE_IMPUTED)) +
  geom_sina(aes(color = Clusters), size = 0.7)+
  scale_color_manual(values =  c("#00AFBB", "#E69F00", "#FC4E07","#000000", "#009E73"))+theme_classic()+ylim(0,50)+
  ylab("Style length")+theme(legend.position = "none")

#OVULES_IMPUTED
Ovules <- ggplot(t, aes(Clusters, OVULES_IMPUTED)) +
  geom_sina(aes(color = Clusters), size = 0.7)+
  scale_color_manual(values =  c("#00AFBB", "#E69F00", "#FC4E07","#000000", "#009E73"))+theme_classic()+ylim(0,1000)+
  ylab("Ovule number")+theme(legend.position = "none")

#IMPUTED_plant_height_mean_m
Plant_height <- ggplot(t, aes(Clusters, IMPUTED_plant_height_mean_m)) +
  geom_sina(aes(color = Clusters), size = 0.7)+
  scale_color_manual(values =  c("#00AFBB", "#E69F00", "#FC4E07","#000000", "#009E73"))+theme_classic()+
  ylab("Plant height")+theme(legend.position = "none")


p_a <- plot_grid(Corolla_diameter,Style_length,Ovules,Plant_height, nrow = 4,align = 'v')

p_b<- plot_grid(breeding,Compatibility,Selfing,Flower_morphology,Flower_symmetry,life_form,lifespan, nrow = 7,align = 'v')

#Breeding system
breeding <- ggplot(t, aes(Clusters, Breeding_system)) +
  geom_jitter(aes(color = Clusters), size = 0.4)+ scale_color_manual(values =  c("#00AFBB", "#E69F00", "#FC4E07","#000000", "#009E73"))+
  theme_classic()+theme(legend.position = "none")+labs(x=NULL,y=NULL,subtitle = "Breeding system")+
  geom_hline(yintercept=c(1.5,2.5),color="black",linetype="longdash")
  
#Compatibility
t$IMPUTED_Compatibility <- as.character(t$IMPUTED_Compatibility)
t$IMPUTED_Compatibility[t$IMPUTED_Compatibility=="monoecious"] <- "Unisexual flowers"
t$IMPUTED_Compatibility[t$IMPUTED_Compatibility=="dioecious"] <- "Unisexual flowers"
t$IMPUTED_Compatibility[t$IMPUTED_Compatibility=="self_incompatible"] <- "Self incompatible"
t$IMPUTED_Compatibility[t$IMPUTED_Compatibility=="self_compatible"] <- "Self compatible"
t$IMPUTED_Compatibility[t$IMPUTED_Compatibility=="partially_self_compatible"] <- "Partially self compatible"


Compatibility <- ggplot(t, aes(Clusters, IMPUTED_Compatibility)) +
  geom_jitter(aes(color = Clusters), size = 0.4)+ scale_color_manual(values =  c("#00AFBB", "#E69F00", "#FC4E07","#000000", "#009E73"))+
  theme_classic()+ theme_classic()+theme(legend.position = "none")+labs(x=NULL,y=NULL,subtitle = "Compatibility system")+
  geom_hline(yintercept=c(1.5,2.5,3.5),color="black",linetype="longdash")
#Autonomous_selfing_level
t$Autonomous_selfing_level <- as.character(t$Autonomous_selfing_level)
t$Autonomous_selfing_level[t$Autonomous_selfing_level=="none"] <- "None"
t$Autonomous_selfing_level[t$Autonomous_selfing_level=="medium"] <- "Medium"
t$Autonomous_selfing_level[t$Autonomous_selfing_level=="low"] <- "Low"
t$Autonomous_selfing_level[t$Autonomous_selfing_level=="high"] <- "High"
t$Autonomous_selfing_level <- factor(t$Autonomous_selfing_level, levels = c("None", "Low", "Medium", "High"))


Selfing <- ggplot(t, aes(Clusters, Autonomous_selfing_level)) +
  geom_jitter(aes(color = Clusters), size = 0.4)+ scale_color_manual(values =  c("#00AFBB", "#E69F00", "#FC4E07","#000000", "#009E73"))+
  theme_classic()+ theme_classic()+theme(legend.position = "none")+labs(x=NULL,y=NULL,subtitle = "Selfing level")+
  geom_hline(yintercept=c(1.5,2.5,3.5),color="black",linetype="longdash")
#Flower_morphology
t$Flower_morphology <- as.character(t$Flower_morphology)
t$Flower_morphology[t$Flower_morphology=="Funnelform"] <- "Tube"
t$Flower_morphology[t$Flower_morphology=="Spike"] <- "Brush"


Flower_morphology <-  ggplot(t, aes(Clusters, Flower_morphology)) +
  geom_jitter(aes(color = Clusters), size = 0.4)+ scale_color_manual(values =  c("#00AFBB", "#E69F00", "#FC4E07","#000000", "#009E73"))+
  theme_classic()+ theme_classic()+theme(legend.position = "none")+labs(x=NULL,y=NULL,subtitle = "Flower shape")+
  geom_hline(yintercept=c(1.5,2.5,3.5,4.5,5.5),color="black",linetype="longdash")
#Flower_symmetry
t$Flower_symmetry <- as.character(t$Flower_symmetry)
t$Flower_symmetry[t$Flower_symmetry=="zygomorphic"] <- "Zygomorphic"
t$Flower_symmetry[t$Flower_symmetry=="actinomorphic"] <- "Actinomorphic"

Flower_symmetry <-  ggplot(t, aes(Clusters, Flower_symmetry)) +
   geom_jitter(aes(color = Clusters), size = 0.4)+ scale_color_manual(values =  c("#00AFBB", "#E69F00", "#FC4E07","#000000", "#009E73"))+
   theme_classic()+ theme_classic()+theme(legend.position = "none")+labs(x=NULL,y=NULL,subtitle = "Flower symmetry")+
  geom_hline(yintercept=c(1.5),color="black",linetype="longdash")
 #life_form
t$life_form <- as.character(t$life_form)
t$life_form[t$life_form=="vine"] <- "Herb"
t$life_form[t$life_form=="herb"] <- "Herb"
t$life_form[t$life_form=="shrub"] <- "Shrub"
t$life_form[t$life_form=="tree"] <- "Tree"

life_form <-  ggplot(t, aes(Clusters,life_form )) +
   geom_jitter(aes(color = Clusters), size = 0.4)+ scale_color_manual(values =  c("#00AFBB", "#E69F00", "#FC4E07","#000000", "#009E73"))+
   theme_classic()+ theme_classic()+theme(legend.position = "none")+labs(x=NULL,y=NULL,subtitle = "Life form")+
  geom_hline(yintercept=c(1.5,2.5),color="black",linetype="longdash")
 #lifespan
lifespan<- ggplot(t, aes(Clusters,lifespan )) +
   geom_jitter(aes(color = Clusters), size = 0.4)+ scale_color_manual(values =  c("#00AFBB", "#E69F00", "#FC4E07","#000000", "#009E73"))+
   theme_classic()+ theme_classic()+theme(legend.position = "none")+labs(x="Plant Functional groups",y=NULL,subtitle = "Life span")+
  geom_hline(yintercept=c(1.5),color="black",linetype="longdash")



ggplot(t, aes(Clusters,lifespan )) +
  geom_jitter(aes(color = Clusters), size = 0.4)+ scale_color_manual(values =  c("#00AFBB", "#E69F00", "#FC4E07","#000000", "#009E73"))+
  theme_classic()+ theme_classic()+theme(legend.position = "none")+labs(x="Plant Functional groups",y=NULL,subtitle = "Life form")+
  geom_hline(yintercept=c(1.5),color="black")+    scale_y_discrete(expand=c(0.05, 0.05))




ggplot(t, aes(lifespan,Clusters )) +
  geom_jitter(aes(color = Clusters), size = 0.4)+ scale_color_manual(values =  c("#00AFBB", "#E69F00", "#FC4E07","#000000", "#009E73"))+
  theme_classic()+ theme_classic()+theme(legend.position = "none")+labs(x="Plant Functional groups",y=NULL,subtitle = "Life form")+
  geom_hline(yintercept=c(1.5),color="black")+    scale_y_discrete(expand=c(0.05, 0.05))




library(Hmisc)
library(cowplot)
 #Plant height
 p1 <- ggplot(t, aes(x = Clusters, y = log(IMPUTED_plant_height_mean_m)))+ geom_violin(aes(color = Clusters, fill = Clusters), 
 binaxis='y', stackdir='center')  +theme_classic()+ ylab("log(Plant height)")+geom_boxplot(width=0.1)+
 theme(legend.position = "none")
 #Flower size
 p2 <- ggplot(t, aes(x = Clusters, y = log(Floral_unit_width)))+ geom_violin(aes(color = Clusters, fill = Clusters), 
 binaxis='y', stackdir='center')  +theme_classic()+ ylab("log(Flower size)")+geom_boxplot(width=0.1)+
 theme(legend.position = "none")
 #Ovule number
 p3 <- ggplot(t, aes(x = Clusters, y = log(OVULES_IMPUTED)))+ geom_violin(aes(color = Clusters, fill = Clusters), 
 binaxis='y', stackdir='center')  +theme_classic()+ ylab("log(Ovules per flower)")+geom_boxplot(width=0.1)+
 theme(legend.position = "none")
 #Style length
 p4 <- ggplot(t, aes(x = Clusters, y = log(STYLE_IMPUTED)))+ geom_violin(aes(color = Clusters, fill = Clusters), 
 binaxis='y', stackdir='center')  +theme_classic()+ ylab("log(Style length)")+geom_boxplot(width=0.1)+
 theme(legend.position = "none")
 #Flowers per plant
 p5 <- ggplot(t, aes(x = Clusters, y = log(Flowers_per_plant)))+ geom_violin(aes(color = Clusters, fill = Clusters), 
 binaxis='y', stackdir='center')  +theme_classic()+ ylab("log(Flowers per plant)")+geom_boxplot(width=0.1)+
 theme(legend.position = "none")
 #Flowers per plant
 p6 <- ggplot(t, aes(x = Clusters, y = Autonomous_selfing_level_fruit_set))+ geom_violin(aes(color = Clusters, fill = Clusters), 
 binaxis='y', stackdir='center')  +theme_classic()+ ylab("log(Selfing)")+geom_boxplot(width=0.1)+
 theme(legend.position = "none")
 
 
 #Plot for legend
 p7 <- ggplot(t, aes(x = Clusters, y = Autonomous_selfing_level_fruit_set))+ geom_violin(aes( fill = Clusters), 
 binaxis='y', stackdir='center')  +theme_classic()+ ylab("log(Selfing)")+geom_boxplot(width=0.1)+scale_fill_discrete(name = "Plant functional\ groups")
 
 #get legend function from cowplot
 legend <- get_legend(
   # create some space to the left of the legend
   p7 + theme(legend.box.margin = margin(0, 0, 0, 12)))
 
 
plots <-  plot_grid(p1,p2,p3,p4,p5,p6, nrow = 2,ncol=3,align = 'v')
 
 plot_grid(plots, legend, rel_widths = c(3, .4))
 





e <- ggplot(t, aes(x = Clusters, y = IMPUTED_plant_height_mean_m))

e + geom_jitter(
  aes( color = Clusters), 
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size = 0.4,alpha=0.5
) +
  stat_summary(
    fun.data="mean_sdl",  fun.args = list(mult=1), 
    geom = "pointrange",  size = 0.6,
    position = position_dodge(0.8)
  )+ theme_classic() + ylab("log(plant height in m)")



e <- ggplot(t, aes(x = Clusters, y = log(OVULES_IMPUTED)))

e + geom_jitter(
  aes( color = Clusters), 
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size = 0.4,alpha=0.5
) +
  stat_summary(
    fun.data="mean_sdl",  fun.args = list(mult=1), 
    geom = "pointrange",  size = 0.6,
    position = position_dodge(0.8)
  )+ theme_classic() + ylab("log(Ovule number)")


e <- ggplot(t, aes(x = Clusters, y = log(STYLE_IMPUTED)))

e + geom_jitter(
  aes( color = Clusters), 
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size = 0.4,alpha=0.5
) +
  stat_summary(
    fun.data="mean_sdl",  fun.args = list(mult=1), 
    geom = "pointrange",  size = 0.6,
    position = position_dodge(0.8)
  )+ theme_classic() + ylab("log(Ovule number)")


e <- ggplot(t, aes(x = Clusters, y = log(Corolla_diameter_mean)))

e + geom_jitter(
  aes( color = Clusters), 
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size = 0.4,alpha=0.5
) +
  stat_summary(
    fun.data="mean_sdl",  fun.args = list(mult=1), 
    geom = "pointrange",  size = 0.6,
    position = position_dodge(0.8)
  )+ theme_classic() + ylab("log(Corolla diameter)")


e <- ggplot(t, aes(x = Clusters, y = Autonomous_selfing_level_fruit_set))

e + geom_jitter(
  aes( color = Clusters), 
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size = 0.4,alpha=0.5
) +
  stat_summary(
    fun.data="mean_sdl",  fun.args = list(mult=1), 
    geom = "pointrange",  size = 0.6,
    position = position_dodge(0.8)
  )+ theme_classic() + ylab("Autonomus selfing")
