##########################################
#Visualization of plant functional groups#
##########################################

library(cowplot)
library(ggplot2)

# Theme for publication
theme_ms <- function(base_size=12, base_family="Helvetica") {
  (theme_bw(base_size = base_size, base_family = base_family)+
     theme(text=element_text(color="black"),
           plot.title = element_text(size = 10, face = "bold"),
           axis.title=element_text( size = rel(1)),
           axis.text=element_text(size = rel(0.9), color = "black"),
           legend.title=element_text(face="bold",size = 10),
           legend.text=element_text(),
           legend.background=element_rect(fill="transparent"),
           legend.key.size = unit(0.9, 'lines'),
           panel.border=element_rect(color="black",size=1),
           panel.grid.minor.x =element_blank(),
           panel.grid.minor.y= element_blank(),
           panel.grid.major= element_blank()
     ))
}
#LOAD DATA
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


#convert character to factors
t[sapply(t, is.character)] <- lapply(t[sapply(t, is.character)], 
                                       as.factor)


#Exploratory summary of the data
summ_clusters <- t  %>% group_by(Clusters) %>% do(the_summary = summary(.))
summ_clusters$the_summary

#######################
#Qualitative variables#
#######################

#######################################################################################################
#Calculate counts per group of breeding system
breeding <- t  %>%group_by(Breeding_system, Clusters) %>%summarise(counts = n()) 
#Calculate percentage of these counts
breeding <- group_by(breeding, Clusters) %>% mutate(percent = counts/sum(counts)*100)
#Organise levels
breeding$Breeding_system <- factor(breeding$Breeding_system, levels=c("Dioecious","Monoecious","Hermaphrodite"))

#Plot BREEDING SYSTEM
p1 <- ggplot(breeding, aes(x = percent, y = Clusters)) +
  geom_bar(
  aes( fill = Breeding_system),alpha=0.90,
  stat = "identity", position = position_stack()) +theme_ms()+xlab("")+
  scale_fill_manual(values=c("black","orange","forestgreen"),name = "a) Breeding system")+
  theme(legend.key.size = unit(0.2, 'cm'))+theme(legend.position="top",legend.justification='left',legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,-10,0),
  legend.title = element_text(face="bold"))+guides(fill=guide_legend(title.position = "top")) +ylab("Functional groups")


#######################################################################################################
#Calculate counts per group of breeding system
Compatibility <- t  %>%group_by(IMPUTED_Compatibility, Clusters) %>%summarise(counts = n()) 
Compatibility$IMPUTED_Compatibility <- as.character(Compatibility$IMPUTED_Compatibility)
Compatibility$IMPUTED_Compatibility[Compatibility$IMPUTED_Compatibility=="dioecious"] <- "Unisexual f."
Compatibility$IMPUTED_Compatibility[Compatibility$IMPUTED_Compatibility=="monoecious"] <- "Unisexual f."
Compatibility$IMPUTED_Compatibility[Compatibility$IMPUTED_Compatibility=="self_incompatible"] <- "Self inc."
Compatibility$IMPUTED_Compatibility[Compatibility$IMPUTED_Compatibility=="partially_self_compatible"] <- "Partially self com."
Compatibility$IMPUTED_Compatibility[Compatibility$IMPUTED_Compatibility=="self_compatible"] <- "Self com."

#Calculate percentage of these counts
Compatibility <- group_by(Compatibility, Clusters) %>% mutate(percent = counts/sum(counts)*100)
#Organise levels

Compatibility$IMPUTED_Compatibility <- factor(Compatibility$IMPUTED_Compatibility, levels=c("Unisexual f.","Self inc.", "Partially self com.",
                                                                                            "Self com."))
#Plot Compatibility
p2 <- ggplot(Compatibility, aes(x = percent, y = Clusters)) +
  geom_bar(
  aes( fill = IMPUTED_Compatibility),alpha=0.90,
  stat = "identity", position = position_stack()) +theme_ms()+xlab("")+
  scale_fill_manual(values=c("gray7", "orange","cyan4","deepskyblue2"),name = "b) Compatibility system")+
  theme(legend.key.size = unit(0.2, 'cm'))+theme(legend.position="top",legend.justification='left',legend.margin=margin(0,0,0,0),legend.box.margin=margin(-0,-10,-10,0),
  legend.title = element_text(face="bold"))+ guides(fill=guide_legend(ncol=2,nrow=2,byrow=TRUE,title.position = "top")) +ylab("")
#######################################################################################################
#Calculate counts per group of Life form
life_form <- t  %>%group_by(life_form, Clusters) %>%summarise(counts = n()) 
life_form$life_form <- as.character(life_form$life_form)
life_form$life_form [life_form$life_form =="vine"] <- "Shrub"
life_form$life_form [life_form$life_form =="tree"] <- "Tree"
life_form$life_form [life_form$life_form =="herb"] <- "Herb"
life_form$life_form [life_form$life_form =="shrub"] <- "Shrub"

#Calculate percentage of these counts
life_form <- group_by(life_form, Clusters) %>% mutate(percent = counts/sum(counts)*100)
#Organise levels
life_form$life_form <- factor(life_form$life_form, levels=c("Tree","Shrub", "Herb"))
#Plot Compatibility
p3 <- ggplot(life_form, aes(x = percent, y = Clusters)) +
  geom_bar(
  aes( fill = life_form),alpha=0.90,
  stat = "identity", position = position_stack()) +theme_ms()+xlab("")+
  scale_fill_manual(values=c("gray7", "darkorange2","forestgreen"),name = "c) Life form")+
  theme(legend.key.size = unit(0.2, 'cm'))+theme(legend.position="top",legend.justification='left',legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,-10,0),
  legend.title = element_text(face="bold"))+guides(fill=guide_legend(title.position = "top"))+ylab("")
#######################################################################################################
#Calculate counts per group of Life span
lifespan <- t  %>%group_by(lifespan, Clusters) %>%summarise(counts = n()) 
#Calculate percentage of these counts
lifespan <- group_by(lifespan, Clusters) %>% mutate(percent = counts/sum(counts)*100)
#Organise levels
lifespan$lifespan <- factor(lifespan$lifespan, levels=c("Perennial","Short lived"))
#Plot Compatibility
p4 <- ggplot(lifespan, aes(x = percent, y = Clusters)) +
  geom_bar(
  aes( fill = lifespan),alpha=0.90,
  stat = "identity", position = position_stack()) +theme_ms()+xlab("Percentage")+
  scale_fill_manual(values=c("sienna4","forestgreen"),name = "d) Life span")+
  theme(legend.key.size = unit(0.2, 'cm'))+theme(legend.position="top",legend.justification='left',legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,-10,0),
  legend.title = element_text(face="bold"))+ guides(fill=guide_legend(ncol=2,nrow=2,byrow=TRUE,title.position = "top")) + ylab("Functional groups")
#######################################################################################################
#Calculate counts per group of Flower symmetry
Flower_symmetry <- t  %>%group_by(Flower_symmetry, Clusters) %>%summarise(counts = n()) 
#Calculate percentage of these counts
Flower_symmetry <- group_by(Flower_symmetry, Clusters) %>% mutate(percent = counts/sum(counts)*100)
Flower_symmetry$Flower_symmetry <- as.character(Flower_symmetry$Flower_symmetry)
Flower_symmetry$Flower_symmetry [Flower_symmetry$Flower_symmetry =="actinomorphic"] <- "Actinomorphic"
Flower_symmetry$Flower_symmetry [Flower_symmetry$Flower_symmetry =="zygomorphic"] <- "Zygomorphic"
#Organise levels
Flower_symmetry$Flower_symmetry <- factor(Flower_symmetry$Flower_symmetry, levels=c("Actinomorphic","Zygomorphic"))
#Plot Compatibility
p5 <- ggplot(Flower_symmetry, aes(x = percent, y = Clusters)) +
  geom_bar(
  aes( fill = Flower_symmetry),alpha=0.90,
  stat = "identity", position = position_stack()) +theme_ms()+xlab("Percentage")+
  scale_fill_manual(values=c("sienna2","purple"),name = "f) Flower symmetry")+
  theme(legend.key.size = unit(0.2, 'cm'))+theme(legend.position="top",legend.justification='left',legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,-10,0),
  legend.title = element_text(face="bold"))+guides(fill=guide_legend(ncol=2,nrow=2,byrow=TRUE,title.position = "top"))+ylab("")
#######################################################################################################
#Calculate counts per group of Flower shape
Flower_morphology <- t  %>%group_by(Flower_morphology, Clusters) %>%summarise(counts = n()) 
levels(Flower_morphology$Flower_morphology)
Flower_morphology$Flower_morphology <- as.character(Flower_morphology$Flower_morphology)
Flower_morphology$Flower_morphology[Flower_morphology$Flower_morphology=="Funnelform"] <- "Campanulate"
Flower_morphology$Flower_morphology[Flower_morphology$Flower_morphology=="Spike"] <- "Brush"

#Calculate percentage of these counts
Flower_morphology <- group_by(Flower_morphology, Clusters) %>% mutate(percent = counts/sum(counts)*100)
#Plot Compatibility
p6 <- ggplot(Flower_morphology, aes(x = percent, y = Clusters)) +
  geom_bar(
  aes( fill = Flower_morphology),alpha=0.90,
  stat = "identity", position = position_stack()) +theme_ms()+xlab("Percentage")+
  scale_fill_manual(values=c("blue","cyan4", "red","forestgreen","purple","gold"),name = "e) Flower shape")+
  theme(legend.key.size = unit(0.2, 'cm'))+theme(legend.position="top",legend.justification='left',legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,-10,0),
  legend.title = element_text(face="bold"))+ guides(fill=guide_legend(ncol=3,nrow=2,byrow=TRUE,title.position = "top"))+ylab("")


top <- plot_grid(p1,p2,p3,p4,p6,p5, ncol=3,nrow=2,  align = "h")


# now add the title
title <- ggdraw() + 
  draw_label(
    "Qualitative variables",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )


top_1 <- plot_grid(title,top,ncol = 1,rel_heights = c(0.1, 1))


#######################################################################################################
#######################################################################################################
#######################################################################################################

########################
#Quantitative variables#
########################

########################
#Try density plots#
########################

#Plant height
pp1 <- ggplot(t, aes(x  = log(IMPUTED_plant_height_mean_m), fill=Clusters)) +geom_density(alpha=0.3)+theme_ms()+theme(legend.position = "none")+
  ylab("Density") + xlab("log(Plant height)") + ggtitle("g) Plant height")  + ylab("Functional groups")+scale_fill_manual(values=c("cyan4", "red","forestgreen","purple","gold"))

#Flower size
pp2 <- ggplot(t, aes(x  = log(Floral_unit_width), fill=Clusters)) +geom_density(alpha=0.3)+theme_ms()+theme(legend.position = "none")+
  ylab("") + xlab("log(Flower size)") + ggtitle("h) Flower size") + ylab("")+scale_fill_manual(values=c("cyan4", "red","forestgreen","purple","gold"))

#Ovule number
pp3 <- ggplot(t, aes(x  = log(OVULES_IMPUTED), fill=Clusters)) +geom_density(alpha=0.3)+theme_ms()+theme(legend.position = "none")+
  ylab("") + xlab("log(Ovule number)") +  ggtitle("i) Ovule number") + ylab("")+scale_fill_manual(values=c("cyan4", "red","forestgreen","purple","gold"))

#Style length
pp4 <- ggplot(t, aes(x  = log(STYLE_IMPUTED), fill=Clusters)) +geom_density(alpha=0.3)+theme_ms()+theme(legend.position = "none")+
  ylab("Density") + xlab("log(Style length)") +  ggtitle("j) Style length") + ylab("Functional groups")+scale_fill_manual(values=c("cyan4", "red","forestgreen","purple","gold"))

#Flowers per plant
pp5 <- ggplot(t, aes(x  = log(Flowers_per_plant), fill=Clusters)) +geom_density(alpha=0.3)+theme_ms()+theme(legend.position = "none")+
  ylab("") + xlab("log(Flower number)") +  ggtitle("k) Flower number") + ylab("")+scale_fill_manual(values=c("cyan4", "red","forestgreen","purple","gold"))

#Selfing
pp6 <- ggplot(t, aes(x  = Autonomous_selfing_level_fruit_set, fill=Clusters)) +geom_density(alpha=0.3)+theme_ms()+theme(legend.position = "none")+
  ylab("") + xlab("Selfing") +  ggtitle("l) Selfing") + ylab("")+scale_fill_manual(values=c("cyan4", "red","forestgreen","purple","gold"))

#For legend
pp7 <- ggplot(t, aes(x  = Autonomous_selfing_level_fruit_set, fill=Clusters)) +geom_density(alpha=0.3)+
  ylab("Density") + xlab("Selfing")+theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title="Functional groups"))+scale_fill_manual(values=c("cyan4", "red","forestgreen","purple","gold"))



#Extract legend
legend_b <- get_legend(
  pp7 + 
    guides(color = guide_legend(nrow = 1)) +theme(legend.position="bottom",legend.direction="horizontal")
)

#generate panel of plots
bottom <- plot_grid(pp1,pp2,pp3,pp4,pp5,pp6, ncol=3,nrow=2,  align = "hv")
#add legend
bottom_1 <- plot_grid(bottom,legend_b,ncol = 1, rel_heights = c(1, .1))

# now add the title
title <- ggdraw() + 
  draw_label(
    "Quantitative variables",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
#add title
bottom_2 <- plot_grid(title,bottom_1,ncol = 1,rel_heights = c(0.1, 1))

####         ####
#### P L O T ####
####         ####


plot_grid(top_1, bottom_2,nrow=2, align = "hv")







########################
#Try violin plots#
########################

#Plant height
pp1 <- ggplot(t, aes(x = Clusters, y = log(IMPUTED_plant_height_mean_m)))+ geom_violin(aes(color = Clusters, fill = Clusters), 
  binaxis='y', stackdir='center')  +theme_classic()+ ylab("log(Plant height)")+geom_boxplot(width=0.1)+
  theme(legend.position = "none",plot.title = element_text(face = "bold"))+xlab("")+ggtitle("Plant height")
#Flower size
pp2 <- ggplot(t, aes(x = Clusters, y = log(Floral_unit_width)))+ geom_violin(aes(color = Clusters, fill = Clusters), 
   binaxis='y', stackdir='center')  +theme_classic()+ ylab("log(Flower size)")+geom_boxplot(width=0.1)+
  theme(legend.position = "none",plot.title = element_text(face = "bold"))+xlab("")+ggtitle("Flower size")
#Ovule number
pp3 <- ggplot(t, aes(x = Clusters, y = log(OVULES_IMPUTED)))+ geom_violin(aes(color = Clusters, fill = Clusters), 
  binaxis='y', stackdir='center')  +theme_classic()+ ylab("log(Ovules per flower)")+geom_boxplot(width=0.1)+
  theme(legend.position = "none",plot.title = element_text(face = "bold"))+xlab("")+ggtitle("Ovule number")
#Style length
pp4 <- ggplot(t, aes(x = Clusters, y = log(STYLE_IMPUTED)))+ geom_violin(aes(color = Clusters, fill = Clusters), 
  binaxis='y', stackdir='center')  +theme_classic()+ ylab("log(Style length)")+geom_boxplot(width=0.1)+
  theme(legend.position = "none",axis.title.x = element_text(face="bold"),plot.title = element_text(face = "bold"))+xlab("")+xlab("Plant functional groups")+
  ggtitle("Style length")
#Flowers per plant
pp5 <- ggplot(t, aes(x = Clusters, y = log(Flowers_per_plant)))+ geom_violin(aes(color = Clusters, fill = Clusters), 
  binaxis='y', stackdir='center')  +theme_classic()+ ylab("log(Flowers per plant)")+geom_boxplot(width=0.1)+
  theme(legend.position = "none",axis.title.x = element_text(face="bold"),plot.title = element_text(face = "bold"))+xlab("Plant functional groups")+
  ggtitle("Flower number")
#Flowers per plant
pp6 <- ggplot(t, aes(x = Clusters, y = Autonomous_selfing_level_fruit_set))+ geom_violin(aes(color = Clusters, fill = Clusters), 
  binaxis='y', stackdir='center')  +theme_classic()+ ylab("log(Selfing)")+geom_boxplot(width=0.1)+
  theme(legend.position = "none",axis.title.x = element_text(face="bold"),plot.title = element_text(face = "bold"))+xlab("Plant functional groups")+
  ggtitle("Selfing")

bottom <- plot_grid(pp1,pp2,pp3,pp4,pp6,pp5, ncol=3,nrow=2,  align = "v")

plot_grid(top, bottom,nrow=2, align = "hv")


