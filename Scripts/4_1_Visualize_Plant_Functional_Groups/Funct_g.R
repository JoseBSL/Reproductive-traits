

library(dplyr)
library(ggplot2)
library(cowplot)

##########################################
#Visualization of plant functional groups#
##########################################
setwd("~/R_Projects/Reproductive Traits")
##########################################
#Visualization of plant functional groups#
##########################################


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
d <- read.csv("Data/Csv/all_species_imputed_trait_data_forest_data.csv")
#read trait data with clusters
#I'll select the column of clusters and include it on the non standardize trait data
hclust_d_5 <- read.csv("Data/Csv/imputed_trait_data_hclust_5_clusters_forest_data.csv") #5 clusters
#No add the cluster columns (it has the same order)
d$Clusters <- as.factor(hclust_d_5$Clusters)


#select columns of interest
t <- d[c("Breeding_system","IMPUTED_Compatibility","Autonomous_selfing_level",
         "Autonomous_selfing_level_fruit_set", "Flower_morphology", "Flower_symmetry", "Flowers_per_plant", "Flowers_per_inflorescence",
         "Floral_unit_width", "Corolla_diameter_mean", "Corolla_length_mean", "STYLE_IMPUTED", "OVULES_IMPUTED", "life_form", "lifespan",
         "IMPUTED_plant_height_mean_m","Nectar_presence_absence", "Clusters")]


#convert character to factors
t[sapply(t, is.character)] <- lapply(t[sapply(t, is.character)], 
                                     as.factor)


#Exploratory summary of the data
summ_clusters <- t  %>% group_by(Clusters) %>% do(the_summary = summary(.))
summ_clusters$the_summary


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
pp1 <- ggplot(t, aes(x  = log(IMPUTED_plant_height_mean_m), fill=Clusters)) +geom_density(alpha=0.2)+theme_ms()+theme(legend.position = "none")+
  ylab("Density") + xlab("log(Plant height)") + ggtitle("a) Plant height")  + ylab("Functional groups")+scale_fill_manual(values=c("cyan4", "red","forestgreen","purple","gold"))

#Flower size
pp2 <- ggplot(t, aes(x  = log(Floral_unit_width), fill=Clusters)) +geom_density(alpha=0.2)+theme_ms()+theme(legend.position = "none")+
  ylab("") + xlab("log(Flower size)") + ggtitle("b) Flower size") + ylab("")+scale_fill_manual(values=c("cyan4", "red","forestgreen","purple","gold"))

#Ovule number
pp3 <- ggplot(t, aes(x  = log(OVULES_IMPUTED), fill=Clusters)) +geom_density(alpha=0.2)+theme_ms()+theme(legend.position = "none")+
  ylab("") + xlab("log(Ovule number)") +  ggtitle("c) Ovule number") + ylab("")+scale_fill_manual(values=c("cyan4", "red","forestgreen","purple","gold"))

#Style length
pp4 <- ggplot(t, aes(x  = log(STYLE_IMPUTED), fill=Clusters)) +geom_density(alpha=0.3)+theme_ms()+theme(legend.position = "none")+
  ylab("Density") + xlab("log(Style length)") +  ggtitle("d) Style length") + ylab("Functional groups")+scale_fill_manual(values=c("cyan4", "red","forestgreen","purple","gold"))

#Flowers per plant
pp5 <- ggplot(t, aes(x  = log(Flowers_per_plant), fill=Clusters)) +geom_density(alpha=0.2)+theme_ms()+theme(legend.position = "none")+
  ylab("") + xlab("log(Flower number)") +  ggtitle("e) Flower number") + ylab("")+scale_fill_manual(values=c("cyan4", "red","forestgreen","purple","gold"))

#Selfing
pp6 <- ggplot(t, aes(x  = Autonomous_selfing_level_fruit_set, fill=Clusters)) +geom_density(alpha=0.2)+theme_ms()+theme(legend.position = "none")+
  ylab("") + xlab("Selfing") +  ggtitle("f) Selfing") + ylab("")+scale_fill_manual(values=c("cyan4", "red","forestgreen","purple","gold"))

#For legend
t$Clusters <- as.character(t$Clusters)

t$Clusters[t$Clusters=="1"] <- "A"
t$Clusters[t$Clusters=="2"] <- "B"
t$Clusters[t$Clusters=="3"] <- "C"
t$Clusters[t$Clusters=="4"] <- "D"
t$Clusters[t$Clusters=="5"] <- "E"

t$Clusters <- factor(t$Clusters, levels=c("A", "B", "C", "D", "E"))

pp7 <- ggplot(t, aes(x  = Autonomous_selfing_level_fruit_set, fill=Clusters)) +geom_density(alpha=0.2)+
  ylab("Density") + xlab("Selfing")+theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title="Functional groups"))+scale_fill_manual(values=c("cyan4", "red","forestgreen","purple","gold"))



#Extract legend
legend_b <- get_legend(
  pp7 + 
    guides(color = guide_legend(nrow = 1)) +theme(legend.position="bottom",legend.direction="horizontal")
)

#generate panel of plots
top <- plot_grid(pp1,pp2,pp3,pp4,pp5,pp6, ncol=3,nrow=2,  align = "hv")
#add legend
top_1 <- plot_grid(top,legend_b,ncol = 1, rel_heights = c(1, .1))

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
top_2 <- plot_grid(title,top_1,ncol = 1,rel_heights = c(0.1, 1))

####         ####
#### P L O T ####
####         ####

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

breeding$Clusters <- as.character(breeding$Clusters)

breeding$Clusters[breeding$Clusters=="1"] <- "A"
breeding$Clusters[breeding$Clusters=="2"] <- "B"
breeding$Clusters[breeding$Clusters=="3"] <- "C"
breeding$Clusters[breeding$Clusters=="4"] <- "D"
breeding$Clusters[breeding$Clusters=="5"] <- "E"


breeding$Clusters <- factor(breeding$Clusters, levels=c("E","D","C", "B", "A"))


#Plot BREEDING SYSTEM
p1 <- ggplot(breeding, aes(x = percent, y = Clusters)) +
  geom_bar(
    aes( fill = Breeding_system),alpha=0.85,
    stat = "identity", position = position_stack()) +theme_ms()+xlab("")+
  scale_fill_manual(values=c("black","orange","forestgreen"),name = "g) Breeding system")+
  theme(legend.key.size = unit(0.2, 'cm'))+theme(legend.position="top",legend.justification='left',legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,-10,0),
                                                 legend.title = element_text(face="bold"))+guides(fill=guide_legend(title.position = "top",reverse = TRUE)) +ylab("Functional groups")


#######################################################################################################
#Calculate counts per group of breeding system
Compatibility <- t  %>%group_by(IMPUTED_Compatibility, Clusters) %>%summarise(counts = n()) 
Compatibility$IMPUTED_Compatibility <- as.character(Compatibility$IMPUTED_Compatibility)
Compatibility$IMPUTED_Compatibility[Compatibility$IMPUTED_Compatibility=="dioecious"] <- "Unisexual flowers"
Compatibility$IMPUTED_Compatibility[Compatibility$IMPUTED_Compatibility=="monoecious"] <- "Unisexual flowers"
Compatibility$IMPUTED_Compatibility[Compatibility$IMPUTED_Compatibility=="self_incompatible"] <- "Self incompatible"
Compatibility$IMPUTED_Compatibility[Compatibility$IMPUTED_Compatibility=="partially_self_compatible"] <- "Partially self compatible"
Compatibility$IMPUTED_Compatibility[Compatibility$IMPUTED_Compatibility=="self_compatible"] <- "Self compatible"

#Calculate percentage of these counts
Compatibility <- group_by(Compatibility, Clusters) %>% mutate(percent = counts/sum(counts)*100)
#Organise levels

Compatibility$IMPUTED_Compatibility <- factor(Compatibility$IMPUTED_Compatibility, levels=c("Unisexual flowers","Self incompatible", "Partially self compatible",  "Self compatible"))

Compatibility$Clusters <- as.character(Compatibility$Clusters)


Compatibility$Clusters[Compatibility$Clusters=="1"] <- "A"
Compatibility$Clusters[Compatibility$Clusters=="2"] <- "B"
Compatibility$Clusters[Compatibility$Clusters=="3"] <- "C"
Compatibility$Clusters[Compatibility$Clusters=="4"] <- "D"
Compatibility$Clusters[Compatibility$Clusters=="5"] <- "E"


Compatibility$Clusters <- factor(Compatibility$Clusters, levels=c("E","D","C", "B", "A"))
#Plot Compatibility
p2 <- ggplot(Compatibility, aes(x = percent, y = Clusters)) +
  geom_bar(
    aes( fill = IMPUTED_Compatibility),alpha=0.85,
    stat = "identity", position = position_stack()) +theme_ms()+xlab("")+
  scale_fill_manual(values=c("gray7", "orange","cyan4","deepskyblue2"),name = "h) Compatibility system")+
  theme(legend.key.size = unit(0.2, 'cm'))+theme(legend.position="top",legend.justification='left',legend.margin=margin(0,0,0,0),legend.box.margin=margin(-0,-10,-10,0),
                                                 legend.title = element_text(face="bold"))+ guides(fill=guide_legend(ncol=2,nrow=2,byrow=TRUE,title.position = "top",reverse = TRUE)) +ylab("")
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

life_form$Clusters <- as.character(life_form$Clusters)


life_form$Clusters[life_form$Clusters=="1"] <- "A"
life_form$Clusters[life_form$Clusters=="2"] <- "B"
life_form$Clusters[life_form$Clusters=="3"] <- "C"
life_form$Clusters[life_form$Clusters=="4"] <- "D"
life_form$Clusters[life_form$Clusters=="5"] <- "E"


life_form$Clusters <- factor(life_form$Clusters, levels=c("E","D","C", "B", "A"))


p3 <- ggplot(life_form, aes(x = percent, y = Clusters)) +
  geom_bar(
    aes( fill = life_form),alpha=0.85,
    stat = "identity", position = position_stack()) +theme_ms()+xlab("")+
  scale_fill_manual(values=c("gray7", "darkorange2","forestgreen"),name = "i) Life form")+
  theme(legend.key.size = unit(0.2, 'cm'))+theme(legend.position="top",legend.justification='left',legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,-10,0),
                                                 legend.title = element_text(face="bold"))+guides(fill=guide_legend(title.position = "top",reverse = TRUE))+ylab("")
#######################################################################################################
#Calculate counts per group of Life span
lifespan <- t  %>%group_by(lifespan, Clusters) %>%summarise(counts = n()) 
#Calculate percentage of these counts
lifespan <- group_by(lifespan, Clusters) %>% mutate(percent = counts/sum(counts)*100)
#Organise levels
lifespan$lifespan <- factor(lifespan$lifespan, levels=c("Perennial","Short lived"))

lifespan$Clusters <- as.character(lifespan$Clusters)

lifespan$Clusters[lifespan$Clusters=="1"] <- "A"
lifespan$Clusters[lifespan$Clusters=="2"] <- "B"
lifespan$Clusters[lifespan$Clusters=="3"] <- "C"
lifespan$Clusters[lifespan$Clusters=="4"] <- "D"
lifespan$Clusters[lifespan$Clusters=="5"] <- "E"


lifespan$Clusters <- factor(lifespan$Clusters, levels=c("E","D","C", "B", "A"))


#Plot Compatibility
p4 <- ggplot(lifespan, aes(x = percent, y = Clusters)) +
  geom_bar(
    aes( fill = lifespan),alpha=0.85,
    stat = "identity", position = position_stack()) +theme_ms()+xlab("Percentage")+
  scale_fill_manual(values=c("sienna4","forestgreen"),name = "j) Life span")+
  theme(legend.key.size = unit(0.2, 'cm'))+theme(legend.position="top",legend.justification='left',legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,-10,0),
                                                 legend.title = element_text(face="bold"))+ guides(fill=guide_legend(ncol=2,nrow=2,byrow=TRUE,title.position = "top",reverse = TRUE)) + ylab("Functional groups")
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

Flower_symmetry$Clusters <- as.character(Flower_symmetry$Clusters)

Flower_symmetry$Clusters[Flower_symmetry$Clusters=="1"] <- "A"
Flower_symmetry$Clusters[Flower_symmetry$Clusters=="2"] <- "B"
Flower_symmetry$Clusters[Flower_symmetry$Clusters=="3"] <- "C"
Flower_symmetry$Clusters[Flower_symmetry$Clusters=="4"] <- "D"
Flower_symmetry$Clusters[Flower_symmetry$Clusters=="5"] <- "E"


Flower_symmetry$Clusters <- factor(Flower_symmetry$Clusters, levels=c("E","D","C", "B", "A"))

#Plot Compatibility
p5 <- ggplot(Flower_symmetry, aes(x = percent, y = Clusters)) +
  geom_bar(
    aes( fill = Flower_symmetry),alpha=0.85,
    stat = "identity", position = position_stack()) +theme_ms()+xlab("Percentage")+
  scale_fill_manual(values=c("sienna2","purple"),name = "l) Flower symmetry")+
  theme(legend.key.size = unit(0.2, 'cm'))+theme(legend.position="top",legend.justification='left',legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,-10,0),
                                                 legend.title = element_text(face="bold"))+guides(fill=guide_legend(ncol=2,nrow=2,byrow=TRUE,title.position = "top",reverse = TRUE))+ylab("")
#######################################################################################################
#Calculate counts per group of Flower shape
Flower_morphology <- t  %>%group_by(Flower_morphology, Clusters) %>%summarise(counts = n()) 
levels(Flower_morphology$Flower_morphology)
Flower_morphology$Flower_morphology <- as.character(Flower_morphology$Flower_morphology)
Flower_morphology$Flower_morphology[Flower_morphology$Flower_morphology=="Funnelform"] <- "Campanulate"
Flower_morphology$Flower_morphology[Flower_morphology$Flower_morphology=="Spike"] <- "Brush"

#Calculate percentage of these counts
Flower_morphology <- group_by(Flower_morphology, Clusters) %>% mutate(percent = counts/sum(counts)*100)

Flower_morphology$Clusters <- as.character(Flower_morphology$Clusters)

Flower_morphology$Clusters[Flower_morphology$Clusters=="1"] <- "A"
Flower_morphology$Clusters[Flower_morphology$Clusters=="2"] <- "B"
Flower_morphology$Clusters[Flower_morphology$Clusters=="3"] <- "C"
Flower_morphology$Clusters[Flower_morphology$Clusters=="4"] <- "D"
Flower_morphology$Clusters[Flower_morphology$Clusters=="5"] <- "E"


Flower_morphology$Clusters <- factor(Flower_morphology$Clusters, levels=c("E","D","C", "B", "A"))



#Plot Compatibility
p6 <- ggplot(Flower_morphology, aes(x = percent, y = Clusters)) +
  geom_bar(
    aes( fill = Flower_morphology),alpha=0.85,
    stat = "identity", position = position_stack()) +theme_ms()+xlab("Percentage")+
  scale_fill_manual(values=c("blue","cyan4", "red","forestgreen","purple","gold"),name = "k) Flower shape")+
  theme(legend.key.size = unit(0.2, 'cm'))+theme(legend.position="top",legend.justification='left',legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,-10,0),
                                                 legend.title = element_text(face="bold"))+ guides(fill=guide_legend(ncol=3,nrow=2,byrow=TRUE,title.position = "top",reverse = TRUE))+ylab("")

#######################################################################################################
#Plot Nectar
#Calculate counts per group of Flower shape
Nectar_presence_absence <- t  %>% group_by(Nectar_presence_absence, Clusters) %>%summarise(counts = n()) 
levels(Nectar_presence_absence$Nectar_presence_absence)
Nectar_presence_absence$Nectar_presence_absence <- as.character(Nectar_presence_absence$Nectar_presence_absence)
Nectar_presence_absence$Nectar_presence_absence[Nectar_presence_absence$Nectar_presence_absence=="yes"] <- "Yes"
Nectar_presence_absence$Nectar_presence_absence[Nectar_presence_absence$Nectar_presence_absence=="no"] <- "No"

Nectar_presence_absence$Nectar_presence_absence <- factor(Nectar_presence_absence$Nectar_presence_absence, levels=c("No","Yes"))

#Calculate percentage of these counts
Nectar_presence_absence <- group_by(Nectar_presence_absence, Clusters) %>% mutate(percent = counts/sum(counts)*100)



Nectar_presence_absence$Clusters <- as.character(Nectar_presence_absence$Clusters)


Nectar_presence_absence$Clusters[Nectar_presence_absence$Clusters=="1"] <- "A"
Nectar_presence_absence$Clusters[Nectar_presence_absence$Clusters=="2"] <- "B"
Nectar_presence_absence$Clusters[Nectar_presence_absence$Clusters=="3"] <- "C"
Nectar_presence_absence$Clusters[Nectar_presence_absence$Clusters=="4"] <- "D"
Nectar_presence_absence$Clusters[Nectar_presence_absence$Clusters=="5"] <- "E"


Nectar_presence_absence$Clusters <- factor(Nectar_presence_absence$Clusters, levels=c("E","D","C", "B", "A"))



p7 <- ggplot(Nectar_presence_absence, aes(x = percent, y = Clusters)) +
  geom_bar(
    aes( fill = Nectar_presence_absence),alpha=0.85,
    stat = "identity", position = position_stack()) +theme_ms()+xlab("Percentage")+
  scale_fill_manual(values=c("black","darkorange"),name = "m) Nectar production")+
  theme(legend.key.size = unit(0.2, 'cm'))+theme(legend.position="top",legend.justification='left',legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,-10,-10,0),
                                                 legend.title = element_text(face="bold"))+ guides(fill=guide_legend(ncol=3,nrow=2,byrow=TRUE,title.position = "top",reverse = TRUE))+ylab("")




bottom <- plot_grid(p1,p2,p3,p4,p6,p5,NULL,p7,NULL, ncol=3,nrow=3,  align = "h")


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


bottom_1 <- plot_grid(title,bottom,ncol = 1,rel_heights = c(0.1, 1))


plot_grid(top_2, bottom_1,nrow=2, align = "hv")


