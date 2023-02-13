#Load libraries
#LOAD LIBRARIES
library(phytools)
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distancelibrary(MASS)
library(reshape2)
library(viridis) #COLOUR GGPLOT
library(MASS)
library(ggplot2)
library(broman) #crayon colours
library(cowplot)
library(ggstar)

# Theme for publication
theme_ms <- function(base_size=12, base_family="Helvetica") {
  (theme_bw(base_size = base_size, base_family = base_family)+
     theme(text=element_text(color="black"),
           axis.title=element_text( size = rel(1.6)),
           axis.text=element_text(size = rel(1.6), color = "black"),
           legend.title=element_text(face="bold"),
           legend.text=element_text(size = rel(1.1)),
           legend.background=element_rect(fill="transparent"),
           legend.key.size = unit(0.4, 'lines'),
           panel.border=element_rect(color="black",size=1),
           panel.grid.minor.x =element_blank(),
           panel.grid.minor.y= element_blank(),
           panel.grid.major= element_blank(),
           plot.title = element_text(size = rel(2.5), face="bold")
     ))
}

#Load trait data
dat_cleaning_5 <- readRDS("Data/RData/data_all_species_for_rmd_plot_ppca_QUALITATIVE.rds")
#convert rownames to column
dat_cleaning_5 <- setDT(dat_cleaning_5, keep.rownames = TRUE)[]
colnames(dat_cleaning_5)[1] <- "Plant_species"

#Load PCA data
phyl_pca_forest <- readRDS("Data/RData/phyl_pca_forest.rds")

#Load visitation data
long_d <- read.csv("Data/Csv/long_format_quantitative_networks.csv", row.names = 1) #quantitative network data|weighted by frequency of visits per plant species
#Select data with interaction greater than 0
long_d_1 <- long_d[long_d$Interaction>0,]
#Remove other orders/guilds that are not these ones
long_d_2 <- long_d_1[!is.na(long_d_1$guild),] #I do it by guild because just these 6 guilds are named in this column
#check levels
levels(factor(long_d_2$guild)) #9 DIFFERENT GUILDS|After I'll select the main fucntional poll. groups for analysis

#Generate long data with poll data
data_analysis <- merge(long_d_2, dat_cleaning_5, by = "Plant_species")

data_bee = data_analysis %>% 
  mutate(guild = case_when(guild=="Non-bee-Hymenoptera" ~ "Non-bee-Hymenoptera", 
                           guild!="Non-bee-Hymenoptera" ~ "Other")) %>% 
  filter(guild == "Non-bee-Hymenoptera") %>% 
  distinct(Plant_species, guild)

#Adding the species visited by the guild of bees
dat_cleaning_5 = full_join(dat_cleaning_5, data_bee) 

dat_cleaning_5 = dat_cleaning_5 %>% 
  mutate(guild = case_when(guild=="Non-bee-Hymenoptera" ~ "Non-bee-Hymenoptera", 
                           is.na(guild) ~ "Other")) 



#Call the output PC for simplicity
PC <- phyl_pca_forest
#CHECK CONTENT
#EIGENVALUES
PC$Eval
#PC score (POINTS)
PC$S
#PC loadings (ARROWS)
PC$L


#############################################################################################################################################################
#Load plots
PCbiplot <- function(PC, x="PC1", y="PC2") {
  # PC being a prcomp object
  data <- data.frame(PC$S)
  plot <- ggplot(data, aes_string(x=x, y=y)) #generate plot
  dat <- data.frame(x = data[,x], y = data[,y])
  
  #######
  #DENSITY FUNCTION
  #######
  get_density <- function(x, y, ...) {
    dens <- MASS::kde2d(x, y, ...)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
  }
  
  dat$density <- get_density(dat$x, dat$y, h = c(2, 2), n = 1000) #obtain density
  dat$guild <- dat_cleaning_5$guild
  
  plot <- plot + geom_star(data=dat, aes(-x, -y, starshape=guild, fill=guild), 
                           size=2.25, color= NA, alpha = 0.85)+ scale_fill_manual(values=c("tomato2", "azure3")) +
    scale_starshape_manual(values = c(13, 15, 28, 5))  
  
  
  ########
  #ADD ARROWS 
  ########
  datapc <- data.frame(PC$L) #CREATE DATAFRAME WITH LOADINGS
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .75 * mult * (get(x)),
                      v2 = .75 * mult * (get(y))
  )
  # add arrows
  plot <- plot + geom_segment(data=datapc,linejoin="round", lineend="round",aes(x=0, y=0, xend=-v1, yend=-v2),size=1.2, arrow=arrow(length=unit(0.5,"cm")), alpha=1, color="black")
  
  #Add axis with perctentage
  percentage <- round(diag(PC$Eval) / sum(PC$Eval) * 100, 2) #calculate percentage
  
  
  #ADD THE OTHER DIRECTION OF THE SEGMENT BECAUSE LOOKS COOL
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2),size=0.8, arrow=arrow(length=unit(0,"cm")),linetype=2, alpha=0.8, color="black")
  
  #ADD LABELS
  rownames(PC$L) <- c("S", "FN", "FS", "SL", "ON", "PH" )
  
  PCAloadings <- data.frame(Variables = rownames(PC$L), PC$L)
  plot <- plot + annotate("text", x = -(PCAloadings$PC1*c(4.6,5.2,5.25,7.9,7.1,6)), y = -(PCAloadings$PC2*c(4.45,4.2,5.4,7.7,6,6.5)+c(0,0,0,0,-0.1,0)),
                          label = PCAloadings$Variables, color="black",size=6, fontface=2)
  
  #CHANGE THEME
  
  plot <- plot + theme_ms() +ylim(-4,4) + xlim(-4,4) +  theme(legend.position = c(0.220, 0.130)) +
    ggtitle("(a) Non-bee-Hymenoptera") + xlab(NULL)  + ylab("Autonomous selfing - Floral display axis")
  
  
  plot
  
}



PCbiplot(PC)


#Save plot
p = PCbiplot(PC)
saveRDS(p, "Data/RData/Non_bee_hymenoptera_PCA.rds")
