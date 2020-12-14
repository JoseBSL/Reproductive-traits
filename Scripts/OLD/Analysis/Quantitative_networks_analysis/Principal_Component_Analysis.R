###################################################################################
####
# PRINCIPAL COMPONENT ANALYSIS (PCA)
####
###################################################################################

#LOAD LIBRARIES
library(readxl) #read excel file (trait data)
library(dplyr) #data manipulation
library(missMDA) #For missing values
library(FactoMineR) #Produce pca biplot with individuals and variables
library(factoextra)
library(ggpubr)

#LOAD DATA
trait_data <- read_excel("Data/Trait_data_raw/Trait_data_final.xlsx",na = "NA")
#select just filled rows
trait_data_1 <- trait_data[1:1701,]
#filter data, select species with flower level info and capitulum
trait_filtered <- filter(trait_data_1, Info_level == "flower" |  Info_level == "capitulum")
levels(as.factor(trait_filtered$Info_level))
#select columns of interest
traits <- trait_filtered %>% select(Order_all,Family_all,Genus_all,Species_all,Breeding_system,IMPUTED_Compatibility,Autonomous_selfing_level,Autonomous_selfing_level_data_type,Autonomous_selfing_level_fruit_set,Flower_morphology,Flower_symmetry,Flowers_per_plant,Flowers_per_inflorescence,Floral_unit_width,Corolla_diameter_mean,Corolla_length_mean,STYLE_IMPUTED,OVULES_IMPUTED,life_form,lifespan,IMPUTED_plant_height_mean_m)
#Remove duplicated species
t <- traits[!duplicated(traits$Species_all), ]
#check structure of the data
str(t)
#remove cases where the species is NA 
t <- t[!is.na(t$Species_all), ]

str(t)
t_trial <- t[,c(5,6,7,9:21)]
str(t_trial)
cols <- c( "Breeding_system","IMPUTED_Compatibility", "Autonomous_selfing_level","Flower_morphology","Flower_symmetry", "life_form", "lifespan")

t_trial[cols] <- lapply(t_trial[cols], factor)  ## as.factor() could also be used

colnames(t_trial) <- c("Breeding system","Compatibility system","Selfing_level", "Quantitative selfing","Flower morphology","Flower symmetry",  "Flower/plant", "Flower/inflorescence", "Inflorescence width", "Flower width",  "Flower length",  "Style length", "Ovule number", "Life form","Life span", "Plant height")

str(t_trial$`Life form`)
t_trial$Selfing_level<- as.factor(t_trial$Selfing_level)
res.impute <- imputeFAMD(t_trial, ncp=3,threshold = 1e-06) 


res.impute$completeObs$Selfing_level <- factor(res.impute$completeObs$Selfing_level, levels = c("high", "medium", "low", "none"))

#from http://www.zhuoyao.net/2019/11/08/principal-component-methods-in-r-practical-guide/
fviz_pca_biplot(prcomp(res.impute$completeObs[-c(414,480,482,634,705,1139),c(4,7,8,9,10,12,13,16)], scale=T), label="var",
                geom.ind = "point",alpha.ind = 1, col.ind=res.impute$completeObs[-c(414,480,482,634,705,1139),c(3)], 
                pointshape = 19, pointsize = 1,
                palette = c("#FDE725FF","#31688EFF","#440154FF", "#35B779FF"),
                # Variables
                col.var = "black",title="",  arrowsize = 0.6,alpha.var=0.7)+xlim(-7, 10) + ylim (-8,8)+border()+labs(color="Selfing")



res.impute$completeObs$Selfing_level <- as.character(res.impute$completeObs$Selfing_level)
res.impute$completeObs$Selfing_level[res.impute$completeObs$Selfing_level=="high"] <- "selfer"
res.impute$completeObs$Selfing_level[res.impute$completeObs$Selfing_level=="medium"] <- "selfer"
res.impute$completeObs$Selfing_level[res.impute$completeObs$Selfing_level=="low"] <- "non_selfer"
res.impute$completeObs$Selfing_level[res.impute$completeObs$Selfing_level=="none"] <- "non_selfer"
res.impute$completeObs$Selfing_level <- as.factor(res.impute$completeObs$Selfing_level)


#from http://www.zhuoyao.net/2019/11/08/principal-component-methods-in-r-practical-guide/
fviz_pca_biplot(prcomp(res.impute$completeObs[-c(414,480,482,634,705,1139),c(4,7,8,9,10,12,13,16)], scale=T), label="var",
                geom.ind = "point",alpha.ind = 1, col.ind=res.impute$completeObs[-c(414,480,482,634,705,1139),c(3)], 
                pointshape = 19, pointsize = 1,
                palette = c("#black","#FDE725FF"),
                # Variables
                col.var = "black",title="",  arrowsize = 0.6,alpha.var=0.7)+xlim(-7, 10) + ylim (-8,8)+border()+labs(color="Selfing")


#I have prepare all in a Rmd file with the pca coloured by the different qualitative variables

#PREPARE NOW PANE OF PLOTS WITH THE INDIVIDUALS COLOURED BY QUALITATIVE VARIABLES

fviz_pca_ind(prcomp(res.impute$completeObs[-c(414,480,482,634,705,1139),c(4,7,8,9,10,12,13,16)], scale=T), label="none",
                geom.ind = "point",alpha.ind = 1, col.ind=res.impute$completeObs[-c(414,480,482,634,705,1139),c(3)], 
                pointshape = 19, pointsize = 1,axes.linetype=NA,
                palette = c("#35B779FF","#FDE725FF"))+xlim(-7, 10) + ylim (-8,8)+border()+labs(color="Selfing")+
  theme(strip.text.x = element_blank(),strip.background = element_rect(colour="white", fill="white"),legend.position=c(0.1,0.8),legend.title=element_text(size=8),legend.key.size = unit(0.7,"line"))+
  guides(color = guide_legend(override.aes = list(size=1.5)))



fviz_pca_biplot(prcomp(res.impute$completeObs[-c(414,480,482,634,705,1139),c(4,7,8,9,10,12,13,16)], scale=T), label="var",
                geom.ind = "point",alpha.ind = 0.8, 
                pointshape = 19, pointsize = 1,
                palette = c("#black"),
                # Variables
                col.var = "#009E73",title="",  arrowsize = 1.2,alpha.var=0.7)+xlim(-7, 10)



#PCA corrected by phylogeny

##################################################################################################################################################################################################################

#Trying phylogenetic PCA

library(adephylo)

res.impute$completeObs[-c(414,480,482,634,705,1139),c(4,7,8,9,10,12,13,16)]

ppca(res.impute$completeObs[,c(4,7,8,9,10,12,13,16)],A_5)




data(lizards)

if(require(ape) && require(phylobase)){
  
  #### ORIGINAL EXAMPLE FROM JOMBART ET AL 2010 ####
  
  
  ## BUILD A TREE AND A PHYLO4D OBJECT
  liz.tre <- read.tree(tex=lizards$hprA)
  liz.4d <- phylo4d(liz.tre, lizards$traits)
  par(mar=rep(.1,4))
  table.phylo4d(liz.4d,var.lab=c(names(lizards$traits),
                                 "ACP 1\n(\"size effect\")"),show.node=FALSE, cex.lab=1.2)
  
  
  ## REMOVE DUPLICATED POPULATIONS
  liz.4d <- prune(liz.4d, c(7,14))
  table.phylo4d(liz.4d)
  
  
  ## CORRECT LABELS
  lab <- c("Pa", "Ph", "Ll", "Lmca", "Lmcy", "Phha", "Pha",
           "Pb", "Pm", "Ae", "Tt", "Ts", "Lviv", "La", "Ls", "Lvir")
  tipLabels(liz.4d) <- lab
  
  
  ## REMOVE SIZE EFFECT
  dat <- tdata(liz.4d, type="tip")
  dat <- log(dat)
  newdat <- data.frame(lapply(dat, function(v) residuals(lm(v~dat$mean.L))))
  rownames(newdat) <- rownames(dat)
  tdata(liz.4d, type="tip") <- newdat[,-1] # replace data in the phylo4d object
  
  
  ## pPCA
  liz.ppca <- ppca(liz.4d,scale=FALSE,scannf=FALSE,nfposi=1,nfnega=1, method="Abouheif")
  liz.ppca
  tempcol <- rep("grey",7)
  tempcol[c(1,7)] <- "black"
  barplot(liz.ppca$eig,main='pPCA eigenvalues',cex.main=1.8,col=tempcol)
  
  par(mar=rep(.1,4))
  plot(liz.ppca,ratio.tree=.7)
  
  
  ## CONTRIBUTIONS TO PC (LOADINGS) (viewed as dotcharts)
  dotchart(liz.ppca$c1[,1],lab=rownames(liz.ppca$c1),main="Global principal
           component 1")
  abline(v=0,lty=2)
  
  dotchart(liz.ppca$c1[,2],lab=rownames(liz.ppca$c1),main="Local principal
           component 1")
  abline(v=0,lty=2)
  
  
  ## REPRODUCE FIGURES FROM THE PAPER
  obj.ppca <- liz.4d
  tdata(obj.ppca, type="tip") <- liz.ppca$li
  myLab <- paste(" ",rownames(liz.ppca$li), sep="")
  
  ## FIGURE 1
  par(mar=c(.1,2.4,2.1,1))
  table.phylo4d(obj.ppca, ratio=.7, var.lab=c("1st global PC", "1st local
                                              PC"), tip.label=myLab,box=FALSE,cex.lab=1.4, cex.sym=1.2, show.node.label=TRUE)
  add.scatter.eig(liz.ppca$eig,1,1,1,csub=1.2, posi="topleft", ratio=.23)
  
  
  ## FIGURE 2
  s.arrow(liz.ppca$c1,xlim=c(-1,1),clab=1.3,cgrid=1.3)
  
  
  
  #### ANOTHER EXAMPLE - INCLUDING NA REPLACEMENT ####
  ## LOAD THE DATA
  data(maples)
  tre <- read.tree(text=maples$tre)
  x <- phylo4d(tre, maples$tab)
  omar <- par("mar")
  par(mar=rep(.1,4))
  table.phylo4d(x, cex.lab=.5, cex.sym=.6, ratio=.1) # note NAs in last trait ('x')
  
  ## FUNCTION TO REPLACE NAS
  f1 <- function(vec){
    if(any(is.na(vec))){
      m <- mean(vec, na.rm=TRUE)
      vec[is.na(vec)] <- m
    }
    return(vec)
  }
  
  
  ## PERFORM THE PPCA
  dat <- apply(maples$tab,2,f1) # replace NAs
  x.noNA <- phylo4d(tre, as.data.frame(dat))
  map.ppca <- ppca(x.noNA, scannf=FALSE, method="Abouheif")
  map.ppca
  
  
  ## SOME GRAPHICS
  screeplot(map.ppca)
  scatter(map.ppca, useLag=TRUE)
  plot(map.ppca, useLag=TRUE)
  
  
  ## MOST STRUCTURED TRAITS
  a <- map.ppca$c1[,1] # loadings on PC 1
  names(a) <- row.names(map.ppca$c1)
  highContrib <- a[a< quantile(a,0.1) | a>quantile(a,0.9)]
  datSel <- cbind.data.frame(dat[, names(highContrib)], map.ppca$li)
  temp <- phylo4d(tre, datSel)
  table.phylo4d(temp) # plot of most structured traits
  
  
  ## PHYLOGENETIC AUTOCORRELATION TESTS FOR THESE TRAITS
  prox <- proxTips(tre, method="Abouheif")
  abouheif.moran(dat[, names(highContrib)], prox)
  
}


mp=barplot(map.ppca,ylab = "% explained variation",
           col=c("black","red","green","orange","yellow","light blue","purple","pink","maroon","blue2","blue"),
           ylim=c(0,100),axisnames=FALSE)
