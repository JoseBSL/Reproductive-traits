########################################################################################################################################################
#SCRIPT TO CALCULATE THE PCA

#1) LOAD DATA 

########################################################################################################################################################
#LOAD LIBRARIES
library(phytools)
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distance
library(DHARMa)
library(brms)
library(cmdstanr)
library(ggplot2)
########################################################################################################################################################
#1) LOAD DATA
########################################################################################################################################################
#read data with missing values filled by data imputation
dat <- read.csv("Data/Csv/all_species_imputed_trait_data.csv", row.names = "X")
########################################################################################################################################################
#2) Calculate phylogenetic distance
########################################################################################################################################################
########################################################################################################################################################
#remove not found species, cannot do PCA with unequal numbers of rows
dat <- dat[-c(414,480,482,634,705,1139),]
cols.num <- c("Family_all","Genus_all","Species_all")
dat[cols.num] <- sapply(dat[cols.num],as.character)
dat$Species_all <- gsub("Species_all_", "", dat$Species_all)
dat <- dat[!dat$Species_all == "Diospyros seychellarum", ]
dat <- dat[!dat$Species_all == "Memecylon eleagni", ]
dat <- dat[!dat$Species_all == "Ocotea laevigata", ]
dat <- dat[!dat$Species_all == "Soulamea terminaloides", ]

#prepare dataframe to calculate tree
phylo <- as.data.frame(cbind(dat$Family_all, dat$Genus_all, dat$Species_all))
colnames(phylo) <-  c("family", "genus", "species")

#Select unique cases
phylo_1 <- phylo[!duplicated(phylo$species),]
phylo_2 <- tibble(phylo_1)
str(phylo_2)

phylo_output <- get_tree(sp_list = phylo_2, tree = tree_plant_otl, taxon = "plant")
str(phylo_output)

#Convert phylogenetic tree into matrix
A_5 <- vcv.phylo(phylo_output)
#Standardize to max value 1
A_5 <- A_5/max(A_5)
#Unify column names; remove underscore and remove asterik
rownames(A_5) <- gsub("\\*", "", rownames(A_5))
colnames(A_5) <- gsub("\\*", "", colnames(A_5))
colnames(A_5) <- gsub("_", " ", colnames(A_5))
rownames(A_5) <- gsub("_", " ", rownames(A_5))

########################################################################################################################################################
#3) Calculate PCA
########################################################################################################################################################

#dat_scaled <- scale(dat[,c(11,13,14,16,17,20)], center = T, scale = T)
dat_scaled <- scale(dat[,c(8,11,14,16,17,20)], center = T, scale = T)
head(dat_scaled)
colnames(dat_scaled) <- c("Selfing", "Flower N.", "Flower width", "Style length", "Ovule N.", "Plant height")



dat$Species_all <- gsub(" ", "_", dat$Species_all)

rownames(dat_scaled) <- dat$Species_all

#This works fine no try the oither method. However, not any clue how to plot it well at the moment

library(ape)
library(phylobase)
library(adephylo)
d.4d <- phylo4d(phylo_output, dat_scaled)
output.4d <- ppca(d.4d,scale=FALSE,scannf=FALSE,nfposi=1,nfnega=1, method="Abouheif")

#
p <- barplot(output.4d$eig,main='pPCA eigenvalues',cex.main=1.8)
text(p,par("usr")[3],
     lab=rownames(output.4d$c1), 
     srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)



#Contribution to principal components
dotchart(output.4d$c1[,1],lab=rownames(output.4d$c1),main="Global principal 1")
dotchart(output.4d$c1[,2],lab=rownames(output.4d$c1),main="Global principal 2")

#check phylo structure
#abouheif.moran(d.4d)
#following this paper https://doi.org/10.1007/978-1-4939-8850-1_13

#Calculate %  
percentage <- round(output.4d$eig / sum(output.4d$eig) * 100, 2)
percentage <- diag(as.matrix(percentage))
percentage <- paste0(names(percentage), " (", percentage, "%)")


#####
#CODE ADAPTED FROM https://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2
######


library(ggpubr)
library(kgc)
library(MASS)
library(reshape2)
PC <- output.4d
colnames(PC$c1) <- c("PC1", "PC2")

PCbiplot <- function(PC, x="PC1", y="PC6") {
  # PC being a prcomp object
  data <- data.frame(PC$li)
  plot <- ggplot(data, aes_string(x=x, y=y)) 
  colnames(PC$c1) <- c("PC1", "PC6")
  datapc <- data.frame(PC$c1)
  percentage <- round(PC$eig / sum(PC$eig) * 100, 2)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .5 * mult * (get(x)),
                      v2 = .5 * mult * (get(y))
  )
  plot <- plot + geom_point(data=data, aes(x=-PC1, y=-PC6), size = I(0.4), alpha=I(0.4),pch=21, colour="black",fill="black") + theme_bw()
  plot <- plot + geom_segment(data=datapc,linejoin="round", lineend="round",aes(x=0, y=0, xend=-v1, yend=-v2),size=1.5, arrow=arrow(length=unit(0.5,"cm")), alpha=1, color="brown4")
  plot <- plot + xlab(paste("PC1 ", "(",(percentage[1]),")","%", sep = ""))
  plot <- plot + ylab(paste("PC2 ", "(",(percentage[2]),"%",")", sep = ""))
  plot <- plot + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                  panel.border = element_rect(linetype = "solid", colour = "black", size=1))
  
  PCAloadings <- data.frame(Variables = rownames(PC$c1), PC$c1)
  
  
  plot <- plot + annotate("text", x = -(PCAloadings$PC1*c(5,7,6,5,12,5)), y = -(PCAloadings$PC6*c(5,7,5,5,5,5)),
                          label = PCAloadings$Variables, color="brown4",size=5)
  
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2),size=1, arrow=arrow(length=unit(0,"cm")),linetype=2, alpha=0.8, color="brown4")
  
  plot
 
  #CALCULATE KERNELS
  mv.kde <- kde2d(data[,1], data[,2], n = 1544)
  dx <- diff(mv.kde$x[1:2])  # lifted from emdbook::HPDregionplot()
  dy <- diff(mv.kde$y[1:2])
  sz <- sort(mv.kde$z)
  c1 <- cumsum(sz) * dx * dy
  
  # specify desired contour levels:
  prob <- c(0.90,0.5)
  
  # plot:
  dimnames(mv.kde$z) <- list(mv.kde$x,mv.kde$y)
  dc <- melt(mv.kde$z)

  dc$prob <- approx(sz,1-c1,dc$value)$y
  
  plot <- plot + geom_contour(data=dc, aes(x=-Var1,y=-Var2,z=prob),colour="brown4",breaks=prob)
  
  plot
  
  
}

PCbiplot(PC)





#####
##tHIS MAYBE LEAD TO ADD SOME DENSITY COLOURS OF THE POINTS R
####
ggplot(data=df,aes(x,y)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  guides(alpha="none") +
  geom_point() + commonTheme





mv.kde <- kde2d(PC$li[,1], PC$li[,2], n = 200)
dx <- diff(mv.kde$x[1:2])  # lifted from emdbook::HPDregionplot()
dy <- diff(mv.kde$y[1:2])
sz <- sort(mv.kde$z)
c1 <- cumsum(sz) * dx * dy

# specify desired contour levels:
prob <- c(0.90,0.5)

# plot:
dimnames(mv.kde$z) <- list(mv.kde$x,mv.kde$y)
dc <- melt(mv.kde$z)

dc$prob <- approx(sz,1-c1,dc$value)$y
head(dc)






# A color palette from blue to yellow to red
library(RColorBrewer)
k <- 11
my.cols <- rev(brewer.pal(k, "RdYlBu"))

## compute 2D kernel density, see MASS book, pp. 130-131
z <- kde2d(PC$li[,1], PC$li[,2], n=1000)

# Make the base plot
plot(data_1, xlab="X label", ylab="Y label", pch=19, cex=.4)

# Draw the colored contour lines
contour(z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE, lwd=2)





dd1<-read.table(text="dist  depth
            4916.64 8.661827
            4916.64 14.789091
            4916.64 13.555909
            4916.64 12.92816
            4916.64 11.708774
            4916.64 15.28
            4916.64 13.369875
            4916.64 14.039655
            4916.64 13.454545
            4916.64 12.638261
            4916.64 13.251081
            4916.64 14.006341
            4916.64 12.64
            4916.64 15.521818
            4916.64 10.202121
            4916.64 14.816667
            4916.64 15.504
            9674.844    23.93
            11000.151   22.157143
            11414.31    22.72
            11414.31    25.7
            11414.31    19.07
            11414.31    23.085714
            9481.57 17.266667
            11414.31    26.8
            11414.31    19.382222
            5616.09 12.016667
            10658.02    18.873913
            11414.31    25.2
            11414.31    20.9
            11414.31    27.65
            11414.31    22.133333
            11414.31    30.9
            5616.09 23.3
            11172.718   20.391667
            9964.755    23.51
            5616.09 19.43
            5616.09 19.1
            4916.64 18.42
            8515.2  17.683333
            11414.31    22.128571
            11414.31    22.8608
            10391.095   24.955882
            10931.125   25.225
            6444.407    20.228571
            11276.257   23.77619
            10585.993   23.285714
            10641.214   20.653333
            9757.676    24.007143
            11414.31    18.817
            11414.31    23.525
            11414.31    22.873684
            11414.31    26.15
            10486.595   21.9
            11000.151   24.142857
            11414.31    24.3875
            10819.621   20.569231
            10360.088   29.345455
            9708.951    21.488235
            11414.31    30.775
            11414.31    25.5
            11414.31    18.477917
            10327.144   26.8625
            11414.31    26.12963
            11414.31    29.28125
            11414.31    23.166667
            10689.532   21.8625
            11414.31    28.328571
            11414.31    22.563158
            11414.31    25.490909
            11414.31    26.0625
            11414.31    34.5
            11414.31    17.375294",header=T)

library(ks)
## auto bandwidth selection
H.pi2<-Hpi(PC$li,binned=TRUE)*1
ddhat<-kde(PC$li,H=H.pi2)
plot(ddhat, xlab="X label", ylab="Y label", pch=19, cex=.4)

plot(ddhat,cont=c(75),add=FALSE,lty=5,lwd=2.5, display="filled.contour2", col=c(NA,"blue"),xlim=c(-10,19))
plot(ddhat,cont=c(95),drawpoints=TRUE,xlab="Distance (m)",lwd=2.5, 
     ylab="Depth (m)",ptcol="grey15",cex=0.7,display="filled.contour2", col=c(NA,"yellow"),
     xlim=c(min(dd1[,1]-dd1[,1]*0.4),max(dd1[,1]+dd1[,1]*0.4)),ylim=c(45,-1),add=TRUE) 

plot(ddhat,cont=c(50),add=TRUE,lwd=2.4, display="filled.contour2", col=c(NA,"green"))
plot(ddhat,cont=c(25),add=TRUE,lwd=2.4, display="filled.contour2",col=c(NA,"red"),alpha=0.5) 








# Generate some data
library(MASS)
set.seed(101)
n <- 50000
X <- mvrnorm(n, mu=c(.5,2.5), Sigma=matrix(c(1,.6,.6,1), ncol=2))

# A color palette from blue to yellow to red
library(RColorBrewer)
k <- 11
my.cols <- rev(brewer.pal(k, "RdYlBu"))

## compute 2D kernel density, see MASS book, pp. 130-131
z <- kde2d(X[,1], X[,2], n=50)

# Make the base plot
plot(X, xlab="X label", ylab="Y label", pch=19, cex=.4)

# Draw the colored contour lines
contour(z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE, lwd=2)

# Add lines for the mean of X and Y
abline(h=mean(X[,2]), v=mean(X[,1]), col="gray", lwd=1.5)

# Add the correlation coefficient to the top left corner
legend("topleft", paste("R=", round(cor(X)[1,2],3)), bty="n")


## Other methods to fix overplotting

# Make points smaller - use a single pixel as the plotting charachter
plot(X, pch=".")

# Hexbinning
library(hexbin)
plot(hexbin(X[,1], X[,2]))

# Make points semi-transparent
library(ggplot2)
qplot(X[,1], X[,2], alpha=I(.1))

# The smoothScatter function (graphics package)
smoothScatter(X)





  
  
  geom_point() + commonTheme




plot <- plot + geom_contour(data=dc, aes(x=-Var1,y=-Var2,z=prob),colour="brown4",breaks=prob)





















set.seed(123)
plot_data <-
  data.frame(
    X = c(rnorm(300, 3, 2.5), rnorm(150, 7, 2)),
    Y = c(rnorm(300, 6, 2.5), rnorm(150, 2, 2)),
    Label = c(rep('A', 300), rep('B', 150))
  )

data_1 <- output.4d$li

ggplot(data_1, aes(PC1, PC6)) +
  stat_density_2d(geom = "polygon",
                  aes(alpha = ..level..,fill = after_stat(..level..)),
                  bins = 120) 
  
  
  
  geom_point( size = 1.4, alpha=1,pch=21, colour="black",fill="black")



  
  
#OTHER EXAMPLE 
  library(MASS)
  library(ggplot2)
  n <- 1000
  x <- mvrnorm(n, mu=c(.5,2.5), Sigma=matrix(c(1,.6,.6,1), ncol=2))
  df = data.frame(x); colnames(df) = c("x","y")
  commonTheme = list(labs(color="Density",fill="Density",
                          x="RNA-seq Expression",
                          y="Microarray Expression"),
                     theme_bw(),
                     theme(legend.position=c(0,1),
                           legend.justification=c(0,1)))
  
  ggplot(data=df,aes(x,y)) + 
    stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
    scale_fill_continuous(low="green",high="red") +
    guides(alpha="none") +
    geom_point() + commonTheme
  
  
  ggplot(data=data_1,aes(PC1,PC6)) + 
    stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
    scale_fill_continuous(low="green",high="red") +
    guides(alpha="none") +
    geom_point() + commonTheme
  
  
  
  
  ggplot(data=data_1,aes(PC1,PC6)) + 
    stat_density2d(aes(fill=..level..,alpha=..level..),contour = T,geom='polygon',colour='black',breaks=1e-6*seq(0,10,by=2),bins = 200,n=200) + 
    scale_fill_continuous(low="green",high="red",limits=c(0,100)) +
    scale_alpha_continuous(range=c(0.1,0.5))+
    guides(alpha="none") +
    geom_point() + commonTheme
  
  
  base_plot <- ggplot(data_1, aes(x = PC1, y = PC6)) + 
    geom_point()
  
  base_plot + 
    stat_density2d(aes(color = ..level..))
  

  set.seed(123)
  plot_data <-
    data.frame(
      X = c(rnorm(300, 3, 2.5), rnorm(150, 7, 2)),
      Y = c(rnorm(300, 6, 2.5), rnorm(150, 2, 2)),
      Label = c(rep('A', 300), rep('B', 150))
    )
  
  ggplot(data_1, aes(PC1, PC6)) +
    stat_density2d(aes(fill=..level..,alpha=..level..),contour = TRUE,geom='polygon',colour='black',breaks=c(50,100),bins = 50,n=200) + 
    scale_fill_continuous(low="green",high="red") 
  
  
  
  
  
  
  
  
  
  
  hpd_contour <- function (x, n = 50, prob = 0.95, ...) {
    post1 <- MASS::kde2d(data_1[[1]], data_1[[2]], n = n, ...)
    
    dx <- diff(post1$x[1:2])
    dy <- diff(post1$y[1:2])
    sz <- sort(post1$z)
    c1 <- cumsum(sz) * dx * dy
    
    levels <- sapply(prob, function(x) {
      approx(c1, sz, xout = 1 - x)$y
    })
    
    as.data.frame(contourLines(post1$x, post1$y, post1$z, levels = levels))
  }
  
  post1 <- MASS::kde2d(data_1[[1]], data_1[[2]], n = n)
  
  
  theme_set(theme_bw(16))
  set.seed(1)
  n=100
  
  str(data_1)
  
  df <- data.frame(x=rnorm(n, 0, 1), y=rnorm(n, 0, 1))
  
  df <- data.frame(x=rnorm(n, 0, 1), y=rnorm(n, 0, 1))
  ContourLines <- hpd_contour(data_1[,1:2], prob=0.8)
  
  ggplot(data_1, aes(x = PC1, y = PC6)) +
    stat_density2d(aes(fill = as.factor(..level..)), bins=5, geom = "polygon") +
    geom_point() +
    geom_polygon(data = ContourLines, color = "blue", fill = NA) +
    scale_fill_manual(values = c("yellow","red","green","royalblue", "brown", "black", "white", "black", "white","black")) +
    scale_colour_manual(values = c("red", "black"))

  
  
  
  
  
  ibrary(ks)
  library(tidyverse)
  library(dplyr)
  set.seed(1001)
  
  ## data
  d <- MASS::mvrnorm(1000, c(0, 0.2), matrix(c(1, 0.4, 1, 0.4), ncol=2)) %>% 
    magrittr::set_colnames(c("x", "y")) %>% 
    as_tibble() 
  
  ## density function
  kd <- ks::kde(data_1[,1:2], compute.cont=TRUE, h=0.2)
  
  ## extract results
  get_contour <- function(kd_out=kd, prob="5%") {
    contour_95 <- with(kd_out, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                            z=estimate, levels=cont[prob])[[1]])
    as_tibble(contour_95) %>% 
      mutate(prob = prob)
  }
  
  prob<-c("10%", "20%","80%", "90%")
  dat_out <- map_dfr(c("10%", "20%","80%", "90%"), ~get_contour(kd, .)) %>% 
    group_by(prob) %>% 
    dplyr::mutate(n_val = 1:n()) %>% 
    ungroup()
  
  ## clean kde output
  kd_df <- expand_grid(x=kd$eval.points[[1]], y=kd$eval.points[[2]]) %>% 
    mutate(z = c(kd$estimate %>% t))
  
  colnames(data_1) <- c("x","y")
  ggplot(data=kd_df, aes(x, y)) +
    geom_point(data = data_1, alpha = I(0.4), size = I(0.4), colour = I("black")) +
    geom_path(aes(x, y, group = prob), 
              data=filter(dat_out, !n_val %in% 1:3), colour = I("black")) +
    geom_text(aes(label = prob), data = 
                filter(dat_out, (prob%in% c("10%", "20%","80%") & n_val==1) | (prob%in% c("90%") & n_val==100)),
              colour = I("black"), size =I(3))+
    scale_fill_viridis_c()+
    theme_bw() +
    theme(legend.position = "none")
  
  
  
  
  
  library(ks)
  library(tidyverse)
  library(dplyr)
  set.seed(1001)
  
  ## data
  d <- MASS::mvrnorm(1000, c(0, 0.2), matrix(c(1, 0.4, 1, 0.4), ncol=2)) %>% 
    magrittr::set_colnames(c("x", "y")) %>% 
    as_tibble() 
  
  ## density function
  kd <- ks::kde(d, compute.cont=TRUE, h=0.2)
  
  ## extract results
  get_contour <- function(kd_out=kd, prob="5%") {
    contour_95 <- with(kd_out, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                            z=estimate, levels=cont[prob])[[1]])
    as_tibble(contour_95) %>% 
      mutate(prob = prob)
  }
  
  dat_out <- map_dfr(c("10%", "20%","80%", "90%"), ~get_contour(kd, .)) %>% 
    group_by(prob) %>% 
    dplyr::mutate(n_val = 1:n()) %>% 
    ungroup()
  
  ## clean kde output
  kd_df <- expand_grid(x=kd$eval.points[[1]], y=kd$eval.points[[2]]) %>% 
    mutate(z = c(kd$estimate %>% t))
  