
#Check species at the end of the axes

#Load data
phyl_pca_forest <- readRDS("Data/RData/phyl_pca_forest.rds")

#Create dataframe
data <- data.frame(phyl_pca_forest$S)

#Load library
library(dplyr)
#Select top and bottom species
data %>% slice_min(PC1, n = 10)
data %>% slice_max(PC1, n = 10)

data %>% slice_min(PC2, n = 10)
data %>% slice_max(PC2, n = 10)



all_spp <- function(PC, x="PC1", y="PC2") {
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
  
  
  
  plot <- plot+stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon') + 
    scale_fill_continuous(low="green",high="red",guide=FALSE,breaks = c(0.02, 0.050, 0.09), labels = c("Low", "Medium", "High")) + theme(legend.position = "none")+guides(fill = guide_legend(override.aes = list(alpha = 0.7),title="Kernel density"))+
    scale_alpha(guide = 'none')
  
  
  plot <- plot + geom_point(data=dat, aes(x, y),size=1.3, color="black") +
    geom_point(data = data[which.min(data$PC1), ], color="blue", 
               size=3) +
    geom_point(data = data[which.max(data$PC1), ], color="blue", 
               size=3) +
    geom_point(data = data[which.max(data$PC2), ], color="blue", 
               size=3) +
    geom_point(data = data[which.min(data$PC2), ], color="blue", 
               size=3)
  ########
  #ADD ARROWS 
  ########
  datapc <- data.frame(PC$L) #CREATE DATAFRAME WITH LOADINGS
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  # add arrows
  plot <- plot + geom_segment(data=datapc,linejoin="round", lineend="round",aes(x=0, y=0, xend=-v1, yend=-v2),size=1.8, arrow=arrow(length=unit(0.5,"cm")), alpha=0.8, colour=c("black"))
  
  
  #ADD THE OTHER DIRECTION OF THE SEGMENT BECAUSE LOOKS COOL
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2),size=1.6, arrow=arrow(length=unit(0,"cm")),linetype=2, alpha=0.5, colour=c("black"))
  
  #Add axis with perctentage
  percentage <- round(diag(PC$Eval) / sum(PC$Eval) * 100, 2) #calculate percentage
  
  plot <- plot + xlab(paste("Flower number - flower size trade-off ", "(",(percentage[1]),"%",")", sep = "")) #XLAB
  plot <- plot + ylab(paste("Pollinator dependence trade-off ", "(",(percentage[2]),"%",")", sep = "")) #YLAB
  
  
  #CHANGE THEME
  
  plot <- plot + theme_bw() 
  
  #ADD LABELS
  rownames(PC$L) <- c("High selfing", "Many flowers", "Large flowers", "Long styles", "Many ovules", "Tall plants" )
  
  PCAloadings <- data.frame(Variables = rownames(PC$L), PC$L)
  plot <- plot + annotate("text", x = -(PCAloadings$PC1*c(4.7,5.05,5.6,7.85,7.3,6.15)), y = -(PCAloadings$PC2*c(4.693,2.5,3.1,6.6,5,5.5)+c(0,0,0,0,0.4,0)),
                          label = PCAloadings$Variables, color="black",fontface =2,size=4)
  
  
  plot <- plot + annotate("text", x = (PCAloadings$PC1*c(4.6,4.95,4.5,6.6,7.3,5.15)), y = (PCAloadings$PC2*c(3.83,6.4,5.7,7.4,-1,7.45)+c(0,0,0,0,-0.41,0)),
                          label =c("Low selfing", "Few flowers", "Small flowers", "Short styles", "Few ovules", "Short plants" ), color="black",fontface =2,size=4)
  
  
  plot <- plot + theme_ms() +ylim(-4,4) + xlim(-4,4) +  theme(legend.position = c(0.095, 0.11)) +ggtitle("") 
  
  
  
  plot
  
}

plot_grid(all_spp(PC))
