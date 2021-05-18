
library(dplyr)
library(kableExtra)

PC <- readRDS("Data/RData/phyl_pca_forest_nectar_pollen.rds")


dat <- PC$L[,c(1:3)]

#Create a row to add total percentage of variance explained by PC

percentage <- round(diag(PC$Eval) / sum(PC$Eval) * 100, 2) #calculate percentage

percen <- percentage[1:3]

dat <- rbind(dat, percen)

rownames(dat) <- c("Autonomous selfing", "Flowers per plant", "Flower width", "Style length", "Ovule number", "Plant height", "Microlitres of Nectar per flower" , "Pollen per flower","Explained variation")

dat <- as.data.frame(dat)

dat <- dat %>% mutate(across(is.numeric, ~ round(., 2)))

kable(dat, longtable = T, booktabs = T,linesep = "\\addlinespace", escape=FALSE) %>%
  kable_styling(latex_options = c("repeat_header","striped"), font_size = 12, full_width=F,position = "center") 
