# Processing data from arroyo-correa 2019
# DOI: https://doi.org/10.1111/j.1442-9993.2005.01474.x
#Data: 10.5281/zenodo.3011651
# CHINA
# Note: Same place as robertson networks but sampled in 2009-2010
# 13 sites within the same woodland

#LOAD LIBRARY
library(reshape2)

#LOAD DATA
#SITE 1
arroyo_1 <- read.csv("Data/Data_processing/arroyo-correa_2019/arroyo-correa_2019_site_1.csv", row.names = 1, check.names = F)

#check species levels
#plants
levels(as.factor(rownames(arroyo_1)))
rownames(arroyo_1) <- gsub(" sp", " sp.", rownames(arroyo_1))
#pollinators
levels(as.factor(colnames(arroyo_1)))
colnames(arroyo_1) <- gsub(" sp", " sp.", colnames(arroyo_1))
colnames(arroyo_1)[colnames(arroyo_1)=="Tachinidae sp.15? Bessa"] <- "Tachinidae sp.15"
colnames(arroyo_1)[colnames(arroyo_1)=="Tachinidae sp.4 ? Microphtalma"] <- "Tachinidae sp.4"
colnames(arroyo_1)[colnames(arroyo_1)=="Leioproctus ?waipounamu"] <- "Leioproctus sp."
colnames(arroyo_1)[colnames(arroyo_1)=="Muscidae"] <- "Muscidae sp."
colnames(arroyo_1)[colnames(arroyo_1)=="Scirtidae"] <- "Scirtidae sp."
colnames(arroyo_1)[colnames(arroyo_1)=="Scirtidae"] <- "Scirtidae sp."
colnames(arroyo_1)[colnames(arroyo_1)=="Pales sp.."] <- "Pales sp."
colnames(arroyo_1)[colnames(arroyo_1)=="Tachinidae"] <- "Tachinidae sp."
colnames(arroyo_1)[colnames(arroyo_1)=="Asilidae"] <- "Asilidae sp."
colnames(arroyo_1)[colnames(arroyo_1)=="Erythronychia sp.."] <- "Erythronychia sp."
colnames(arroyo_1)[colnames(arroyo_1)=="Leioproctus ?waipounamu"] <- "Leioproctus sp."


write.csv(arroyo_1, "Data/Data_processing/arroyo-correa_2019/arroyo_correa_new_zealand_2019_1")

#SITE 2
arroyo_2 <- read.csv("Data/Data_processing/arroyo-correa_2019/arroyo-correa_2019_site_2.csv", row.names = 1, check.names = F)
#check species levels
#plants
levels(as.factor(rownames(arroyo_2)))
rownames(arroyo_2) <- gsub(" sp", " sp.", rownames(arroyo_2))
#pollinators
levels(as.factor(colnames(arroyo_2)))
colnames(arroyo_2) <- gsub(" sp", " sp.", colnames(arroyo_2))
colnames(arroyo_2) <- gsub("  ", " ", colnames(arroyo_2))
colnames(arroyo_2)[colnames(arroyo_2)=="Oscinosoma sp.eighti"] <- "Oscinosoma speighti"
colnames(arroyo_2)[colnames(arroyo_2)=="Pales sp.."] <- "Pales sp."
colnames(arroyo_2)[colnames(arroyo_2)=="Campylia sp.."] <- "Campylia sp."
colnames(arroyo_2)[colnames(arroyo_2)=="Dixidae"] <- "Dixidae sp."
colnames(arroyo_2)[colnames(arroyo_2)=="Heterocera"] <- "Heterocera sp."
colnames(arroyo_2)[colnames(arroyo_2)=="Sciomyzidae"] <- "Sciomyzidae sp."
colnames(arroyo_2)[colnames(arroyo_2)=="Tachinidae"] <- "Tachinidae sp."
colnames(arroyo_2)[colnames(arroyo_2)=="Chalcidoidea"] <- "Chalcidoidea sp."
colnames(arroyo_2)[colnames(arroyo_2)=="Simuliidae"] <- "Simuliidae sp."
colnames(arroyo_2)[colnames(arroyo_2)=="Chironomidae"] <- "Chironomidae sp."
colnames(arroyo_2)[colnames(arroyo_2)=="Muscidae"] <- "Muscidae sp."
colnames(arroyo_2)[colnames(arroyo_2)=="Syrphidae"] <- "Syrphidae sp."

write.csv(arroyo_2, "Data/Data_processing/arroyo-correa_2019/arroyo_correa_new_zealand_2019_2")


#SITE 2
arroyo_3 <- read.csv("Data/Data_processing/arroyo-correa_2019/arroyo-correa_2019_site_3.csv", row.names = 1, check.names = F)
#check species levels
#plants
levels(as.factor(rownames(arroyo_3)))
rownames(arroyo_3) <- gsub(" sp", " sp.", rownames(arroyo_3))
#pollinators
levels(as.factor(colnames(arroyo_3)))
colnames(arroyo_3) <- gsub(" sp", " sp.", colnames(arroyo_3))
colnames(arroyo_3) <- gsub("  ", " ", colnames(arroyo_3))
colnames(arroyo_3)[colnames(arroyo_3)=="Oscinosoma sp.eighti"] <- "Oscinosoma speighti"
colnames(arroyo_3)[colnames(arroyo_3)=="Pales sp.."] <- "Pales sp."
colnames(arroyo_3)[colnames(arroyo_3)=="Campylia sp.."] <- "Campylia sp."
colnames(arroyo_3)[colnames(arroyo_3)=="Dixidae"] <- "Dixidae sp."
colnames(arroyo_3)[colnames(arroyo_3)=="Heterocera"] <- "Heterocera sp."
colnames(arroyo_3)[colnames(arroyo_3)=="Sciomyzidae"] <- "Sciomyzidae sp."
colnames(arroyo_3)[colnames(arroyo_3)=="Tachinidae"] <- "Tachinidae sp."
colnames(arroyo_3)[colnames(arroyo_3)=="Chalcidoidea"] <- "Chalcidoidea sp."
colnames(arroyo_3)[colnames(arroyo_3)=="Simuliidae"] <- "Simuliidae sp."
colnames(arroyo_3)[colnames(arroyo_3)=="Chironomidae"] <- "Chironomidae sp."
colnames(arroyo_3)[colnames(arroyo_3)=="Muscidae"] <- "Muscidae sp."
colnames(arroyo_3)[colnames(arroyo_3)=="Syrphidae"] <- "Syrphidae sp."
colnames(arroyo_3)[colnames(arroyo_3)=="Scirtidae"] <- "Scirtidae sp."
colnames(arroyo_3)[colnames(arroyo_3)=="Asilidae"] <- "Asilidae sp."
colnames(arroyo_3)[colnames(arroyo_3)=="Leioproctus ?imitatus"] <- "Leioproctus sp."
colnames(arroyo_3)[colnames(arroyo_3)=="Leioproctus sp. aff boltoni"] <- "Leioproctus sp.1"


write.csv(arroyo_3, "Data/Data_processing/arroyo-correa_2019/arroyo_correa_new_zealand_2019_3")

