# Processing data from Bartomeus 2008
# DOI: https://doi.org/10.1007/s00442-007-0946-1
# Catalonia, Spain
# Note: Sites sampled in 2005


#load library
library(reshape2)

#read data
bartomeus <- read.csv("Data/Data_processing/bartomeus_spain_2008/bartomeus_spain_2008.csv", header=T, stringsAsFactors=F)

#check levels
levels(as.factor(bartomeus$site))
#Nacho said that the sites can be grouped by the ones that start equally because they are homogeneous systems close to each other
#so lets do it

#Adequate plant species name to database
levels(as.factor(bartomeus$plant))
bartomeus$plant[bartomeus$plant=="Carpobrotus affinis acinaciformis"] <- "Carpobrotus affinis"
bartomeus$plant[bartomeus$plant=="Linum"] <- "Linum sp."

#now insects
levels(as.factor(bartomeus$gen_sp))
bartomeus$gen_sp[bartomeus$gen_sp=="AdelidaeL2"] <- "Adelidae sp"
bartomeus$gen_sp[bartomeus$gen_sp=="BibionidaeD24"] <- "Bibionidae sp"
bartomeus$gen_sp[bartomeus$gen_sp=="BibionidaeD25"] <- "Bibionidae sp2"
bartomeus$gen_sp[bartomeus$gen_sp=="BibionidaeD26"] <- "Bibionidae sp3"
bartomeus$gen_sp[bartomeus$gen_sp=="BraconidaeH80"] <- "Braconidae sp"
bartomeus$gen_sp[bartomeus$gen_sp=="BraconidaeH82"] <- "Braconidae sp2"
bartomeus$gen_sp[bartomeus$gen_sp=="BuprestidaeC53"] <- "Buprestidae sp"
bartomeus$gen_sp[bartomeus$gen_sp=="ColeopteraC4"] <- "Coleoptera sp"
bartomeus$gen_sp[bartomeus$gen_sp=="ColeopteraC7"] <- "Coleoptera sp2"
bartomeus$gen_sp[bartomeus$gen_sp=="ColeopteraC56"] <- "Coleoptera sp3"
bartomeus$gen_sp[bartomeus$gen_sp=="ColeopteraC57"] <- "Coleoptera sp4"
bartomeus$gen_sp[bartomeus$gen_sp=="ColeopteraC58"] <- "Coleoptera sp5"
bartomeus$gen_sp[bartomeus$gen_sp=="ColeopteraC58"] <- "Coleoptera sp6"
bartomeus$gen_sp[bartomeus$gen_sp=="ColeopteraC59"] <- "Coleoptera sp7"
bartomeus$gen_sp[bartomeus$gen_sp=="DasytidaeC4"] <- "Dasytidae sp"
bartomeus$gen_sp[bartomeus$gen_sp=="DipteraD32"] <- "Diptera sp"
bartomeus$gen_sp[bartomeus$gen_sp=="DipteraD35"] <- "Diptera sp2"
bartomeus$gen_sp[bartomeus$gen_sp=="HymenopteraH27"] <- "Hymenoptera sp"
bartomeus$gen_sp[bartomeus$gen_sp=="IchneumonidaeH81"] <- "Ichneumonidae sp"
bartomeus$gen_sp[bartomeus$gen_sp=="MegachilidaeH94"] <- "Megachilidae sp"
bartomeus$gen_sp[bartomeus$gen_sp=="MilichidaeD9"] <- "Milichidae sp"
bartomeus$gen_sp[bartomeus$gen_sp=="SirphidaeD29"] <- "Sirphidae sp"
bartomeus$gen_sp[bartomeus$gen_sp=="TachinidaeD33"] <- "Tachinidae sp"
bartomeus$gen_sp <- gsub("sp", "sp.", bartomeus$gen_sp)

#subset
batca_1_2 <- subset(bartomeus, site == "BAT1CA" | site == "BAT2CA")
fraop_1_2<- subset(bartomeus, site == "FRA1OP" | site == "FRA2OP")
medca_1_2 <- subset(bartomeus, site == "MED1CA" | site == "MED2CA")
medca_2_3 <- subset(bartomeus, site == "MED3CA" | site == "MED4CA")
miqop_1_2 <- subset(bartomeus, site == "MIQ1OP" | site == "MIQ2OP")
selop_1_2 <- subset(bartomeus, site == "SEL1OP" | site == "SEL2OP")


########
#6 sites
########

#SITE 1 site == "BAT1CA" | site == "BAT2CA"

batca <- acast(batca_1_2, batca_1_2$plant ~ batca_1_2$gen_sp , value.var='freq', 
                   fun.aggregate=sum, margins=F)

#save data
write.csv(batca, "Data/Data_processing/bartomeus_spain_2008/bartomeus_spain_2008_batca_1_2.csv")

#SITE 2  site == "FRA1OP" | site == "FRA2OP"

fraop <- acast(fraop_1_2, fraop_1_2$plant ~ fraop_1_2$gen_sp , value.var='freq', 
               fun.aggregate=sum, margins=F)

write.csv(fraop, "Data/Data_processing/bartomeus_spain_2008/bartomeus_spain_2008_fraop_1_2.csv")

#SITE 3  site == "MED1CA" | site == "MED2CA"

medca <- acast(medca_1_2, medca_1_2$plant ~ medca_1_2$gen_sp , value.var='freq', 
               fun.aggregate=sum, margins=F)

write.csv(medca, "Data/Data_processing/bartomeus_spain_2008/bartomeus_spain_2008_medca_1_2.csv")


#SITE 4  site == "MED3CA" | site == "MED4CA"

medca_2 <- acast(medca_2_3, medca_2_3$plant ~ medca_2_3$gen_sp , value.var='freq', 
               fun.aggregate=sum, margins=F)

write.csv(medca_2, "Data/Data_processing/bartomeus_spain_2008/bartomeus_spain_2008_medca_2_3.csv")


#SITE 5 site == "MIQ1OP" | site == "MIQ2OP"

miqop <- acast(miqop_1_2, miqop_1_2$plant ~ miqop_1_2$gen_sp , value.var='freq', 
                 fun.aggregate=sum, margins=F)

write.csv(miqop, "Data/Data_processing/bartomeus_spain_2008/bartomeus_spain_2008_miqop_1_2.csv")


#SITE 6 site == "SEL1OP" | site == "SEL2OP"

selop <- acast(selop_1_2, selop_1_2$plant ~ selop_1_2$gen_sp , value.var='freq', 
               fun.aggregate=sum, margins=F)

write.csv(selop, "Data/Data_processing/bartomeus_spain_2008/bartomeus_spain_2008_selop_1_2.csv")

