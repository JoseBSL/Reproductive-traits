###################################################
#LONG FORMAT DATA WITH TRAITS AND POLLINATOR GUILDS
###################################################

#LOAD LIBRARIES
library(reshape2)
library(stringr)
library(tibble)
library(dplyr)
##################
#LOAD NETWORK DATA
##################
#Set working directory to read files
setwd("~/R_Projects/Reproductive traits/Data/Data_networks_quantitative") 

#Workflow found on stackoverflow to read all the files in a list
temp <- list.files(pattern="*.csv")
my.list <- list(for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])))
my_files <- list.files(pattern = "\\.csv$")
my_data <- lapply(my_files, read.csv)
#Add "id" to the list to the long format data
data_id_list <- lapply(seq_along(my_data), function(x) cbind(my_data[[x]], unique.id=my_files[x]))


##############################
#MELT THEM IN A LONG DATAFRAME
##############################

#For loop to melt each data frame and merge
i <- NULL
y <- NULL

for (i in data_id_list){
  i <- melt(i)
  y <- rbind(y, i)
}
#Output seems right

#Renaming columns
colnames(y) <- c("Plant_species", "Id", "Pollinator_species", "Interaction") 
#Reordering columns in a way that makes more sense for me
Long_data <- select(y, "Plant_species", "Pollinator_species", "Interaction", "Id") 
#Changing dot for white space
Long_data$Pollinator_species=gsub("\\."," ",Long_data$Pollinator_species)


##############################
#MERGE WITH POLLINATOR SPECIES
##############################

#PREPARE LONG FORMAT DATA TO MERGE
poll_species_uniq <- as.data.frame(Long_data[!duplicated(Long_data$Pollinator_species),])
poll_species_uniq$genus_old <- word(poll_species_uniq$Pollinator_species)

setwd("~/R_Projects/Reproductive traits") 
#READ POLLINATOR ORDERS, FAMILIES AND GENUSES (UNIQUE CASES)
poll_spp_names_corrected <- read.csv("Data/Data_processing/pollinator_species_names/poll_spp_names_corrected.csv")

all_long_format <- merge(poll_species_uniq, poll_spp_names_corrected, by= "genus_old", all = T)
all_long_format_1 <- all_long_format[complete.cases(all_long_format$Id),]

#Remove extra columns
all_long_format_2 <- all_long_format_1[,-c(6,7)]
#Add missing species,families and orders
all_long_format_3 <- add_column(all_long_format_2, R_number = seq(length(all_long_format_2$genus_old)), .before = 1)
all_long_format_4 <- data.frame(all_long_format_3[,-1], row.names = all_long_format_3[,1])
all_l <- as.data.frame(all_long_format_4, stringsAsFactors=F)
all_l$order <- as.character(all_l$order)
all_l$family <- as.character(all_l$family)
all_l$genus <- as.character(all_l$genus)

#Add missing species
all_l[4,c(6:8)] <- c("Hymenoptera","Megachilidae","Megachile")
all_l[9,c(6:8)] <- c("Hymenoptera","Formicidae","Acromyrmex")
all_l[26,c(6:8)] <- c("Hymenoptera","Apidae","Alepidosceles")
all_l[38,c(6:8)] <- c("Hymenoptera","Apidae","Alloscirtetica")
all_l[119,c(6:8)] <- c("Lepidoptera","Nymphalidae","Anthanassa")
all_l[141,c(6:8)] <- c("Hemiptera",NA,NA)
all_l[149,c(6:8)] <- c("Lepidoptera","Riodinidae","Apodemia")
all_l[154,c(6:8)] <- c("Hymenoptera","Andrenidae","Arhysosage")
all_l[155,c(6:8)] <- c("Lepidoptera","Riodinidae","Aricoris")
all_l[158,c(6:8)] <- c("Lepidoptera","Pieridae","Ascia")
all_l[163,c(6:8)] <- c("Diptera","Ceratopogonidae","Atrichopogon")
all_l[165,c(6:8)] <- c("Lepidoptera","Riodinidae","Ariconius")
all_l[168,c(6:8)] <- c("Hymenoptera","Halictidae","Augochlorella")
all_l[174,c(6:8)] <- c("Coleoptera","Chrysomelidae","Babiohaltica")
all_l[175,c(6:8)] <- c("Coleoptera","Chrysomelidae","Babiohaltica")
all_l[177,c(6:8)] <- c("Coleoptera",NA,NA)
all_l[178,c(6:8)] <- c("Coleoptera",NA,NA)
all_l[190,c(6:8)] <- c("Diptera","Bombyliidae","Bombylius")
all_l[239,c(6:8)] <- c("Coleoptera","Chrysomelidae",NA)
all_l[241,c(6:8)] <- c("Hymenoptera","Halictidae","Caenohalictus")
all_l[265,c(6:8)] <- c("Coleoptera","Nitidulidae","Camptodes")
all_l[271,c(6:8)] <- c("Coleoptera","Elateridae","Cardiophorus")
all_l[272,c(6:8)] <- c("Diptera","Cecidomyiidae",NA)
all_l[273,c(6:8)] <- c("Diptera","Cecidomyiidae",NA)
all_l[274,c(6:8)] <- c("Diptera","Nolidae","Celama")
all_l[275,c(6:8)] <- c("Lepidoptera","Sphingidae","Cenophodes")
all_l[276,c(6:8)] <- c("Hymenoptera","Apidae","Centris")
all_l[277,c(6:8)] <- c("Hymenoptera","Apidae",NA)
all_l[278,c(6:8)] <- c("Hymenoptera","Apidae","Centris")
all_l[279,c(6:8)] <- c("Hymenoptera","Apidae","Centris")
all_l[280,c(6:8)] <- c("Hymenoptera","Apidae","Centris")
all_l[286,c(6:8)] <- c("Hymenoptera","Halictidae","Ceratalictus")
all_l[303,c(6:8)] <- c("Diptera","Neriidae","Chaetonerius")
all_l[305,c(6:8)] <- c("Hymenoptera",NA,NA)
all_l[306,c(6:8)] <- c("Hymenoptera",NA,NA)
all_l[322,c(6:8)] <- c("Hymenoptera","Megachilidae","Chelostoma")
all_l[323,c(6:8)] <- c("Lepidoptera","Hesperiidae","Chioides")
all_l[335,c(6:8)] <- c("Diptera","Calliphoridae","Chloroprocta")
all_l[336,c(6:8)] <- c("Apodiformes","Trochilidae","Chlorostilbon")
all_l[337,c(6:8)] <- c("Coleoptera","Chrysomelidae","Cryptocephalus")
all_l[340,c(6:8)] <- c("Lepidoptera","Pyralidae",NA)
all_l[351,c(6:8)] <- c("Passeriformes","Nectariniidae","Cinnyris")
all_l[353,c(6:8)] <- c("Coleoptera","Coccinellidae",NA)
all_l[387,c(6:8)] <- c("Diptera","Calliphoridae","Compsomyiops")
all_l[395,c(6:8)] <- c("Diptera","Syrphidae","Copestylum")
all_l[396,c(6:8)] <- c("Diptera","Syrphidae","Copestylum")
all_l[397,c(6:8)] <- c("Diptera","Syrphidae","Copestylum")
all_l[400,c(6:8)] <- c("Lepidoptera","Noctuidae","Coronarta")
all_l[405,c(6:8)] <- c("Lepidoptera","Nymphalidae","Cosmosatyrus")
all_l[406,c(6:8)] <- c("Coleoptera","Curculionidae",NA)
all_l[407,c(6:8)] <- c("Coleoptera","Curculionidae",NA)
all_l[408,c(6:8)] <- c("Coleoptera","Curculionidae",NA)
all_l[420,c(6:8)] <- c("Coleoptera","Cryptophagidae",NA)
all_l[421,c(6:8)] <- c("Coleoptera","Cryptophagidae",NA)
all_l[422,c(6:8)] <- c("Coleoptera","Cryptophagidae",NA)
all_l[423,c(6:8)] <- c("Diptera","Dolichopodidae","Cryptophleps")
all_l[424,c(6:8)] <- c("Coleoptera","Curculionidae","Cryptorhynchidius")
all_l[432,c(6:8)] <- c("Diptera","Ceratopogonidae","Dasyhelea")
all_l[451,c(6:8)] <- c("Hymenoptera","Eumenidae","Delta")
all_l[453,c(6:8)] <- c("Coleoptera","Dermestidae",NA)
all_l[454,c(6:8)] <- c("Coleoptera","Dermestidae",NA)
all_l[455,c(6:8)] <- c("Coleoptera","Dermestidae",NA)
all_l[496,c(6:8)] <- c("Diptera","Dixidae",NA)
all_l[497,c(6:8)] <- c("Diptera","Formicidae","Dolichoderus")
all_l[518,c(6:8)] <- c("Coleoptera","Elateridae",NA)
all_l[519,c(6:8)] <- c("Coleoptera","Elateridae",NA)
all_l[520,c(6:8)] <- c("Coleoptera","Elateridae",NA)
all_l[521,c(6:8)] <- c("Coleoptera","Elateridae",NA)
all_l[522,c(6:8)] <- c("Lepidoptera","Geometridae","Ematurga")
all_l[538,c(6:8)] <- c("Lepidoptera","Pyralidae","Endotricha")
all_l[539,c(6:8)] <- c("Hymenoptera","Megachilidae","Epanthidium")
all_l[540,c(6:8)] <- c("Diptera","Ephydridae",NA)
all_l[541,c(6:8)] <- c("Diptera","Ephydridae",NA)
all_l[544,c(6:8)] <- c("Coleoptera","Tenebrionidae","Epitragus")
all_l[581,c(6:8)] <- c("Coleoptera","Curculionidae","Eudiagogini")
all_l[582,c(6:8)] <- c("Coleoptera","Curculionidae","Eudiagogini")
all_l[591,c(6:8)] <- c("Hymenoptera","Halictidae","Eupetersia")
all_l[593,c(6:8)] <- c("Coleoptera","Salpingidae","Eurycratus")
all_l[597,c(6:8)] <- c("Coleoptera","Curculionidae","Exapion")
all_l[607,c(6:8)] <- c("Hymenoptera","Figitidae",NA)
all_l[608,c(6:8)] <- c("Hymenoptera","Figitidae",NA)
all_l[632,c(6:8)] <- c("Coleoptera","Curculionidae","Geraeus")
all_l[636,c(6:8)] <- c("Lepidoptera","Pieridae","Appias")
all_l[639,c(6:8)] <- c("Lepidoptera","Erebidae","Gracilodes")
all_l[659,c(6:8)] <- c("Coleoptera","Erotylidae","Hapalips")
all_l[660,c(6:8)] <- c("Coleoptera","Erotylidae","Hapalips")
all_l[661,c(6:8)] <- c("Hymenoptera","Apidae","Hasinamelissa")
all_l[662,c(6:8)] <- c("Hymenoptera","Apidae","Hasinamelissa")
all_l[677,c(6:8)] <- c("Hymenoptera","Heloridae","Helorus")
all_l[679,c(6:8)] <- c("Squamata",NA,NA)
all_l[680,c(6:8)] <- c("Diptera","Calliphoridae","Hemilucilia")
all_l[689,c(6:8)] <- c("Lepidoptera","Crambidae","Herpetogramma")
all_l[690,c(6:8)] <- c("Lepidoptera","Hesperiidae",NA)
all_l[691,c(6:8)] <- c("Lepidoptera","Hesperiidae",NA)
all_l[692,c(6:8)] <- c("Lepidoptera",NA,NA)
all_l[693,c(6:8)] <- c("Lepidoptera",NA,NA)
all_l[694,c(6:8)] <- c("Lepidoptera",NA,NA)
all_l[698,c(6:8)] <- c("Hymenoptera",NA,NA)
all_l[732,c(6:8)] <- c("Hymenoptera",NA,NA)
all_l[733,c(6:8)] <- c("Hymenoptera",NA,NA)
all_l[734,c(6:8)] <- c("Hymenoptera",NA,NA)
all_l[735,c(6:8)] <- c("Hymenoptera",NA,NA)
all_l[736,c(6:8)] <- c("Hymenoptera",NA,NA)
all_l[737,c(6:8)] <- c("Lepidoptera","Erebidae",NA)
all_l[740,c(6:8)] <- c("Lepidoptera","Nymphalidae","Hypolimnas")
all_l[741,c(6:8)] <- c("Passeriformes", NA,NA)
all_l[742,c(6:8)] <- c("Diptera","Muscidae","Hyrdotaea")
all_l[751,c(6:8)] <- c("Hymenoptera","Platygastridae","Inostemma")
all_l[753,c(6:8)] <- c("Lepidoptera","Geometridae","Macaria")
all_l[755,c(6:8)] <- c("Hymenoptera","Megachilidae","Larocanthidium")
all_l[810,c(6:8)] <- c("Diptera","Asilidae","Lasiopogon")
all_l[831,c(6:8)] <- c("Hymenoptera","Apidae","Leptometriella")
all_l[846,c(6:8)] <- c("Diptera","Sarcophagagidae","Liosarcophaga")
all_l[847,c(6:8)] <- c("Diptera","Sciaridae","Lobosciara")
all_l[849,c(6:8)] <- c("Diptera","Lonchopteridae","Lonchoptera")
all_l[924,c(6:8)] <- c("Hymenoptera","Apidae","Melectoides")
all_l[933,c(6:8)] <- c("Hymenoptera","Apidae","Mesonychium")
all_l[936,c(6:8)] <- c("Lepidoptera", NA, NA)
all_l[945,c(6:8)] <- c("Hymenoptera", "Vespidae", "Mischocyttarus")
all_l[970,c(6:8)] <- c("Coleoptera", "Salpingidae", NA)
all_l[971,c(6:8)] <- c("Coleoptera", "Salpingidae", NA)
all_l[985,c(6:8)] <- c("Lepidoptera", "Nolidae", NA)
all_l[986,c(6:8)] <- c("Lepidoptera", "Nolidae", NA)
all_l[1023,c(6:8)] <- c("Diptera", "Syrphidae", "Othonevra")
all_l[1024,c(6:8)] <- c("Diptera", "Chloropidae", "Oscinosoma")
all_l[1072,c(6:8)] <- c("Diptera", "Tabanoidea", "Pelecorhychus")
all_l[1094,c(6:8)] <- c("Diptera", "Ulidiidae", "Physiphora")
all_l[1095,c(6:8)] <- c("Diptera", "Ulidiidae", "Physiphora")
all_l[1097,c(6:8)] <- c("Lepidoptera", "Nymphalidae", "Phystis")
all_l[1107,c(6:8)] <- c("Diptera", "Pipunculidae", NA)
all_l[1120,c(6:8)] <- c("Hymenoptera", "Platygastridae", NA)
all_l[1121,c(6:8)] <- c("Hymenoptera", "Platygastridae", NA)
all_l[1124,c(6:8)] <- c("Hymenoptera", "Apidae", "Plebeia")
all_l[1126,c(6:8)] <- c("Diptera", "Bombyliidae", "Poecilognathus")
all_l[1127,c(6:8)] <- c("Diptera", "Bombyliidae", "Poecilognathus")
all_l[1128,c(6:8)] <- c("Diptera", "Bombyliidae", "Poecilognathus")
all_l[1130,c(6:8)] <- c("Hymenoptera", "Cabronidae", "Polemistus")
all_l[1140,c(6:8)] <- c("Hymenoptera", "Pompilidae", NA)
all_l[1155,c(6:8)] <- c("Hymenoptera", "Andrenidae", "Psaenythia")
all_l[1158,c(6:8)] <- c("Hymenoptera", "Megachilidae", "Pseudocentron")
all_l[1159,c(6:8)] <- c("Hymenoptera", "Megachilidae", "Pseudocentron")
all_l[1161,c(6:8)] <- c("Diptera", "Sciaridae", "Pseudolycoriella")
all_l[1162,c(6:8)] <- c("Diptera", "Sciaridae", "Pseudolycoriella")
all_l[1163,c(6:8)] <- c("Hymenoptera", "Formicidae", "Pseudomyrmex")
all_l[1171,c(6:8)] <- c("Coleoptera", "Dasytidae", "Psylotrix")
all_l[1174,c(6:8)] <- c("Hymenoptera", "Colletidae", "Ptiloglossa")
all_l[1181,c(6:8)] <- c("Lepidoptera", "Hesperiidae", NA)
all_l[1183,c(6:8)] <- c("Lepidoptera", "Pieridae", "Pyristia")
all_l[1184,c(6:8)] <- c("Lepidoptera", "Pieridae", "Pyristia")
all_l[1190,c(6:8)] <- c("Coleoptera", "Scarabaeidae", "Pyronota")
all_l[1205,c(6:8)] <- c("Diptera", "Rhiniidae", "Rhinia")
all_l[1212,c(6:8)] <- c("Lepidoptera", "Riodinidae", NA)
all_l[1248,c(6:8)] <- c("Diptera", "Sciomyzidae", NA)
all_l[1249,c(6:8)] <- c("Diptera", "Sciomyzidae", NA)
all_l[1257,c(6:8)] <- c("Coleoptera", "Curculionidae", NA)
all_l[1258,c(6:8)] <- c("Coleoptera", "Curculionidae", NA)
all_l[1259,c(6:8)] <- c("Coleoptera", "Curculionidae", NA)
all_l[1260,c(6:8)] <- c("Coleoptera", "Curculionidae", NA)
all_l[1261,c(6:8)] <- c("Coleoptera", "Curculionidae", NA)
all_l[1279,c(6:8)] <- c("Diptera", "Simuliidae", NA)
all_l[1283,c(6:8)] <- c("Hymenoptera", "Siricidae", NA)
all_l[1284,c(6:8)] <- c("Diptera", "Syrphidae", NA)
all_l[1285,c(6:8)] <- c("Diptera", "Chironomidae", "Smitta")
all_l[1313,c(6:8)] <- c("Coleoptera", "Chrysomelidae", "Spintherophyta")
all_l[1315,c(6:8)] <- c("Coleoptera", "Staphylinidae", NA)
all_l[1316,c(6:8)] <- c("Coleoptera", "Staphylinidae", NA)
all_l[1317,c(6:8)] <- c("Coleoptera", "Staphylinidae", NA)
all_l[1319,c(6:8)] <- c("Coleoptera", "Mordellidae", "Stenomorda")
all_l[1320,c(6:8)] <- c("Coleoptera", "Cerambycidae", "Stenopturus")
all_l[1321,c(6:8)] <- c("Coleoptera", "Curculionidae", "Stenotrupis")
all_l[1322,c(6:8)] <- c("Coleoptera", "Cerambycidae", "Stenurella")
all_l[1323,c(6:8)] <- c("Lepidoptera", "Noctuidae", NA)
all_l[1370,c(6:8)] <- c("Hymenoptera", "Pompilidae", "Tachipompillus")
all_l[1374,c(6:8)] <- c("Lepidoptera", "Pieridae", "Tatochila")
all_l[1389,c(6:8)] <- c("Hymenoptera", "Apidae", "Tetragonisca")
all_l[1390,c(6:8)] <- c("Coleoptera", "Buprestidae", "Tetragonoschema")
all_l[1399,c(6:8)] <- c("Diptera", "Scatopsidae", "Thripomorpha")
all_l[1402,c(6:8)] <- c("Hymenoptera", "Thynnidae", NA)
all_l[1408,c(6:8)] <- c("Diptera", "Bombyliidae", "Toxophora")
all_l[1409,c(6:8)] <- c("Hemiptera", "Aleyrodidae", "Trialeurodes")
all_l[1418,c(6:8)] <- c("Diptera", "Tephritidae", "Trupanea")
all_l[1442,c(6:8)] <- c("Hymenoptera", "Vespidae", NA)
all_l[1443,c(6:8)] <- c("Hymenoptera", "Vespidae", NA)
all_l[1457,c(6:8)] <- c("Diptera", "Calliphoridae", "Xenocalliphora")
all_l[1458,c(6:8)] <- c("Diptera", "Helosciomyzidae", "Xenosciomyza")
all_l[1469,c(6:8)] <- c("Lepidoptera", "Nymphalidae", "Yphthimoides")
all_l[1476,c(6:8)] <- c("Hymenoptera", "Vespidae", "Zeta")
all_l[1478,c(6:8)] <- c("Lepidoptera", "Lycaenidae", "Zizinia")


##############################
#MERGE WITH POLLINATOR SPECIES
##############################
str(Long_data)
str(all_l)
all_l_1 <- all_l[,-c(1,2,4,5)]
str(all_l_1)

#merge
merged_all <- merge(Long_data,unique(all_l_1),by ="Pollinator_species",sort=FALSE)

levels(as.factor(merged_all$order))




