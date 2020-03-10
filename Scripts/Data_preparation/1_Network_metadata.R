#Metadata of the networks

#load package to extract references from bib.file
#install.packages("bib2df")

library(bib2df)
library(reshape2)

metadata <- read.csv("Data/Data_networks_processing/metadata.csv", row.names = 1)

#Read bibtex file with references
df <- bib2df("scripts/references.bib")
#Select column of interest
df <- df[,-c(3,4,6:12,14:16,20,22,24,28:30)] 

df$Id_number <- seq.int(nrow(df))

d <- merge(df, metadata, by="BIBTEXKEY")
str(d)
d <- apply(d,2,as.character)
write.csv(d, "Data/Data_networks_processing/networks_metadata.csv")




#Metadata of the networks

longitude <- c(3.296797, 10.216667, 10.233333, -68.015892, 1.575532, 9.1, 18.5,
               -20.5, 99.63806 ,135.866667,148.266667, 57.443254,129.493741, 
               -71.3,-52, 57.43, -90.600747, 171.566667,171.78466,171.720224,
               -61.716667, -67.416667, -89.8968771, -75.5, -57.885, -91.012863)

latitude <- c(42.315336, 56.066667, 56.066667,-32.008985, 52.762395, 56.1, 68.35,
              74.5, 27.90139,35.166667,-36.45,-20.452076, 28.377248, 81.816667,
              71, -20.25, -0.290164, -42.95,-43.02823, -43.099531, 5.583333,
              8.933333, 39.278958, 45.4,-21.701111, -0.6907)

locality <- c("Spain", "Denmark", "Denmark","Argentina", "England", "Denmark", 
              "Sweden", "Greenland", "China", "Japan", "Australia", "Mauritius",
              "Japan", "Canada", "Greenland","Mauritius", "Galapagos", "New Zealand",
              "New Zealand","New Zealand", "Canaima Nat. Park, Venezuela",
              "Guarico State, Venezuela", "Carlinville Illinois, USA","Ottawa Canada",
              "Brazil")

id <- c("bartomeus_2008_bat1ca.csv", "beck_2006.csv", "bundgaard_2003.csv", 
        "chacoff_2011.csv", "dicks_2002_1.csv", "dupont_2009_denmark.csv",
        "elberling_1999.csv", "elberling_unpublished_data.csv","fang_huang_2008.csv",
        "Inoue_1990.csv", "inouye_1988.csv", "kaiser_bunbury_2010_1.csv",
        "kato_2000.csv", "kevan_1970.csv", "lundgren_2005.csv", "Mauritius_valerie_unpublished_data.csv",
        "mcmullen_1993.csv", "primack_1983_arthurs_pass.csv", "primack_1983_cass.csv", 
        "primack_1983_craigieburn.csv", "ramirez_1989.csv", "ramirez_1992.csv",
        "robertson_1929.csv", "small_1976.csv", "souza_2018_chaco.csv", "traveset_2013.csv")

data <- data.frame(longitude, latitude, locality, id)


