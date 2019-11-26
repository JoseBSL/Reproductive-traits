
#Load library
library(readxl)
library(kgc)
library(ggplot2)
data <- read_excel("data/Traits_Data.xlsx")

data <- data.frame(data,
                   rndCoord.lon = RoundCoordinates(data$Longitude),
                   rndCoord.lat = RoundCoordinates(data$Latitude))

data <- data.frame(data,ClimateZ=LookupCZ(data))

LookupCZ(data, res = "course", rc = FALSE)

levels(data$ClimateZ)

write.csv(data, "data/data_traits.csv",na="")
