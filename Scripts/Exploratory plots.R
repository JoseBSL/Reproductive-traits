#Processing data


#Step 1 Combine dataframe with traits and networks

#Here I'm going to read all the networks, create a list of spp
#and associate each spp to a network id

#load libraries
library(dplyr)
library(readxl)
library(tidyverse)
library(ggplot2)

#To read all the csv's I setwd on the specific folder

#load data
setwd("~/Reproductive Traits/")
data <- read_excel("data/data_traits.xlsx",na="")

data$corolla_diameter_mean_mm <- as.numeric(data$corolla_diameter_mean_mm)
str(data$corolla_diameter_mean_mm)
data$plant_height_mean_m <- as.numeric(data$plant_height_mean_m)

data$corolla_length_mean_mm <- as.numeric(data$corolla_length_mean_mm)


herbs <- subset(data, life_form=="herb")
data$`autonomous_selfing_level (none/low/medium/high)`

selfing <- subset(data, `autonomous_selfing_level (none/low/medium/high)`=="high"|
`autonomous_selfing_level (none/low/medium/high)`=="medium"|
  `autonomous_selfing_level (none/low/medium/high)`=="low"|
  `autonomous_selfing_level (none/low/medium/high)`=="none")

life_form <- subset(data, life_form=="herb"|
life_form=="tree"|life_form=="shrub")

life_span <- subset(data, lifespan=="annual"|
                      lifespan=="perennial")


ggplot(selfing, aes(`autonomous_selfing_level (none/low/medium/high)`, corolla_diameter_mean_mm)) + 
  geom_boxplot() + geom_jitter(width = 0.05) +ylab("Corolla diameter") + xlab("Selfing level")+
  theme_minimal()

ggplot(selfing, aes(`autonomous_selfing_level (none/low/medium/high)`, corolla_length_mean_mm)) + 
  geom_boxplot() + geom_jitter(width = 0.05) +ylab("Corolla length") + xlab("Selfing level")+
  theme_minimal()

ggplot(life_form, aes(life_form, corolla_length_mean_mm)) + 
  geom_boxplot() + geom_jitter(width = 0.05) +ylab("Corolla length") + xlab("Life form")+
  theme_minimal()

ggplot(life_span, aes(lifespan, corolla_diameter_mean_mm)) + 
  geom_boxplot() + geom_jitter(width = 0.05) +ylab("Corolla length") + xlab("Selfing level")+
  theme_minimal()

data$ClimateZ

ggplot(data, aes(ClimateZ, corolla_diameter_mean_mm)) + 
  geom_boxplot() + geom_jitter(width = 0.05) +ylab("Corolla length") + xlab("Selfing level")+
  theme_minimal()

ggplot(data, aes(ClimateZ, corolla_length_mean_mm)) + 
  geom_boxplot() + geom_jitter(width = 0.05) +ylab("Corolla length") + xlab("Selfing level")+
  theme_minimal()
data$plant_height_mean_m

ggplot(herbs, aes(ClimateZ, plant_height_mean_m)) + 
  geom_boxplot() + geom_jitter(width = 0.05) +ylab("Corolla length") + xlab("Selfing level")+
  theme_minimal()
