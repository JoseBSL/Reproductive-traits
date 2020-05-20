#####################################
###
#### MODELS METRICS ~ OVULE NUMBER
#####################################

library(GLMMadaptive)
library(emmeans)
library(multcomp)
library(ggplot2)
library(ds4psy)
library(dplyr)
library(ggeffects)
#load data
all_df <- readRDS("Data/RData/network_metrics_main_traits.RData")
colnames(all_df)[27]<-"Id"
all_df <- as.data.frame(all_df) 
all_df$Visits <- as.integer(all_df$Visits)

#Convert all NA'S to same type of NA's
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
all_df$style_length_mm <- make.true.NA(all_df$style_length_mm)
all_df <- all_df[complete.cases(all_df$style_length_mm),]
colnames(all_df) <- make.unique(names(all_df))
all_df$Id <- as.factor(all_df$Id)
all_df$style_length_mm <- as.integer(all_df$style_length_mm)
all_df$Visits <- as.integer(all_df$Visits)

#####################################
###
#### MODEL 1 VISITS ~ STYLE LENGTH
###
#####################################


#Need to detach libraries before using mixed_effects from GLMMadaptive
detach("package:lme4", unload=TRUE)
detach("package:emmeans", unload=TRUE)
detach("package:multcomp", unload=TRUE)
detach("package:DHARMa", unload=TRUE)
detach("package:ds4psy", unload=TRUE)
detach("package:ciTools", unload=TRUE)
detach("package:TH.data", unload=TRUE)
detach("package:MASS", unload=TRUE)

model_1 <- mixed_model(Visits ~ style_length_mm, random = ~ 1 | Id, data = all_df,
                       family = negative.binomial())

resids_plot(model_1, all_df$Visits)

mydf <- ggpredict(model_1, terms = c("style_length_mm"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) + xlab("Style length")+ ylab("Predicted visits")+
  geom_line( alpha = 1)+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)


#####################################
###
#### MODEL 2 d ~ STYLE LENGTH
###
#####################################
detach(GLMMadaptive)
library(lme4)
library(DHARMa)
library(emmeans)
library(multcomp)
hist(all_df$d)
model_2 <- lmer(d ~style_length_mm + (1|Id) , data = all_df)
summary(model_2)
fit<- simulateResiduals(fittedModel = model_2, plot = T)

mydf <- ggpredict(model_2, terms = c("style_length_mm"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) + xlab("Style length")+ ylab("Predicted d")+
  geom_line( alpha = 1, fill=c("blue", "red", "green"))+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)


#####################################
###
#### MODEL 3 Degree ~ STYLE LENGTH
###
#####################################

#Need to detach libraries before using mixed_effects from GLMMadaptive
detach("package:lme4", unload=TRUE)
detach("package:emmeans", unload=TRUE)
detach("package:multcomp", unload=TRUE)
detach("package:DHARMa", unload=TRUE)
detach("package:ds4psy", unload=TRUE)
detach("package:ciTools", unload=TRUE)
detach("package:TH.data", unload=TRUE)
detach("package:MASS", unload=TRUE)

model_3 <- mixed_model(degree ~ style_length_mm, random = ~ 1 | Id, data = all_df,
                       family = negative.binomial())


resids_plot(model_3, all_df$degree)

mydf <- ggpredict(model_3, terms = c("style_length_mm"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) + xlab("Style length")+ ylab("Predicted degree")+
  geom_line( alpha = 1, fill=c("blue", "red", "green"))+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)

#####################################
###
#### MODEL 4 Normalised degree  ~ STYLE LENGTH
###
#####################################


model_4 <- mixed_model(normalised.degree ~ style_length_mm, random = ~ 1 | Id, data = all_df,
                       family = beta.fam(), max_phis_value=0)


resids_plot(model_4, all_df$normalised.degree)
#After trying with A Gaussian distribution and a Poisson, this is the family that best fit

mydf <- ggpredict(model_4, terms = c("style_length_mm"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) + xlab("Style length") + ylab("Predicted normalise degree") +
  geom_line( alpha = 1, fill=c("blue", "red", "green"))+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)



#####################################
###
#### MODEL 5 Closeness ~ STYLE LENGTH
###
#####################################


library(lme4)
library(DHARMa)
library(emmeans)
library(multcomp)

hist(all_df$closeness)
model_5 <- lmer(closeness ~ style_length_mm + (1|Id) , data = all_df)
fit<- simulateResiduals(fittedModel = model_5, plot = T)


mydf <- ggpredict(model_5, terms = c("style_length_mm"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) + xlab("Style length") + ylab("Predicted closeness") +
  geom_line( alpha = 1, fill=c("blue", "red", "green"))+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)

#####################################
###
#### MODEL 6 betweenness ~ OVULE NUMBER
###
#####################################

model_6 <- mixed_model(betweenness ~ style_length_mm, random = ~ 1 | Id, data = all_df, 
                       family = hurdle.lognormal(), n_phis = 1,
                       zi_fixed = ~ 1)

resids_plot(model_6, all_df$betweenness)
#Looks good! Despite how ugly is these data... 

mydf <- ggpredict(model_6, terms = c("style_length_mm"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) + xlab("Corolla diameter") +
  geom_line( alpha = 1, fill=c("blue", "red", "green"))+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)



