#####################################
###
#### MODELS METRICS ~ COMPATIBILITY + FLOWER SIZE
###
#####################################

library(GLMMadaptive)
library(emmeans)
library(multcomp)
library(ggplot2)
library(ds4psy)
library(dplyr)
library(ggeffects)
#load data
all_df <- readRDS("Data/RData/network_metrics_flower_size_and_shape.RData")
colnames(all_df)[20]<-"Id"
all_df <- as.data.frame(all_df) 
all_df$Visits <- as.integer(all_df$Visits)

#Convert all NA'S to same type of NA's
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
all_df$Compatibility <- make.true.NA(all_df$Compatibility)
all_df <- all_df[complete.cases(all_df$Compatibility),]
colnames(all_df) <- make.unique(names(all_df))
all_df$Id <- as.factor(all_df$Id)
all_df$Compatibility <- as.factor(all_df$Compatibility)
all_df$Visits <- as.integer(all_df$Visits)
all_df$Corolla_diameter_mean <- as.integer(all_df$Corolla_diameter_mean)


#Funtion to check goodness of fit from https://drizopoulos.github.io/GLMMadaptive/articles/Goodness_of_Fit.html
resids_plot <- function (object, y, nsim = 1000,
                         type = c("subject_specific", "mean_subject"),
                         integerResponse = NULL) {
  if (!inherits(object, "MixMod"))
    stop("this function works for 'MixMod' objects.\n")
  type <- match.arg(type)
  if (is.null(integerResponse)) {
    integer_families <- c("binomial", "poisson", "negative binomial",
                          "zero-inflated poisson", "zero-inflated negative binomial", 
                          "hurdle poisson", "hurdle negative binomial")
    numeric_families <- c("hurdle log-normal", "beta", "hurdle beta")
    if (object$family$family %in% integer_families) {
      integerResponse <- TRUE
    } else if (object$family$family %in% numeric_families) {
      integerResponse <- FALSE
    } else {
      stop("non build-in family object; you need to specify the 'integerResponse',\n",
           "\targument indicating whether the outcome variable is integer or not.\n")
    }
  }
  sims <- simulate(object, nsim = nsim, type = type)
  fits <- fitted(object, type = type)
  dharmaRes <- DHARMa::createDHARMa(simulatedResponse = sims, observedResponse = y, 
                                    fittedPredictedResponse = fits, 
                                    integerResponse = integerResponse)
  DHARMa:::plot.DHARMa(dharmaRes, quantreg = FALSE)
}


#####################################
###
#### MODEL 1 VISITS ~ SELFING
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

model_1 <- mixed_model(Visits ~ Compatibility + Corolla_diameter_mean, random = ~ 1 | Id, data = all_df,
                       family = negative.binomial())

summary(model_1)

resids_plot(model_1, all_df$Visits)

library(ggeffects)
a <- ggpredict(model_1, terms = c("Corolla_diameter_mean", "Compatibility"))
plot(a)


mydf <- ggpredict(model_1, terms = c("Corolla_diameter_mean", "Compatibility"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
  geom_line( alpha = 1)+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)






model_1 <- mixed_model(Visits ~ Compatibility + Corolla_diameter_mean + Flower_morphology, random = ~ 1 | Id, data = all_df,
                       family = negative.binomial())

mydf <- ggpredict(model_1, terms = c("Corolla_diameter_mean","Flower_morphology"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
  geom_line( alpha = 1)+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)






#####################################
###
#### MODEL 2 d ~ SELFING
###
#####################################
detach(GLMMadaptive)
library(lme4)
library(DHARMa)
library(emmeans)
library(multcomp)
hist(all_df$d)
model_2 <- lmer(d ~ Compatibility + Corolla_diameter_mean + (1|Id) , data = all_df)
fit<- simulateResiduals(fittedModel = model_2, plot = T)


mydf <- ggpredict(model_2, terms = c("Corolla_diameter_mean", "Compatibility"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
  geom_line( alpha = 1, fill=c("blue", "red", "green"))+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)

#####################################
###
#### MODEL 3 Degree ~ SELFING
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

model_3 <- mixed_model(degree ~ Compatibility + Corolla_diameter_mean, random = ~ 1 | Id, data = all_df,
                       family = negative.binomial())

resids_plot(model_3, all_df$degree)

mydf <- ggpredict(model_3, terms = c("Corolla_diameter_mean", "Compatibility"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
  geom_line( alpha = 1, fill=c("blue", "red", "green"))+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)


#####################################
###
#### MODEL 4 Normalised degree  ~ SELFING
###
#####################################


model_4 <- mixed_model(normalised.degree ~ Compatibility + Corolla_diameter_mean, random = ~ 1 | Id, data = all_df,
                       family = beta.fam(), max_phis_value=0)


resids_plot(model_4, all_df$normalised.degree)
#After trying with A Gaussian distribution and a Poisson, this is the family that best fit

summary(model_1)

mydf <- ggpredict(model_4, terms = c("Corolla_diameter_mean", "Compatibility"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) + xlab("Corolla diameter") +
  geom_line( alpha = 1, fill=c("blue", "red", "green"))+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)


#####################################
###
#### MODEL 5 Closeness ~ SELFING
###
#####################################


library(lme4)
library(DHARMa)
library(emmeans)
library(multcomp)

hist(all_df$closeness)
model_5 <- lmer(closeness ~ Compatibility + Corolla_diameter_mean + (1|Id) , data = all_df)
fit<- simulateResiduals(fittedModel = model_5, plot = T)
#Not too bad, there is quantile deviation but all the rest looks ok

mydf <- ggpredict(model_5, terms = c("Corolla_diameter_mean", "Compatibility"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) + xlab("Corolla diameter") +
  geom_line( alpha = 1, fill=c("blue", "red", "green"))+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)


#####################################
###
#### MODEL 6 betweenness ~ COMPATIBILITY
###
#####################################

model_6 <- mixed_model(betweenness ~ Compatibility + Corolla_diameter_mean, random = ~ 1 | Id, data = all_df, 
                       family = hurdle.lognormal(), n_phis = 1,
                       zi_fixed = ~ 1)

resids_plot(model_6, all_df$betweenness)
#Looks good! Despite how ugly is these data... 

mydf <- ggpredict(model_6, terms = c("Corolla_diameter_mean", "Compatibility"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) + xlab("Corolla diameter") +
  geom_line( alpha = 1, fill=c("blue", "red", "green"))+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)



