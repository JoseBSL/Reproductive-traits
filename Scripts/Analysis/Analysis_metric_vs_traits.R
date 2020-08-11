library(GLMMadaptive)
library(emmeans)
library(multcomp)
library(ggplot2)
library(ds4psy)
library(dplyr)
library(ggeffects)
library(performance)

#load data
all_df <- readRDS("Data/RData/network_metrics_main_traits.RData")
colnames(all_df)[27]<-"Id"
all_df <- as.data.frame(all_df) 
all_df$Visits <- as.integer(all_df$Visits)

#Convert all NA'S to same type of NA's
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
all_df$Compatibility <- make.true.NA(all_df$Compatibility)
all_df <- all_df[complete.cases(all_df$Compatibility),]
colnames(all_df) <- make.unique(names(all_df))

all_df$Visits <- as.integer(all_df$Visits)
all_df$ovules_mean <- make.true.NA(all_df$ovules_mean)
all_df <- all_df[complete.cases(all_df$ovules_mean),]
all_df$ovules_mean <- as.integer(all_df$ovules_mean)
all_df$Corolla_diameter_mean <- make.true.NA(all_df$Corolla_diameter_mean)
all_df <- all_df[complete.cases(all_df$Corolla_diameter_mean),]
all_df$Corolla_diameter_mean <- as.integer(all_df$Corolla_diameter_mean)
all_df$style_length_mm <- make.true.NA(all_df$style_length_mm)
all_df <- all_df[complete.cases(all_df$style_length_mm),]
all_df$style_length_mm <- as.integer(all_df$style_length_mm)
str(all_df)
all_df$Id <- as.factor(all_df$Id)
all_df$Compatibility <- as.factor(all_df$Compatibility)  
all_df$plant_height_mean_m <- as.integer(all_df$plant_height_mean_m)


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
#### MODEL 1 VISITS ~ .
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

all_df$style_length_mm <- scale(all_df$style_length_mm)
all_df$Corolla_diameter_mean <- scale(all_df$Corolla_diameter_mean)
all_df$ovules_mean <- scale(all_df$ovules_mean)



model_1 <- mixed_model(Visits ~ Compatibility  + Corolla_diameter_mean + ovules_mean, random = ~ 1 | Id, data = all_df,
                       family = negative.binomial())


resids_plot(model_1, all_df$Visits)
#Residuals looks good
summary(model_1)
VIF(model_1)
#Values over 5/10 are possible problematic variables for the model, we have one of 5.41
#One value over 5, maybe remove one variable, we don't gain too much with it

r2(model_1)
#With the fixed and random effect the model "explains" 90% of the variance
#Just the fixed effects (marginal) 74%


#####################################
###
#### MODEL 2 d ~ .
###
#####################################
detach(GLMMadaptive)
library(lme4)
library(DHARMa)
library(emmeans)
library(multcomp)
hist(all_df$d)
model_2 <- lmer(d ~ Compatibility  +Corolla_diameter_mean + ovules_mean + (1|Id) , data = all_df)
summary(model_2)
fit<- simulateResiduals(fittedModel = model_2, plot = T)

#library for r2
library(MuMIn)
r.squaredGLMM(model_2)

#####################################
###
#### MODEL 3 Degree ~ .
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

model_3 <- mixed_model(degree ~ Compatibility  +Corolla_diameter_mean + ovules_mean, random = ~ 1 | Id, data = all_df,
                       family = negative.binomial())


resids_plot(model_3, all_df$degree)
#Looks good

r2(model_3)
#Nothing here

#####################################
###
#### MODEL 4 Normalised degree  ~ .
###
#####################################


model_4 <- mixed_model(normalised.degree ~ Compatibility  +Corolla_diameter_mean + ovules_mean, random = ~ 1 | Id, data = all_df,
                       family = beta.fam(), max_phis_value=0)


resids_plot(model_4, all_df$normalised.degree)

r2(model_4)
#Nothing here

#####################################
###
#### MODEL 5 Closeness ~ .
###
#####################################

library(lme4)
library(DHARMa)
library(emmeans)
library(multcomp)

hist(all_df$closeness)
model_5 <- lmer(closeness ~ Compatibility  +Corolla_diameter_mean + ovules_mean + (1|Id) , data = all_df)
fit<- simulateResiduals(fittedModel = model_5, plot = T)


r.squaredGLMM(model_5)


#####################################
###
#### MODEL 6 betweenness ~ .
###
#####################################

model_6 <- mixed_model(betweenness ~ Compatibility  +Corolla_diameter_mean + ovules_mean, random = ~ 1 | Id, data = all_df, 
                       family = hurdle.lognormal(), n_phis = 1,
                       zi_fixed = ~ 1)

resids_plot(model_6, all_df$betweenness)

r2(model_6)

