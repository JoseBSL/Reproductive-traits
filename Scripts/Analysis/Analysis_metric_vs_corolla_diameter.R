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
all_df$Visits <- as.numeric(all_df$Visits)

#Convert all NA'S to same type of NA's
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
all_df$Corolla_diameter_mean <- make.true.NA(all_df$Corolla_diameter_mean)
all_df <- all_df[complete.cases(all_df$Corolla_diameter_mean),]
colnames(all_df) <- make.unique(names(all_df))
all_df$Id <- as.factor(all_df$Id)
all_df$Corolla_diameter_mean <- as.numeric(all_df$Corolla_diameter_mean)
all_df$Visits <- as.numeric(all_df$Visits)


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
#### MODEL 1 VISITS ~ STYLE LENGTH
###
#####################################


#Need to detach libraries before using mixed_effects from GLMMadaptive
detach("package:lme4", unload=TRUE)
detach("package:emmeans", unload=TRUE)
detach("package:multcomp", unload=TRUE)
detach("package:DHARMa", unload=TRUE)
detach("package:ciTools", unload=TRUE)
detach("package:TH.data", unload=TRUE)
detach("package:MASS", unload=TRUE)

model_1 <- mixed_model(Visits ~ Corolla_diameter_mean, random = ~ 1 | Id, data = all_df,
                       family = negative.binomial())

resids_plot(model_1, all_df$Visits)

all_tf<- all_df
all_tf <-predict(model_1, type="subject_specific",newdata = all_tf,return_newdata = TRUE)


mydf <- ggpredict(model_1, terms = c("Corolla_diameter_mean"))
ggplot(mydf, aes(x = x, y = predicted)) + theme_ds4psy() + theme(legend.position = "none") + xlab("Corolla diameter (mm)")+ ylab("Predicted visits")+geom_point(data= all_tf, aes(x =Corolla_diameter_mean , y = pred))+
  geom_line( alpha = 1)+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)


saveRDS(model_1, "Data/RData/model_1_visits_corolla_diameter.rds")
saveRDS(all_tf, "Data/RData/data_plot_visits_corolla_diameter.rds")

#####################################
###
#### MODEL 2 d ~ PLANT HEIGHT
###
#####################################
detach(GLMMadaptive)
library(lme4)
library(DHARMa)
library(emmeans)
library(multcomp)
hist(all_df$d)
model_2 <- lmer(d ~Corolla_diameter_mean + (1|Id) , data = all_df)
summary(model_2)
fit<- simulateResiduals(fittedModel = model_2, plot = T)

pred <-predict(model_2)
all_tf <- all_df
all_tf$pred <- pred


mydf <- ggpredict(model_2, terms = c("Corolla_diameter_mean"))
ggplot(mydf, aes(x = x, y = predicted)) + theme_ds4psy() + theme(legend.position = "none") + xlab("Corolla diameter")+ ylab("Predicted d")+geom_point(data= all_tf, aes(x =Corolla_diameter_mean , y = pred))+
  geom_line( alpha = 1, fill=c("blue", "red", "green"))+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)


saveRDS(all_tf, "Data/RData/data_plot_visits_corolla_diameter_2.rds")
saveRDS(model_2, "Data/RData/model_2_visitscorolla_diameter_2.rds")
saveRDS(pred, "Data/RData/pred_plot_visits_corolla_diameter_2.rds")


#####################################
###
#### MODEL 3 Degree ~ PLANT HEIGHT
###
#####################################

#Need to detach libraries before using mixed_effects from GLMMadaptive
detach("package:lme4", unload=TRUE)
detach("package:emmeans", unload=TRUE)
detach("package:multcomp", unload=TRUE)
detach("package:DHARMa", unload=TRUE)
detach("package:ciTools", unload=TRUE)
detach("package:TH.data", unload=TRUE)
detach("package:MASS", unload=TRUE)

model_3 <- mixed_model(degree ~ Corolla_diameter_mean, random = ~ 1 | Id, data = all_df,
                       family = negative.binomial())


resids_plot(model_3, all_df$degree)

all_tf<- all_df
all_tf <-predict(model_3, type="subject_specific",newdata = all_tf,return_newdata = TRUE)

mydf <- ggpredict(model_3, terms = c("Corolla_diameter_mean"))
ggplot(mydf, aes(x = x, y = predicted)) + theme_ds4psy() + theme(legend.position = "none") + xlab("Corolla diameter")+ ylab("Predicted degree")+ geom_point(data= all_tf, aes(x =Corolla_diameter_mean , y = pred))+
  geom_line( alpha = 1, fill=c("blue", "red", "green"))+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)


saveRDS(all_tf, "Data/RData/data_plot_visits_corolla_diameter_3.rds")
saveRDS(model_3, "Data/RData/model_3_visits_corolla_diameter_3.rds")
saveRDS(pred, "Data/RData/pred_plot_visits_corolla_diameter_3.rds")

#####################################
###
#### MODEL 4 Normalised degree  ~ PLANT HEIGHT
###
#####################################


model_4 <- mixed_model(normalised.degree ~ Corolla_diameter_mean, random = ~ 1 | Id, data = all_df,
                       family = beta.fam(), max_phis_value=0)


resids_plot(model_4, all_df$normalised.degree)
#After trying with A Gaussian distribution and a Poisson, this is the family that best fit

all_tf<- all_df
all_tf <-predict(model_4, type="subject_specific",newdata = all_tf,return_newdata = TRUE)


mydf <- ggpredict(model_4, terms = c("Corolla_diameter_mean"))
ggplot(mydf, aes(x = x, y = predicted)) + xlab("Corolla diameter (mm)") + theme_ds4psy() + theme(legend.position = "none") + ylab("Predicted normalise degree") +geom_point(data= all_tf, aes(x =Corolla_diameter_mean , y = pred))+
  geom_line( alpha = 1, fill=c("blue", "red", "green"))+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)

saveRDS(all_tf, "Data/RData/data_plot_visits_corolla_diameter_4.rds")
saveRDS(model_4, "Data/RData/model_4_visits_corolla_diameter_4.rds")
saveRDS(pred, "Data/RData/pred_plot_visits_corolla_diameter_4.rds")
