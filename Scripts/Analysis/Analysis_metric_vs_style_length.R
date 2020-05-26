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
all_df$style_length_mm <- make.true.NA(all_df$style_length_mm)
all_df <- all_df[complete.cases(all_df$style_length_mm),]
colnames(all_df) <- make.unique(names(all_df))
all_df$Id <- as.factor(all_df$Id)
all_df$style_length_mm <- as.numeric(all_df$style_length_mm)
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

model_1 <- mixed_model(Visits ~ style_length_mm, random = ~ 1 | Id, data = all_df,
                       family = negative.binomial())

resids_plot(model_1, all_df$Visits)

all_tf<- all_df
all_tf <-predict(model_1, type="subject_specific",newdata = all_tf,return_newdata = TRUE)


mydf <- ggpredict(model_1, terms = c("style_length_mm"))
ggplot(mydf, aes(x = x, y = predicted)) + theme_ds4psy() + theme(legend.position = "none") + xlab("Style length")+ ylab("Predicted visits")+geom_point(data= all_tf, aes(x =style_length_mm , y = pred))+
  geom_line( alpha = 1)+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)




saveRDS(model_1, "Data/RData/model_1_visits_style_length.rds")
saveRDS(all_tf, "Data/RData/data_plot_visits_style_length.rds")



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
#library for r2
library(MuMIn)
r.squaredGLMM(model_2)

pred <-predict(model_2)

all_tf <- all_df
all_tf$pred <- pred

mydf <- ggpredict(model_2, terms = c("style_length_mm"))
ggplot(mydf, aes(x = x, y = predicted)) + xlab("Style length")+ theme_ds4psy() + theme(legend.position = "none") + ylab("Predicted visits")+geom_point(data= all_tf, aes(x =style_length_mm , y = pred))+
  geom_line( alpha = 1)+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)


saveRDS(all_tf, "Data/RData/data_plot_visits_style_length_2.rds")
saveRDS(model_2, "Data/RData/model_2_visits_style_length_2.rds")
saveRDS(pred, "Data/RData/pred_plot_visits_style_length_2.rds")

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
detach("package:ciTools", unload=TRUE)
detach("package:TH.data", unload=TRUE)
detach("package:MASS", unload=TRUE)

model_3 <- mixed_model(degree ~ style_length_mm, random = ~ 1 | Id, data = all_df,
                       family = negative.binomial())


resids_plot(model_3, all_df$degree)

mydf <- ggpredict(model_3, terms = c("style_length_mm"))
all_tf<- all_df
all_tf <-predict(model_3, type="subject_specific",newdata = all_tf,return_newdata = TRUE)

ggplot(mydf, aes(x = x, y = predicted)) + xlab("Style length") + theme_ds4psy() + theme(legend.position = "none") + ylab("Predicted degree") + geom_point(data= all_tf, aes(x =style_length_mm , y = pred))+
  geom_line( alpha = 1)+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)

saveRDS(all_tf, "Data/RData/data_plot_visits_style_length_3.rds")
saveRDS(model_3, "Data/RData/model_3_visits_style_length_3.rds")
saveRDS(pred, "Data/RData/pred_plot_visits_style_length_3.rds")


#####################################
###
#### MODEL 4 Normalised degree  ~ STYLE LENGTH
###
#####################################


model_4 <- mixed_model(normalised.degree ~ style_length_mm, random = ~ 1 | Id, data = all_df,
                       family = beta.fam(), max_phis_value=0)


resids_plot(model_4, all_df$normalised.degree)
#After trying with A Gaussian distribution and a Poisson, this is the family that best fit

all_tf<- all_df
all_tf <-predict(model_4, type="subject_specific",newdata = all_tf,return_newdata = TRUE)

mydf <- ggpredict(model_4, terms = c("style_length_mm"))
ggplot(mydf, aes(x = x, y = predicted)) + xlab("Style length (mm)") + theme_ds4psy() + theme(legend.position = "none") + ylab("Predicted normalise degree") +geom_point(data= all_tf, aes(x =style_length_mm , y = pred))+
  geom_line( alpha = 1, fill=c("blue", "red", "green"))+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)

saveRDS(all_tf, "Data/RData/data_plot_visits_style_length_4.rds")
saveRDS(model_4, "Data/RData/model_4_visits_style_length_4.rds")
saveRDS(pred, "Data/RData/pred_plot_visits_style_length_4.rds")



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

r2(model_5)


all_tf<- all_df
pred <-predict(model_5)
all_tf$pred


mydf <- ggpredict(model_5, terms = c("style_length_mm"))
ggplot(mydf, aes(x = x, y = predicted)) + xlab("style_length_mm diameter")  + ylab("Predicted closeness") + geom_point(data= all_tf, aes(x =style_length_mm , y = pred))+
  geom_line( alpha = 1)+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA)


#####################################
###
#### MODEL 6 betweenness ~ STYLE LENGTH
###
#####################################

model_6 <- mixed_model(betweenness ~ style_length_mm, random = ~ 1 | Id, data = all_df, 
                       family = zi.poisson(), zi_fixed = ~ 1, iter_EM = 0)

resids_plot(model_6, all_df$betweenness)


r2(model_6)

#Looks good! Despite how ugly is these data... 
all_tf<- all_df
all_tf <-predict(model_6, type="subject_specific",newdata = all_tf,return_newdata = TRUE)



mydf <- ggpredict(model_6, terms = c("style_length_mm"))
ggplot(mydf, aes(x = x, y = predicted)) + xlab("Corolla diameter") + geom_point(data= all_tf, aes(x =style_length_mm , y = pred))+
  geom_line( alpha = 1)+ geom_ribbon(aes(x = x,ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, colour = NA) + ylab("Predicted betweenness")




