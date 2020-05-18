#####################################
###
#### MODELS METRICS ~ SELFING LEVEL
###
#####################################

library(GLMMadaptive)
library(emmeans)
library(multcomp)
library(ggplot2)
library(ds4psy)
library(dplyr)

#load data
all_df <- readRDS("Data/RData/network_metrics.RData")
colnames(all_df)[17]<-"Id"
all_df <- as.data.frame(all_df) 
all_df$Visits <- as.integer(all_df$Visits)

#Convert all NA'S to same type of NA's
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
all_df$Autonomous_selfing_level <- make.true.NA(all_df$Autonomous_selfing_level)
all_df <- all_df[complete.cases(all_df$Autonomous_selfing_level),]
colnames(all_df) <- make.unique(names(all_df))
all_df$Id <- as.factor(all_df$Id)
all_df$Autonomous_selfing_level <- as.factor(all_df$Autonomous_selfing_level)

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

model_1 <- mixed_model(Visits ~ Autonomous_selfing_level, random = ~ 1 | Id, data = all_df,
                         family = negative.binomial())

resids_plot(model_1, all_df$Visits)
#looks good

library(emmeans)
library(multcomp)
library(forecast)
library(tidyverse)
#Analyse data and plot it
lsm = lsmeans(model_1, "Autonomous_selfing_level", type = "response")
pairs(lsm)
summary(model_1.1)
CLD <- cld(lsm,alpha=0.05,adjust="tukey")

#Convert to factor to set plotting order
CLD <- CLD %>%
mutate(Autonomous_selfing_level = fct_relevel(Autonomous_selfing_level, levels=c("high", "medium", "low", "none")))
CLD$Autonomous_selfing_level <- as.factor(CLD$Autonomous_selfing_level)

ggplot(CLD,aes(x= Autonomous_selfing_level,y= lsmean,label = .group)) +theme()+geom_point(size   = 4) +ggtitle("Lsm difference between groups")+
  geom_errorbar(aes(ymin  =  asymp.LCL,ymax  =  asymp.UCL), width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Selfing level")

#Plot predicted values and residuals

#Predict values
all_tf <- all_df
preds <- predict(model_1, newdata = all_tf,
                 type = "subject_specific",
                 se.fit = TRUE, return_newdata = TRUE)

#Function to calculate SEM
std <- function(x) sd(x)/sqrt(length(x))

i <- levels(all_df$Autonomous_selfing_level)
mean_ci <- NULL
i <- c("high", "medium", "low", "none")
for (i in levels(preds$Autonomous_selfing_level)){
  
  mean <- mean(preds[preds$Autonomous_selfing_level == i,18])
  
  upper <- mean(preds[preds$Autonomous_selfing_level == i,18]) + qt(0.975, df=length(preds[preds$Autonomous_selfing_level == i,18])) * 
    std(preds[preds$Autonomous_selfing_level == i,18]) 
  
  lower <- mean(preds[preds$Autonomous_selfing_level == i,18]) - qt(0.975, df=length(preds[preds$Autonomous_selfing_level == i,18])) * 
    std(preds[preds$Autonomous_selfing_level == i,18]) 
  
  Autonomous_selfing_level <- i
  
  #Create data frame with mean and CI of predicted values per variable
  mean_ci_1 <- data.frame(mean,upper,lower, Autonomous_selfing_level)
  
  mean_ci <- rbind(mean_ci, mean_ci_1)
  
  print(mean_ci)
}

preds <- merge(preds,mean_ci, by="Autonomous_selfing_level")


preds <- preds %>%
  mutate(Autonomous_selfing_level = fct_relevel(Autonomous_selfing_level, levels=c("high", "medium", "low", "none")))
preds$Autonomous_selfing_level <- as.factor(preds$Autonomous_selfing_level)
#Plot predicted values and mean predicted value with CI
ggplot(preds,aes(x= Autonomous_selfing_level,y= pred, color=Autonomous_selfing_level)) +theme()+
  geom_point(Autonomous_selfing_level  = 1,size   = 4, alpha=0.2, colour="lightblue") + geom_point(aes(x= Autonomous_selfing_level,y= mean, color=Autonomous_selfing_level),Autonomous_selfing_level  = 2,size   = 4,colour="steelblue", alpha=1) +
  geom_errorbar(aes(ymin  =  lower,ymax  =  upper),width =  0.2,size =  0.7, colour="steelblue")


#####################################
###
#### MODEL 2 d ~ SELFING
###
#####################################


library(lme4)
library(DHARMa)
library(emmeans)
library(multcomp)
hist(all_df$d)
model_2 <- lmer(d ~ Autonomous_selfing_level + (1|Id) , data = all_df)
fit<- simulateResiduals(fittedModel = model_2, plot = T)
plot(fit)
#Looks good


lsm = lsmeans(model_2, "Autonomous_selfing_level", type = "response")
pairs(lsm)
CLD <- cld(lsm,alpha=0.05,adjust="tukey")
#Convert to factor to set plotting order
CLD <- CLD %>%
  mutate(Autonomous_selfing_level = fct_relevel(Autonomous_selfing_level, levels=c("high", "medium", "low", "none")))
CLD$Autonomous_selfing_level <- as.factor(CLD$Autonomous_selfing_level)

ggplot(CLD,aes(x= Autonomous_selfing_level,y= lsmean,
               label = .group)) +theme()+
  geom_point(Compatibility  = 15,size   = 4) +ggtitle("Lsm difference between groups")+
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Selfing level")


#Plot predicted values and residuals

#Predict values
all_tf <- all_df
pred <- predict(model_2, type = "response")

all_tf <- cbind(all_tf,pred)
mean_ci <- NULL

i <- c("high", "medium", "low", "none")
for (i in levels(all_tf$Autonomous_selfing_level)){
  
  mean <- mean(all_tf[all_tf$Autonomous_selfing_level == i,18])
  
  upper <- mean(all_tf[all_tf$Autonomous_selfing_level == i,18]) + qt(0.975, df=length(all_tf[all_tf$Autonomous_selfing_level == i,18])) * 
    std(all_tf[all_tf$Autonomous_selfing_level == i,18]) 
  
  lower <- mean(all_tf[all_tf$Autonomous_selfing_level == i,18]) - qt(0.975, df=length(all_tf[all_tf$Autonomous_selfing_level == i,18])) * 
    std(all_tf[all_tf$Autonomous_selfing_level == i,18]) 
  
  Autonomous_selfing_level <- i
  
  mean_ci_1 <- data.frame(mean,upper,lower, Autonomous_selfing_level)
  
  mean_ci <- rbind(mean_ci, mean_ci_1)
  
  print(mean_ci)
}

all_tf <- merge(all_tf,mean_ci, by="Autonomous_selfing_level")

all_tf <- all_tf %>%
  mutate(Autonomous_selfing_level = fct_relevel(Autonomous_selfing_level, levels=c("high", "medium", "low", "none")))
all_tf$Autonomous_selfing_level <- as.factor(all_tf$Autonomous_selfing_level)

ggplot(all_tf,aes(x= Autonomous_selfing_level,y= pred, color=Autonomous_selfing_level)) +theme()+
  geom_point(Autonomous_selfing_level  = 1,size   = 4, alpha=0.2, colour="lightblue") + geom_point(aes(x= Autonomous_selfing_level,y= mean, color=Autonomous_selfing_level),Autonomous_selfing_level  = 2,size   = 4,colour="steelblue", alpha=1) +
  geom_errorbar(aes(ymin  =  lower,ymax  =  upper),width =  0.2,size =  0.7, colour="steelblue")


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

model_3 <- mixed_model(degree ~ Autonomous_selfing_level, random = ~ 1 | Id, data = all_df,
                       family = negative.binomial())
resids_plot(model_3, all_df$degree)


library(emmeans)
library(multcomp)

#Analyse data and plot it
lsm = lsmeans(model_3, "Autonomous_selfing_level", type = "response")
pairs(lsm)
CLD <- cld(lsm,alpha=0.05,adjust="tukey")



#Convert to factor to set plotting order
CLD <- CLD %>%
  mutate(Autonomous_selfing_level = fct_relevel(Autonomous_selfing_level, levels=c("high", "medium", "low", "none")))
CLD$Autonomous_selfing_level <- as.factor(CLD$Autonomous_selfing_level)

ggplot(CLD,aes(x= Autonomous_selfing_level,y= lsmean,
               label = .group)) +theme()+
  geom_point(Autonomous_selfing_level  = 15,size   = 4) +ggtitle("Lsm difference between groups")+
  geom_errorbar(aes(ymin  =  asymp.LCL,ymax  =  asymp.UCL),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Autonomous_selfing_level")

#Predict values
all_tf <- all_df
preds <- predict(model_3, newdata = all_tf,
                 type = "subject_specific",
                 se.fit = TRUE, return_newdata = TRUE)

#Function to calculate SEM
std <- function(x) sd(x)/sqrt(length(x))

i <- levels(all_df$Autonomous_selfing_level)
mean_ci <- NULL
i <- i <- c("high", "medium", "low", "none")
for (i in levels(preds$Autonomous_selfing_level)){
  
  mean <- mean(preds[preds$Autonomous_selfing_level == i,18])
  
  upper <- mean(preds[preds$Autonomous_selfing_level == i,18]) + qt(0.975, df=length(preds[preds$Autonomous_selfing_level == i,18])) * 
    std(preds[preds$Autonomous_selfing_level == i,18]) 
  
  lower <- mean(preds[preds$Autonomous_selfing_level == i,18]) - qt(0.975, df=length(preds[preds$Autonomous_selfing_level == i,18])) * 
    std(preds[preds$Autonomous_selfing_level == i,18]) 
  
  Autonomous_selfing_level <- i
  
  #Create data frame with mean and CI of predicted values per variable
  mean_ci_1 <- data.frame(mean,upper,lower, Autonomous_selfing_level)
  
  mean_ci <- rbind(mean_ci, mean_ci_1)
  
  print(mean_ci)
}

preds <- merge(preds,mean_ci, by="Autonomous_selfing_level")



preds <- preds %>%
  mutate(Autonomous_selfing_level = fct_relevel(Autonomous_selfing_level, levels=c("high", "medium", "low", "none")))
preds$Autonomous_selfing_level <- as.factor(preds$Autonomous_selfing_level)
#Plot predicted values and mean predicted value with CI
ggplot(preds,aes(x= Autonomous_selfing_level,y= pred, color=Autonomous_selfing_level)) +theme()+
  geom_point(size   = 4, alpha=0.2, colour="lightblue") + geom_point(aes(x= Autonomous_selfing_level,y= mean, color=Autonomous_selfing_level),size   = 4,colour="steelblue", alpha=1) +
  geom_errorbar(aes(ymin  =  lower,ymax  =  upper),width =  0.2,size =  0.7, colour="steelblue")

#####################################
###
#### MODEL 4 Degree ~ SELFING
###
#####################################

library(lme4)
library(DHARMa)
library(emmeans)
library(multcomp)
hist(all_df$normalised.degree)
model_4 <- lmer(normalised.degree ~ Autonomous_selfing_level + (1|Id) , data = all_df)
fit<- simulateResiduals(fittedModel = model_4, plot = T)
plot(fit)

#Need to detach libraries before using mixed_effects from GLMMadaptive
detach("package:lme4", unload=TRUE)
detach("package:emmeans", unload=TRUE)
detach("package:multcomp", unload=TRUE)
detach("package:DHARMa", unload=TRUE)
detach("package:ds4psy", unload=TRUE)
detach("package:ciTools", unload=TRUE)
detach("package:TH.data", unload=TRUE)
detach("package:MASS", unload=TRUE)

model_4 <- mixed_model(normalised.degree ~ Autonomous_selfing_level, random = ~ 1 | Id, data = all_df,
                       family = poisson())

resids_plot(model_4, all_df$normalised.degree)


library(emmeans)
library(multcomp)
library(forecast)
library(tidyverse)
#Analyse data and plot it
lsm = lsmeans(model_3, "Autonomous_selfing_level", type = "response")
pairs(lsm)
summary(model_1.1)
CLD <- cld(lsm,alpha=0.05,adjust="tukey")

#Convert to factor to set plotting order
CLD <- CLD %>%
  mutate(Autonomous_selfing_level = fct_relevel(Autonomous_selfing_level, levels=c("high", "medium", "low", "none")))
CLD$Autonomous_selfing_level <- as.factor(CLD$Autonomous_selfing_level)

ggplot(CLD,aes(x= Autonomous_selfing_level,y= lsmean,label = .group)) +theme()+geom_point(size   = 4) +ggtitle("Lsm difference between groups")+
  geom_errorbar(aes(ymin  =  asymp.LCL,ymax  =  asymp.UCL), width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Selfing level")

#Plot predicted values and residuals

#Predict values
all_tf <- all_df
preds <- predict(model_4, newdata = all_tf,
                 type = "subject_specific",
                 se.fit = TRUE, return_newdata = TRUE)

#Function to calculate SEM
std <- function(x) sd(x)/sqrt(length(x))

i <- levels(all_df$Autonomous_selfing_level)
mean_ci <- NULL
i <- c("high", "medium", "low", "none")
for (i in levels(preds$Autonomous_selfing_level)){
  
  mean <- mean(preds[preds$Autonomous_selfing_level == i,18])
  
  upper <- mean(preds[preds$Autonomous_selfing_level == i,18]) + qt(0.975, df=length(preds[preds$Autonomous_selfing_level == i,18])) * 
    std(preds[preds$Autonomous_selfing_level == i,18]) 
  
  lower <- mean(preds[preds$Autonomous_selfing_level == i,18]) - qt(0.975, df=length(preds[preds$Autonomous_selfing_level == i,18])) * 
    std(preds[preds$Autonomous_selfing_level == i,18]) 
  
  Autonomous_selfing_level <- i
  
  #Create data frame with mean and CI of predicted values per variable
  mean_ci_1 <- data.frame(mean,upper,lower, Autonomous_selfing_level)
  
  mean_ci <- rbind(mean_ci, mean_ci_1)
  
  print(mean_ci)
}

preds <- merge(preds,mean_ci, by="Autonomous_selfing_level")


preds <- preds %>%
  mutate(Autonomous_selfing_level = fct_relevel(Autonomous_selfing_level, levels=c("high", "medium", "low", "none")))
preds$Autonomous_selfing_level <- as.factor(preds$Autonomous_selfing_level)
#Plot predicted values and mean predicted value with CI
ggplot(preds,aes(x= Autonomous_selfing_level,y= pred, color=Autonomous_selfing_level)) +theme()+
  geom_point(Autonomous_selfing_level  = 1,size   = 4, alpha=0.2, colour="lightblue") + geom_point(aes(x= Autonomous_selfing_level,y= mean, color=Autonomous_selfing_level),Autonomous_selfing_level  = 2,size   = 4,colour="steelblue", alpha=1) +
  geom_errorbar(aes(ymin  =  lower,ymax  =  upper),width =  0.2,size =  0.7, colour="steelblue")


#####################################
###
#### MODEL 5 Degree ~ SELFING
###
#####################################

library(lme4)
library(DHARMa)
library(emmeans)
library(multcomp)

hist(all_df$closeness)
model_5 <- lmer(closeness ~ Autonomous_selfing_level + (1|Id) , data = all_df)
fit<- simulateResiduals(fittedModel = model_5, plot = T)
plot(fit)
#Looks good


#Check differences in d between compatibility modes
lsm = lsmeans(model_5, "Autonomous_selfing_level", type = "response")
pairs(lsm)
CLD <- cld(lsm,alpha=0.05,adjust="tukey")
#Convert to factor to set plotting order
CLD <- CLD %>%
  mutate(Autonomous_selfing_level = fct_relevel(Autonomous_selfing_level, levels=c("high", "medium", "low", "none")))
CLD$Autonomous_selfing_level <- as.factor(CLD$Autonomous_selfing_level)

ggplot(CLD,aes(x= Autonomous_selfing_level,y= lsmean,
               label = .group)) +theme()+
  geom_point(Compatibility  = 15,size   = 4) +ggtitle("Lsm difference between groups")+
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Selfing level")


#Plot predicted values and residuals

#Predict values
all_tf <- all_df
pred <- predict(model_5, type = "response")

all_tf <- cbind(all_tf,pred)


mean_ci <- NULL

i <- c("high", "medium", "low", "none")
for (i in levels(all_tf$Autonomous_selfing_level)){
  
  mean <- mean(all_tf[all_tf$Autonomous_selfing_level == i,18])
  
  upper <- mean(all_tf[all_tf$Autonomous_selfing_level == i,18]) + qt(0.975, df=length(all_tf[all_tf$Autonomous_selfing_level == i,18])) * 
    std(all_tf[all_tf$Autonomous_selfing_level == i,18]) 
  
  lower <- mean(all_tf[all_tf$Autonomous_selfing_level == i,18]) - qt(0.975, df=length(all_tf[all_tf$Autonomous_selfing_level == i,18])) * 
    std(all_tf[all_tf$Autonomous_selfing_level == i,18]) 
  
  Autonomous_selfing_level <- i
  
  mean_ci_1 <- data.frame(mean,upper,lower, Autonomous_selfing_level)
  
  mean_ci <- rbind(mean_ci, mean_ci_1)
  
  print(mean_ci)
}

all_tf <- merge(all_tf,mean_ci, by="Autonomous_selfing_level")


all_tf <- all_tf %>%
  mutate(Autonomous_selfing_level = fct_relevel(Autonomous_selfing_level, levels=c("high", "medium", "low", "none")))
all_tf$Autonomous_selfing_level <- as.factor(all_tf$Autonomous_selfing_level)


ggplot(all_tf,aes(x= Autonomous_selfing_level,y= pred, color=Autonomous_selfing_level)) +theme()+
  geom_point(Autonomous_selfing_level  = 1,size   = 4, alpha=0.2, colour="lightblue") + geom_point(aes(x= Autonomous_selfing_level,y= mean, color=Autonomous_selfing_level),Autonomous_selfing_level  = 2,size   = 4,colour="steelblue", alpha=1) +
  geom_errorbar(aes(ymin  =  lower,ymax  =  upper),width =  0.2,size =  0.7, colour="steelblue")


#####################################
###
#### MODEL 6 betweenness ~ COMPATIBILITY
###
#####################################

model_6 <- mixed_model(betweenness ~ Autonomous_selfing_level, random = ~ 1 | Id, data = all_df, 
                   family = hurdle.lognormal(), n_phis = 1,
                   zi_fixed = ~ 1)
#These data is a bit ugly and the models too...
#At the moment this is not a main part of the analysis so I won't lose much time on the analysis with this

library(emmeans)
library(multcomp)
#Analyse data and plot it
lsm = lsmeans(model_6, "Autonomous_selfing_level", type = "response")
pairs(lsm)
CLD <- cld(lsm,alpha=0.05,adjust="tukey")
#Convert to factor to set plotting order
CLD <- CLD %>%
  mutate(Autonomous_selfing_level = fct_relevel(Autonomous_selfing_level, levels=c("high", "medium", "low", "none")))
CLD$Autonomous_selfing_level <- as.factor(CLD$Autonomous_selfing_level)

ggplot(CLD,aes(x= Autonomous_selfing_level,y= lsmean,
               label = .group)) +theme()+
  geom_point(Autonomous_selfing_level  = 15,size   = 4) +ggtitle("Lsm difference between groups")+
  geom_errorbar(aes(ymin  =  asymp.LCL,ymax  =  asymp.UCL),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Selfing level")+
  geom_text(nudge_x = c(0, 0, 0), nudge_y = c(0.8, 0.8, 0.8),color   = "black")


