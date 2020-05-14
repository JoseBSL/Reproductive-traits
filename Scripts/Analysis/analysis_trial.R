#####################################
###
#### MODELS
###
#####################################

library(GLMMadaptive)
library(emmeans)
library(multcomp)
library(ggplot2)
library(ds4psy)


#load data
all_df <- readRDS("Data/RData/network_metrics.RData")
colnames(all_df)[17]<-"Id"
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
#### MODEL 1 VISITS ~ COMPATIBILITY
###
#####################################


model_1 <- mixed_model(Visits ~ Compatibility, random = ~ 1 | Id, data = all_df,
                   family = poisson())

resids_plot(model_1, all_df$Visits)
#Not nice

#Need to detach libraries before using mixed_effects from GLMMadaptive
detach("package:MASS", unload=TRUE)
detach("package:lme4", unload=TRUE)
detach("package:emmeans", unload=TRUE)
detach("package:multcomp", unload=TRUE)

model_1.1 <- mixed_model(Visits ~ Compatibility, random = ~ 1 | Id, data = all_df,
                   family = negative.binomial())

resids_plot(model_1.1, all_df$Visits)
#Much better now

#Analyse data and plot it
lsm = lsmeans(model_1.1, "Compatibility", type = "response")
pairs(lsm)
summary(model_1.1)
CLD <- cld(lsm,alpha=0.05,adjust="tukey")

ggplot(CLD,aes(x= Compatibility,y= lsmean,
               label = .group)) +theme()+
  geom_point(Compatibility  = 15,size   = 4) +ggtitle("Lsm difference between groups")+
  geom_errorbar(aes(ymin  =  asymp.LCL,ymax  =  asymp.UCL),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Compatibility")+
  geom_text(nudge_x = c(0, 0, 0), nudge_y = c(0.8, 0.8, 0.8),color   = "black")

#Plot predicted values and residuals

#Predict values
all_tf <- all_df
preds <- predict(model_1.1, newdata = all_tf,
                 type = "subject_specific",
                 se.fit = TRUE, return_newdata = TRUE)

#Function to calculate SEM
std <- function(x) sd(x)/sqrt(length(x))

i <- levels(all_df$Compatibility)
mean_ci <- NULL
i <- c("partially_self_compatible", "self_compatible", "self_incompatible")
for (i in levels(preds$Compatibility)){
  
  mean <- mean(preds[preds$Compatibility == i,18])
  
  upper <- mean(preds[preds$Compatibility == i,18]) + qt(0.975, df=length(preds[preds$Compatibility == i,18])) * 
    std(preds[preds$Compatibility == i,18]) 
  
  lower <- mean(preds[preds$Compatibility == i,18]) - qt(0.975, df=length(preds[preds$Compatibility == i,18])) * 
    std(preds[preds$Compatibility == i,18]) 
  
  Compatibility <- i
  
  #Create data frame with mean and CI of predicted values per variable
  mean_ci_1 <- data.frame(mean,upper,lower, Compatibility)
  
  mean_ci <- rbind(mean_ci, mean_ci_1)
  
  print(mean_ci)
}

preds <- merge(preds,mean_ci, by="Compatibility")

#Plot predicted values and mean predicted value with CI
ggplot(preds,aes(x= Compatibility,y= pred, color=Compatibility)) +theme()+
  geom_point(Compatibility  = 1,size   = 4, alpha=0.2, colour="lightblue") + geom_point(aes(x= Compatibility,y= mean, color=Compatibility),Compatibility  = 2,size   = 4,colour="steelblue", alpha=1) +
  geom_errorbar(aes(ymin  =  lower,ymax  =  upper),width =  0.2,size =  0.7, colour="steelblue")


#####################################
###
#### MODEL 2 d ~ COMPATIBILITY
###
#####################################

library(lme4)
library(DHARMa)
library(emmeans)
library(multcomp)
hist(all_df$d)
model_2 <- lmer(d ~ Compatibility + (1|Id) , data = all_df)
fit<- simulateResiduals(fittedModel = model_2, plot = T)
plot(fit)
#Looks good

#Check differences in d between compatibility modes
lsm = lsmeans(model_2, "Compatibility", type = "response")
pairs(lsm)
summary(model_1.1)
CLD <- cld(lsm,alpha=0.05,adjust="tukey")

ggplot(CLD,aes(x= Compatibility,y= lsmean,
               label = .group)) +theme()+
  geom_point(Compatibility  = 15,size   = 4) +ggtitle("Lsm difference between groups")+
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Compatibility")+
  geom_text(nudge_x = c(0, 0, 0), nudge_y = c(0.8, 0.8, 0.8),color   = "black")

#Plot predicted values and residuals

#Predict values
all_tf <- all_df
pred <- predict(model_2, type = "response")

all_tf <- cbind(all_tf,pred)
mean_ci <- NULL

i <- c("partially_self_compatible", "self_compatible", "self_incompatible")
for (i in levels(all_tf$Compatibility)){
  
mean <- mean(all_tf[all_tf$Compatibility == i,18])

upper <- mean(all_tf[all_tf$Compatibility == i,18]) + qt(0.975, df=length(all_tf[all_tf$Compatibility == i,18])) * 
  std(all_tf[all_tf$Compatibility == i,18]) 

lower <- mean(all_tf[all_tf$Compatibility == i,18]) - qt(0.975, df=length(all_tf[all_tf$Compatibility == i,18])) * 
  std(all_tf[all_tf$Compatibility == i,18]) 

Compatibility <- i

mean_ci_1 <- data.frame(mean,upper,lower, Compatibility)

mean_ci <- rbind(mean_ci, mean_ci_1)

print(mean_ci)
}

all_tf <- merge(all_tf,mean_ci, by="Compatibility")


ggplot(all_tf,aes(x= Compatibility,y= pred, color=Compatibility)) +theme()+
  geom_point(Compatibility  = 1,size   = 4, alpha=0.2, colour="lightblue") + geom_point(aes(x= Compatibility,y= mean, color=Compatibility),Compatibility  = 2,size   = 4,colour="steelblue", alpha=1) +
  geom_errorbar(aes(ymin  =  lower,ymax  =  upper),width =  0.2,size =  0.7, colour="steelblue")

#####################################
###
#### MODEL 3 Degree ~ COMPATIBILITY
###
#####################################

library(emmeans)
library(multcomp)

model_3 <- lmer(degree ~ Compatibility + (1|Id) , data = all_df)

hist(all_df$degree)
descdist(all_df$degree, discrete = FALSE)
fit<- simulateResiduals(fittedModel = model_3, plot = T)
plot(fit)


#Analyse data and plot it
lsm = lsmeans(model_3, "Compatibility", type = "response")
pairs(lsm)
summary(model_1.1)
CLD <- cld(lsm,alpha=0.05,adjust="tukey")

ggplot(CLD,aes(x= Compatibility,y= lsmean,
               label = .group)) +theme()+
  geom_point(Compatibility  = 15,size   = 4) +ggtitle("Lsm difference between groups")+
  geom_errorbar(aes(ymin  =  asymp.LCL,ymax  =  asymp.UCL),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Compatibility")+
  geom_text(nudge_x = c(0, 0, 0), nudge_y = c(0.8, 0.8, 0.8),color   = "black")


#Predict values
all_tf <- all_df
preds <- predict(model_3, newdata = all_tf,
                 type = "subject_specific",
                 se.fit = TRUE, return_newdata = TRUE)

#Function to calculate SEM
std <- function(x) sd(x)/sqrt(length(x))

i <- levels(all_df$Compatibility)
mean_ci <- NULL
i <- c("partially_self_compatible", "self_compatible", "self_incompatible")
for (i in levels(preds$Compatibility)){
  
  mean <- mean(preds[preds$Compatibility == i,18])
  
  upper <- mean(preds[preds$Compatibility == i,18]) + qt(0.975, df=length(preds[preds$Compatibility == i,18])) * 
    std(preds[preds$Compatibility == i,18]) 
  
  lower <- mean(preds[preds$Compatibility == i,18]) - qt(0.975, df=length(preds[preds$Compatibility == i,18])) * 
    std(preds[preds$Compatibility == i,18]) 
  
  Compatibility <- i
  
  #Create data frame with mean and CI of predicted values per variable
  mean_ci_1 <- data.frame(mean,upper,lower, Compatibility)
  
  mean_ci <- rbind(mean_ci, mean_ci_1)
  
  print(mean_ci)
}

preds <- merge(preds,mean_ci, by="Compatibility")

#Plot predicted values and mean predicted value with CI
ggplot(preds,aes(x= Compatibility,y= pred, color=Compatibility)) +theme()+
  geom_point(Compatibility  = 1,size   = 4, alpha=0.2, colour="lightblue") + geom_point(aes(x= Compatibility,y= mean, color=Compatibility),Compatibility  = 2,size   = 4,colour="steelblue", alpha=1) +
  geom_errorbar(aes(ymin  =  lower,ymax  =  upper),width =  0.2,size =  0.7, colour="steelblue")





#Need to detach libraries before using mixed_effects from GLMMadaptive
#Do not know which one is giving trouble but I do it for all just in case
detach("package:lme4", unload=TRUE)
detach("package:emmeans", unload=TRUE)
detach("package:multcomp", unload=TRUE)
detach("package:DHARMa", unload=TRUE)
detach("package:ds4psy", unload=TRUE)
detach("package:ciTools", unload=TRUE)
detach("package:TH.data", unload=TRUE)
detach("package:MASS", unload=TRUE)