#Load libraries

library(bipartite)
library(plyr)
library(data.table)
library(tidyverse) 
library(lubridate)
library(readxl)
library(ggplot2)
library(lme4)
library('fitdistrplus')
library(AER)
library(DHARMa)
library(MASS)
library(fitdistrplus)
library(logspline)
library(emmeans)
library(multcomp)


#load data
all_df <- readRDS("Data/RData/network_metrics.RData")



#####
####
##MODEL TIME
####
####

#Convert all NA'S to same type of NA's
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
all_df$Compatibility <- make.true.NA(all_df$Compatibility)
all_df <- all_df[complete.cases(all_df),]


#VISUALIZA DATA
#Visits
hist(all_df$Visits)
#Degree
hist(all_df$degree)
#Normalise degree
hist(all_df$normalised.degree)
#specialization d'
hist(all_df$d)



#####
##### MODEL 1  VISITS~COMPATIBILITY   POISSON
#####

#Check data distribution for visits
descdist(df$Visits, discrete = FALSE)

model_1 <- glmer(Visits~Compatibility+(1|unique.id),data=all_df,
                     family=poisson)
#Checking residuals
plot(model_1)
plot(residuals(model_1, type = "pearson") ~ predict(model_1, type = "link"))
plot(residuals(model_1, type = "pearson") ~ as.numeric(all_df$Visits))

summary(model_1)

#Checking ddata distribution 
#Our data does not foloow a normal distribution
plot(fitdist(all_df$Visits,"norm"))
#Our data seems to follow a Poisson dist
plot(fitdist(all_df$Visits,"pois"))


# OR using Ben Bolker's function
overdisp_fun <- function(model_1) {
## number of variance parameters in an n-by-n variance-covariance matrix
       vpars <- function(m) {
        nrow(m) * (nrow(m) + 1)/2
    }
       model.df <- sum(sapply(VarCorr(model_1), vpars)) + length(fixef(model_1))
       rdf <- nrow(model.frame(model_1)) - model.df
       rp <- residuals(model_1, type = "pearson")
       Pearson.chisq <- sum(rp^2)
       prat <- Pearson.chisq/rdf
       pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
       c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
   }

overdisp_fun(model_1)
#Seems that our residuals are overdispersed 
#Alternative way to check dispersion from dharma package
sim_fmp <- simulateResiduals(model_1, refit=T) 
testOverdispersion(sim_fmp)
plotSimulatedResiduals(sim_fmp)
#Also shows that my data is overdispersed

#Now we have an alternative family for our distribution
#Quasipoison and negative binomial are next candidates

#convert to factors and integers to avoid issues
df <- as.data.frame(all_df) 
df$Compatibility <- as.factor(df$Compatibility)
df$Visits <- as.integer(df$Visits)
df$unique.id.1 <- as.factor(df$unique.id.1)
colnames(df)[13]<-"Id"


###
### MODEL 2  VISITS ~ COMPATIBILITY QUASI-POISSON
###

model_2<-glmmPQL(Visits ~ Compatibility ,
                 random = list(~ 1 | Id),
                 family = quasipoisson(link=log), 
                 data = df)

summary(model_2)
plot(model_2)
plot(residuals(model_2, type = "pearson") ~ predict(model_2, type = "link"))
plot(residuals(model_2, type = "pearson") ~ as.numeric(all_df$Visits))



colnames(all_df) <- make.unique(names(all_df))


ggplot(all_df, aes(x = Visits)) + geom_histogram() + facet_wrap(~Compatibility)
ggplot(all_df, aes(y = Compatibility, x = Visits)) + geom_point() + facet_wrap(~unique.id.1)


plot(model_2)
#Checking overdispersion
#FROM: http://www.flutterbys.com.au/stats/tut/tut11.7a.html
data.hqp.resid <- sum(resid(model_2, type = "pearson")^2)
str(model_2)
rdf <- length(model_2$data$Visits) - length(fixef(model_2)) - 1
rdf
1 - pchisq(data.hqp.resid, rdf)
 data.hqp.resid/(nrow(all_df) - length(fixef(model_2)) - 1)
#P>0.05 data is not overdispersed
 #Coefficient of determination
totals <- var(resid(model_2, type='pearson')+predict(model_2, type='link'))
1-var(residuals(model_2, type='pearson'))/(totals)
#Quasi-R2 output [1] 0.2855506
#Plot


#Plot predicted values and residuals
par(mar = c(4, 5, 0, 1))
res <- predict(model_2, type = "response") + residuals(model_2)
plot.default(res ~ df$Compatibility, data = df, type = "n", xlim = c(0.5, 3.5), ann = F,axes = F)
points(res ~ Compatibility, data = df, pch = 16, col = "grey")
coefs <- fixef(model_2)
pred <- data.frame(Compatibility = gl(3, 1, 3, c("partially_self_compatible", "self_compatible", "self_incompatible")))
mm <- model.matrix(~Compatibility, data = pred)
fit <- as.vector(coefs %*% t(mm))
pred$fit <- exp(fit)
se <- sqrt(diag(mm %*% vcov(model_2) %*% t(mm)))
pred$lwr <- exp(fit - qt(0.975, model_2$fixDF$X[2]) * se)
pred$upr <- exp(fit + qt(0.975, model_2$fixDF$X[2]) * se)
points(fit ~ Compatibility, data = pred, pch = 16)
with(pred, arrows(as.numeric(Compatibility), lwr, as.numeric(Compatibility), upr, code = 3, angle = 0,length = 0.1))
axis(1, at = 1:3, lab = c("partially_self_compatible", "self_compatible", "self_incompatible"),cex = 0.5)
mtext("Compatibility system", 1, cex = 1, line = 3)
axis(2, las = 2)
mtext("Visits", 2, cex = 1, line = 3)
box(bty = "l")

hist(df$Visits)
mean(df$Visits)
median(df$Visits)

predict(model_2, level = 0)
predict(model_2,  type='link')



confint(model_2)


#Boxplot of residuals
plot(model_2,Compatibility~resid(.,type="pearson"),xlim=c(-1.5,2))

#Analyse data and plot it
lsm = lsmeans(model_2, "Compatibility", type = "response")
pairs(lsm)
summary(model_2)
CLD <- cld(lsm,alpha=0.05,adjust="tukey")

ggplot(CLD,aes(x= Compatibility,y= rate,
               label = .group)) +theme()+
  geom_point(shape  = 15,size   = 4) +ggtitle("B) Comparison of ratios as donor")+
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Compatibility")+
  geom_text(nudge_x = c(0, 0, 0), nudge_y = c(0.8, 0.8, 0.8),color   = "black")


###
### MODEL 3  d ~ COMPATIBILITY GAUSSIAN
###

#For curiosity a package that checks the possible distribution of your data
library(modelr)

descdist(df$d, discrete = FALSE)

model_3<-glmmPQL(d ~ Compatibility ,
                 random = list(~ 1 | Id),
                 family = gaussian, 
                 data = df)
summary(model_3)
plot(model_3,Compatibility~resid(.,type="pearson"),xlim=c(-2,3))

plot(model_3)
qqnorm(model_3, abline=c(0,1))
#Resiaduals looks ok

lsm = lsmeans(model_3, "Compatibility", type = "response")
pairs(lsm)
summary(model_3)
CLD <- cld(lsm,alpha=0.05,adjust="tukey")

ggplot(CLD,aes(x= Compatibility,y= lsmean,
               label = .group)) +theme()+
  geom_point(shape  = 15,size   = 4) +ggtitle("B) Comparison of ratios as donor")+
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Compatibility")+
  geom_text(nudge_x = c(0, 0, 0), nudge_y = c(0.8, 0.8, 0.8),color   = "black")


par(mar = c(4, 5, 0, 1))
res <- predict(model_3, type = "response") + residuals(model_3)
plot.default(res ~ df$Compatibility, data = df, type = "n", xlim = c(0.5, 3.5), ann = F,axes = F)
points(res ~ Compatibility, data = df, pch = 16, col = "grey")
coefs <- fixef(model_3)
pred <- data.frame(Compatibility = gl(3, 1, 3, c("partially_self_compatible", "self_compatible", "self_incompatible")))
mm <- model.matrix(~Compatibility, data = pred)
fit <- as.vector(coefs %*% t(mm))
pred$fit <- fit
se <- sqrt(diag(mm %*% vcov(model_3) %*% t(mm)))
pred$lwr <- fit - qt(0.975, model_3$fixDF$X[2]) * se
pred$upr <- fit + qt(0.975, model_3$fixDF$X[2]) * se
points(fit ~ Compatibility, data = pred, pch = 16)
with(pred, arrows(as.numeric(Compatibility), lwr, as.numeric(Compatibility), upr, code = 3, angle = 0,length = 0.1))
axis(1, at = 1:3, lab = c("partially_self_compatible", "self_compatible", "self_incompatible"),cex = 0.5)
mtext("Compatibility system", 1, cex = 1, line = 3)
axis(2, las = 2)
mtext("d", 2, cex = 1, line = 3)
box(bty = "l")

