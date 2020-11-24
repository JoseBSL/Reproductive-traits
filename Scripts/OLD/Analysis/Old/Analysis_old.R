#####################################
###
#### MODELS
###
#####################################
library(plyr)
library(data.table)
library(tidyverse) 
library(lubridate)
library(ggplot2)
library(lme4)
library(AER)
library(DHARMa)
library(MASS)
library(fitdistrplus)
library(logspline)
library(emmeans)
library(multcomp)
library(modelr)
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


#Check data distribution

#Visits
hist(all_df$Visits)
#Degree
hist(all_df$degree)
#Normalise degree
hist(all_df$normalised.degree)
#specialization d'
hist(all_df$d)


######
#####
####
### MODEL 1  VISITS~COMPATIBILITY   POISSON
####
#####
#####


#Check data distribution for visits
descdist(df$Visits, discrete = FALSE)
hist(all_df$Visits)
ggplot(all_df, aes(x = Visits)) + geom_histogram() + facet_wrap(~Compatibility)
#Seems that a poisson will fit the data nicely
#Our data seems to follow a Poisson dist
plot(fitdist(all_df$Visits,"pois"))

# MODEL 1
model_1 <- glmer(Visits~Compatibility+(1|unique.id),data=all_df,
                     family=poisson)
#Checking residuals
plot(model_1)
#Model output
summary(model_1)


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
#TRY ONTHER DISTRIBUTION


######
#####
####
### MODEL 2  VISITS ~ COMPATIBILITY QUASI-POISSON
####
#####
#####

model_2<-glmmPQL(Visits ~ Compatibility ,
                 random = list(~ 1 | Id),
                 family = quasipoisson(link=log), 
                 data = all_df)

summary(model_2)
plot(model_2)
qqnorm(model_2, abline=c(0,1))



#Boxplot of residuals
plot(model_2,Compatibility~resid(.,type="pearson"),xlim=c(-1.5,2))
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
#This model seems to be ok

#Plot predicted values and residuals
par(mar = c(4, 5, 0, 1))
res <- predict(model_2, type = "response") + residuals(model_2)
plot.default(res ~ all_df$Compatibility, data = all_df, type = "n", xlim = c(0.5, 3.5), ann = F,axes = F)
points(res ~ Compatibility, data = all_df, pch = 16, col = "grey")
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


#COMPARE MEANS 

#Analyse data and plot it
lsm = lsmeans(model_2, "Compatibility", type = "response")
pairs(lsm)
summary(model_2)
CLD <- cld(lsm,alpha=0.05,adjust="tukey")

ggplot(CLD,aes(x= Compatibility,y= rate,
               label = .group)) +theme()+
  geom_point(shape  = 15,size   = 4) +ggtitle("Lsm difference between groups")+
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Compatibility")+
  geom_text(nudge_x = c(0, 0, 0), nudge_y = c(0.8, 0.8, 0.8),color   = "black")


###
### MODEL 3  d ~ COMPATIBILITY GAUSSIAN
###

#For curiosity a package that checks the possible distribution of your data

descdist(df$d, discrete = FALSE)

model_3<-glmmPQL(d ~ Compatibility ,
                 random = list(~ 1 | Id),
                 family = gaussian, 
                 data = all_df)
summary(model_3)
plot(model_3,Compatibility~resid(.,type="pearson"),xlim=c(-2,3))

plot(model_3)
qqnorm(model_3, abline=c(0,1))
#Residuals looks ok

par(mar = c(4, 5, 0, 1))
res <- predict(model_3, type = "response") + residuals(model_3)
plot.default(res ~ all_df$Compatibility, data = all_df, type = "n", xlim = c(0.5, 3.5), ann = F,axes = F)
points(res ~ Compatibility, data = all_df, pch = 16, col = "grey")
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


######
#####
####
### MODEL 4  Degree ~ COMPATIBILITY QUASI-POISSON
####
#####
#####


descdist(all_df$degree, discrete = FALSE)
hist(all_df$degree)


model_4<-glmmPQL(degree ~ Compatibility ,
                 random = list(~ 1 | Id),
                 family = quasipoisson(link=log), 
                 data = all_df)
summary(model_4)
plot(model_4)
qqnorm(model_4, abline=c(0,1))

#Check residuals
xyplot(residuals(model_4) ~ fitted(model_4) | all_df$Id 
       , main = "model_4 – full model by plot",
       panel=function(x, y){
         panel.xyplot(x, y)
         panel.loess(x, y, span = 0.75)
         panel.lmline(x, y, lty = 2)  # Least squares broken line
       }
)

plot(fitted(model_4), residuals(model_4),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(model_4), residuals(model_4)))

#Looks fine


par(mar = c(4, 5, 0, 1))
res <- predict(model_4, type = "response") + residuals(model_4)
plot.default(res ~ all_df$Compatibility, data = all_df, type = "n", xlim = c(0.5, 3.5), ann = F,axes = F)
points(res ~ Compatibility, data = all_df, pch = 16, col = "grey")
coefs <- fixef(model_4)
pred <- data.frame(Compatibility = gl(3, 1, 3, c("partially_self_compatible", "self_compatible", "self_incompatible")))
mm <- model.matrix(~Compatibility, data = pred)
fit <- as.vector(coefs %*% t(mm))
pred$fit <- exp(fit)
se <- sqrt(diag(mm %*% vcov(model_4) %*% t(mm)))
pred$lwr <- exp(fit - qt(0.975, model_4$fixDF$X[2]) * se)
pred$upr <- exp(fit + qt(0.975, model_4$fixDF$X[2]) * se)
points(fit ~ Compatibility, data = pred, pch = 16)
with(pred, arrows(as.numeric(Compatibility), lwr, as.numeric(Compatibility), upr, code = 3, angle = 0,length = 0.1))
axis(1, at = 1:3, lab = c("partially_self_compatible", "self_compatible", "self_incompatible"),cex = 0.5)
mtext("Compatibility system", 1, cex = 1, line = 3)
axis(2, las = 2)
mtext("Degree", 2, cex = 1, line = 3)
box(bty = "l")

#Ggplot predicted residuals and mean with sd
str(res)
res_1 <- stack(res)
all_df_1 <- all_df
gg_model <- cbind(all_df_1, res_1)
gg_model_1 <- merge(gg_model, pred, by="Compatibility")
gg_model_1$Compatibility <- as.character(gg_model_1$Compatibility)
gg_model_1$Compatibility[gg_model_1$Compatibility=="partially_self_compatible"] <- "Partially self-compatible"  
gg_model_1$Compatibility[gg_model_1$Compatibility=="self_compatible"] <- "Self compatible"  
gg_model_1$Compatibility[gg_model_1$Compatibility=="self_incompatible"] <- "Self incompatible"  


ggplot(gg_model_1,aes(x= Compatibility,y= values, color=Compatibility)) +theme_ds4psy()+
  geom_point(shape  = 1,size   = 4, alpha=0.6) +ggtitle("Mean +/- sd")+
  geom_errorbar(aes(ymin  =  lwr,ymax  =  upr),
                width =  0.2,size =  0.7)+geom_point(aes(y= fit),size   = 4)+ theme(legend.position = "none")+
  scale_color_manual(values=c("goldenrod","darkseagreen4", "red4")) + ylab("Predicted degree")


#Differences between groups
lsm = lsmeans(model_4, "Compatibility", type = "response")
pairs(lsm)
summary(model_4)
CLD <- cld(lsm,alpha=0.05,adjust="tukey")

ggplot(CLD,aes(x= Compatibility,y= rate,
               label = .group)) +theme()+
  geom_point(shape  = 15,size   = 4) +ggtitle("B) Comparison of ratios as donor")+
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Compatibility")+
  geom_text(nudge_x = c(0, 0, 0), nudge_y = c(0.8, 0.8, 0.8),color   = "black")


######
#####
####
### MODEL 5  Normalise Degree ~ COMPATIBILITY GAUSSIAN
####
#####
#####

descdist(all_df$normalised.degree, discrete = FALSE)
hist(all_df$normalised.degree)

model_5<-glmmPQL(normalised.degree ~ Compatibility ,
                 random = list(~ 1 | Id),
                 family = gaussian, 
                 data = all_df)

plot(model_5)
qqnorm(model_5, abline=c(0,1))
#Doesn't look to bad

#Plot predicted residuald and mean +/-sd
par(mar = c(4, 5, 0, 1))
res <- predict(model_5, type = "response") + residuals(model_5)
plot.default(res ~ all_df$Compatibility, data = all_df, type = "n", xlim = c(0.5, 3.5), ann = F,axes = F)
points(res ~ Compatibility, data = all_df, pch = 16, col = "grey")
coefs <- fixef(model_5)
pred <- data.frame(Compatibility = gl(3, 1, 3, c("partially_self_compatible", "self_compatible", "self_incompatible")))
mm <- model.matrix(~Compatibility, data = pred)
fit <- as.vector(coefs %*% t(mm))
pred$fit <- fit
se <- sqrt(diag(mm %*% vcov(model_5) %*% t(mm)))
pred$lwr <- fit - qt(0.975, model_5$fixDF$X[2]) * se
pred$upr <- fit + qt(0.975, model_5$fixDF$X[2]) * se
points(fit ~ Compatibility, data = pred, pch = 16)
with(pred, arrows(as.numeric(Compatibility), lwr, as.numeric(Compatibility), upr, code = 3, angle = 0,length = 0.1))
axis(1, at = 1:3, lab = c("partially_self_compatible", "self_compatible", "self_incompatible"),cex = 0.5)
mtext("Compatibility system", 1, cex = 1, line = 3)
axis(2, las = 2)
mtext("Degree", 2, cex = 1, line = 3)
box(bty = "l")

#gGPLOT
#Ggplot predicted residuals and mean with sd
str(res)
res_1 <- stack(res)
all_df_1 <- all_df
gg_model <- cbind(all_df_1, res_1)
gg_model_1 <- merge(gg_model, pred, by="Compatibility")
gg_model_1$Compatibility <- as.character(gg_model_1$Compatibility)
gg_model_1$Compatibility[gg_model_1$Compatibility=="partially_self_compatible"] <- "Partially self-compatible"  
gg_model_1$Compatibility[gg_model_1$Compatibility=="self_compatible"] <- "Self compatible"  
gg_model_1$Compatibility[gg_model_1$Compatibility=="self_incompatible"] <- "Self incompatible"  


ggplot(gg_model_1,aes(x= Compatibility,y= values, color=Compatibility)) +theme_ds4psy()+
  geom_point(shape  = 1,size   = 4, alpha=0.6) +ggtitle("Mean +/- sd")+
  geom_errorbar(aes(ymin  =  lwr,ymax  =  upr),
                width =  0.2,size =  0.7)+geom_point(aes(y= fit),size   = 4)+ theme(legend.position = "none")+
  scale_color_manual(values=c("goldenrod","darkseagreen4", "red4")) + ylab("Predicted normalize degree")


######
#####
####
### MODEL 6  CLOSENESS ~ COMPATIBILITY GAUSSIAN
####
#####
#####

descdist(all_df$closeness, discrete = FALSE)
hist(all_df$closeness)


model_6<-glmmPQL(closeness ~ Compatibility ,
                 random = list(~ 1 | Id),
                 family = gaussian, 
                 data = all_df)


#Plot predicted residuals and mean +/-sd
par(mar = c(4, 5, 0, 1))
res <- predict(model_6, type = "response") + residuals(model_6)
plot.default(res ~ all_df$Compatibility, data = all_df, type = "n", xlim = c(0.5, 3.5), ann = F,axes = F)
points(res ~ Compatibility, data = all_df, pch = 16, col = "grey")
coefs <- fixef(model_6)
pred <- data.frame(Compatibility = gl(3, 1, 3, c("partially_self_compatible", "self_compatible", "self_incompatible")))
mm <- model.matrix(~Compatibility, data = pred)
fit <- as.vector(coefs %*% t(mm))
pred$fit <- fit
se <- sqrt(diag(mm %*% vcov(model_6) %*% t(mm)))
pred$lwr <- fit - qt(0.975, model_6$fixDF$X[2]) * se
pred$upr <- fit + qt(0.975, model_6$fixDF$X[2]) * se
points(fit ~ Compatibility, data = pred, pch = 16)
with(pred, arrows(as.numeric(Compatibility), lwr, as.numeric(Compatibility), upr, code = 3, angle = 0,length = 0.1))
axis(1, at = 1:3, lab = c("partially_self_compatible", "self_compatible", "self_incompatible"),cex = 0.5)
mtext("Compatibility system", 1, cex = 1, line = 3)
axis(2, las = 2)
mtext("Degree", 2, cex = 1, line = 3)
box(bty = "l")


#gGPLOT
#Ggplot predicted residuals and mean with sd
str(res)
res_1 <- stack(res)
all_df_1 <- all_df
gg_model <- cbind(all_df_1, res_1)
gg_model_1 <- merge(gg_model, pred, by="Compatibility")
gg_model_1$Compatibility <- as.character(gg_model_1$Compatibility)
gg_model_1$Compatibility[gg_model_1$Compatibility=="partially_self_compatible"] <- "Partially self-compatible"  
gg_model_1$Compatibility[gg_model_1$Compatibility=="self_compatible"] <- "Self compatible"  
gg_model_1$Compatibility[gg_model_1$Compatibility=="self_incompatible"] <- "Self incompatible"  


ggplot(gg_model_1,aes(x= Compatibility,y= values, color=Compatibility)) +theme_ds4psy()+
  geom_point(shape  = 1,size   = 4, alpha=0.6) +ggtitle("Mean +/- sd")+
  geom_errorbar(aes(ymin  =  lwr,ymax  =  upr),
                width =  0.2,size =  0.7)+geom_point(aes(y= fit),size   = 4)+ theme(legend.position = "none")+
  scale_color_manual(values=c("goldenrod","darkseagreen4", "red4")) + ylab("Predicted closeness")


######
#####
####
### MODEL 7  BETWEENNESS ~ COMPATIBILITY GAUSSIAN
####
#####
#####


descdist(all_df$betweenness, discrete = FALSE)
hist(all_df$betweenness)

a <- glm.nb(betweenness ~ Compatibility, data = all_df)
theta <- a$theta
model_7<-glmmPQL(betweenness ~ Compatibility ,
                 random = list(~ 1 | Id),
                 family = negative.binomial(theta), 
                 data = all_df)


plot(model_7)
plot(residuals(model_7, type = "pearson") ~ predict(model_7, type = "link"))

plot(residuals(model_7, type = "pearson") ~ as.numeric(all_df$Compatibility))


 # proportion of 0's in the data
 data.tab <- table(all_df$betweenness == 0)
 data.tab/sum(data.tab)
FALSE  TRUE 
0.5   0.5 

#Lets use a zero inflated poisson

library(glmmTMB)
a <- glmmTMB(betweenness~Compatibility+(1|Id),
                         data=all_df,
                         ziformula=~1,
                         family = tweedie(link = "log"))

summary(a)
simulationOutput <- simulateResiduals(fittedModel = a)
plot(simulationOutput)


#Plot predicted residuals and mean +/-sd
par(mar = c(4, 5, 0, 1))
res <- predict(a, type = "response") + residuals(model_7)
plot.default(res ~ all_df$Compatibility, data = all_df, type = "n", xlim = c(0.5, 3.5), ann = F,axes = F)
points(res ~ Compatibility, data = all_df, pch = 16, col = "grey")
coefs <- fixef(model_7)
pred <- data.frame(Compatibility = gl(3, 1, 3, c("partially_self_compatible", "self_compatible", "self_incompatible")))
mm <- model.matrix(~Compatibility, data = pred)
fit <- as.vector(coefs %*% t(mm))
pred$fit <- fit
se <- sqrt(diag(mm %*% vcov(model_7) %*% t(mm)))
pred$lwr <- fit - qt(0.975, model_7$fixDF$X[2]) * se
pred$upr <- fit + qt(0.975, model_7$fixDF$X[2]) * se
points(fit ~ Compatibility, data = pred, pch = 16)
with(pred, arrows(as.numeric(Compatibility), lwr, as.numeric(Compatibility), upr, code = 3, angle = 0,length = 0.1))
axis(1, at = 1:3, lab = c("partially_self_compatible", "self_compatible", "self_incompatible"),cex = 0.5)
mtext("Compatibility system", 1, cex = 1, line = 3)
axis(2, las = 2)
mtext("Degree", 2, cex = 1, line = 3)
box(bty = "l")
