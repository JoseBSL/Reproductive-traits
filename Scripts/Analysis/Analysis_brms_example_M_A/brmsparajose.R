library(brms)
library(ape)

#Carga el dat3b que te he pasado
dat3b

setwd("~/R_Projects/Reproductive Traits/Scripts/Analysis/Analysis_brms_example_M_A")

dat3b <- read.csv("dat3b.csv")


#Cargamos el árbol
tree1 <- read.nexus("tree1.nexus")
A <- ape::vcv.phylo(tree1)
A <- A/max(A)

#Este sería el análisis, estamos viendo aquí, si tener éxito en un test de aprendizaje
#está relacionado con el peso absoluto de unos cerebros, debido a que el cerebro en sí mismo
#está muy relacionado con la especie que sea, añadimos random factor 1|Species, y debido a que 
#igualmente, el tamaño del cerebro dependerá de la filogenia de cada especie, añadimos 1|phylo


# "cov_ranef =" es donde metemos el árbol
m7b3 <-brm(Success.test ~ Brain.weight + (1|Species) + (1|phylo), 
           data = dat3b,
           cores=4,
           family = "bernoulli", cov = list("phylo" = A),
           control = list(adapt_delta = 0.99,max_treedepth=15))

summary(m7b3)

#Y esto simplemente para la representación gráfica, que pienso te puede ayudar
# marginal_effects() es una función increíblemente buena para trabajar con brms, te saca ya los
# predict y puedes hacer lo que quieras como en este ejemplo
plot(Success.test.as.numeric ~ Brain.weight, data = dat3b, 
     main="Success related to \nbrain size (a)", xlab="Absolute brain size", 
     cex.lab= 1.3 ,ylab = "Success learning test", las = 1, col = c("red","darkgreen", "blue", "black")[Family])
legend(x=4, y=0.4, legend = levels(Success8trials.ITf$Family),
       col = c("red","darkgreen", "blue", "black"), pch=19, cex=0.85,ncol = 1)
box(which = "plot", lty = "solid")
fit <- marginal_effects(m7b3)
fits<-as.data.frame(fit$Brain.weight)
fits$Brain.weight
polygon(c((fits$Brain.weight), rev((fits$Brain.weight))), c(fits$upper__, rev(fits$lower__)),
        col = "Gray95", border = NA)

lines((fits$Brain.weight), fits$estimate__, lwd=2)
lines((fits$Brain.weight), fits$lower__, col = "purple")
lines((fits$Brain.weight), fits$upper__, col = "purple")
points(Success.test.as.numeric ~ (Brain.weight), data = dat3b,col = c("red","darkgreen", "blue", "black")[Family])



