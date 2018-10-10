# load csv's

a <- read.csv("data/data manipulation/robertson 2010/robertson_2010_1.csv", stringsAsFactors = F)
b <- read.csv("data/data manipulation/robertson 2010/robertson_2010_2.csv", stringsAsFactors = F)
c <- read.csv("data/data manipulation/robertson 2010/robertson_2010_3.csv", stringsAsFactors = F)
d <- read.csv("data/data manipulation/robertson 2010/robertson_2010_4.csv", stringsAsFactors = F)
e <- read.csv("data/data manipulation/robertson 2010/robertson_2010_5.csv", stringsAsFactors = F)
f <- read.csv("data/data manipulation/robertson 2010/robertson_2010_6.csv", stringsAsFactors = F)
g <- read.csv("data/data manipulation/robertson 2010/robertson_2010_7.csv", stringsAsFactors = F)
#REFERENCES
h <- read.csv("data/data manipulation/robertson 2010/robertson_2010_references.csv", header=F)

#trying for loop

x <- list(b,c,d,e,f,g)
i <- NULL
y <- NULL
for (i in x){
colnames(i) <-  i[1, ]
i = i[-1, ]
y <- rbind(y, i)
}
colnames(y) <- colnames(a)
robertson_2010 <- rbind(a,y)
robertson_2010=robertson_2010[,-c(3,5)]

#split references to take just year, author and journal
colnames(h)[1] <- "references"
h$references <- as.character(h$references)

library(stringr)

h <- str_split(h$references, " ", simplify = T)





