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

h_refs <- str_split(h$references, " ", simplify = T)

robertson_2010$author <- NA
robertson_2010$year <- NA
robertson_2010$journal <- NA
robertson_2010$id <- seq.int(nrow(robertson_2010))


#1
robertson_2010[1,4] <- "fryxell"
robertson_2010[1,5] <- "1957"
robertson_2010[1,6] <- "botanical review"

#2
robertson_2010[2:18,4] <- "ando et al.,"
robertson_2010[2:18,5] <- "2002"
robertson_2010[2:18,6] <- "journal of plant research"

#3
robertson_2010[19:29,4] <- "onus & pickersgill"
robertson_2010[19:29,5] <- "2004"
robertson_2010[19:29,6] <- "annals of botany"

#4
robertson_2010[20:36,4] <- "buchholz"
robertson_2010[20:36,5] <- "1935"
robertson_2010[20:36,6] <- "genetics"

#5
robertson_2010[37,4] <- "smith & baum"
robertson_2010[37,5] <- "2006"
robertson_2010[37,6] <- "american journal of botany"


#6
robertson_2010[38:43,4] <- "hawkes et al.,"
robertson_2010[38:43,5] <- "1979"
robertson_2010[38:43,6] <- "linnean society of london"

#7
robertson_2010[40,4] <- "hawkes et al.,; golz et al;"
robertson_2010[40,5] <- "1979;1998"
robertson_2010[40,6] <- "linnean society of london; the plant journal"

#8
robertson_2010[40,4] <- "goodspeed"
robertson_2010[40,5] <- "1954"
robertson_2010[40,6] <- "international collection of studies in the method and history of biology and agriculture"


robertson_2010$author[robertson_2010[3]=="[36]"]<-"goodspeed"
robertson_2010$year[robertson_2010[3]=="[36]"]<-"1954"
robertson_2010$journal[robertson_2010[3]=="[36]"]<-"international collection of studies in the method and history of biology and agriculture"


robertson_2010$author[robertson_2010[3]=="[74]"]<-"hawkes et al.,"
robertson_2010$year[robertson_2010[3]=="[74]"]<-"1979"
robertson_2010$journal[robertson_2010[3]=="[74]"]<-"linnean society of london"

robertson_2010$author[robertson_2010[3]=="[35]"]<-"golz et al.,"
robertson_2010$year[robertson_2010[3]=="[35]"]<-"1998"
robertson_2010$journal[robertson_2010[3]=="[35]"]<-"the plant journal"

robertson_2010$author[robertson_2010[3]=="[30]"]<-"east"
robertson_2010$year[robertson_2010[3]=="[30]"]<-"1940"
robertson_2010$journal[robertson_2010[3]=="[30]"]<-"proceedings of the american philosophical society"

robertson_2010$author[robertson_2010[3]=="[89]"]<-"stone"
robertson_2010$year[robertson_2010[3]=="[89]"]<-"2002"
robertson_2010$journal[robertson_2010[3]=="[89]"]<-"the quarterly review of biology"

robertson_2010$author[robertson_2010[3]=="[29]"]<-"east"
robertson_2010$year[robertson_2010[3]=="[29]"]<-"1925"
robertson_2010$journal[robertson_2010[3]=="[29]"]<-"proceedings of the national academy of science"

robertson_2010$author[robertson_2010[3]=="[91]"]<-"tsukamato et al.,"
robertson_2010$year[robertson_2010[3]=="[91]"]<-"1998"
robertson_2010$journal[robertson_2010[3]=="[91]"]<-"acta phytotaxonomica et geobotanica"

robertson_2010$author[robertson_2010[3]=="[57]"]<-"menzel"
robertson_2010$year[robertson_2010[3]=="[57]"]<-"1951"
robertson_2010$journal[robertson_2010[3]=="[57]"]<-"proceedings of the american philosophical society"

robertson_2010$author[robertson_2010[3]=="[81]"]<-"richman & kohn"
robertson_2010$year[robertson_2010[3]=="[81]"]<-"1999"
robertson_2010$journal[robertson_2010[3]=="[81]"]<-"proceedings of the national academy of science"

robertson_2010$author[robertson_2010[3]=="[71]"]<-"pandey"
robertson_2010$year[robertson_2010[3]=="[71]"]<-"1957"
robertson_2010$journal[robertson_2010[3]=="[71]"]<-"american journal of botany"

robertson_2010$author[robertson_2010[3]=="[66]"]<-"morales & galetto"
robertson_2010$year[robertson_2010[3]=="[66]"]<-"2003"
robertson_2010$journal[robertson_2010[3]=="[66]"]<-"plant biology"

robertson_2010$author[robertson_2010[3]=="[100]"]<-"whalen & anderson"
robertson_2010$year[robertson_2010[3]=="[100]"]<-"1981"
robertson_2010$journal[robertson_2010[3]=="[100]"]<-"taxon"

robertson_2010$author[robertson_2010[3]=="[21]"]<-"bohs"
robertson_2010$year[robertson_2010[3]=="[21]"]<-"2007"
robertson_2010$journal[robertson_2010[3]=="[21]"]<-"taxon"

robertson_2010$author[robertson_2010[3]=="[16]"]<-"bohs"
robertson_2010$year[robertson_2010[3]=="[16]"]<-"1990"
robertson_2010$journal[robertson_2010[3]=="[16]"]<-"annals of the missouri botanical garden"

robertson_2010$author[robertson_2010[3]=="[19]"]<-"bohs"
robertson_2010$year[robertson_2010[3]=="[19]"]<-"2001"
robertson_2010$journal[robertson_2010[3]=="[19]"]<-"systematic botany monographs"

robertson_2010$author[robertson_2010[3]=="[93]"]<-"vallejo-marin & o_brien"
robertson_2010$year[robertson_2010[3]=="[93]"]<-"2007"
robertson_2010$journal[robertson_2010[3]=="[93]"]<-"new phytologist"

robertson_2010$author[robertson_2010[3]=="[75]"]<-"peralta & spooner"
robertson_2010$year[robertson_2010[3]=="[75]"]<-"2001"
robertson_2010$journal[robertson_2010[3]=="[75]"]<-"american journal of botany"

robertson_2010$author[robertson_2010[3]=="[17]"]<-"ireland & buck"
robertson_2010$year[robertson_2010[3]=="[17]"]<-"2001"
robertson_2010$journal[robertson_2010[3]=="[17]"]<-"flora neotropica"

robertson_2010$author[robertson_2010[3]=="[96]"]<-"wagner"
robertson_2010$year[robertson_2010[3]=="[96]"]<-"1999"
robertson_2010$journal[robertson_2010[3]=="[96]"]<-"manual of the flowering plants of hawai"

robertson_2010$author[robertson_2010[3]=="[80]"]<-"richman & kao"
robertson_2010$year[robertson_2010[3]=="[80]"]<-"1995"
robertson_2010$journal[robertson_2010[3]=="[80]"]<-"heredity"

robertson_2010$author[robertson_2010[3]=="[53]"]<-"lopez & hawkes"
robertson_2010$year[robertson_2010[3]=="[53]"]<-"1991"
robertson_2010$journal[robertson_2010[3]=="[53]"]<-"taxonomy chemestry and evolution"

robertson_2010$author[robertson_2010[3]=="[8]"]<-"anderson"
robertson_2010$year[robertson_2010[3]=="[8]"]<-"1989"
robertson_2010$journal[robertson_2010[3]=="[8]"]<-"evolution"

robertson_2010$author[robertson_2010[3]=="[88]"]<-"spooner & systma"
robertson_2010$year[robertson_2010[3]=="[88]"]<-"1991"
robertson_2010$journal[robertson_2010[3]=="[88]"]<-"systematic botany"

robertson_2010$author[robertson_2010[3]=="[90]"]<-"symon"
robertson_2010$year[robertson_2010[3]=="[90]"]<-"1971"
robertson_2010$journal[robertson_2010[3]=="[90]"]<-"transactions of the royal society of south australia"

robertson_2010$author[robertson_2010[3]=="[64]"]<-"mione & anderson"
robertson_2010$year[robertson_2010[3]=="[64]"]<-"1992"
robertson_2010$journal[robertson_2010[3]=="[64]"]<-"american journal of botany"

robertson_2010$author[robertson_2010[3]=="[64]"]<-"mione & anderson"
robertson_2010$year[robertson_2010[3]=="[64]"]<-"1992"
robertson_2010$journal[robertson_2010[3]=="[64]"]<-"american journal of botany"

robertson_2010$author[robertson_2010[3]=="[10]"]<-"arroyo & uslar"
robertson_2010$year[robertson_2010[3]=="[10]"]<-"1993"
robertson_2010$journal[robertson_2010[3]=="[10]"]<-"botanical journal of the linnean society"

robertson_2010$author[robertson_2010[3]=="[69]"]<-"omidiji"
robertson_2010$year[robertson_2010[3]=="[69]"]<-"1979"
robertson_2010$journal[robertson_2010[3]=="[69]"]<-"linnean society symposium series"


robertson_2010$author[robertson_2010[3]=="[3]"]<-"online source"
robertson_2010$year[robertson_2010[3]=="[3]"]<-"NA"
robertson_2010$journal[robertson_2010[3]=="[3]"]<-"http://solanaceaesource.org/taxonomy/term/106594/descriptions"

robertson_2010$author[robertson_2010[3]=="[22]"]<-"personal communication"
robertson_2010$year[robertson_2010[3]=="[22]"]<-"NA"
robertson_2010$journal[robertson_2010[3]=="[22]"]<-"NA"

robertson_2010$author[robertson_2010[3]=="[87]"]<-"spooner et al.,"
robertson_2010$year[robertson_2010[3]=="[87]"]<-"1992"
robertson_2010$journal[robertson_2010[3]=="[87]"]<-"american journal of botany"

robertson_2010$author[robertson_2010[3]=="[44]"]<-"lande & kohn"
robertson_2010$year[robertson_2010[3]=="[44]"]<-"2008"
robertson_2010$journal[robertson_2010[3]=="[44]"]<-"international journal of plant sciences"

robertson_2010$author[robertson_2010[3]=="[79]"]<-"rao"
robertson_2010$year[robertson_2010[3]=="[79]"]<-"1979"
robertson_2010$journal[robertson_2010[3]=="[79]"]<- "linnean society of london academic press"

robertson_2010$author[robertson_2010[3]=="[76]"]<-"pushkarnath"
robertson_2010$year[robertson_2010[3]=="[76]"]<-"1942"
robertson_2010$journal[robertson_2010[3]=="[76]"]<- "indian journal of genetics and plant breeding"

robertson_2010$author[robertson_2010[3]=="[56]"]<-"martins"
robertson_2010$year[robertson_2010[3]=="[56]"]<-"2006"
robertson_2010$journal[robertson_2010[3]=="[56]"]<- "conservation genetics"

robertson_2010$author[robertson_2010[3]=="[82]"]<-"roe"
robertson_2010$year[robertson_2010[3]=="[82]"]<-"1979"
robertson_2010$journal[robertson_2010[3]=="[82]"]<- "academic press"

robertson_2010$author[robertson_2010[3]=="[59]"]<-"miller & diggle"
robertson_2010$year[robertson_2010[3]=="[59]"]<-"2007"
robertson_2010$journal[robertson_2010[3]=="[59]"]<- "amaerican journal of botany"

robertson_2010$author[robertson_2010[3]=="[59]"]<-"miller & diggle"
robertson_2010$year[robertson_2010[3]=="[59]"]<-"2007"
robertson_2010$journal[robertson_2010[3]=="[59]"]<- "amaerican journal of botany"

robertson_2010$author[robertson_2010[3]=="[26]"]<-"coleman & coleman"
robertson_2010$year[robertson_2010[3]=="[26]"]<-"1982"
robertson_2010$journal[robertson_2010[3]=="[26]"]<- "biotropica"

robertson_2010$author[robertson_2010[3]=="[40]"]<-"hermanutz"
robertson_2010$year[robertson_2010[3]=="[40]"]<-"1991"
robertson_2010$journal[robertson_2010[3]=="[40]"]<- "journal of botany"

robertson_2010$author[robertson_2010[3]=="[2]"]<-"online source"
robertson_2010$year[robertson_2010[3]=="[2]"]<-"NA"
robertson_2010$journal[robertson_2010[3]=="[2]"]<- "http://www.botany.hawaii.edu/"

robertson_2010$author[robertson_2010[3]=="[31]"]<-"edmonds"
robertson_2010$year[robertson_2010[3]=="[31]"]<-"1977"
robertson_2010$journal[robertson_2010[3]=="[31]"]<- "journal of the linnean society of botany"

robertson_2010$author[robertson_2010[3]=="[84]"]<-"smith & baum"
robertson_2010$year[robertson_2010[3]=="[84]"]<-"2006"
robertson_2010$journal[robertson_2010[3]=="[84]"]<- "american journal of botany"

robertson_2010$author[robertson_2010[3]=="[47]"]<-"kaul"
robertson_2010$year[robertson_2010[3]=="[47]"]<-"2005"
robertson_2010$journal[robertson_2010[3]=="[47]"]<- "current science"

robertson_2010$author[robertson_2010[3]=="[85]"]<-"sousa-pena"
robertson_2010$year[robertson_2010[3]=="[85]"]<-"2001"
robertson_2010$journal[robertson_2010[3]=="[85]"]<- "thesis"

robertson_2010$author[robertson_2010[3]=="[18]"]<-"bohs"
robertson_2010$year[robertson_2010[3]=="[18]"]<-"2000"
robertson_2010$journal[robertson_2010[3]=="[18]"]<- "biotropica"







