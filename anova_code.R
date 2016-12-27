oviposition<-read.csv(file="oviposition.csv", header=TRUE) #read in oviposition file
head(oviposition) #check it out

oviposition<-na.omit(oviposition) #get rid of na's. There were several incidents when we were unable to count eggs (broken plants, plants were covered by exclosures, etc)


#install and use the summaryBy function to average across subsamples (plant triplets and individual plants)
#install.packages("doBy")
library(doBy)
avg <- summaryBy(monarch_eggs ~ block + treatment + date + time, FUN=mean, data=oviposition)
avg 

#make block, date, and time into a factor
avg$block <- as.factor(avg$block)
avg$date <- as.factor(avg$date)
avg$time <- as.factor(avg$time)

#do the anova using lm function
result <- lm(monarch_eggs.mean ~ block + treatment, data=avg)
aov(result)
summary(aov(result))
TukeyHSD(aov(result))

install.packages("gplots") #install a plotting package
library(gplots)

plotmeans(monarch_eggs.mean~treatment,xlab="Habitat Treatment",
          ylab="Monarch eggs per plant observation", main="Mean Plot of Monarch Eggs/Plant Obs", data=avg)