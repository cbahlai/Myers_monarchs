

#read in 2016 data
oviposition2016<-read.csv(file="oviposition2016.csv", header=TRUE) #read in oviposition2016 file
oviposition2016<-na.omit(oviposition2016) #get rid of na's. There were several incidents when we were unable to count eggs (broken plants, plants were covered by exclosures, etc)

#drop the turf treatment
oviposition2016<-oviposition2016[ which(oviposition2016$treatment != 'turf'), ]

#####for days with more than one egg check, doug wants me to add up all the eggs and divide by the 
######number of plants in the plot (or average if it changed).
### do this. also doing the same for 2017
####USE BELOW FOR PAPER

#install and use the ddply function to find the sum of the number of eggs/patch/check and the number of plants present in each check
library(plyr)
oviposition2016.avg <-ddply(oviposition2016, .(treatment, date, time, block, deployment), summarize, 
                            monarch_eggs.sum=sum(monarch_eggs),
                            nplants=length(monarch_eggs))

#average plants checked per day and sum all the eggs found per day
oviposition2016.avg.2 <-ddply(oviposition2016.avg, .(treatment, date, block, deployment), summarize, 
                              nplants.mean=mean(nplants),
                              monarch_eggs.sum=sum(monarch_eggs.sum))

#divide number of eggs seen in a day by average number of plants present that day
oviposition2016.avg.2 <-ddply(oviposition2016.avg.2, .(treatment, date, block, deployment, monarch_eggs.sum, nplants.mean), summarize,
                              monarch_eggs.per.plant=monarch_eggs.sum/nplants.mean)


#then, repeating as above, make block, date, deployment into factors (but not time this time)
oviposition2016.avg.2$block <- as.factor(oviposition2016.avg.2$block)
oviposition2016.avg.2$date <- as.factor(oviposition2016.avg.2$date)
oviposition2016.avg.2$deployment <- as.factor(oviposition2016.avg.2$deployment)

#Test fit with negative binomial model
library(pscl)
result_covariates.nb <- glm.nb(monarch_eggs.sum ~ block + treatment + deployment, offset=log(nplants.mean), data=oviposition2016.avg.2)
summary(result_covariates.nb)
anova(result_covariates.nb, test="Rao")
summary(anova(result_covariates.nb, test="Rao"))
#need a holm-adjusted t-test here because Tukey doesn't work with NB models
with(oviposition2016.avg, pairwise.t.test(monarch_eggs.sum, treatment, p.adjust.method="holm"))

###doing more summary stats for plotting###
library(plyr)
library(plotrix)
#first need to average across all dates, treating each date like a subsample#####
oviposition2016.avg.3<-ddply(oviposition2016.avg.2, .(treatment, deployment, block), summarize,
                               monarch_eggs.mean=mean(monarch_eggs.per.plant))

##next we can calculate the grand mean and SEM for each treatment##
oviposition2016.summary<-ddply(oviposition2016.avg.3, .(treatment), summarize,
                                 grand.mean=mean(monarch_eggs.mean),
                                 n=length(deployment),
                                 se = std.error(monarch_eggs.mean, na.rm))

#this one includes deployment number for faceting ggplot
oviposition2016.summary.2<-ddply(oviposition2016.avg.3, .(treatment, deployment), summarize,
                               grand.mean=mean(monarch_eggs.mean),
                               n=length(deployment),
                               se = std.error(monarch_eggs.mean, na.rm))


#make a bar plot with ggplot
library(ggplot2)
library(ggthemes)
# Error bars represent standard error of the mean
labels <- c("1" = "June", "2" = "July", "3" = "August") #make labeller
ggplot(oviposition2016.summary.2, aes(x=treatment, y=grand.mean)) + 
  geom_bar(position=position_dodge(), stat="identity", size=1, fill="white", colour = "black") +
  geom_errorbar(aes(ymin=grand.mean-se, ymax=grand.mean+se), colour="black", width=.2, position=position_dodge(.9)) +
  theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())+
  facet_grid(~deployment, labeller=labeller(deployment = labels))+
  ggtitle("Oviposition 2016")+
  xlab("")+
  ylab("Monarch eggs/stem/day\n")+
  theme_few()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, .4))
ggsave('faceted_ovipostion_nocolor_2016.png', width = 7, height = 3)




#####making table for manuscript
library(plotrix)
#treat each date measurement of eggs per stem as a subsample and average across dates to get a grand mean
table2016.1 <-ddply(oviposition2016.avg.2, .(treatment, deployment, block), summarize, grand.mean=mean(monarch_eggs.per.plant))
#now average within deployment. n = 16, beacuse there are 16 plots per deployment
table2016.2 <-ddply(table2016.1, .(deployment), summarize, mean=mean(grand.mean), sem=std.error(grand.mean, na.rm))


























###################OK! trying the above for 2017 

#read in 2017 data
oviposition2017<-read.csv(file="oviposition2017.csv", header=TRUE) #read in oviposition2017 file
oviposition2017<-na.omit(oviposition2017) #get rid of na's. There were several incidents when we were unable to count eggs (broken plants, plants were covered by exclosures, etc)

#drop the turf treatment
oviposition2017<-oviposition2017[ which(oviposition2017$treatment != 'turf'), ]

#####for days with more than one egg check, doug wants me to add up all the eggs and divide by the 
######number of plants in the plot (or average if it changed).
### do this. also doing the same for 2017
####USE BELOW FOR PAPER

#install and use the ddply function to find the sum of the number of eggs/patch/check and the number of plants present in each check
library(plyr)
oviposition2017.avg <-ddply(oviposition2017, .(treatment, date, time, block, deployment), summarize, 
                            monarch_eggs.sum=sum(monarch_eggs),
                            nplants=length(monarch_eggs))

#average plants checked per day and sum all the eggs found per day
oviposition2017.avg.2 <-ddply(oviposition2017.avg, .(treatment, date, block, deployment), summarize, 
                              nplants.mean=mean(nplants),
                              monarch_eggs.sum=sum(monarch_eggs.sum))

#divide number of eggs seen in a day by average number of plants present that day
oviposition2017.avg.2 <-ddply(oviposition2017.avg.2, .(treatment, date, block, deployment, monarch_eggs.sum, nplants.mean), summarize,
                              monarch_eggs.per.plant=monarch_eggs.sum/nplants.mean)


#then, repeating as above, make block, date, deployment into factors (but not time this time)
oviposition2017.avg.2$block <- as.factor(oviposition2017.avg.2$block)
oviposition2017.avg.2$date <- as.factor(oviposition2017.avg.2$date)
oviposition2017.avg.2$deployment <- as.factor(oviposition2017.avg.2$deployment)

#Test fit with negative binomial model
library(pscl)
result_covariates.nb <- glm.nb(monarch_eggs.sum ~ block + treatment + deployment, offset=log(nplants.mean), data=oviposition2017.avg.2)
summary(result_covariates.nb)
anova(result_covariates.nb, test="Rao")
summary(anova(result_covariates.nb, test="Rao"))
#need a holm-adjusted t-test here because Tukey doesn't work with NB models
with(oviposition2017.avg, pairwise.t.test(monarch_eggs.sum, treatment, p.adjust.method="holm"))

###doing more summary stats for plotting###
library(plyr)
library(plotrix)
#first need to average across all dates, treating each date like a subsample#####
oviposition2017.avg.3<-ddply(oviposition2017.avg.2, .(treatment, deployment, block), summarize,
                             monarch_eggs.mean=mean(monarch_eggs.per.plant))

##next we can calculate the grand mean and SEM for each treatment##
oviposition2017.summary<-ddply(oviposition2017.avg.3, .(treatment), summarize,
                               grand.mean=mean(monarch_eggs.mean),
                               n=length(deployment),
                               se = std.error(monarch_eggs.mean, na.rm))

#this one includes deployment number for faceting ggplot
oviposition2017.summary.2<-ddply(oviposition2017.avg.3, .(treatment, deployment), summarize,
                                 grand.mean=mean(monarch_eggs.mean),
                                 n=length(deployment),
                                 se = std.error(monarch_eggs.mean, na.rm))


#make a bar plot with ggplot
library(ggplot2)
library(ggthemes)
labels <- c("1" = "June", "2" = "July", "3" = "August") #make labeller
# Error bars represent standard error of the mean
ggplot(oviposition2017.summary.2, aes(x=treatment, y=grand.mean)) + 
  geom_bar(position=position_dodge(), stat="identity", size=1, fill="white", colour = "black") +
  geom_errorbar(aes(ymin=grand.mean-se, ymax=grand.mean+se), colour="black", width=.2, position=position_dodge(.9)) +
  theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())+
  facet_grid(~deployment, labeller=labeller(deployment = labels))+
  ggtitle("Oviposition 2017")+
  xlab("")+
  ylab("Monarch eggs/stem/day\n")+
  theme_few()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, .4))
ggsave('faceted_ovipostion_nocolor_2017.png', width = 7, height = 3)








#####making table for manuscript
library(plotrix)
#treat each date measurement of eggs per stem as a subsample and average across dates to get a grand mean
table2017.1 <-ddply(oviposition2017.avg.2, .(treatment, deployment, block), summarize, grand.mean=mean(monarch_eggs.mean))
#now average within deployment. n = 16, beacuse there are 16 plots per deployment
table2017.2 <-ddply(table2017.1, .(deployment), summarize, mean=mean(grand.mean), sem=std.error(grand.mean, na.rm))








###### having trouble doing the color faceted plot again


#for 2017
cols2017 <- c("firebrick1","gold2",  "yellowgreen", "mediumpurple" )
labels <- c("1" = "June", "2" = "July", "3" = "August") #make labeller

ggplot(oviposition2017.summary.2, aes(x=treatment, y=grand.mean, colour=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols2017) +
  geom_errorbar(aes(ymin=grand.mean-se, ymax=grand.mean+se), colour="black", width=.2, position=position_dodge(.9)) +
  scale_color_manual(values=cols2017)+
  theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())+
  facet_grid(~deployment, labeller=labeller(deployment = labels))+
  xlab("")+
  ylab("")+
  theme_few()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, .3))
  ggsave('faceted_ovipostion_2017.png', width = 7, height = 3)

  
  
  
  


  
  
  
  
  
  
  
  
  #####making summary stats for adult observations during oviposition experiment#######
  #read in data
  adult_obs<-read.csv(file="oviposition_adult_observations.csv", header=TRUE) #read in file
  #make a new column for adults per hour
  adult_obs["adults.per.hour"] <- NA #make new column
  adult_obs$adults.per.hour <- adult_obs$adults_observed/adult_obs$number_hours_checking
  
  library(plotrix)
  library(plyr)
  adult_table<- ddply(adult_obs, .(year, deployment), summarize,
                      n.days = length(adults_observed),
                      adults.hour.mean=mean(adults.per.hour),
                      sem = std.error(adults.per.hour, na.rm)
  )
  