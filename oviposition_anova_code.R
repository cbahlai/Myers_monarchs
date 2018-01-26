
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


##average across all dates, treating each date like a subsample, calculate the number of plant checks, sum all eggs found#####
oviposition2016.avg.3<-ddply(oviposition2016.avg.2, .(treatment, block), summarize,
                             monarch_eggs.mean=mean(monarch_eggs.per.plant),
                             nplants.checks = (sum(nplants.mean)),
                             monarch_eggs.sum = sum(monarch_eggs.sum),
                             days.checked = length(treatment))






#make block, date, deployment into factors (but not time this time)
oviposition2016.avg.2$block <- as.factor(oviposition2016.avg.2$block)
oviposition2016.avg.2$date <- as.factor(oviposition2016.avg.2$date)
oviposition2016.avg.2$deployment <- as.factor(oviposition2016.avg.2$deployment)



#Negative binomial model
library(pscl)
library(MASS)
library(lme4)
nb.m1 <- glm.nb(monarch_eggs.sum ~ treatment + block + offset(log(nplants.checks)), data=oviposition2016.avg.3)
summary(nb.m1)
anova(nb.m1, test="Rao")
summary(anova(nb.m1, test="Rao"))

#need a holm-adjusted t-test here because Tukey doesn't work with NB models
with(oviposition2016.avg.3, pairwise.t.test(monarch_eggs.sum, treatment, p.adjust.method="holm"))

##make null model without treatment
nb.m2 <- glm.nb(monarch_eggs.sum ~ block + offset(log(nplants.checks)), data=oviposition2016.avg.3)
nb.m3 <- glm.nb(monarch_eggs.sum ~ 1, data=oviposition2016.avg.3)
##ANOVA comparing models
anova(nb.m1, nb.m2)

library(bbmle)
AICctab(nb.m1, nb.m2, nb.m3)

library(MuMIn)
dredge(glm.nb(monarch_eggs.sum ~ treatment + block, data=oviposition2016.avg.3, na.action = na.fail),
       rank = AIC)



###doing more summary stats for plotting###
library(plotrix)

##next we can calculate the grand mean and SEM for each treatment##
oviposition2016.summary<-ddply(oviposition2016.avg.2, .(treatment), summarize,
                                 grand.mean=mean(monarch_eggs.per.plant),
                                 n=length(treatment),
                                 se = std.error(monarch_eggs.per.plant, na.rm))

#here, want to calculate separate means for each deployment to make the faceted bar plot
#first average across dates, so no just means for each deployment we have an average valuce for each block and treatment
#essentially doing the same that we did above for the analysis but retained deployment number
oviposition2016.summary.2<-ddply(oviposition2016.avg.2, .(block, treatment, deployment), summarize,
                              mean.monarch_eggs.per.plant=mean(monarch_eggs.per.plant),
                               n=length(deployment),
                               se = std.error(monarch_eggs.per.plant, na.rm))
#next, average across block so that we have appropriate  n=4 blocks and a mean for each treatment within each deployment
oviposition2016.summary.3<-ddply(oviposition2016.avg.2, .(treatment, deployment), summarize,
                                 grand.mean=mean(monarch_eggs.per.plant),
                                 n=length(deployment),
                                 se = std.error(monarch_eggs.per.plant, na.rm))


#make a bar plot with ggplot
library(ggplot2)
library(ggthemes)
# Error bars represent standard error of the mean
labels <- c("1" = "June", "2" = "July", "3" = "August") #make labeller
ggplot(oviposition2016.summary.3, aes(x=treatment, y=grand.mean)) + 
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



#also want a bar plot combining all deployments
library(ggplot2)
library(ggthemes)
ggplot(oviposition2016.summary, aes(x=treatment, y=grand.mean)) + 
  geom_bar(position=position_dodge(), stat="identity", size=1, fill="white", colour = "black") +
  geom_errorbar(aes(ymin=grand.mean-se, ymax=grand.mean+se), colour="black", width=.2, position=position_dodge(.9)) +
  theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())+
  ggtitle("Oviposition 2016")+
  xlab("")+
  ylab("Monarch eggs/stem/day\n")+
  theme_few()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, .1))
ggsave('ovipostion_nocolor_2016.png', width = 7, height = 3)




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


##average across all dates, treating each date like a subsample, calculate the number of plant checks, sum all eggs found#####
oviposition2017.avg.3<-ddply(oviposition2017.avg.2, .(treatment, block), summarize,
                             monarch_eggs.mean=mean(monarch_eggs.per.plant),
                             nplants.checks = (sum(nplants.mean)),
                             monarch_eggs.sum = sum(monarch_eggs.sum),
                             days.checked = length(treatment))






#make block, date, deployment into factors (but not time this time)
oviposition2017.avg.2$block <- as.factor(oviposition2017.avg.2$block)
oviposition2017.avg.2$date <- as.factor(oviposition2017.avg.2$date)
oviposition2017.avg.2$deployment <- as.factor(oviposition2017.avg.2$deployment)



#Negative binomial model
library(pscl)
library(MASS)
library(lme4)
nb.m1 <- glm.nb(monarch_eggs.sum ~ treatment + block + offset(log(nplants.checks)), data=oviposition2017.avg.3)
summary(nb.m1)
anova(nb.m1, test="Rao")
summary(anova(nb.m1, test="Rao"))

#need a holm-adjusted t-test here because Tukey doesn't work with NB models
with(oviposition2017.avg.3, pairwise.t.test(monarch_eggs.sum, treatment, p.adjust.method="holm"))

##make null model without treatment
nb.m2 <- glm.nb(monarch_eggs.sum ~ block + offset(log(nplants.checks)), data=oviposition2017.avg.3)
nb.m3 <- glm.nb(monarch_eggs.sum ~ 1, data=oviposition2017.avg.3)
##ANOVA comparing models
anova(nb.m1, nb.m2)

library(bbmle)
AICctab(nb.m1, nb.m2, nb.m3)

library(MuMIn)
dredge(glm.nb(monarch_eggs.sum ~ treatment + block, data=oviposition2017.avg.3, na.action = na.fail),
       rank = AIC)



###doing more summary stats for plotting###
library(plotrix)

##next we can calculate the grand mean and SEM for each treatment##
oviposition2017.summary<-ddply(oviposition2017.avg.2, .(treatment), summarize,
                               grand.mean=mean(monarch_eggs.per.plant),
                               n=length(treatment),
                               se = std.error(monarch_eggs.per.plant, na.rm))

#here, want to calculate separate means for each deployment to make the faceted bar plot
#first average across dates, so no just means for each deployment we have an average valuce for each block and treatment
#essentially doing the same that we did above for the analysis but retained deployment number
oviposition2017.summary.2<-ddply(oviposition2017.avg.2, .(block, treatment, deployment), summarize,
                                 mean.monarch_eggs.per.plant=mean(monarch_eggs.per.plant),
                                 n=length(deployment),
                                 se = std.error(monarch_eggs.per.plant, na.rm))
#next, average across block so that we have appropriate  n=4 blocks and a mean for each treatment within each deployment
oviposition2017.summary.3<-ddply(oviposition2017.avg.2, .(treatment, deployment), summarize,
                                 grand.mean=mean(monarch_eggs.per.plant),
                                 n=length(deployment),
                                 se = std.error(monarch_eggs.per.plant, na.rm))


#make a bar plot with ggplot
library(ggplot2)
library(ggthemes)
# Error bars represent standard error of the mean
labels <- c("1" = "June", "2" = "July", "3" = "August") #make labeller
ggplot(oviposition2017.summary.3, aes(x=treatment, y=grand.mean)) + 
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



#also want a bar plot combining all deployments
library(ggplot2)
library(ggthemes)
ggplot(oviposition2017.summary, aes(x=treatment, y=grand.mean)) + 
  geom_bar(position=position_dodge(), stat="identity", size=1, fill="white", colour = "black") +
  geom_errorbar(aes(ymin=grand.mean-se, ymax=grand.mean+se), colour="black", width=.2, position=position_dodge(.9)) +
  theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())+
  ggtitle("Oviposition 2017")+
  xlab("")+
  ylab("Monarch eggs/stem/day\n")+
  theme_few()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, .1))
ggsave('ovipostion_nocolor_2017.png', width = 7, height = 3)

  
  
  
  
  
  
  
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
  