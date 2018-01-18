
oviposition2016<-read.csv(file="oviposition2016.csv", header=TRUE) #read in oviposition2016 file
oviposition2016<-na.omit(oviposition2016) #get rid of na's. There were several incidents when we were unable to count eggs (broken plants, plants were covered by exclosures, etc)


#install and use the ddply function to average across subsamples (individual plants within plant patches)
library(plyr)
oviposition2016.avg <-ddply(oviposition2016, .(treatment, date, time, block, deployment), summarize, 
                                    monarch_eggs.mean=mean(monarch_eggs),
                                    monarch_eggs.sum=sum(monarch_eggs),
                                    nplants=length(monarch_eggs))

#drop the turf treatment
oviposition2016.avg<-oviposition2016.avg[ which(oviposition2016.avg$treatment != 'turf'), ]


#make block, date, time, deployment into factors
oviposition2016.avg$block <- as.factor(oviposition2016.avg$block)
oviposition2016.avg$date <- as.factor(oviposition2016.avg$date)
oviposition2016.avg$time <- as.factor(oviposition2016.avg$time)
oviposition2016.avg$deployment <- as.factor(oviposition2016.avg$deployment)

#Test fit with negative binomial model
library(pscl)
result_covariates.nb <- glm.nb(monarch_eggs.sum ~ block + treatment + deployment, offset =log(nplants), data=oviposition2016.avg)
summary(result_covariates.nb)
anova(result_covariates.nb, test="Rao")
summary(anova(result_covariates.nb, test="Rao"))
#need a holm-adjusted t-test here because Tukey doesn't work with NB models
with(oviposition2016.avg, pairwise.t.test(monarch_eggs.sum, treatment, p.adjust.method="holm"))

#need to use ddply to compute summary stats for plotting
#this one includes deployment number for faceting ggplot later
oviposition2016.summary<-ddply(oviposition2016.avg, .(treatment, deployment), summarize,
                           N=length(monarch_eggs.mean),
                           mean=mean(monarch_eggs.mean),
                           sd   = sd(monarch_eggs.mean),
                           se   = sd / sqrt(N) )
#this one does not include deployment number
oviposition2016.summary.overall<-ddply(oviposition2016.avg, .(treatment), summarize,
                                   N=length(monarch_eggs.mean),
                                   mean=mean(monarch_eggs.mean),
                                   sd   = sd(monarch_eggs.mean),
                                   se   = sd / sqrt(N) )

#this one doesn't include treatment
oviposition2016.summary.by.deployment<- ddply(oviposition2016.avg, .(deployment), summarize,
                                              N=length(monarch_eggs.mean),
                                              mean=mean(monarch_eggs.mean),
                                              sd   = sd(monarch_eggs.mean),
                                              se   = sd / sqrt(N) )

#make a bar plot with ggplot
library(ggplot2)
# Error bars represent standard error of the mean
#cols is my personalized colour palette.
cols <- c("gold2", "firebrick1", "yellowgreen", "mediumpurple")
ggplot(oviposition2016.summary.overall, aes(x=treatment, y=mean, fill=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  scale_color_manual(values=cols)+
  ylab("monarch eggs / plant obervation" )+
  ggtitle("Monarch Butterfly oviposition2016 by Habitat with SE Bars") +
  theme(panel.background = element_blank(), complete=FALSE)

oviposition2016.deployment3.avg<- subset(oviposition2016.avg, deployment==3)

#Doug asked me to redo the ANOVA with only the third egg check
# this is where the most important separations will be observed- sample sizes were very low in first two
oviposition2016.deployment3.avg<- subset(oviposition2016.avg, deployment==3)
#since we used a NB model for the whole dataset, let's just dive right in with an NB on the subset

library(pscl)

result_covariates.deployment3.nb <- glm.nb(monarch_eggs.sum ~ block + treatment, offset= log(nplants), data=oviposition2016.deployment3.avg)
summary(result_covariates.deployment3.nb)
anova(result_covariates.deployment3.nb, test="Rao")
summary(anova(result_covariates.deployment3.nb, test="Rao"))
#need a holm-adjusted t-test here because Tukey doesn't work with NB models
with(oviposition2016.deployment3.avg, pairwise.t.test(monarch_eggs.sum, treatment, p.adjust.method="holm"))

#need to use ddply to compute summary stats for plotting

oviposition2016.deployment3.summary<-ddply(oviposition2016.deployment3.avg, .(treatment), summarize,
                                       N=length(monarch_eggs.mean),
                                       mean=mean(monarch_eggs.mean),
                                       sd   = sd(monarch_eggs.mean),
                                       se   = sd / sqrt(N) )

head(oviposition2016.deployment3.summary)

#make a bar plot with ggplot
# Error bars represent standard error of the mean
#cols is my personalized colour palette.
cols <- c("gold2", "firebrick1", "yellowgreen", "mediumpurple")
ggplot(oviposition2016.deployment3.summary, aes(x=treatment, y=mean, fill=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  scale_color_manual(values=cols)+
  ylab("monarch eggs / plant obervation" )+
  ggtitle("Monarch Butterfly oviposition2016.deployment3 by Habitat with SE Bars") +
  theme(panel.background = element_blank())+
theme(panel.background = element_blank(), axis.text.x = element_text(face="bold", 
                                                                     size=14),
      axis.text.y = element_text(face="bold", size=14))

#faceted bar chart!
labels <- c("1" = "June", "2" = "July", "3" = "August") #make labeller

ggplot(oviposition2016.summary, aes(x=treatment, y=mean, colour=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=position_dodge(.9)) +
  scale_color_manual(values=cols)+
  ylab("monarch eggs / plant observation" )+
  ggtitle("Monarch Butterfly oviposition2016 by Habitat with SE Bars") +
  theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())+
  facet_grid(~deployment, labeller=labeller(deployment = labels))


















#####for days with more than one egg check, doug wants me to add up all the eggs and divide by the 
######number of plants in the plot (or average if it changed). so redoing the former code from above to 
### do this. also doing the same for 2017
####USE BELOW FOR PAPER

oviposition2016<-read.csv(file="oviposition2016.csv", header=TRUE) #read in oviposition2016 file
oviposition2016<-na.omit(oviposition2016) #get rid of na's. There were several incidents when we were unable to count eggs (broken plants, plants were covered by exclosures, etc)

#install and use the ddply function to average across subsamples (individual plants within plant patches)
library(plyr)
oviposition2016.avg <-ddply(oviposition2016, .(treatment, date, time, block, deployment), summarize, 
                            monarch_eggs.mean=mean(monarch_eggs),
                            monarch_eggs.sum=sum(monarch_eggs),
                            nplants=length(monarch_eggs))


#drop the turf treatment
oviposition2016.avg<-oviposition2016.avg[ which(oviposition2016.avg$treatment != 'turf'), ]

#average plants checked per day and sum all the eggs found per day
oviposition2016.avg.2 <-ddply(oviposition2016.avg, .(treatment, date, block, deployment), summarize, 
                              nplants.mean=mean(nplants),
                              monarch_eggs.sum=sum(monarch_eggs.sum))

#divide number of eggs seen in a day by average number of plants present that day
oviposition2016.avg.2 <-ddply(oviposition2016.avg.2, .(treatment, date, block, deployment, monarch_eggs.sum, nplants.mean), summarize,
                              monarch_eggs.mean=monarch_eggs.sum/nplants.mean)


#then, repeating as above, make block, date, deployment into factors (but not time this time)
oviposition2016.avg.2$block <- as.factor(oviposition2016.avg.2$block)
oviposition2016.avg.2$date <- as.factor(oviposition2016.avg.2$date)
oviposition2016.avg.2$deployment <- as.factor(oviposition2016.avg.2$deployment)

#negative binomial model fit
library(lme4)
result_covariates.nb <- glmer.nb(monarch_eggs.sum ~ block + treatment + deployment + (1|block/treatment), offset=log(nplants.mean), data=oviposition2016.avg.2)
summary(result_covariates.nb)
anova(result_covariates.nb, test="Rao")
summary(anova(result_covariates.nb, test="Rao"))
#need a holm-adjusted t-test here because Tukey doesn't work with NB models
with(oviposition2016.avg, pairwise.t.test(monarch_eggs.sum, treatment, p.adjust.method="holm"))





#need to use ddply to compute summary stats for plotting
#this one includes deployment number for faceting ggplot later
oviposition2016.summary<-ddply(oviposition2016.avg.2, .(treatment, deployment), summarize,
                               N=length(monarch_eggs.mean),
                               mean=mean(monarch_eggs.mean),
                               sd   = sd(monarch_eggs.mean),
                               se   = sd / sqrt(N) )
#this one does not include deployment number
oviposition2016.summary.overall<-ddply(oviposition2016.avg.2, .(treatment), summarize,
                                       N=length(monarch_eggs.mean),
                                       mean=mean(monarch_eggs.mean),
                                       sd   = sd(monarch_eggs.mean),
                                       se   = sd / sqrt(N) )


#make a bar plot with ggplot
library(ggplot2)
library(ggthemes)
# Error bars represent standard error of the mean
#cols is my personalized colour palette. it doesn't seem to work any more

cols <- c("gold2", "firebrick1", "yellowgreen", "mediumpurple")
ggplot(oviposition2016.summary.overall, aes(x=treatment, y=mean, colour=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9), colour="black") +
  scale_color_manual(values=cols)+
  ylim(0,.1)+
  theme(panel.background = element_blank(), text = element_text(size=20, colour = "black"), complete=FALSE)











###################OK! trying the above for 2017 

oviposition2017<-read.csv(file="oviposition2017.csv", header=TRUE) #read in oviposition2017 file
oviposition2017<-na.omit(oviposition2017) #get rid of na's. There were several incidents when we were unable to count eggs (broken plants, plants were covered by exclosures, etc)


#install and use the ddply function to average across subsamples (individual plants within plant patches)
library(plyr)
oviposition2017.avg <-ddply(oviposition2017, .(treatment, date, time, block, deployment), summarize, 
                            monarch_eggs.mean=mean(monarch_eggs),
                            monarch_eggs.sum=sum(monarch_eggs),
                            nplants=length(monarch_eggs))


#average plants checked per day and sum all the eggs found per day
oviposition2017.avg.2 <-ddply(oviposition2017.avg, .(treatment, date, block, deployment), summarize, 
                              nplants.mean=mean(nplants),
                              monarch_eggs.sum=sum(monarch_eggs.sum))

#divide number of eggs seen in a day by average number of plants present that day
oviposition2017.avg.2 <-ddply(oviposition2017.avg.2, .(treatment, date, block, deployment, monarch_eggs.sum, nplants.mean), summarize,
                              monarch_eggs.mean=monarch_eggs.sum/nplants.mean)


#then, repeating as above, make block, date, deployment into factors (but not time this time)
oviposition2017.avg.2$block <- as.factor(oviposition2017.avg.2$block)
oviposition2017.avg.2$date <- as.factor(oviposition2017.avg.2$date)
oviposition2017.avg.2$deployment <- as.factor(oviposition2017.avg.2$deployment)


#do negative binomial model
library(pscl)
result_covariates.nb <- glm.nb(monarch_eggs.sum ~ block + treatment + deployment, offset(log(nplants.mean)), data=oviposition2017.avg.2)
summary(result_covariates.nb)
anova(result_covariates.nb, test="Rao")
summary(anova(result_covariates.nb, test="Rao"))
#need a holm-adjusted t-test here because Tukey doesn't work with NB models
with(oviposition2017.avg.2, pairwise.t.test(monarch_eggs.sum, treatment, p.adjust.method="holm"))







#need to use ddply to compute summary stats for plotting
#this one includes deployment number for faceting ggplot later
oviposition2017.summary<-ddply(oviposition2017.avg.2, .(treatment, deployment), summarize,
                               N=length(monarch_eggs.mean),
                               mean=mean(monarch_eggs.mean),
                               sd   = sd(monarch_eggs.mean),
                               se   = sd / sqrt(N) )
#this one does not include deployment number
oviposition2017.summary.overall<-ddply(oviposition2017.avg.2, .(treatment), summarize,
                                       N=length(monarch_eggs.mean),
                                       mean=mean(monarch_eggs.mean),
                                       sd   = sd(monarch_eggs.mean),
                                       se   = sd / sqrt(N) )


#make a bar plot with ggplot
library(ggplot2)
library(ggthemes)
# Error bars represent standard error of the mean
#cols is my personalized colour palette. i can't get it to work any more, so i took out the argument

cols2017 <- c("gold2", "firebrick1", "yellowgreen", "mediumpurple" )
ggplot(oviposition2017.summary.overall, aes(x=treatment, y=mean, colour=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols2017) +
  ylim(0,.1)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9), colour="black") +
  scale_color_manual(values=cols2017)+
  theme(panel.background = element_blank(), text = element_text(size=20, colour = "black"), complete=FALSE)









###### more ggplotting. can he make a faceted chart again? let's find out

#faceted bar chart!
cols <- c( "firebrick1", "gold2","yellowgreen", "mediumpurple" )
labels <- c("1" = "June", "2" = "July", "3" = "August") #make labeller

ggplot(oviposition2016.summary, aes(x=treatment, y=mean, colour=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=position_dodge(.9)) +
  scale_color_manual(values=cols)+
  theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())+
  facet_grid(~deployment, labeller=labeller(deployment = labels))+
  xlab("")+
ylab("")+
  theme_few()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, .3))
ggsave('faceted_ovipostion_2016.png', width = 7, height = 3)




#for 2017
cols2017 <- c("firebrick1","gold2",  "yellowgreen", "mediumpurple" )
labels <- c("1" = "June", "2" = "July", "3" = "August") #make labeller

ggplot(oviposition2017.summary, aes(x=treatment, y=mean, colour=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols2017) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=position_dodge(.9)) +
  scale_color_manual(values=cols2017)+
  theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())+
  facet_grid(~deployment, labeller=labeller(deployment = labels))+
  xlab("")+
  ylab("")+
  theme_few()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, .3))
  ggsave('faceted_ovipostion_2017.png', width = 7, height = 3)

  
  
  
  

  #faceted bar charts with no colors
  labels <- c("1" = "June", "2" = "July", "3" = "August") #make labeller
  
  ggplot(oviposition2016.summary, aes(x=treatment, y=mean)) + 
    geom_bar(position=position_dodge(), stat="identity", size=1, fill="white", colour = "black") +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=position_dodge(.9)) +
    theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())+
    facet_grid(~deployment, labeller=labeller(deployment = labels))+
    ggtitle("Oviposition 2016")+
    xlab("")+
    ylab("Monarch eggs/stem/day\n")+
    theme_few()+
    scale_y_continuous(expand = c(0, 0), limits = c(0, .3))
  ggsave('faceted_ovipostion_nocolor_2016.png', width = 7, height = 3)
  
  
  
  #for 2017
  cols2017 <- c("firebrick1","gold2",  "yellowgreen", "mediumpurple" )
  labels <- c("1" = "June", "2" = "July", "3" = "August") #make labeller
  
  ggplot(oviposition2017.summary, aes(x=treatment, y=mean, colour=treatment)) + 
    geom_bar(position=position_dodge(), stat="identity", size=1, fill="white", colour = "black") +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=position_dodge(.9)) +
    scale_color_manual(values=cols2017)+
    theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())+
    facet_grid(~deployment, labeller=labeller(deployment = labels))+
    ggtitle("Oviposition 2017")+
    xlab("")+
    ylab("Monarch eggs/stem/day\n")+
    theme_few()+
    scale_y_continuous(expand = c(0, 0), limits = c(0, .3))
  ggsave('faceted_ovipostion_nocolor_2017.png', width = 7, height = 3)

  
  
  
  
  
  
  
  ###working on summary stats for paper
  mean.by.deployment <
  
  
