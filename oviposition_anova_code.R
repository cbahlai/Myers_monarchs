
oviposition<-read.csv(file="oviposition.csv", header=TRUE) #read in oviposition file

oviposition<-na.omit(oviposition) #get rid of na's. There were several incidents when we were unable to count eggs (broken plants, plants were covered by exclosures, etc)

#install and use the ddply function to average across subsamples (individual plants within plant patches)
library(plyr)
oviposition.avg <-ddply(oviposition, .(treatment, date, time, block, deployment), summarize, monarch_eggs.mean=mean(monarch_eggs))

#make block, date, time, deployment into a factor
oviposition.avg$block <- as.factor(oviposition.avg$block)
oviposition.avg$date <- as.factor(oviposition.avg$date)
oviposition.avg$time <- as.factor(oviposition.avg$time)
oviposition.avg$deployment <- as.factor(oviposition.avg$deployment)

#do the anova using glm function
result <- glm(monarch_eggs.mean ~ block + treatment, data=oviposition.avg)
aov(result)
summary(aov(result))
TukeyHSD(aov(result))

#make the mean plot
library(gplots)
plotmeans(monarch_eggs.mean~treatment,xlab="Habitat Treatment",
          ylab="Mean Eggs/Plant Obs", main="Mean Plot Across Entire Field Season\nwith 95% CI", data=oviposition.avg)

#make date and time variables factors
as.factor(oviposition.avg$date)
as.factor(oviposition.avg$time)

#rerun anova with date and time as covariates
result_covariates <- glm(monarch_eggs.mean ~ block + treatment + date + time, data=oviposition.avg)
aov(result_covariates)
summary(aov(result_covariates))
TukeyHSD(aov(result_covariates))
#rerun this with poisson distribution
result_covariates.poisson <- glm(monarch_eggs.mean ~ block + treatment + date + time, data=oviposition.avg, family = poisson())
aov(result_covariates.poisson)
summary(aov(result_covariates.poisson))
TukeyHSD(aov(result_covariates.poisson))

#need to use ddply to compute summary stats for plotting

oviposition.summary<-ddply(oviposition.avg, .(treatment, deployment), summarize,
                           N=length(monarch_eggs.mean),
                           mean=mean(monarch_eggs.mean),
                           sd   = sd(monarch_eggs.mean),
                           se   = sd / sqrt(N) )

head(oviposition.summary)

#make a bar plot with ggplot
# Error bars represent standard error of the mean
#cols is my personalized colour palette.
cols <- c("gold2", "firebrick1", "yellowgreen", "mediumpurple", "dodgerblue2" )
ggplot(oviposition.summary, aes(x=treatment, y=mean, fill=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  scale_color_manual(values=cols)+
  ylab("monarch eggs / plant obervation" )+
  ggtitle("Monarch Butterfly oviposition by Habitat with SE Bars") +
  theme(panel.background = element_blank())









#Doug asked me to redo the ANOVA with only the third egg check
oviposition.deployment3<-subset(oviposition, deployment==3)

#install and use the ddply function to average across subsamples (individual plants within plant patches)
oviposition.deployment3.avg <-ddply(oviposition.deployment3, .(treatment, date, time, block), summarize, monarch_eggs.mean=mean(monarch_eggs))

#make block, date, and time into a factor
oviposition.deployment3.avg$block <- as.factor(oviposition.deployment3.avg$block)
oviposition.deployment3.avg$date <- as.factor(oviposition.deployment3.avg$date)
oviposition.deployment3.avg$time <- as.factor(oviposition.deployment3.avg$time)

#do the anova using glm function
result.deployment3 <- glm(monarch_eggs.mean ~ block + treatment, data=oviposition.deployment3.avg)
aov(result.deployment3)
summary(aov(result.deployment3))
TukeyHSD(aov(result.deployment3))

#make the mean plot
library(gplots)
plotmeans(monarch_eggs.mean~treatment,xlab="Habitat Treatment",
          ylab="Mean Eggs/Plant Obs", main="Mean Plot Across Entire Field Season\nwith 95% CI", data=oviposition.deployment3.avg)

#make date and time variables factors
as.factor(oviposition.deployment3.avg$date)
as.factor(oviposition.deployment3.avg$time)

#rerun anova with date and time as covariates
result_covariates.deployment3 <- glm(monarch_eggs.mean ~ block + treatment + date + time, data=oviposition.deployment3.avg)
aov(result_covariates.deployment3)
summary(aov(result_covariates.deployment3))
TukeyHSD(aov(result_covariates.deployment3))
#rerun this with poisson distribution
result_covariates.deployment3.poisson <- glm(monarch_eggs.mean ~ block + treatment + date + time, data=oviposition.deployment3.avg, family = poisson())
aov(result_covariates.deployment3.poisson)
summary(aov(result_covariates.deployment3.poisson))
TukeyHSD(aov(result_covariates.deployment3.poisson))

#need to use ddply to compute summary stats for plotting

oviposition.deployment3.summary<-ddply(oviposition.deployment3.avg, .(treatment), summarize,
                                       N=length(monarch_eggs.mean),
                                       mean=mean(monarch_eggs.mean),
                                       sd   = sd(monarch_eggs.mean),
                                       se   = sd / sqrt(N) )

head(oviposition.deployment3.summary)

#make a bar plot with ggplot
# Error bars represent standard error of the mean
#cols is my personalized colour palette.
cols <- c("gold2", "firebrick1", "yellowgreen", "mediumpurple", "dodgerblue2" )
ggplot(oviposition.deployment3.summary, aes(x=treatment, y=mean, fill=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  scale_color_manual(values=cols)+
  ylab("monarch eggs / plant obervation" )+
  ggtitle("Monarch Butterfly oviposition.deployment3 by Habitat with SE Bars") +
  theme(panel.background = element_blank())





#making individual bar plots for deployments 1 & 2
#trying to facet it...
cols <- c("gold2", "firebrick1", "yellowgreen", "mediumpurple", "dodgerblue2" )
ggplot(oviposition.summary, aes(x=treatment, y=mean, fill=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  scale_color_manual(values=cols)+
  ylab("monarch eggs / plant obervation" )+
  ggtitle("Monarch Butterfly oviposition by Habitat with SE Bars") +
  theme(panel.background = element_blank())+
  facet_wrap(~deployment)


#doing it the long way...
oviposition.deployment1<-subset(oviposition, deployment==1)
oviposition.deployment2<-subset(oviposition, deployment==2)

#install and use the ddply function to average across subsamples (individual plants within plant patches)
oviposition.deployment1.avg <-ddply(oviposition.deployment1, .(treatment, date, time, block), summarize, monarch_eggs.mean=mean(monarch_eggs))
oviposition.deployment2.avg <-ddply(oviposition.deployment2, .(treatment, date, time, block), summarize, monarch_eggs.mean=mean(monarch_eggs))

#make block, date, and time into a factor
oviposition.deployment1.avg$block <- as.factor(oviposition.deployment1.avg$block)
oviposition.deployment1.avg$date <- as.factor(oviposition.deployment1.avg$date)
oviposition.deployment1.avg$time <- as.factor(oviposition.deployment1.avg$time)

oviposition.deployment2.avg$block <- as.factor(oviposition.deployment2.avg$block)
oviposition.deployment2.avg$date <- as.factor(oviposition.deployment2.avg$date)
oviposition.deployment2.avg$time <- as.factor(oviposition.deployment2.avg$time)

#need to use ddply to compute summary stats for plotting

oviposition.deployment1.summary<-ddply(oviposition.deployment1.avg, .(treatment), summarize,
                                       N=length(monarch_eggs.mean),
                                       mean=mean(monarch_eggs.mean),
                                       sd   = sd(monarch_eggs.mean),
                                       se   = sd / sqrt(N) )

oviposition.deployment2.summary<-ddply(oviposition.deployment2.avg, .(treatment), summarize,
                                       N=length(monarch_eggs.mean),
                                       mean=mean(monarch_eggs.mean),
                                       sd   = sd(monarch_eggs.mean),
                                       se   = sd / sqrt(N) )

#make a bar plot with ggplot
#cols is my personalized colour palette.
cols <- c("gold2", "firebrick1", "yellowgreen", "mediumpurple", "dodgerblue2" )
ggplot(oviposition.deployment1.summary, aes(x=treatment, y=mean, fill=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols) +
  scale_color_manual(values=cols)+
  ylab("monarch eggs / plant obervation" )+
  ggtitle("Monarch Butterfly oviposition.deployment1 by Habitat with SE Bars") +
  theme(panel.background = element_blank())

ggplot(oviposition.deployment2.summary, aes(x=treatment, y=mean, fill=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  scale_color_manual(values=cols)+
  ylab("monarch eggs / plant obervation" )+
  ggtitle("Monarch Butterfly oviposition.deployment2 by Habitat with SE Bars") +
  theme(panel.background = element_blank())


#faceted bar chart!

as.factor(oviposition.summary$deployment)
labels <- c("1" = "June", "2" = "July", "3" = "August") #make labeller

ggplot(oviposition.summary, aes(x=treatment, y=mean, colour=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=position_dodge(.9)) +
  scale_color_manual(values=cols)+
  ylab("monarch eggs / plant obervation" )+
  ggtitle("Monarch Butterfly oviposition by Habitat with SE Bars") +
  theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())+
  facet_grid(~deployment, labeller=labeller(deployment = labels))
