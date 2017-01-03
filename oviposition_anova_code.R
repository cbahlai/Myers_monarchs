
oviposition<-read.csv(file="oviposition.csv", header=TRUE) #read in oviposition file
oviposition<-na.omit(oviposition) #get rid of na's. There were several incidents when we were unable to count eggs (broken plants, plants were covered by exclosures, etc)


#install and use the ddply function to average across subsamples (individual plants within plant patches)
library(plyr)
oviposition.avg <-ddply(oviposition, .(treatment, date, time, block, deployment), summarize, 
                                    monarch_eggs.mean=mean(monarch_eggs),
                                    monarch_eggs.sum=sum(monarch_eggs),
                                    nplants=length(monarch_eggs))

#make block, date, time, deployment into factors
oviposition.avg$block <- as.factor(oviposition.avg$block)
oviposition.avg$date <- as.factor(oviposition.avg$date)
oviposition.avg$time <- as.factor(oviposition.avg$time)
oviposition.avg$deployment <- as.factor(oviposition.avg$deployment)

#do the anova using glm function
result <- glm(monarch_eggs.sum ~ block + treatment, offset=nplants, data=oviposition.avg)
summary(result)
aov(result)
summary(aov(result))
TukeyHSD(aov(result))

#rerun anova with appropriate data as factors, with sum of eggs as response variable
result_covariates <- glm(monarch_eggs.sum ~ block + treatment, offset=nplants, data=oviposition.avg)
summary(result_covariates)
aov(result_covariates)
summary(aov(result_covariates))
TukeyHSD(aov(result_covariates))
#rerun this with poisson distribution
result_covariates.poisson <- glm(monarch_eggs.sum ~ block + treatment, offset=nplants, data=oviposition.avg, family = "poisson")
summary(result_covariates.poisson)
aov(result_covariates.poisson)
summary(aov(result_covariates.poisson))
TukeyHSD(aov(result_covariates.poisson))
#residual deviance is high. Switch to a negative binomial model
library(pscl)
result_covariates.nb <- glm.nb(monarch_eggs.sum ~ block + treatment +offset(nplants), data=oviposition.avg)
summary(result_covariates.nb)
aov(result_covariates.nb)
summary(aov(result_covariates.nb))
#need a holm-adjusted t-test here because Tukey doesn't work with NB models
with(oviposition.avg, pairwise.t.test(monarch_eggs.sum, treatment, p.adjust.method="holm"))

#need to use ddply to compute summary stats for plotting
#this one includes deployment number for faceting ggplot later
oviposition.summary<-ddply(oviposition.avg, .(treatment, deployment), summarize,
                           N=length(monarch_eggs.mean),
                           mean=mean(monarch_eggs.mean),
                           sd   = sd(monarch_eggs.mean),
                           se   = sd / sqrt(N) )
#this one does not include deployment number
oviposition.summary.overall<-ddply(oviposition.avg, .(treatment), summarize,
                                   N=length(monarch_eggs.mean),
                                   mean=mean(monarch_eggs.mean),
                                   sd   = sd(monarch_eggs.mean),
                                   se   = sd / sqrt(N) )

#make a bar plot with ggplot
library(ggplot2)
# Error bars represent standard error of the mean
#cols is my personalized colour palette.
cols <- c("gold2", "firebrick1", "yellowgreen", "mediumpurple", "dodgerblue2" )
ggplot(oviposition.summary.overall, aes(x=treatment, y=mean, fill=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  scale_color_manual(values=cols)+
  ylab("monarch eggs / plant obervation" )+
  ggtitle("Monarch Butterfly oviposition by Habitat with SE Bars") +
  theme(panel.background = element_blank())

oviposition.deployment3.avg<- subset(oviposition.avg, deployment==3)

#Doug asked me to redo the ANOVA with only the third egg check
# this is where the most important separations will be observed- sample sizes were very low in first two
oviposition.deployment3.avg<- subset(oviposition.avg, deployment==3)

#do the anova using glm function
result.deployment3 <- glm(monarch_eggs.sum ~ block + treatment, offset=nplants, data=oviposition.deployment3.avg)
summary(result.deployment3)
aov(result.deployment3)
summary(aov(result.deployment3))
TukeyHSD(aov(result.deployment3))

#rerun anova with appropriate data as factors, with sum of eggs as response variable
result_covariates.deployment3 <- glm(monarch_eggs.sum ~ block + treatment, offset=nplants, data=oviposition.deployment3.avg)
summary(result_covariates.deployment3)
aov(result_covariates.deployment3)
summary(aov(result_covariates.deployment3))
TukeyHSD(aov(result_covariates.deployment3))
#rerun this with poisson distribution
result_covariates.deployment3.poisson <- glm(monarch_eggs.sum ~ block + treatment, offset=nplants, data=oviposition.deployment3.avg, family = "poisson")
summary(result_covariates.deployment3.poisson)
aov(result_covariates.deployment3.poisson)
summary(aov(result_covariates.deployment3.poisson))
TukeyHSD(aov(result_covariates.deployment3.poisson))
#residual deviance is high. Switch to a negative binomial model
library(pscl)

result_covariates.deployment3.nb <- glm.nb(monarch_eggs.sum ~ block + treatment +offset(nplants), data=oviposition.deployment3.avg)
summary(result_covariates.deployment3.nb)
aov(result_covariates.deployment3.nb)
summary(aov(result_covariates.deployment3.nb))
#need a holm-adjusted t-test here because Tukey doesn't work with NB models
with(oviposition.deployment3.avg, pairwise.t.test(monarch_eggs.sum, treatment, p.adjust.method="holm"))

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

#faceted bar chart!

as.factor(oviposition.summary$deployment)
labels <- c("1" = "June", "2" = "July", "3" = "August") #make labeller

ggplot(oviposition.summary, aes(x=treatment, y=mean, colour=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=position_dodge(.9)) +
  scale_color_manual(values=cols)+
  ylab("monarch eggs / plant observation" )+
  ggtitle("Monarch Butterfly oviposition by Habitat with SE Bars") +
  theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())+
  facet_grid(~deployment, labeller=labeller(deployment = labels))
