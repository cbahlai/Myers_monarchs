
#read in shade data
shade_data<-read.csv(file="shade_exp.csv", header=TRUE)

#drop the treatment  80 m inside field, because found no eggs
shade_data<-shade_data[ which(shade_data$treatment != 'full shade, 80 m inside field'), ]


#omit NAs
shade_data<-na.omit(shade_data)

#make the appropriate variables into factors
shade_data$block <- as.factor(shade_data$date)
shade_data$block <- as.factor(shade_data$time)
shade_data$block <- as.factor(shade_data$site)
shade_data$block <- as.factor(shade_data$obs)
shade_data$block <- as.factor(shade_data$treatment)
shade_data$block <- as.factor(shade_data$plant)

#load plyr
library(plyr)

#sum monarch eggs within each plot, and create a column for how many plants were there
shade_data.avg <-ddply(shade_data, .(date, treatment, site, canopy.cover), summarize, 
                            monarch_eggs.sum=sum(live_eggs),
                            nplants=length(live_eggs))
                                             
#load(pscl)                                                   
library(pscl)
#do a negative binomial glm
result.nb<-glm.nb(monarch_eggs.sum ~ site + treatment + canopy.cover, offset(nplants), data=shade_data.avg)
#get the summary
summary(result.nb)
#do an anova
anova(result.nb, test="Rao")
summary(anova(result.nb, test="Rao"))

#do holm-adjusted pairwise t-tests
with(shade_data.avg, pairwise.t.test(monarch_eggs.sum, treatment, p.adjust.method="holm"))


#summarize data for plotting
shade_data.summary<-ddply(shade_data.avg, .(treatment), summarize,
                               N=length(monarch_eggs.sum),
                               mean=mean(monarch_eggs.sum/nplants),
                               sd   = sd(monarch_eggs.sum/nplants),
                               se   = sd / sqrt(N) )

#load ggplot
library(ggplot2)
ggplot(shade_data.summary, aes(x=treatment, y=mean)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  ylab("monarch eggs / plant" )+
  theme(panel.background = element_blank(), complete=FALSE)










