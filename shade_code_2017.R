
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
                            nplants=length(live_eggs),
                            monarch_eggs.mean=mean(live_eggs))
                                             
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
                               mean=mean(monarch_eggs.sum),
                               sd   = sd(monarch_eggs.sum),
                               se   = sd / sqrt(N) )

#load ggplot
library(ggplot2)
ggplot(shade_data.summary, aes(x=treatment, y=mean)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  ylab("monarch eggs / plant" )+
  theme(panel.background = element_blank(), complete=FALSE)










#do the same as above, but without grassland treatment. so just comparing among 4 corn edge treatments
#read in shade data
shade_data<-read.csv(file="shade_exp.csv", header=TRUE)

#drop the treatment  80 m inside field and the grassland treatment
shade_data_corn_edge<-shade_data[ which(shade_data$treatment != 'full shade, 80 m inside field'), ]
shade_data_corn_edge<-shade_data_corn_edge[ which(shade_data_corn_edge$treatment != 'grass margin'), ]

#omit NAs
shade_data_corn_edge<-na.omit(shade_data_corn_edge)

#make the appropriate variables into factors
shade_data_corn_edge$block <- as.factor(shade_data_corn_edge$date)
shade_data_corn_edge$block <- as.factor(shade_data_corn_edge$time)
shade_data_corn_edge$block <- as.factor(shade_data_corn_edge$site)
shade_data_corn_edge$block <- as.factor(shade_data_corn_edge$obs)
shade_data_corn_edge$block <- as.factor(shade_data_corn_edge$treatment)
shade_data_corn_edge$block <- as.factor(shade_data_corn_edge$plant)

#load plyr
library(plyr)

#sum monarch eggs within each plot, and create a column for how many plants were there
shade_data_corn_edge.avg <-ddply(shade_data_corn_edge, .(date, treatment, site, canopy.cover), summarize, 
                       monarch_eggs.sum=sum(live_eggs),
                       nplants=length(live_eggs))

#load(pscl)                                                   
library(pscl)
#do a negative binomial glm
result.nb<-glm.nb(monarch_eggs.sum ~ site + treatment + canopy.cover, offset(nplants), data=shade_data_corn_edge.avg)
#get the summary
summary(result.nb)
#do an anova
anova(result.nb, test="Rao")
summary(anova(result.nb, test="Rao"))

#do holm-adjusted pairwise t-tests
with(shade_data_corn_edge.avg, pairwise.t.test(monarch_eggs.sum, treatment, p.adjust.method="holm"))


#summarize data for plotting
shade_data_corn_edge.summary<-ddply(shade_data_corn_edge.avg, .(treatment), summarize,
                          N=length(monarch_eggs.sum),
                          mean=mean(monarch_eggs.sum),
                          sd   = sd(monarch_eggs.sum),
                          se   = sd / sqrt(N) )

#load ggplot
library(ggplot2)
ggplot(shade_data_corn_edge.summary, aes(x=treatment, y=mean)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  ylab("monarch eggs / plant" )+
  theme(panel.background = element_blank(), complete=FALSE)

















##here want to make a plot with just full shade corn vs edge
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



#very inefficient way to drop all but corn and grass treatments
corn_and_grass<-shade_data.avg[ which(shade_data$treatment != 'full shade, 80 m inside field'), ]
corn_and_grass<-corn_and_grass[ which(corn_and_grass$treatment != 'full sun'), ]
corn_and_grass<-corn_and_grass[ which(corn_and_grass$treatment != '1/3 plants removed'), ]
corn_and_grass<-corn_and_grass[ which(corn_and_grass$treatment != '2/3 plants removed'), ]
#summarize for plotting
corn_and_grass.summary<-ddply(corn_and_grass, .(treatment), summarize,
                                    N=length(monarch_eggs.sum),
                                    mean=mean(monarch_eggs.sum),
                                    sd   = sd(monarch_eggs.sum),
                                    se   = sd / sqrt(N) )
#plot with just full shade corn vs grass margin
ggplot(corn_and_grass.summary, aes(x=treatment, y=mean)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.5, fill="white",color="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  ylab("")+
  xlab("")+
  theme(panel.background = element_blank(), complete=FALSE)

