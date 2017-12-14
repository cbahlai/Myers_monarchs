
#read in shade data
shade_data<-read.csv(file="shade_exp.csv", header=TRUE)

#drop the treatment  80 m inside field, because found no eggs
shade_data<-shade_data[ which(shade_data$treatment != 'full shade, 80 m inside field'), ]


#omit NAs
shade_data<-na.omit(shade_data)

#make the appropriate variables into factors
shade_data$block <- as.factor(shade_data$date)
shade_data$time <- as.factor(shade_data$time)
shade_data$site <- as.factor(shade_data$site)
shade_data$obs <- as.factor(shade_data$obs)
shade_data$treatment <- as.factor(shade_data$treatment)
shade_data$plant <- as.factor(shade_data$plant)

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
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill="cornflowerblue") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  ylab("monarch eggs / plant" )+
  theme(panel.background = element_blank(), complete=FALSE)+ 
  theme_few()
  ggsave("shade_experiment.png", width = 6, height = 3)

#make an object with just grass margin and full shade  
shade_data.summary2<-shade_data.summary[ which(shade_data.summary$treatment != 'full sun'), ]
shade_data.summary2<-shade_data.summary2[ which(shade_data.summary2$treatment != '1/3 plants removed'), ]
shade_data.summary2<-shade_data.summary2[ which(shade_data.summary2$treatment != '2/3 plants removed'), ]

#make an object without grass margin and full shade
shade_data.summary3<-shade_data.summary[ which(shade_data.summary$treatment != 'grass margin'), ]
shade_data.summary3<-shade_data.summary3[ which(shade_data.summary3$treatment != 'full shade'), ]

#now plot as above
ggplot(shade_data.summary2, aes(x=treatment, y=mean)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill="cornflowerblue") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  ylab("monarch eggs / plant" )+
  theme(panel.background = element_blank(), complete=FALSE)+ 
  theme_few()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, .4))
  
ggsave("shade_experiment_grassvscorn.png", width = 3, height = 3)

ggplot(shade_data.summary3, aes(x=treatment, y=mean)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill="palegreen") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  ylab("monarch eggs / plant" )+
  theme(panel.background = element_blank(), complete=FALSE)+ 
  theme_few()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, .4))
ggsave("shade_experiment_cutcorn.png", width = 5, height = 3)










####### read in hobo_logger data
hobo1.1<-read.csv(file="LandisLogger1_SITE1_TRT1.csv", header=TRUE)
hobo1.2<-read.csv(file="LandisLogger2_SITE1_TRT2.csv", header=TRUE)
hobo1.3<-read.csv(file="LandisLogger3_SITE1_TRT3.csv", header=TRUE)
hobo1.4<-read.csv(file="LandisLogger4_SITE1_TRT4.csv", header=TRUE)
hobo1.5<-read.csv(file="LandisLogger5_SITE1_TRT5.csv", header=TRUE)
hobo1.6<-read.csv(file="LandisLogger6_SITE1_TRT6.csv", header=TRUE)
hobolux.1<-read.csv(file="LandisLogger7_LUXARBOR_TRT1.csv", header=TRUE)
hobolux.2<-read.csv(file="LandisLogger13_LUXARBOR_TRT2.csv", header=TRUE)
hobolux.3<-read.csv(file="LandisLogger8_LUXARBOR_TRT3.csv", header=TRUE)
hobolux.4<-read.csv(file="LandisLogger14_LUXARBOR_TRT4.csv", header=TRUE)
hobolux.5<-read.csv(file="LandisLogger15_LUXARBOR_TRT5.csv", header=TRUE)
hobolux.6<-read.csv(file="LandisLogger16_LUXARBOR_TRT6.csv", header=TRUE)

#####put all the files together
hobo<-rbind(hobo1.1, hobo1.2, hobo1.3, hobo1.4, hobo1.5, hobo1.6, hobolux.1, hobolux.2, hobolux.3, 
             hobolux.4, hobolux.5, hobolux.6)

### omit nas that were at the ends of some of the files for some reason
hobo<-na.omit(hobo)

### treatment is a factor
hobo$treatment<-as.factor(hobo$treatment)

#do holm-adjusted pairwise t-tests
with(hobo.summary, pairwise.t.test(mean.temp, treatment, p.adjust.method="holm"))
with(hobo.summary, pairwise.t.test(mean.RH, treatment, p.adjust.method="holm"))

#make a plot
#summaraize for plotting
library(plyr)
hobo.summary<-ddply(hobo, .(site, treatment), summarize, 
                    N=length(temp),
                    mean.temp=mean(temp),
                    mean.RH=mean(RH),
                    sd.temp   = sd(temp),
                    se.temp   = sd.temp / sqrt(N),
                    sd.RH   = sd(RH),
                    se.RH   = sd.RH / sqrt(N)
)

library(ggthemes)
ggplot(hobo.summary, aes(x=treatment, y=mean.temp, fill=site)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3) +
  geom_errorbar(aes(ymin=mean.temp-se.temp, ymax=mean.temp+se.temp), width=.2, position=position_dodge(.9)) +
  ylab("Temp in Degrees F" )+
  theme(panel.background = element_blank(), complete=FALSE)+ 
  theme_few()
ggsave("shade_hobo_temp.png", width = 10, height = 6)

library(ggthemes)
ggplot(hobo.summary, aes(x=treatment, y=mean.RH, fill=site)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3) +
  geom_errorbar(aes(ymin=mean.RH-se.RH, ymax=mean.RH+se.RH), width=.2, position=position_dodge(.9)) +
  ylab("Relative Humidity" )+
  theme(panel.background = element_blank(), complete=FALSE)+ 
  theme_few()
ggsave("shade_hobo_RH.png", width = 10, height = 6)


