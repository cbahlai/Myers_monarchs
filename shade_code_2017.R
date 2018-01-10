
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
library(MASS)
#do a negative binomial glm
result.nb<-glm.nb(monarch_eggs.sum ~ site + treatment + canopy.cover, offset(nplants), data=shade_data.avg)
#get the summary
summary(result.nb)
#do an anova
anova(result.nb, test="Rao")
summary(anova(result.nb, test="Rao"))

#do holm-adjusted pairwise t-tests
t.tests<-with(shade_data.avg, pairwise.t.test(monarch_eggs.sum, treatment, p.adjust.method="holm"))


#want to do a power test
#first have to caluclate effect size, ie cohen's d
library(effsize)
## this function requires making dataframes with only two treatment levels, below is every combo. trt 5 is missing because it was removed above
data_trt_1.2=shade_data.avg[shade_data.avg$treatment == 'full sun'|shade_data.avg$treatment =='2/3 plants removed',]
data_trt_1.3=shade_data.avg[shade_data.avg$treatment == 'full sun'|shade_data.avg$treatment =='1/3 plants removed',]
data_trt_1.4=shade_data.avg[shade_data.avg$treatment == 'full sun'|shade_data.avg$treatment =='full shade',]
data_trt_1.6=shade_data.avg[shade_data.avg$treatment == 'full sun'|shade_data.avg$treatment =='grass margin',]
data_trt_2.3=shade_data.avg[shade_data.avg$treatment == '2/3 plants removed'|shade_data.avg$treatment =='1/3 plants removed',]
data_trt_2.4=shade_data.avg[shade_data.avg$treatment == '2/3 plants removed'|shade_data.avg$treatment =='full shade',]
data_trt_2.6=shade_data.avg[shade_data.avg$treatment == '2/3 plants removed'|shade_data.avg$treatment =='grass margin',]
data_trt_3.4=shade_data.avg[shade_data.avg$treatment == '1/3 plants removed'|shade_data.avg$treatment =='full shade',]
data_trt_3.6=shade_data.avg[shade_data.avg$treatment == '1/3 plants removed'|shade_data.avg$treatment =='grass margin',]
data_trt_4.6=shade_data.avg[shade_data.avg$treatment == 'full shade'|shade_data.avg$treatment =='grass margin',]

##get rid of the unused levels
data_trt_1.2$treatment=factor(data_trt_1.2$treatment)
data_trt_1.3$treatment=factor(data_trt_1.3$treatment)
data_trt_1.4$treatment=factor(data_trt_1.4$treatment)
data_trt_1.6$treatment=factor(data_trt_1.6$treatment)
data_trt_2.3$treatment=factor(data_trt_2.3$treatment)
data_trt_2.4$treatment=factor(data_trt_2.4$treatment)
data_trt_2.6$treatment=factor(data_trt_2.6$treatment)
data_trt_3.4$treatment=factor(data_trt_3.4$treatment)
data_trt_3.6$treatment=factor(data_trt_3.6$treatment)
data_trt_4.6$treatment=factor(data_trt_4.6$treatment)

#calculate cohen's d
cohen.d(data_trt_1.2$monarch_eggs.sum, data_trt_1.2$treatment)
cohen.d(data_trt_1.3$monarch_eggs.sum, data_trt_1.3$treatment)
cohen.d(data_trt_1.4$monarch_eggs.sum, data_trt_1.4$treatment)
cohen.d(data_trt_1.6$monarch_eggs.sum, data_trt_1.6$treatment)
cohen.d(data_trt_2.3$monarch_eggs.sum, data_trt_2.3$treatment)
cohen.d(data_trt_2.4$monarch_eggs.sum, data_trt_2.4$treatment)
cohen.d(data_trt_2.6$monarch_eggs.sum, data_trt_2.6$treatment)
cohen.d(data_trt_3.4$monarch_eggs.sum, data_trt_3.4$treatment)
cohen.d(data_trt_3.6$monarch_eggs.sum, data_trt_3.6$treatment)
cohen.d(data_trt_4.6$monarch_eggs.sum, data_trt_4.6$treatment)

#now doing power test using above cohen's d and alpha = 0.05 and beta = 0.8
library(pwr)
pwr.t.test(d = -0.2220087, sig.level = 0.05, power =0.8 , type = c("paired"))
pwr.t.test(d = -0.9933889, sig.level = 0.05, power =0.8 , type = c("paired"))
pwr.t.test(d = -0.2906633, sig.level = 0.05, power =0.8 , type = c("paired"))
pwr.t.test(d =  0.7035623, sig.level = 0.05, power =0.8 , type = c("paired"))
pwr.t.test(d = -0.5486318, sig.level = 0.05, power =0.8 , type = c("paired"))
pwr.t.test(d =  0.0701350, sig.level = 0.05, power =0.8 , type = c("paired"))
pwr.t.test(d =  0.3370833, sig.level = 0.05, power =0.8 , type = c("paired"))
pwr.t.test(d = -0.4295437, sig.level = 0.05, power =0.8 , type = c("paired"))
pwr.t.test(d = -0.3478487, sig.level = 0.05, power =0.8 , type = c("paired"))
pwr.t.test(d =  0.2341778, sig.level = 0.05, power =0.8 , type = c("paired"))

# indicates I would need very large sample sizes






#summarize data for plotting 
=======
#do power analysis on t tests
library(pwr)
#1/3 vs 2/3 removed
pwr.t2n.test(n1 = 21, n2= 22, d = 0.548631, sig.level = 0.05,
             alternative = c("two.sided"))
#1/3 vs full shade
0.440719

#1/3 vs

#summarize data for plotting
library(plyr)
>>>>>>> b7ff29f80cb3723163a0fc22cb3a86d4f97cd82d
shade_data.summary<-ddply(shade_data.avg, .(treatment), summarize,
                               N=length(monarch_eggs.sum),
                               mean=mean(monarch_eggs.sum/nplants),
                               sd   = sd(monarch_eggs.sum/nplants),
                               se   = sd / sqrt(N) )



#load ggplot
library(ggplot2)
library(ggthemes)
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












##also collected hobo logger temp and humidity data at 2 sites (site 1 and Lux Arbor)

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


