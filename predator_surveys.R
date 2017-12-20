

#read in predator survey files for 3 periods (excluding July 2016, because it was only prairie and soy)
aug_2016<-read.csv(file="predator_surveys_august_2016.csv", header=TRUE) 
july_2017<-read.csv(file="predator_surveys_july_2017.csv", header=TRUE) 
aug_2017<-read.csv(file="predator_surveys_august_2017.csv", header=TRUE) 

aug_2016<-aug_2016[ which(aug_2016$treatment != 'turf'), ]
july_2017<-july_2017[ which(july_2017$treatment != 'turf'), ]
aug_2017<-aug_2017[ which(aug_2017$treatment != 'turf'), ]


#get rid of nas
aug_2016<-na.omit(aug_2016) 
july_2017<-na.omit(july_2017) 
aug_2017<-na.omit(aug_2017) 

#pair data down to everything I want except ants
aug_2016_4<-aug_2016[,1:10]
july_2017_4<-july_2017[,1:10]
aug_2017_4<-aug_2017[,1:10]


library(reshape2)
aug_2016_4_melt<- melt(aug_2016_4, id.vars = c("date", "time","block","treatment"), 
     measure.vars = c("nabidae", "coccin_total","miridae","earwigs","ncme_total","pres_ants"))

july_2017_4_melt<- melt(july_2017_4, id.vars = c("date", "time","block","treatment"), 
                        measure.vars = c("nabidae", "coccin_total","miridae","earwigs", "ncme_total", "ants_total"))

aug_2017_4_melt<- melt(aug_2017_4, id.vars = c("date", "time","block","treatment"), 
                        measure.vars = c("nabidae", "coccin_total","miridae","earwigs","ncme_total", "ants_total"))

library(plyr)

aug_2016_plot<-ddply(aug_2016_4_melt, .(treatment, variable), summarize,
                     n=length(value),
                     mean=mean(value/3),
                     sd   = sd(value/3),
                     se   = sd / sqrt(n) )
                     
aug_2017_plot<-ddply(aug_2017_4_melt, .(treatment, variable), summarize,
                     n=length(value),
                     mean=mean(value/3),
                     sd   = sd(value/3),
                     se   = sd / sqrt(n))
july_2017_plot<-ddply(july_2017_4_melt, .(treatment, variable), summarize,
                     n=length(value),
                     mean=mean(value/3),
                     sd   = sd(value/3),
                     se   = sd / sqrt(n) )
library(ggthemes)
library(ggplot2)

#make plots
ggplot_aug_2016<-ggplot(aug_2016_plot, aes(variable, mean, fill=treatment)) +
  geom_bar(position = "dodge", stat="identity")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9), colour="black")+
theme_few()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, .3))
ggplot_aug_2016               
ggsave('predators_aug_2016.png', width=6, height=3)

ggplot_july_2017<-ggplot(july_2017_plot, aes(variable, mean, fill=treatment)) +
  geom_bar(position = "dodge", stat="identity")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9), colour="black")+
  theme_few()+
scale_y_continuous(expand = c(0, 0), limits = c(0, .9))
ggplot_july_2017
ggsave('predators_july_2017.png', width=6, height=3)

ggplot_aug_2017<-ggplot(aug_2017_plot, aes(variable, mean, fill=treatment)) +
  geom_bar(position = "dodge", stat="identity")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9), colour="black")+
  theme_few()+
scale_y_continuous(expand = c(0, 0), limits = c(0, .9))
ggplot_aug_2017  
ggsave('predators_aug_2017.png', width=6, height=3)

##### IS GITHUB WORKING FROM LAPTOP??#######try again###
