#bring data in

data<-read.csv(file="deployment2_2016_notsmooth_csv.csv", header=TRUE)

#drop turf and post 72 hour obs
data <- data[ which(data$hours_since_deployment < 73 & data$treatment != 'turf'), ]

#make block into a factor
data$block <- as.factor(data$block)

data$total<-rowSums(data[7:13])
data$surviving<-data$total/data$Initial_count



library(reshape2)

data1<-dcast(data, date+hours_since_deployment+block+treatment~exclosure_treatment, mean)
data1$open<-NULL


open.only<-data[which(data$exclosure_treatment=="open"),]
open.only$exclosure_treatment<-NULL

data2<-merge(open.only, data1)
#ok, let's do this as a linear mixed effects model. We have to also call some other packages if we want p values

library(lmerTest)


#do the anova using lmer function
result <- lmer(surviving~ hours_since_deployment * treatment + (1|block:treatment) + closed, data=data2)
result
summary(result)


#and an anova

anova(result)
#analysis of random and fixed parts and post hoc
#analysis of time and Treatment effects
step(result)

t.test(data2$surviving,data2$closed)



##
###below is lots of plotting code


















#load library(ddply) compute summary stats for plotting
library(plyr)
data2.summary<-ddply(data2, .(hours_since_deployment, treatment), summarize,
                            N=length(surviving),
                            mean=mean(surviving),
                            sd   = sd(surviving),
                            se   = sd / sqrt(N) )

data2.summary.closed<-ddply(data2, .(hours_since_deployment, treatment), summarize,
                     N=length(closed),
                     mean=mean(closed),
                     sd   = sd(closed),
                     se   = sd / sqrt(N) )

data2.summary.sham<-ddply(data2, .(hours_since_deployment, treatment), summarize,
                     N=length(sham),
                     mean=mean(sham),
                     sd   = sd(sham),
                     se   = sd / sqrt(N) )

#creating the plot!

#make my colour palette
cols <- c("corn" = "gold2", "prairie" = "limegreen", "soy" = "mediumpurple", "fallow" = "firebrick1", "turf" ="dodgerblue2" )
#load ggplot2
library(ggplot2)
#make the pot
ggplot1<- ggplot(data2.summary, 
          aes(x=data2.summary$hours_since_deployment, y=mean, colour=treatment, shape=treatment)) +
          scale_color_manual(values=cols)+
          geom_point()+
          geom_line(size=1.5)+
          geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position="dodge")+
          xlab("Hours Since Deployment")+
          ylab("Surviving")+
          theme(text = element_text(size=14))+
          scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))
        
#view the plot
ggplot1


#cooler ggplot 
cols <- c("corn" = "gold2", "prairie" = "limegreen", "soy" = "mediumpurple", "fallow" = "firebrick1", "turf" ="dodgerblue2" )
ggplot2<- ggplot(data2.summary, 
          aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
          geom_point()+
          geom_ribbon(aes(ymin=mean-se, ymax=mean+se, alpha=1/2))+
          scale_colour_manual(values=cols)+ 
          scale_fill_manual(values=cols)+
          geom_line(size=1.5)+
          xlab("Hours Since Deployment")+
          ylab("Surviving")+
          scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))
        

ggplot2

#make it for closed and sham

ggplot.closed<- ggplot(data2.summary.closed, 
                 aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
  geom_point()+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se, alpha=1/2))+
  scale_colour_manual(values=cols)+ 
  scale_fill_manual(values=cols)+
  geom_line(size=1.5)+
  xlab("Hours Since Deployment")+
  ylab("Surviving")+
  theme(text = element_text(size=14))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))

ggplot.closed

ggplot.sham<- ggplot(data2.summary.sham, 
                 aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
  geom_point()+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se, alpha=1/2))+
  scale_colour_manual(values=cols)+ 
  scale_fill_manual(values=cols)+
  geom_line(size=1.5)+
  xlab("Hours Since Deployment")+
  ylab("Surviving")+
  theme(text = element_text(size=14))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))


ggplot.sham


#crazy faceted bar plot
ggplot(data2.summary, aes(x=treatment, y=mean, colour=treatment, fill=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  scale_color_manual(values=cols)+
  scale_fill_manual(values=cols)+
  ylab("surviving")+
  ggtitle("Monarch Survival by Treatment") +
  theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())+
  facet_grid(~hours_since_deployment)





#####Plots for presentation

cols <- c("corn" = "gold2", "prairie" = "yellowgreen", "soy" = "mediumpurple", "fallow" = "firebrick1", "turf" ="dodgerblue2" )
ggplot(data2.summary, 
                 aes(x=data2.summary$hours_since_deployment, y=mean, colour=treatment, shape=treatment)) +
  scale_color_manual(values=cols)+
  geom_point()+
  geom_line(size=1.5)+
  theme(panel.background = element_blank(), axis.text.x = element_text(face="bold", 
                                   size=14),
        axis.text.y = element_text(face="bold", size=14))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))
  
ggplot(data2.summary.sham, 
         aes(x=data2.summary$hours_since_deployment, y=mean, colour=treatment, shape=treatment)) +
    scale_color_manual(values=cols)+
    geom_point()+
    geom_line(size=1.5)+
    theme(axis.text.x = element_text(face="bold", 
                                     size=14),
          axis.text.y = element_text(face="bold", size=14))
  scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))
  
ggplot(data2.summary.closed, 
         aes(x=data2.summary$hours_since_deployment, y=mean, colour=treatment, shape=treatment)) +
    scale_color_manual(values=cols)+
    geom_point()+
    geom_line(size=1.5)+
    theme(axis.text.x = element_text(face="bold", 
                                     size=14),
          axis.text.y = element_text(face="bold", size=14))
  scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))  
  
  
  ##for presentation bar plots for individual times
  ##subset the summary by time
  data2.summary.6.open<-subset(data2.summary, hours_since_deployment==6)
  data2.summary.13.open<-subset(data2.summary, hours_since_deployment==13)
  data2.summary.26.open<-subset(data2.summary, hours_since_deployment==26)
  data2.summary.49.open<-subset(data2.summary, hours_since_deployment==49)
  
  #make the plots
  ggplot(data2.summary.6.open, aes(x=treatment, y=mean, colour=treatment, fill=treatment)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    scale_color_manual(values=cols)+
    scale_fill_manual(values=cols)+
    ylab("surviving")+
    ggtitle("6 Hours") +
    theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())
  
  ggplot(data2.summary.13.open, aes(x=treatment, y=mean, colour=treatment, fill=treatment)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    scale_color_manual(values=cols)+
    scale_fill_manual(values=cols)+
    ylab("surviving")+
    ggtitle("13 Hours") +
    theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank()) 
  
  ggplot(data2.summary.26.open, aes(x=treatment, y=mean, colour=treatment, fill=treatment)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    scale_color_manual(values=cols)+
    scale_fill_manual(values=cols)+
    ylab("surviving")+
    ggtitle("26 Hours") +
    theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())
  
  ggplot(data2.summary.49.open, aes(x=treatment, y=mean, colour=treatment, fill=treatment)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    scale_color_manual(values=cols)+
    scale_fill_manual(values=cols)+
    ylab("surviving")+
    ggtitle("49 Hours") +
    theme(panel.background = element_blank(), axis.text.x = element_blank(),  axis.ticks = element_blank())
  
  
  
  
  

  