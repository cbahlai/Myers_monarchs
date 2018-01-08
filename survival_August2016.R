#bring data in

data<-read.csv(file="deployment2_2016_smooth_csv.csv", header=TRUE)

#drop turf and post 72 hour obs
data <- data[ which(data$hours_since_deployment < 73 & data$treatment != 'turf'), ]

#make block into a factor
data$block <- as.factor(data$block)

##not using this code bit for 2016, because i did it in excel when smoothing: data$total<-rowSums(data[7:13])
####data$surviving<-data$total_all_stages/data$Initial_count

library(reshape2)

data1<-dcast(data, date+hours_since_deployment+block+treatment~exclosure_treatment, mean)
data1$open<-NULL

open.only<-data[which(data$exclosure_treatment=="open"),]
open.only$exclosure_treatment<-NULL

data2<-merge(open.only, data1)
#ok, let's do this as a linear mixed effects model. We have to also call some other packages if we want p values

library(lmerTest)

#do the anova using lmer function
result <- lmer(surviving~ hours_since_deployment * treatment + (1|block:treatment), data=data2)
result
summary(result)

#and an anova

anova(result)
#analysis of random and fixed parts and post hoc
#analysis of time and Treatment effects
step(result)








#####need to do a t-test comparing number surviving in closed vs sham and closed vs open


###make object data70 with only surviving at 70 hours
data70<-data2[ which(data2$hours_since_deployment == "70"), ]
###do t-test comparing closed and sham
t.test(data70$close,data70$sham, paired=TRUE, "greater")
###t-test comparing open vs sham
t.test(data70$surviving,data70$sham, paired=TRUE, "greater")

##wilcox test of same
wilcox.test(data70$close, data70$sham, paired=TRUE)
wilcox.test(data70$surviving, data70$sham, paired=TRUE)

##
###below is lots of plotting code














#load library(ddply) compute summary stats for plotting
library(plyr)
library(ggthemes)
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
cols <- c("corn" = "gold2", "prairie" = "lightreen", "soy" = "mediumpurple", "bare" = "firebrick1", "turf" ="dodgerblue2" )
#load ggplot2
library(ggplot2)
library(ggthemes)

##error bar plots

cols <- c("corn" = "gold2", "prairie" = "yellowgreen", "soy" = "mediumpurple", "bare" = "firebrick1", "turf" ="dodgerblue2" )
ggplot.eb.open<- ggplot(data2.summary, 
                 aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
  geom_point()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position="dodge")+
  scale_colour_manual(values=cols)+ 
  scale_fill_manual(values=cols)+
  geom_line(size=1)+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
  xlab("")+
  ylab("")+
  theme_few()
  ggsave('ggplot.eb.open_aug_2016.png', width=4, height=2)
ggplot.eb.open


ggplot.eb.closed<- ggplot(data2.summary.closed, 
                       aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
  geom_point()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position="dodge")+
  scale_colour_manual(values=cols)+ 
  scale_fill_manual(values=cols)+
  geom_line(size=1)+
  theme_bw()+
  theme(text = element_text(size=14))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
  xlab("")+
  ylab("")+
  theme_few()
ggsave('ggplot.eb.closed_aug_2016.png', width=4, height=2)
ggplot.eb.closed


ggplot.eb.sham<- ggplot(data2.summary.sham, 
                     aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
  geom_point()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position="dodge")+
  scale_colour_manual(values=cols)+ 
  scale_fill_manual(values=cols)+
  geom_line(size=1)+
  theme(text = element_text(size=14))+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
  xlab("")+
  ylab("")+
  theme_few()
ggsave('ggplot.eb.sham_aug_2016.png', width=4, height=2)


ggplot.eb.sham











#ribbon plots 
cols <- c("corn" = "gold2", "prairie" = "limegreen", "soy" = "mediumpurple", "bare" = "firebrick1", "turf" ="dodgerblue2" )
ggplot.open<- ggplot(data2.summary, 
          aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
          geom_point()+
          geom_ribbon(aes(ymin=mean-se, ymax=mean+se, alpha=1/2))+
          scale_colour_manual(values=cols)+ 
          scale_fill_manual(values=cols)+
          geom_line(size=1.5)+
          xlab("Hours Since Deployment")+
          ylab("Surviving")+
          scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))
          ggplot.open

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


##more plotting, making chart for comparing among exlcosure treatments
melt<-melt(data2, measure.vars = c("surviving", "closed", "sham"))

summary.melt<-ddply(melt, .(hours_since_deployment, variable), summarize,
                    N=length(value),
                    mean=mean(value),
                    sd   = sd(value),
                    se   = sd / sqrt(N) )

ggplot(summary.melt, aes(x=hours_since_deployment, y=mean, fill=variable))+
  ggtitle("August 2016")+
  geom_point()+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se, alpha=1/2))+
  geom_line(size=1)+
  xlab("\nHours Since Deployment")+
  ylab("Surviving\n")+
  theme_few()+
  guides(alpha=FALSE)+
  theme(text = element_text(size=14))+
  ylim(0,1)+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))
ggsave('ggplotsurvivalbyexclosureAug2016.png', width=5, height=4)


