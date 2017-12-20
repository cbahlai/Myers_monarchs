#bring data in

data<-read.csv(file="deployment2_2017_smooth_csv.csv", header=TRUE)
data<-na.omit(data)

#drop all observations past 72 hours
data <- data[ which(data$hours_since_deployment < 80), ]

data$total<-rowSums(data[7:14])
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
result <- lmer(total~ hours_since_deployment * treatment + (1|block:treatment), data=data2)
result
summary(result)


#and an anova

anova(result)
#analysis of random and fixed parts and post hoc
#analysis of time and Treatment effects
step(result)

###make object data72 with only surviving at 72 hours
data72<-data2[ which(data2$hours_since_deployment == "72"), ]
###do t-test comparing closed and sham
t.test(data72$close,data72$sham, paired=TRUE, "greater")
###t-test comparing open vs sham
t.test(data72$surviving,data72$sham, paired=TRUE, "greater")

##wilcox test of same
wilcox.test(data72$close, data72$sham, paired=TRUE)
wilcox.test(data72$surviving, data72$sham, paired=TRUE)


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
                            N=length(close),
                            mean=mean(close),
                            sd   = sd(close),
                            se   = sd / sqrt(N) )

data2.summary.sham<-ddply(data2, .(hours_since_deployment, treatment), summarize,
                          N=length(sham),
                          mean=mean(sham),
                          sd   = sd(sham),
                          se   = sd / sqrt(N) )

#creating the plot!

#make my colour palette
cols <- c("corn" = "gold2", "prairie" = "yellowgreen", "soy" = "mediumpurple", "bare" = "firebrick1" )
#load ggplot2
library(ggplot2)
##error bar plots

cols <- c("corn" = "gold2", "prairie" = "yellowgreen", "soy" = "mediumpurple", "bare" = "firebrick1")
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
ggsave('ggplot.eb.open_aug_2017.png', width=4, height=2)



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
ggsave('ggplot.eb.closed_aug_2017.png', width=4, height=2)


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
ggsave('ggplot.eb.sham_aug_2017.png', width=4, height=2)
ggplot.eb.sham



#RIBBON PLOTS
cols <- c("corn" = "gold2", "prairie" = "yellowgreen", "soy" = "mediumpurple", "bare" = "firebrick1")
ggplot.open<- ggplot(data2.summary, 
                     aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
  geom_point()+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se, alpha=1/2))+
  scale_colour_manual(values=cols)+ 
  scale_fill_manual(values=cols)+
  geom_line(size=1)+
  xlab("Hours Since Deployment")+
  ylab("Surviving")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))
ggplot.open

#make it for close and sham

ggplot.close<- ggplot(data2.summary.close, 
                      aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
  geom_point()+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se, alpha=1/2))+
  scale_colour_manual(values=cols)+ 
  scale_fill_manual(values=cols)+
  geom_line(size=1)+
  xlab("Hours Since Deployment")+
  ylab("Surviving")+
  theme(text = element_text(size=14))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))
ggplot.close

ggplot.sham<- ggplot(data2.summary.sham, 
                     aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
  geom_point()+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se, alpha=1/2))+
  scale_colour_manual(values=cols)+ 
  scale_fill_manual(values=cols)+
  geom_line(size=1)+
  xlab("Hours Since Deployment")+
  ylab("Surviving")+
  theme(text = element_text(size=14))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))
ggplot.sham

##more plotting, making chart for comparing among exlcosure treatments
melt<-melt(data2, measure.vars = c("surviving", "close", "sham"))

summary.melt<-ddply(melt, .(hours_since_deployment, variable), summarize,
                    N=length(value),
                    mean=mean(value),
                    sd   = sd(value),
                    se   = sd / sqrt(N) )

ggplot(summary.melt, aes(x=hours_since_deployment, y=mean, fill=variable))+
  ggtitle("August 2017")+
  geom_point()+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se, alpha=1/2))+
  geom_line(size=1)+
  xlab("Hours Since Deployment")+
  ylab("Surviving")+
  theme_few()+
  guides(alpha=FALSE)+
  theme(text = element_text(size=14))+
  ylim(0,1)+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70))
ggsave('ggplotsurvivalbyexclosureAugust2017.png', width=7, height=6)
