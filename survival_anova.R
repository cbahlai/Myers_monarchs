#bring data in

data<-read.csv(file="deployment2_csv.csv", header=TRUE)

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


#do the anova using glm function
result <- glm(cbind(total, Initial_count) ~ hours_since_deployment + treatment + 
                block + (1+block:treatment)+offset(closed), data=data2, 
              family=binomial(link='logit'))
result
summary(result)


#also, let's do anova as an AoD becuase of data structure 
#here's some scratch code to work from, lifted from Safarzoda thesis


anova(result, test="Rao")#analysis of deviance
#need to create concatenated variable for interaction
data2$hours.treatment<-paste(data2$treatment, ".", data2$hours_since_deployment)

with(data2, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))

#need to do pairwise t-test only comparing within a given hours_since_deployment. I couldn't figure out how
#to do it using a for loop, so I took the brute force approach for the time being

hours.0<-subset(data2, hours_since_deployment==0)
hours.6<-subset(data2, hours_since_deployment==6)
hours.8<-subset(data2, hours_since_deployment==8)
hours.11<-subset(data2, hours_since_deployment==11)
hours.13<-subset(data2, hours_since_deployment==13)
hours.19<-subset(data2, hours_since_deployment==19)
hours.22<-subset(data2, hours_since_deployment==22)
hours.26<-subset(data2, hours_since_deployment==26)
hours.49<-subset(data2, hours_since_deployment==49)
hours.71<-subset(data2, hours_since_deployment==71)
hours.97<-subset(data2, hours_since_deployment==97)
hours.145<-subset(data2, hours_since_deployment==145)
hours.312<-subset(data2, hours_since_deployment==312)
hours.480<-subset(data2, hours_since_deployment==480)
hours.600<-subset(data2, hours_since_deployment==600)

with(hours.0, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))
with(hours.6, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))
with(hours.8, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))
with(hours.11, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))
with(hours.13, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))
with(hours.19, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))
with(hours.22, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))
with(hours.26, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))
with(hours.49, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))
with(hours.71, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))
with(hours.97, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))
with(hours.145, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))
with(hours.312, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))
with(hours.480, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))
with(hours.600, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))


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
          geom_line()+
          geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position="dodge")+
          xlab("Hours Since Deployment")+
          ylab("Surviving")
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
          geom_line(size=1)+
          xlab("Hours Since Deployment")+
          ylab("Surviving")
        

ggplot2

#make it for closed and sham

ggplot.closed<- ggplot(data2.summary.closed, 
                 aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
  geom_point()+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se, alpha=1/2))+
  scale_colour_manual(values=cols)+ 
  scale_fill_manual(values=cols)+
  geom_line(size=1)+
  xlab("Hours Since Deployment")+
  ylab("Surviving")


ggplot.closed

ggplot.sham<- ggplot(data2.summary.sham, 
                 aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
  geom_point()+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se, alpha=1/2))+
  scale_colour_manual(values=cols)+ 
  scale_fill_manual(values=cols)+
  geom_line(size=1)+
  xlab("Hours Since Deployment")+
  ylab("Surviving")


ggplot.sham

#spare code
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position="dodge")+
  geom_point(size=6, position="dodge")+
  scale_color_manual(values = pal1, breaks=c("closed","bottom", "top", "open"))+
  xlab("Hours Since Deployment")+
  ylab("Surviving")+
  theme_bw()+
  geom_line(size=1, position="dodge")+
  scale_shape_manual(values = shapepal, breaks=c("closed","bottom", "top", "open"))+
  ggtitle("R. padi")


