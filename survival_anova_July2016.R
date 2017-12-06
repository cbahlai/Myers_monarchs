#bring exp1.data in

#during the first part of the experiment, I recorded the fates of eggs and larvae on individual plants 
#(July 13-18). later, as they hatched and moved between plants (July 20 and on), I began to only 
#record the number of individuals within the "patch" of three plants. In excel, I summed across
#the 3 plants for July 13-18 in each patch and then brought in the data into R.

exp1.data<-read.csv(file="deployment1_excel_shaped_2016_csv.csv", header=TRUE)
exp1.data<-na.omit(exp1.data) #omit the na's, which included lots of empty spaces, which were artifacts from the excel manipulation

#make block into a factor
exp1.data$block <- as.factor(exp1.data$block)


#create total and surviving variables
exp1.data$total<-rowSums(exp1.data[9:15])
exp1.data$surviving<-exp1.data$total/exp1.data$Initial_count

library(reshape2)

exp1.data1<-dcast(exp1.data, date+hours_since_deployment+block+treatment~exclosure_treatment, mean)
exp1.data1$open<-NULL


exp1.open.only<-exp1.data[which(exp1.data$exclosure_treatment=="open"),]
exp1.open.only$exclosure_treatment<-NULL

exp1.data2<-merge(exp1.open.only, exp1.data1)


#do the anova using glm function
exp1.result <- glm(cbind(total, Initial_count) ~ hours_since_deployment*treatment + 
                block + (1+block:treatment)+offset(closed), data=exp1.data2, 
              family=binomial(link='logit'))
exp1.result
summary(exp1.result)


#also, let's do anova as an AoD becuase of exp1.data structure 
#here's some scratch code to work from, lifted from Safarzoda thesis


anova(exp1.result, test="Rao")#analysis of deviance
#need to create concatenated variable for interaction
exp1.data2$hours.treatment<-paste(exp1.data2$treatment, ".", exp1.data2$hours_since_deployment)

with(exp1.data2, pairwise.t.test(surviving, hours.treatment, p.adjust.method="holm"))

#need to do pairwise t-test only comparing within a given hours_since_deployment. I couldn't figure out how
#to do it using a for loop, so I took the brute force approach for the time being



#load library(ddply) compute summary stats for plotting
library(plyr)
exp1.data2.summary<-ddply(exp1.data2, .(hours_since_deployment, treatment), summarize,
                     N=length(surviving),
                     mean=mean(surviving),
                     sd   = sd(surviving),
                     se   = sd / sqrt(N) )

exp1.data2.summary.closed<-ddply(exp1.data2, .(hours_since_deployment, treatment), summarize,
                            N=length(closed),
                            mean=mean(closed),
                            sd   = sd(closed),
                            se   = sd / sqrt(N) )

exp1.data2.summary.sham<-ddply(exp1.data2, .(hours_since_deployment, treatment), summarize,
                          N=length(sham),
                          mean=mean(sham),
                          sd   = sd(sham),
                          se   = sd / sqrt(N) )

#creating the plot!

#make my colour palette
cols <- c("prairie" = "limegreen", "soy" = "mediumpurple")
#load ggplot2
library(ggplot2)
#make the pot
ggplot1<- ggplot(exp1.data2.summary, 
                 aes(x=exp1.data2.summary$hours_since_deployment, y=mean, colour=treatment, shape=treatment)) +
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
ggplot2<- ggplot(exp1.data2.summary, 
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

ggplot.closed<- ggplot(exp1.data2.summary.closed, 
                       aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
  geom_point()+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se, alpha=1/2))+
  scale_colour_manual(values=cols)+ 
  scale_fill_manual(values=cols)+
  geom_line(size=1)+
  xlab("Hours Since Deployment")+
  ylab("Surviving")


ggplot.closed

ggplot.sham<- ggplot(exp1.data2.summary.sham, 
                     aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
  geom_point()+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se, alpha=1/2))+
  scale_colour_manual(values=cols)+ 
  scale_fill_manual(values=cols)+
  geom_line(size=1)+
  xlab("Hours Since Deployment")+
  ylab("Surviving")


ggplot.sham



####plots for presentation


ggplot1<- ggplot(exp1.data2.summary, 
                 aes(x=exp1.data2.summary$hours_since_deployment, y=mean, colour=treatment, shape=treatment)) +
  scale_color_manual(values=cols)+
  geom_point()+
  geom_line(size=1)+
  xlab("Hours Since Deployment")+
  ylab("Surviving")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=14),
        axis.text.y = element_text(face="bold", size=14))+
scale_x_continuous(expand = c(0, 0), limits = c(0, 550))

#view the plot
ggplot1

#make it for closed and sham

ggplot.closed<- ggplot(exp1.data2.summary.closed, 
                       aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
  geom_point()+
  scale_colour_manual(values=cols)+ 
  scale_fill_manual(values=cols)+
  geom_line(size=1)+
  xlab("Hours Since Deployment")+
  ylab("Surviving")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=14),
        axis.text.y = element_text(face="bold", size=14))+
scale_x_continuous(expand = c(0, 0), limits = c(0, 550))



ggplot.closed

ggplot.sham<- ggplot(exp1.data2.summary.sham, 
                     aes(x=hours_since_deployment, y=mean, shape=treatment, colour=treatment, fill=treatment))+
  geom_point()+
  scale_colour_manual(values=cols)+ 
  scale_fill_manual(values=cols)+
  geom_line(size=1)+
  xlab("Hours Since Deployment")+
  ylab("Surviving")+
  theme(axis.text.x = element_text(face="bold", 
                                   size=14),
        axis.text.y = element_text(face="bold", size=14))+
scale_x_continuous(expand = c(0, 0), limits = c(0, 550))



ggplot.sham
