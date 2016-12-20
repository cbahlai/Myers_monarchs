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


#do the anova using lm function
result <- glm(cbind(total, Initial_count) ~ hours_since_deployment + treatment + 
                block + (1+block:treatment)+offset(closed), data=data2, 
              family=binomial(link='logit'))
result
summary(result)
aov(result)
summary(aov(result))
#tukey test is not going to work because of model structure. 
#posthoc test should be a Holm-adjusted t instead

#also, let's redo the anova as an AoD becuase of data structure 
#here's some scratch code to work from, lifted from Safarzoda thesis


anova(expt1.BCHO.model.12, test="Rao")
with(expt1.2012, pairwise.t.test(BCHO, treatment, p.adjust.method="holm", paired=TRUE))
#compute summary stats for plotting
expt.bcho.summary.12<-ddply(expt1.2012, .(year, day, treatment), summarize, 
                            N=length(BCHO.tiller),
                            mean=mean(BCHO.tiller),
                            sd   = sd(BCHO.tiller),
                            se   = sd / sqrt(N) )
#create plot
expt1.bcho.2012<-ggplot(expt.bcho.summary.12, 
                        aes(day, mean, colour=treatment, 
                            shape=treatment)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=pd)+
  geom_point(size=6, position=pd)+
  scale_color_manual(values = pal1, breaks=c("closed","bottom", "top", "open"))+
  xlab(NULL)+
  ylab("2012")+
  theme_bw()+
  theme(text=element_text(size=18),
        plot.title=element_text(face="italic"), 
        legend.position="none", plot.margin=unit(c(1,1,2,1), "lines"))+
  geom_line(size=1, position=pd)+
  scale_shape_manual(values = shapepal, breaks=c("closed","bottom", "top", "open"))+
  ggtitle("R. padi")


