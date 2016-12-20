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
result <- glm(cbind(total, Initial_count) ~ block + treatment * hours_since_deployment+offset(closed), data=data2, 
              family=binomial(link='logit'))
result
summary(result)
aov(result)
summary(aov(result))
TukeyHSD(aov(result))
