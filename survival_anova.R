#bring data in

data<-read.csv(file="deployment2_csv.csv", header=TRUE)

#make block into a factor
data$block <- as.factor(data$block)



#do the anova using lm function
result <- lm(percent_survival_all_stages ~ block + treatment + exclosure_treatment + treatment:exclosure_treatment, data=data)
aov(result)
summary(aov(result))
TukeyHSD(aov(result))
