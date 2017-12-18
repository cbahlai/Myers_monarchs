


###read in hobo logger data


b2.corn<-read.csv(file="LandisLogger2_B2_CORN.csv", header=TRUE)
b3.corn<-read.csv(file="LandisLogger12_B3_CORN.csv", header=TRUE)
b4.corn<-read.csv(file="LandisLogger15_B4_CORN.csv", header=TRUE)
b1.soy<-read.csv(file="LandisLogger8_B1_SOY.csv", header=TRUE)
b2.soy<-read.csv(file="LandisLogger1_B2_SOY.csv", header=TRUE)
b3.soy<-read.csv(file="LandisLogger9_B3_SOY.csv", header=TRUE)
b4.soy<-read.csv(file="LandisLogger14_B4_SOY.csv", header=TRUE)
b1.bare<-read.csv(file="LandisLogger7_B1_BARE.csv", header=TRUE)
b2.bare<-read.csv(file="LandisLogger4_B2_BARE.csv", header=TRUE)
b3.bare<-read.csv(file="LandisLogger11_B3_BARE.csv", header=TRUE)
b4.bare<-read.csv(file="LandisLogger13_B4_BARE.csv", header=TRUE)
b1.prairie<-read.csv(file="LandisLogger5_B1_PRAIRIE.csv", header=TRUE)
b2.prairie<-read.csv(file="LandisLogger3_B2_PRAIRIE.csv", header=TRUE)
b3.prairie<-read.csv(file="LandisLogger10_B3_PRAIRIE.csv", header=TRUE)
b4.prairie<-read.csv(file="LandisLogger16_B4_PRAIRIE.csv", header=TRUE)
##### leave out logger6, because it recorded every minute and only recorded in the field 6/12-6/27
#####b1.corn<-read.csv(file="LandisLogger6_B1_CORN.csv", header=TRUE)

#####put all the files together
hobo.cbe <-rbind(b2.corn, b3.corn, b4.corn, b1.soy, b2.soy, b3.soy, 
            b4.soy, b1.bare, b2.bare, b3.bare, b4.bare, b1.prairie, b2.prairie, b3.prairie, b4.prairie)


### omit nas that were at the ends of some of the files for some reason
hobo<-na.omit(hobo.cbe)

### treatment is a factor
hobo.cbe$treatment<-as.factor(hobo.cbe$treatment)

library(ggplot2)
library(ggthemes)
#make a boxplot for temperature
ggplot(hobo.cbe, aes(x = treatment, y = temp)) +
  geom_boxplot(fill="cornflowerblue")+
  theme_few()+
  xlab("Treatment") + ylab("Temp in Degrees F")
ggsave("CBE_temp_boxplot.png", width = 4, height = 5)

library(ggplot2)
library(ggthemes)
#make a boxplot for RH
ggplot(hobo.cbe, aes(x = treatment, y = RH)) +
  geom_boxplot(fill="palegreen")+
theme_few()+
  xlab("Treatment") + ylab("Relative Humidity")
ggsave("CBE_RH_boxplot.png", width = 4, height = 5)


#summaraize for plotting
library(plyr)
hobo.cbe.summary<-ddply(hobo.cbe, .(site, treatment, block), summarize, 
                    N=length(temp),
                    mean.temp=mean(temp),
                    mean.RH=mean(RH),
                    sd.temp   = sd(temp),
                    se.temp   = sd.temp / sqrt(N),
                    sd.RH   = sd(RH),
                    se.RH   = sd.RH / sqrt(N)
)
