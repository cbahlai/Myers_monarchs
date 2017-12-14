


###read in hobo logger data

b1.corn<-read.csv(file="LandisLogger6_B1_CORN.csv", header=TRUE)
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

#####put all the files together
hobo.cbe <-rbind(b1.corn, b2.corn, b3.corn, b4.corn, b1.soy, b2.soy, b3.soy, 
            b4.soy, b1.bare, b2.bare, b3.bare, b4.bare, b1.prairie, b2.prairie, b3.prairie, b4.prairie)


### omit nas that were at the ends of some of the files for some reason
hobo<-na.omit(hobo.cbe)

### treatment is a factor
hobo.cbe$treatment<-as.factor(hobo.cbe$treatment)

#make a plot
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
