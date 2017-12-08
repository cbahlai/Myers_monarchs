

#read in predator survey files for 3 periods (excluding July 2016, because it was only prairie and soy)
aug_2016<-read.csv(file="predator_surveys_august_2016.csv", header=TRUE) 
july_2017<-read.csv(file="predator_surveys_july_2017.csv", header=TRUE) 
aug_2017<-read.csv(file="predator_surveys_august_2017.csv", header=TRUE) 


#omit the NAs
aug_2016<-na.omit(aug_2016) 
july_2017<-na.omit(july_2017) 
aug_2017<-na.omit(aug_2017) 

#remove turf from the 2016 
aug_2016<- aug_2016[ which(aug_2016$treatment != 'turf'), ]

#create new objects for each peroid where it's just the 
#four pred groups of interest minus ants and the total of them
aug_2016_4 <-aug_2016[,1:9]
july_2017_4 <-july_2017[,1:9]
aug_2017_4 <-aug_2017[,1:9]

#load ddply
library(plyr)
aug_2016_graph<-ddply(aug_2016_4, .(treatment), summarize,
                              N= length(coccin_total), 
                              mean_coccinellidae=mean(coccin_total),
                              sd_coccin_total   = sd(coccin_total),
                              se_coccin_total   = sd_coccin_total / sqrt(N),
                              mean_miridae=mean(miridae),
                              sd_miridae   = sd(miridae),
                              se_miridae   = sd_miridae / sqrt(N),
                              mean_nabidae=mean(nabidae),
                              sd_nabidae   = sd(nabidae),
                              se_nabidae   = sd_nabidae / sqrt(N),
                              mean_earwigs=mean(earwigs),
                              sd_earwigs   = sd(earwigs),
                              se_earwigs   = sd_earwigs / sqrt(N))

july_2017_graph<-ddply(july_2017_4, .(treatment), summarize,
                      N= length(coccin_total), 
                      mean_miridae=mean(coccin_total),
                      sd_coccin_total   = sd(coccin_total),
                      se_coccin_total   = sd_coccin_total / sqrt(N),
                      mean_miridae=mean(miridae),
                      sd_miridae   = sd(miridae),
                      se_miridae   = sd_miridae / sqrt(N),
                      mean_nabidae=mean(nabidae),
                      sd_nabidae   = sd(nabidae),
                      se_nabidae   = sd_nabidae / sqrt(N),
                      mean_earwigs=mean(earwigs),
                      sd_earwigs   = sd(earwigs),
                      se_earwigs   = sd_earwigs / sqrt(N))

aug_2017_graph<-ddply(aug_2017_4, .(treatment), summarize,
                      N= length(coccin_total), 
                      mean_miridae=mean(coccin_total),
                      sd_coccin_total   = sd(coccin_total),
                      se_coccin_total   = sd_coccin_total / sqrt(N),
                      mean_miridae=mean(miridae),
                      sd_miridae   = sd(miridae),
                      se_miridae   = sd_miridae / sqrt(N),
                      mean_nabidae=mean(nabidae),
                      sd_nabidae   = sd(nabidae),
                      se_nabidae   = sd_nabidae / sqrt(N),
                      mean_earwigs=mean(earwigs),
                      sd_earwigs   = sd(earwigs),
                      se_earwigs   = sd_earwigs / sqrt(N))

#load ggplot
library(ggplot2)


cols <- c("gold2", "firebrick1", "yellowgreen", "mediumpurple")
ggplot(aug_2016_graph, aes(x=treatment, y=mean_miridae, fill=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", size=.3, fill=cols) 



  