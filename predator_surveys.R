

#read in predator survey files for 3 periods (excluding July 2016, because it was only prairie and soy)
aug_2016<-read.csv(file="predator_surveys_august_2016.csv", header=TRUE) 
july_2017<-read.csv(file="predator_surveys_july_2017.csv", header=TRUE) 
aug_2017<-read.csv(file="predator_surveys_august_2017.csv", header=TRUE) 



aug_2016<-na.omit(aug_2016) 
july_2017<-na.omit(july_2017) 
aug_2017<-na.omit(aug_2017) 