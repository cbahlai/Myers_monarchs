library(vegan)

#read in predator data from the first survival experiment
pred_data1<-read.csv(file="predator_surveys_deployment1.csv", header=TRUE)

#create a column that combines block and treatment
pred_data1$block.treatment = paste(pred_data1$block, pred_data1$treatment, sep=".")
block.treatment<-pred_data1[,24]

#make it into a numeric matrix without the extra stuff
pred_data1_matrix<-data.matrix(pred_data1[,5:23])


library(vegan)
#try NMDS, not sure if it worked
metaMDS(pred_data1_matrix, k=2, distance="bray")

