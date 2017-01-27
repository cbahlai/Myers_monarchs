library(vegan)

#read in predator data from the first survival experiment
pred_data1<-read.csv(file="predator_surveys_deployment1.csv", header=TRUE)

#create a column that combines block and treatment
pred_data1$block.treatment = paste(pred_data1$block, pred_data1$treatment, sep=".")

#make it into a matrix without the extra stuff
pred_data1_matrix<-as.matrix(pred_data1[,5:24])

#moving the block.treatment column to the front of the matrix in probably a very inefficient way
block.treatment<-pred_data1_matrix[,20]
pred_data1_matrix<-pred_data1_matrix[,1:19]
pred_data1_matrix<-cbind(block.treatment,pred_data1_matrix)

#try NMDS
metaMDS(pred_data1_matrix, k=2, distance="bray")
