mydata <- read.csv("cleaned_data.csv",sep=",")

#data for reading score
mydata_4_var <- mydata[,c(1:4,17:21,5:16)]

na_matrix2 <- is.na(mydata_4_var[,2:9])
row_sum2 <- rowSums(na_matrix2)
#only the students without any missing values in the predictors are kept
mydata_4_var_2_continuous <- mydata_4_var[row_sum2==0,]
#we are only concerned about the reading score
mydata_4_var_2_continuous <- mydata_4_var_2_continuous[,c(1:9,10,12,14,16,18,20)]

#change the data table from wide format into long format, for easier processing in SAS;
#add actual corresponding time value for each reading score
library(tidyr)
reading <- gather(mydata_4_var_2_continuous,time,reading,10:15)
reading2 <- reading[!is.na(reading$reading),]
for (i in 1:nrow(reading2)) {
  if (reading2$time[i]=="C1_Reading") {
    reading2$time_dup[i] <- 0
  }
  else if (reading2$time[i]=="C2_Reading") {
    reading2$time_dup[i] <- 0.5
  }
  else if (reading2$time[i]=="C4_Reading") {
    reading2$time_dup[i] <- 1.5
  }
  else if (reading2$time[i]=="C5_Reading") {
    reading2$time_dup[i] <- 3.5
  }
  else if (reading2$time[i]=="C6_Reading") {
    reading2$time_dup[i] <- 5.5
  }
  else if (reading2$time[i]=="C7_Reading") {
    reading2$time_dup[i] <- 8.5
  }
}
reading2$time <- NULL

#write to .csv file
write.csv(reading2,file="data_4_var_reading_full.csv",row.names=F)