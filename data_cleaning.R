mydata <- read.csv("raw_data.csv",sep=",",header=T)
mydata$W1INCOME <- NULL
mydata$P2INCOME <- NULL
mydata$C3R4RSCL <- NULL
mydata$C3R4MSCL <- NULL
mydata$RACE <- NULL

#Cleaning Parents Highest Education Level: 0 (level 1-3), 1 (level 4-6), 2 (level 7-9)
for (i in 1:nrow(mydata)) {
  if (is.na(mydata$WKPARED[i])==FALSE) {
    if (mydata$WKPARED[i]>=1 & mydata$WKPARED[i]<=3) {
      mydata$PARENT_ED[i] <- 0
    }
    else if (mydata$WKPARED[i]>=4 & mydata$WKPARED[i]<=6) {
      mydata$PARENT_ED[i] <- 1
    }
    else {
      mydata$PARENT_ED[i] <- 2
    }
  }
  else {
    mydata$PARENT_ED[i] <- NA
  }
}
mydata$WKPARED <- NULL

#Cleaning Speak Non English at Home (0 is Speaking English, 1 is NOT Speaking English)
for (i in 1:nrow(mydata)) {
  if (is.na(mydata$C2SCREEN[i])==FALSE) {
    if (mydata$C2SCREEN[i]==2) {
      mydata$Speak_Non_English[i] <- 0
    }
    else {
      mydata$Speak_Non_English[i] <- 1
    }
  }
  else {
    mydata$Speak_Non_English[i] <- NA
  }
}
mydata$C2SCREEN <- NULL

#Cleaning Gender (0: Male; 1:Female)
mydata$GENDER[mydata$GENDER==-9] <- NA
mydata$GENDER <- mydata$GENDER-1

#Cleaning F2CLASS (0 means part-time, and 1 means full-time)
mydata$F2CLASS[mydata$F2CLASS==-9] <- NA
for (i in 1:nrow(mydata)) {
  if (is.na(mydata$F2CLASS[i])==FALSE) {
    if (mydata$F2CLASS[i]==1 | mydata$F2CLASS[i]==2) {
      mydata$full_time_program[i] <- 0
    }
    else {
      mydata$full_time_program[i] <- 1
    }
  }
  else {
    mydata$full_time_program[i] <- NA
  }
}
mydata$F2CLASS <- NULL

#any value that is equal to -1 or -9 are missing values
mydata[mydata==-1] <- NA
mydata[mydata==-9] <- NA

#rename columns
colnames(mydata)[c(2,3,5:19)] <- c("Region","Urbanicity","C1_Reading","C1_Math",
                                   "C2_Reading","C2_Math","C4_Reading","C4_Math",
                                   "C5_Reading","C5_Math","C6_Reading","C6_Math",
                                   "C7_Reading","C7_Math","P2_Family_Type",
                                   "WK_Income","Parent_Education")
colnames(mydata)[c(1,4)] <- c("Child_ID","Gender")
colnames(mydata)[c(20,21)] <- c("Non_Home_English_Speaker","Full_Time_Program")

#count students that do not have missing values
mydata_sub <- mydata[,5:16]
count_na <- is.na(mydata_sub)
row_na_count <- rowSums(count_na)
sum(row_na_count==0)
#There are 7,080 students who do NOT have missing values
#in any of their 6 repeated measurements

write.csv(mydata,file="cleaned_data.csv",row.names=F)
