##This script is uploaded to accomplish the final assignment for the
##"Getting and Cleaning Data" course.

##Basically, it loads data files and selects features with means and standard deviations.
##After changing names for those measures, it joins test and train datasets into
##one dataset called "ds".

##At last, another dataset is created using dplyr package, in order to calculate means
##for every measure grouped by subject and activity.


#Clear global environment
rm(list=ls())

#Loading libraries
library(dplyr)
library(tidyr)

#Setting of relative paths for file opening
mainpath <- "./UCI HAR Dataset/"
testpath <- "./UCI HAR Dataset/test/"
trainpath  <- "./UCI HAR Dataset/train/"

#Opening attribute files
activities <- read.table(paste0(mainpath,"activity_labels.txt"), sep=" ", stringsAsFactors = F)
names(activities) <- c("activityid","activity")

features <- read.table(paste0(mainpath,"features.txt"), sep=" ", stringsAsFactors = F)
names(features) <- c("featureid","feature")

#Work with features. Filter means and stds only, and change description
mainfeatures <- features[grep("mean\\(|std\\(",features$feature),] #indexes containing mean or std

changename <- function(x) {
      if (length(x)==3) {
            newname <- paste0(x[1],x[3],x[2])
      } else {
            newname <- paste0(x[1],x[2])
      }
      newname <- sub("\\(","",newname)
      newname <- sub("\\)","",newname)
      return(newname)
}

mainfeatures$feature <- sapply(strsplit(mainfeatures$feature,"-"), changename)


#Reading testing files
testds <- read.table(paste0(testpath,"X_test.txt"), sep="", stringsAsFactors = F)
testsubjects <- read.table(paste0(testpath,"subject_test.txt"), sep="", stringsAsFactors = F)
testactivities <- read.table(paste0(testpath,"y_test.txt"), sep="", stringsAsFactors = F)
names(testactivities) <- "activityid"

#Reading training files
trainds <- read.table(paste0(trainpath,"X_train.txt"), sep="", stringsAsFactors = F)
trainsubjects <- read.table(paste0(trainpath,"subject_train.txt"), sep="", stringsAsFactors = F)
trainactivities <- read.table(paste0(trainpath,"y_train.txt"), sep="", stringsAsFactors = F)
names(trainactivities) <- "activityid"

#Naming activities (join)
testactivities <- left_join(testactivities,activities,by="activityid")
trainactivities <- left_join(trainactivities,activities,by="activityid")

#Selecting measures
testds <- testds[,mainfeatures$featureid] #Filtering means and stds only
names(testds) <- mainfeatures$feature

trainds <- trainds[,mainfeatures$featureid] #Filtering means and stds only
names(trainds) <- mainfeatures$feature

#Adding subjects and activities to each dataset and a new column for each source
testds <- cbind(origin="test",testsubjects,testactivities$activity,testds)
trainds <- cbind(origin="train",trainsubjects,trainactivities$activity,trainds)

#Changing names to new columns
names(testds)[2:3] <- c("subject","activity")
names(trainds)[2:3] <- c("subject","activity")

#Union of datasets
ds <- rbind(trainds,testds)

#Convert columns
ds$subject <- as.factor(ds$subject)

#Making some memory space
rm(list=c("testds","trainds","features","mainfeatures",
          "testsubjects","trainsubjects","testactivities","trainactivities"))

#New DS grouped by subject and activity
avgds <- group_by(ds,subject,activity) %>% summarise_at(names(ds)[4:69],mean)

#Writing file
write.table(avgds,file="avgds.txt",row.names = F)