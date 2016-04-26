#Author: ipac
#Date: April 24, 2016



#download data
setwd("/Users/Ivonne/Dropbox/Ivy/0. Data Scientist/Coursera Courses/datasciencecoursera/3. Cleaning Data/Course Project")
if(!file.exists("./data")){dir.create("./data")}
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file<-"./data/dataproject.zip"
download.file(url, file,method="curl")

if(!file.exists("./results")){dir.create("./results")}

#Example of reading one text file
testinertial1<-read.table("/Users/Ivonne/Dropbox/Ivy/0. Data Scientist/Coursera Courses/datasciencecoursera/3. Cleaning Data/Course Project/data/UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt", sep="", stringsAsFactors=F)

#function to read a text file 
datafolder <- "./data/UCI HAR Dataset"
gettables <- function (filename,cols = NULL){
        print(paste("Getting table:", filename))
        f <- paste(datafolder,filename,sep="/")
        data <- data.frame()
        if(is.null(cols)){
                data <- read.table(f,sep="",stringsAsFactors=F)
        } else {
                data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
        }
        data
}

#all the tables I need to read are in features
features <- gettables("features.txt")
testinertial2<-gettables("test/Inertial Signals/body_acc_x_test.txt") #it works :-)

#to read every 3 files I want, using the variable features, and compiling it in one table
getdata <- function(type, features){
        print(paste("Getting data", type))
        subject_data <- gettables(paste(type,"/","subject_",type,".txt",sep=""),"id")
        y_data <- gettables(paste(type,"/","y_",type,".txt",sep=""),"activity")
        x_data <- gettables(paste(type,"/","X_",type,".txt",sep=""),features$V2)
        return (cbind(subject_data,y_data,x_data))
}

#run and check getdata
test <- getdata("test", features)
train <- getdata("train", features)

#save the resulting data in the indicated folder
saveresults <- function (data,name){
        print(paste("saving results", name))
        file <- paste("results", "/", name,".csv" ,sep="")
        write.csv(data,file)
}


#1 Merging the training and the test sets to create one data set.
library(plyr)
data <- rbind(train, test) #by row because train and test have different individuals
data <- arrange(data, id)

#2 Extracts only the measurements on the mean and standard deviation for each measurement. 
meanstd <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
saveresults(meanstd,"meanstd")

#3 Uses descriptive activity names to name the activities in the data set
activity_labels <- gettables("activity_labels.txt")

#4 Appropriately labels the data set with descriptive variable names. 
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2) #labeling factors
table(data$activity)

#5 Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidy_dataset <- ddply(meanstd, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(tidy_dataset)[-c(1:2)] <- paste(colnames(tidy_dataset)[-c(1:2)], "_mean", sep="")
saveresults(tidy_dataset,"tidy_dataset")
