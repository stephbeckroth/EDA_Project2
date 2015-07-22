# This program is written to fulfill the requirements of
# the Getting and Cleaning Data Coursera Course. Smartphone
# Data was extracted from the following websites belonging
# to UCI Machine Learning Repository.
# "http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones# "
# 
# Program written by Stephanie R. Beck Roth
# Due 7-26-2015

library(doBy)
library(dplyr)
library(tidyr)
library(gtools)
#Get Data

if (!file.exists("./UCI\ HAR\ Dataset/test")){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl,destfile = "./SmartPhone.zip", method="curl")
    mycsv <- unzip("./SmartPhone.zip", exdir = ".")
}
# The first 5 text files are descriptors of the data.
# Files 5-13 contain Inertial Signals tests data.
# Files 14-16 contain subject, X and Y tests data, respectively.
# Files 17-25 contain the Inertial Signals training data.
# Files 26-28 contain subject, X and Y training data, respectively.
# Only including the following files for analysis per discussion forum post by course TA, 
# David Hood at https://class.coursera.org/getdata-030/forum/thread?thread_id=37

# Read file containing activity labels for dytest and dytrain activity codes
aln <- read.delim("./UCI\ HAR\ Dataset/activity_labels.txt", sep=" ",
                  col.names=c("code","activitylabel"),header=FALSE)

dytest <- read.table("./UCI\ HAR\ Dataset/test/y_test.txt", 
                     col.names = "code", header=FALSE)
dytrain <- read.table("./UCI\ HAR\ Dataset/train/y_train.txt", 
                      col.names = "code", header=FALSE)

# Read file containing column headers for dxtest and dxtrain
coln <- read.delim("./UCI\ HAR\ Dataset/features.txt", sep=" ",header=FALSE)
cnames <- as.character(coln$V2)

# Read file containing participant identififiers for dstest 
# Note: There are only 30 participants and each participant is either in the
# training dataset or the test dataset.
dstest <- read.delim("./UCI\ HAR\ Dataset/test/subject_test.txt",header=FALSE)
part_id_test <- as.factor(dstest$V1)

# Read test Content Files and wrap the data frames as data tables and setting variables to those in features.txt
dxtest <- tbl_df(read.table("./UCI\ HAR\ Dataset/test/X_test.txt", col.names=cnames))
# Create testdata which will have the following new variables stored in the beginning of the table dxtest
# dxtest$set <- "test"
# dxtest$part_id <- part_id_test
# dxtest$code <- dytest$code
testdata <- cbind(part_id=part_id_test,set="test",code=dytest$code,dxtest)

# Read file containing participant identififiers for dstrain 
dstrain <- read.delim("./UCI\ HAR\ Dataset/train/subject_train.txt",header=FALSE)
part_id_train <- as.factor(dstrain$V1)

# Read train Content Files and wrap the data frames as data tables and setting variables to those in features.txt
dxtrain <- tbl_df(read.table("./UCI\ HAR\ Dataset/train/X_train.txt", col.names=cnames))

# Create traindata which will have the following new variables stored in the beginning of the table dxtrain
# dxtrain$set <- "train"
# dxtrain$part_id <- part_id_train
# dxtrain$code <- dytrain$code
traindata <- cbind(part_id=part_id_train,set="train",code=dytrain$code,dxtrain)

# Create Data set with both test and train data
# Using rbind since the variable names are identical
alldata <- rbind(testdata,traindata)
# Introducde the activitylabel variable which exlains the information in the variable code.
alldata <- merge(alldata, aln, by="code")
# Reorder the table so that the activity label variable is near the beginning in the dataset
alldata <- alldata[, c(1,2,3,565,4:564)]
# Deleting the code variable to make it tidy since the activitylabel variable is now in the dataset
# to convey the same information.
alldata <- select(alldata, -code)
# Select only part_id, set, activitylabel (columns 1-3)
# and those variables containing mean or std in their names.
cols <- 1:3
colsm <- grep("mean", colnames(alldata))
cols <- append(cols,colsm)
colstd <- grep("std", colnames(alldata))
cols <- sort(append(cols,colstd))
alldatatrim <- alldata[,cols]

# Creating the tidy dataset
tidytable <-summaryBy(. ~set+part_id+activitylabel,data=alldatatrim,FUN=mean)

# Writing the tidy dataset
write.table(tidytable, file="../Data/tidytable.txt", row.name=FALSE)
