#Collect raw data
trainData <- read.table(".\\Dataset\\train\\X_train.txt")
trainLabel <- read.table(".\\Dataset\\train\\y_train.txt")
trainSubject <- read.table(".\\Dataset\\train\\subject_train.txt")
testData <- read.table(".\\Dataset\\test\\X_test.txt")
testLabel <- read.table(".\\Dataset\\test\\y_test.txt") 
testSubject <- read.table(".\\Dataset\\test\\subject_test.txt")

#Merge Datasets via rbind method
mergeData <- rbind(trainData, testData)
mergeLabel <- rbind(trainLabel, testLabel)
mergeSubject <- rbind(trainSubject, testSubject)

features <- read.table(".\\Dataset\\features.txt")
index <- grep("mean\\(\\)|std\\(\\)", features[, 2])
mergeData <- mergeData[, index ]

#Format headers removing certain characters and setting certain characters to capitals

names(mergeData) <- gsub("\\(\\)", "", features[meanStdIndices, 2])
names(mergeData) <- gsub("mean", "Mean", names(joinData))
names(mergeData) <- gsub("std", "Std", names(joinData))
names(mergeData) <- gsub("-", "", names(joinData)) 

activity <- read.table(".\\Dataset\\activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[mergeLabel[, 1], 2]
mergeLabel[, 1] <- activityLabel
names(mergeLabel) <- "activity"
names(mergeSubject) <- "subject"

tidyData <- cbind(mergeSubject, mergeLabel, mergeData)

#Export Tidy Data set
write.table(tidyData , "tidy_data.txt")
means
#Collect and set data about means of previous tidy data set

sLen <- length(table(mergeSubject))
aLen <- dim(activity)[1]
cLen <- dim(tidyData )[2]
means <- matrix(NA, nrow=sLen*aLen, ncol=cLen) 
means <- as.data.frame(means)
colnames(means) <- colnames(tidyData)
index <- 1
for(i in 1:sLen) {
    for(j in 1:aLen) {
        means[index, 1] <- sort(unique(mergeSubject)[, 1])[i]
        means[index, 2] <- activity[j, 2]
        t1 <- i == tidyData$subject
        t2 <- activity[j, 2] == tidyData$activity
        means[index, 3:cLen] <- colMeans(tidyData[t1&t2, 3:cLen])
        index <- index + 1
    }
}
#Export the tidy_data_with_means

write.table(means, "tidy_data_with_means.txt",row.name=FALSE)