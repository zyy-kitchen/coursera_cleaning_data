cleaning <- function ()
        {
                library(dplyr)
        url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(url,"./data.zip",method = "curl")
        unzip("./data.zip")
        
        features <- read.table("./UCI HAR Dataset/features.txt", col.names = c("n","functions"))
        activity <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("label", "activity"))
        subtest <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
        xtest <- read.table("./UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
        ytest <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "label")
        subtrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
        xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
        ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "label")
        x<- rbind(xtrain,xtest)
        y<- rbind(ytrain,ytest)
        sub<- rbind(subtrain,subtest)
        all<- cbind(sub,y,x)
        mean_std_only <- select(all,subject, label, contains("mean"), contains("std"))
        ##mean_std_only <- activity[mean_std_only$label,2]
        names(mean_std_only)[2] = "activity"
        names(mean_std_only)<-gsub("Acc", "Accelerometer", names(mean_std_only))
        names(mean_std_only)<-gsub("Gyro", "Gyroscope", names(mean_std_only))
        names(mean_std_only)<-gsub("BodyBody", "Body", names(mean_std_only))
        names(mean_std_only)<-gsub("Mag", "Magnitude", names(mean_std_only))
        names(mean_std_only)<-gsub("^t", "Time ", names(mean_std_only))
        names(mean_std_only)<-gsub("^f", "Frequency ", names(mean_std_only))
        names(mean_std_only)<-gsub("tBody", "TimeBody", names(mean_std_only))
        names(mean_std_only)<-gsub("-mean()", " Mean", names(mean_std_only), ignore.case = TRUE)
        names(mean_std_only)<-gsub("-std()", " STD", names(mean_std_only), ignore.case = TRUE)
        names(mean_std_only)<-gsub("-freq()", " Frequency", names(mean_std_only), ignore.case = TRUE)

        final <- mean_std_only %>% group_by(subject,activity)
        
        summarize_all(final,funs(mean))
        final$activity <- activity[final$activity,2]
        write.table(final,"final.txt",row.name=FALSE)

}



