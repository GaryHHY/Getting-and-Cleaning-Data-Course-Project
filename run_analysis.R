library(dplyr)

#Reading data set into variables

features_set <- read.table("./UCI HAR Dataset/features.txt", header = FALSE, sep = "")

activity_set <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "")

train_set <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "", dec = ".")

train_set_labels <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = "", dec = ".")

train_set_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "", dec = ".")

test_set <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "", dec = ".")

test_set_labels <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "", dec = ".")

test_set_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "", dec = ".")

#Merges the training and the test sets to create one data set.

data_set_merge <- rbind(train_set,test_set)                            

data_set_label_merge <- rbind(train_set_labels,test_set_labels)

data_set_subject_merge <- rbind(train_set_subject,test_set_subject)

#Uses descriptive activity names to name the activities in the data set
label_desc_set <- sapply(data_set_label_merge[,1],function(x) activity_set[activity_set[,1] == x,2])


# Assign corresponding features name   
select_col_idx <- grep("(mean\\(\\))|(std\\(\\))",features_set[,2])

select_col_names <- features_set[grep("(mean\\(\\))|(std\\(\\))",features_set[,2]),2]

select_col_names <- gsub("\\(\\)","",select_col_names)

select_col_names <- gsub("\\-","_",select_col_names)

#Extracts only the measurements on the mean and standard deviation for each measurement.
select_data_set <- data_set_merge[,select_col_idx]                      

names(select_data_set) <- select_col_names

data_set_merge <- cbind(data_set_subject_merge,data_set_label_merge,label_desc_set,select_data_set)   

names(data_set_merge)[c(1,2,3)] <- c("Subject_No","Activity_Label","Activity_Desc")  #Appropriately labels the data set with descriptive variable names. 

data_set_merge <- as_tibble(data_set_merge)


#creates a second, independent tidy data set with the average of each variable for each activity and each subject.
data_set_summary <- data_set_merge %>%
                        group_by(Subject_No,Activity_Desc) %>%
                        summarise_at(vars(tBodyAcc_mean_X:fBodyBodyGyroJerkMag_std),mean) 

#output the tidy data set
write.table(data_set_summary,"./tidy_data_set.txt",row.name=FALSE)
