
#sub_dataet working directory
setwd("C:/R Coursera/Course004-Assignment3")

#Get temp data from training and test dataset and merge it using rbind
tmpdata_train = read.table("UCI HAR Dataset/train/X_train.txt")
tmpdata_test = read.table("UCI HAR Dataset/test/X_test.txt")
x_data = rbind(tmpdata_train, tmpdata_test)

#Get temp data from training and test dataset and merge it using rbind
tmpdata_train = read.table("UCI HAR Dataset/train/y_train.txt")
tmpdata_test = read.table("UCI HAR Dataset/test/y_test.txt")
y_data = rbind(tmpdata_train, tmpdata_test)


#Get temp data from training and test dataset and merge it using rbind
tmpdata_train = read.table("UCI HAR Dataset/train/subject_train.txt")
tmpdata_test = read.table("UCI HAR Dataset/test/subject_test.txt")
sub_data = rbind(tmpdata_train, tmpdata_test)

# Get mean and standard deviation for each measurement.

features = read.table("UCI HAR Dataset/features.txt")
feature_index = grep("-mean\\(\\)|-std\\(\\)", features[, 2])
x_data = x_data[, feature_index]
names(x_data) = features[feature_index, 2]
names(x_data) = gsub("\\(|\\)", "", names(x_data))
names(x_data) = tolower(names(x_data))  

#name the activities in the data set
activities = read.table("UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
y_data[,1] = activities[y_data[,1], 2]
names(y_data) = "activity"

#label the data set with activity names and write data as clean data
names(sub_data) = "subject"
cleaned = cbind(sub_data, y_data, x_data)
write.table(cleaned, "clean_data.txt")

#Create temp data set with clean data with average of each variable
uniquesub_dataubjects = unique(sub_data)[,1]
numsub_dataubjects = length(unique(sub_data)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numsub_dataubjects*numActivities), ]

row = 1
for (s in 1:numsub_dataubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniquesub_dataubjects[s]
    result[row, 2] = activities[a, 2]
    tmp = cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:numCols] = colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
#Write data back to file
write.table(result, "data_set_modified.txt")
