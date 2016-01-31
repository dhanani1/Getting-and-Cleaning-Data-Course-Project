#set wd to the location where the UCI HAR Dataset is
setwd('~Coursera/Getting and Cleaning Data/UCI HAR Dataset');

#read in the data files
xTrain       = read.table('./train/x_train.txt',header=FALSE);
yTrain       = read.table('./train/y_train.txt',header=FALSE);
activityType = read.table('./activity_labels.txt',header=FALSE);
features     = read.table('./features.txt',header=FALSE);
subjectTrain = read.table('./train/subject_train.txt',header=FALSE);



#change column names as appropriate
colnames(xTrain)        = features[,2]; 
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(yTrain)        = "activityId";

#create training set by merging  the following 3 datasets
trainingData = cbind(yTrain,subjectTrain,xTrain);

#read test datasets
yTest       = read.table('./test/y_test.txt',header=FALSE);
subjectTest = read.table('./test/subject_test.txt',header=FALSE);
xTest       = read.table('./test/x_test.txt',header=FALSE);

#name columns appropriately to test datasets
colnames(yTest)       = "activityId";
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 


#merge the following datasets to create a new dataset called Datatest
Datatest = cbind(yTest,subjectTest,xTest);


#combine training and test data to get last dataset
LastData = rbind(trainingData,Datatest);

#Create a vector for the column names from the finalData, which will be used
#to select the desired mean() & stddev() columns
colNames  = colnames(LastData); 

#2. Extract only the measurements on the mean and standard deviation for each measurement. 

#Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector to keep only desired columns
LastData = LastData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
LastData = merge(LastData,activityType,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(LastData); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(LastData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = LastData[,names(LastData) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
TidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
TidyData    = merge(TidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(TidyData, './TidyData.txt',row.names=TRUE,sep='\t');
