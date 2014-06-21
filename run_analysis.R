#Reading the files

testx=read.table("X_test.txt")
trainx=read.table("X_train.txt")
testy=read.table("y_test.txt")
trainy=read.table("y_train.txt")
subject_test=read.table("subject_test.txt")
subject_train=read.table("subject_train.txt")
activity=read.table("activity_labels.txt")
features=read.table("features.txt")

#Merging the training and test sets 

x=rbind(testx,trainx)
y=rbind(testy,trainy)
subject=rbind(subject_test,subject_train)

#Extracting only the measurements on the mean and standard deviation for each measurement
meansd=grep("-mean\\(\\)|-std\\(\\)", features[, 2])
x = x[, meansd]

#labelling the data set with descriptive variable names 

names(x) = features[meansd, 2]
names(x) = gsub("\\(|\\)", "", names(x))
names(x) = tolower(names(x)) 

#Giving descriptive activity names to name the activities in the data set

activity[, 2] = gsub("_", "", tolower(as.character(activity[, 2])))
y[,1] = activity[y[,1], 2]
names(y)  = "activity"
names(subject) = "subject"

#Creating the tidy data set
tidydata = cbind(subject, y, x)

#Second, independent tidy data set with the average of each variable for each activity and each subject

uniquesubjects = unique(subject)[,1]
numsubjects = length(unique(subject)[,1])
numactivity = length(activity[,1])
numcols = dim(tidydata)[2]
result = tidydata[1:(numsubjects*numactivity), ]
row = 1
for (s in 1:numsubjects) {
  for (a in 1:numactivity) {
    result[row, 1] = uniquesubjects[s]
    result[row, 2] = activity[a, 2]
    tmp <- tidydata[tidydata$subject==s & tidydata$activity==activity[a, 2], ]
    result[row, 3:numcols] = colMeans(tmp[, 3:numcols])
    row = row+1
  }
}
