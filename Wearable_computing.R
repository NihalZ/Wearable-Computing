#loading dplyr library
library(dplyr)

#loading the test data and creating test data frame

y_test <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/x_test.txt")
df_test <- as.data.frame(c(subject_test, y_test,x_test))


#loading the train data and creating train data frame

x_train<-read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/x_train.txt")
y_train<-read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
df_train <- as.data.frame(c(subject_train, y_train,x_train))

#loading the feartures names
features <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")

# Merging the two data set
df <- merge(df_test,df_train, all.x = TRUE, all.y=TRUE)

#Renaming the columns in the data Frame
df<- rename(df,Subject=V1, Test_Type = V1.1)
colnames(df)[3:563]<- as.character(features[,2])

#Naming the activity in the data set
activity_label<- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
for(i in 1:10299){
  df$Test_Type[i]<- as.character(activity_label[as.numeric(df$Test_Type[i]),2])
  
  
}

#Extracting the measurment on the mean and the standard deviation
# mean here is interepreted as mean() and frequency mean

mean <- grep("mean",names(df))
std<-grep("std",names(df))
vector<- c(mean, std)
s<- 0


for(i in 3:563){
  if(!(i %in% vector)){
    df[[i-s]]<- NULL
    s<- s+1
    
  }

  

}
# forming a data frame with the average of each measurement for each subject per activity

df_new<-aggregate(df$`tBodyAcc-mean()-X`, by=list(df$Subject,df$Test_Type), FUN= mean)
for(i in 4:81){
  agg <- aggregate(df[[i]], by=list(df$Subject,df$Test_Type), FUN= mean)
  df_new <- cbind(df_new, agg$x)
  
}
df_new<- as.data.frame(df_new)
colnames(df_new)<- as.character(names(df))

