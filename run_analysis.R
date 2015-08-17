run_analysis<-function(){
  #loading all data into memory
  datxtest<-read.table("UCI HAR Dataset/test/X_test.txt")
  datytest<-read.table("UCI HAR Dataset/test/Y_test.txt")
  datsubtest<-read.table("UCI HAR Dataset/test/Subject_test.txt")
  datxtrain<-read.table("UCI HAR Dataset/train/X_train.txt")
  datytrain<-read.table("UCI HAR Dataset/train/Y_train.txt")
  datsubtrain<-read.table("UCI HAR Dataset/train/Subject_train.txt")
  feats<-read.table("UCI HAR Dataset/features.txt")
  acts<-read.table("UCI HAR Dataset/activity_labels.txt")
  
  #merging all data into one dataset
  totaldat<-rbind(cbind(datsubtest,datytest,datxtest),cbind(datsubtrain,datytrain,datxtrain))
  
  #getting descriptive names for activities
  actcol<-acts[totaldat[,2],2]
  
  #adding column with descriptive activity names after the actvity id column
  totaldat<-cbind(totaldat[,1:2],actcol,totaldat[3:ncol(totaldat)])
  
  #adding names to all the columns some manually and the rest directly from features.txt
  colnames(totaldat)<-c("SubjectID","ActivityID","Activity",as.character(feats[,2]))
  
  #subsetting the data to a smaller dataset that contain mean() or std()
  meanandstddat<-totaldat[,sort(c(1,2,3,grep("mean\\(\\)",colnames(totaldat)),grep("std\\(\\)",colnames(totaldat))))]
  
  #getting the mean values for all the data with respect to each subject and activity
  datagg<-aggregate(meanandstddat[,4:ncol(meanandstddat)], by = list(Subject = meanandstddat$SubjectID, Activity = meanandstddat$Activity),mean)
  
  #writing that data to .txt file for submission
  write.table(format(datagg, scientific = TRUE), "tidydata.txt",row.names = FALSE, col.names = TRUE)
  
  
  
}