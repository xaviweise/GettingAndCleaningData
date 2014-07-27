#run_analysis script
#Inputs: path = route to your data folder, where a /test and a /train folder are expected with the test and train data
#Output: it will create a .txt file in the path with the tidy data set

run_analysis <- function(path)
{
  
  # Read data from path
  X_test <- read.table(paste(path, "/test/X_test.txt", sep=""))
  Y_test <- read.table(paste(path, "/test/Y_test.txt", sep=""))
  X_train <- read.table(paste(path, "/train/X_train.txt", sep=""))
  Y_train <- read.table(paste(path, "/train/Y_train.txt", sep=""))
  
  subject_train <- read.table(paste(path, "/train/subject_train.txt", sep=""))
  subject_test <- read.table(paste(path, "/test/subject_test.txt", sep=""))
  
  features <- read.table(paste(path, "/features.txt", sep=""))  
  activity_labels <- read.table(paste(path, "/activity_labels.txt", sep=""))
  
  # Add features labels to X_test and X_train, and descriptive names to Y and subject 
  names(X_test) <- features$V2
  names(X_train) <- features$V2
  names(Y_test) <- c("Activity")
  names(Y_train) <- c("Activity")
  names(subject_test) <- c("Subject")
  names(subject_train) <- c("Subject")
  
  ## Use descriptive activity names to name the activities in the data set
  map <- setNames(activity_labels$V1, activity_labels$V2)
  train_activity_translation <- map[Y_train$Activity]
  test_activity_translation <- map[Y_test$Activity]
  
  Y_train$Activity <- names(train_activity_translation)
  Y_test$Activity <- names(test_activity_translation)
  
  # Merge test and train data sets
  X_data <- rbind(X_test, X_train)
  activity <- rbind(Y_test, Y_train)
  subject <- rbind(subject_test, subject_train)
  
  # Extracts only the measurements on the mean and standard deviation for each measurement. 
  # Notice that we avoid the meanFreq in data set, as it is a differente variable (see features_info)
  index_mean <- grep("mean()", names(X_data), fixed = TRUE)
  index_std <- grep("std()", names(X_data), fixed = TRUE)
  
  X_data_mean <- X_data[,index_mean]
  X_data_std <- X_data[,index_std]
  
  # Merge mean, std, subject and activity
  dataset <- cbind(X_data_mean, X_data_std)
  dataset <- cbind(dataset, activity)
  dataset <- cbind(dataset, subject)
  
  # Add descriptive names to the variables in the data set
  # A bit of logic is used to process the short names into descriptive names
  # Since all the short names follow a pattern variable-stat()-axis, we use
  # maps to substitute the three components into a human readable description
  map_vars <- setNames(c("Body Acceleration Signal", 
                         "Gravity Acceleration Signal", 
                         "Jerk Signal of Body Acceleration",
                         "Body Angular Velocity Signal",
                         "Jerk Signal of Body Angular Velocity", 
                         "Magnitude of Body Acceleration Signal", 
                         "Magnitude of Gravity Acceleration Signal", 
                         "Magnitude of Jerk Signal of Body Acceleration", 
                         "Magnitude of Body Angular Velocity Signal", 
                         "Magnitude of Jerk Signal of Body Angular Velocity", 
                         "FFT of Body Acceleration Signal", 
                         "FFT of Jerk Signal of Body Angular Velocity", 
                         "FFT of Body Angular Velocity Signal", 
                         "FFT of Magnitude of Body Acceleration Signal", 
                         "FFT of Magnitude of Jerk Signal of Body Acceleration", 
                         "FFT of Magnitude of Body Angular Velocity Signal", 
                         "FFT of Magnitude of Jerk Signal of Body Angular Velocity"), 
                       c("tBodyAcc", 
                         "tGravityAcc",
                         "tBodyAccJerk", 
                         "tBodyGyro", 
                         "tBodyGyroJerk", 
                         "tBodyAccMag", 
                         "tGravityAccMag", 
                         "tBodyAccJerkMag",
                         "tBodyGyroMag", 
                         "tBodyGyroJerkMag", 
                         "fBodyAcc", 
                         "fBodyAccJerk", 
                         "fBodyGyro", 
                         "fBodyAccMag", 
                         "fBodyBodyAccJerkMag", 
                         "fBodyBodyGyroMag", 
                         "fBodyBodyGyroJerkMag"))
  
  map_type <- setNames(c("Mean of", "Std of"), c("-mean()", "-std()"))
  map_axis <- setNames(c("", "in the X direction", "in the Y direction", "in the Z direction"), c("", "-X", "-Y", "-Z"))
  
  for (name in names(map_vars))
  {
    
    for(type in names(map_type))
    {
     
      for(axis in names(map_axis))
      {
        name_var <- name
        name_var <- paste(name_var, type, sep = "")
        name_var <- paste(name_var, axis, sep = "")
        index <- which(names(dataset) %in% name_var)
        
        if(length(index) > 0){
          descriptive_name <- paste(map_type[type], map_vars[name])
          if (!(axis == "")) descriptive_name <- paste(descriptive_name, map_axis[axis])
          names(dataset)[index] <- descriptive_name
        }
      }
      
    }
    
  }
  
  # Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
  mean_sp <- aggregate(dataset[1:66], by = list(dataset$Activity, dataset$Subject), FUN = mean)
  names(mean_sp)[1] <- "Subject"
  names(mean_sp)[2] <- "Activity"
  
  # Creates file in path
  write.table(mean_sp, file= paste(path, "/tidy_data_set.txt", sep = ""))
  
}