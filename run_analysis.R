run_analysis <- function(...) {
  
  # Step 1: Merge Training and Test Data
  # Load the X of Training and Test Data first and Merge Them into one data table
  # Now, bind the Y of Training and Test Data to Merge Them into one data table
  # Finally, bind the subject data for Training and Test Data to merge into a data table
  
  # script assumes working directory to be set to a folder containing UCI HAR DataSet
  # eg: setwd ("C:/Users/Rubymaz/Documents/getdata_projectfiles_UCI HAR DataSet")
  
  train_data <- read.table("UCI HAR DataSet/train/X_train.txt")
  test_data <- read.table("UCI HAR DataSet/test/X_test.txt")
  merged_X <- rbind(train_data, test_data)
  
  train_Y <- read.table("UCI HAR DataSet/train/y_train.txt")
  test_Y <- read.table("UCI HAR DataSet/test/y_test.txt")
  merged_Y <- rbind(train_Y, test_Y)
  
  subject_train <- read.table("UCI HAR DataSet/train/subject_train.txt")
  subject_test <- read.table("UCI HAR DataSet/test/subject_test.txt")
  merged_subject <- rbind(subject_train, subject_test)
  
  # Step 2: Find column names that contain mean or std
  # Adding feature names as column names to merged_X for easier search for Step 2 of the project
  # Now using grep() to find the required columns
  
  features <- read.table("UCI HAR DataSet/features.txt")
  featurenames <- features [,2]
  names(merged_X) <- featurenames
  
  merged_X <- merged_X[, grep("mean|std", names(merged_X))]
  
  # Step 3: Change the outcomes (Y) to have descriptive label names
  # First, read the activity_labels file
  # Add names to columns so it's easier to merge
  # Now Merge merged_Y and activity_labels
  # NOTE: Since merge() do not keep the order intact,
  # I will add an id column prior to merge and then re-order based on that post merge
  # pick up only the description column for the final outcome of step3
  
  activity_labels <- read.table("UCI HAR DataSet/activity_labels.txt")
  names(activity_labels) <- c("num", "description")
  names(merged_Y) <- c("num")
  merged_Y$id <- 1:nrow(merged_Y)
  merged_Y_labeled <- merge(merged_Y,activity_labels)
  merged_Y_labeled <- merged_Y_labeled[order(merged_Y_labeled$id), ]
  labels <- merged_Y_labeled$description
  
  # Step 4: Labeling the data set
  # explored various possibilities for regular expression based matches
  # but nothing came out very readable
  # hence using hand-crafted column names
  
  colnames(merged_X) <-
    c("body_Accelerometer_Mean_X_axis", "body_Accelerometer_Mean_Y_axis",
      "body_Accelerometer_Mean_Z_axis", "body_Accelerometer_Standard_Deviation_X_axis",
      "body_Accelerometer_Standard_Deviation_Y_axis", "body_Accelerometer_Standard_Deviation_Z_axis",
      "gravity_Accelerometer_Mean_X_axis", "gravity_Accelerometer_Mean_Y_axis",
      "gravity_Accelerometer_Mean_Z_axis", "gravity_Accelerometer_Standard_Deviation_X_axis",
      "gravity_Accelerometer_Standard_Deviation_Y_axis", "gravity_Accelerometer_Standard_Deviation_Z_axis",
      "body_Accelerometer_Jerk_Mean_X_axis", "body_Accelerometer_Jerk_Mean_Y_axis",
      "body_Accelerometer_Jerk_Mean_Z_axis", "body_Accelerometer_Jerk_Standard_Deviation_X_axis", 
      "body_Accelerometer_Jerk_Standard_Deviation_Y_axis", "body_Accelerometer_Jerk_Standard_Deviation_Z_axis",
      "body_Gyro_Mean_X_axis", "body_Gyro_Mean_Y_axis", "body_Gyro_Mean_Z_axis",
      "body_Gyro_Standard_Deviation_X", "body_Gyro_Standard_Deviation_Y",
      "body_Gyro_Standard_Deviation_Z", "body_Gyro_Jerk_Mean_X_axis", "body_Gyro_Jerk_Mean_Y_axis",
      "body_Gyro_Jerk_Mean_Z_axis", "body_Gyro_Jerk_Standard_Deviation_X_axis",
      "body_Gyro_Jerk_Standard_Deviation_Y_axis", "body_Gyro_Jerk_Standard_Deviation_Z_axis",
      "body_Accelerometer_Magnitude_Mean", "body_Accelerometer_Magnitude_Standard_Deviation", 
      "body_Gravity_Accelerometer_Magnitude_Mean", "body_Gravity_Accelerometer_Standard_Deviation",
      "body_Accelerometer_Jerk_Magnitude_Mean", "body_Accelerometer_Jerk_Magnitude_Standard_Deviation",
      "body_Gyro_Magnitude_Mean", "body_Gyro_Magnitude_Standard_Deviation", "body_Gyro_Jerk_Magnitude_Mean",
      "body_Gyro_Jerk_Magnitude_Standard_Deviation", "body_Acceleration_Mean_X_axis",
      "body_Accelerometer_Mean_Y_axis", "body_Accelerometer_Mean_Z_axis",
      "body_Accelerometer_Standard_Deviation_X_axis", "body_Accelerometer_Standard_Deviation_Y_axis",
      "body_Accelerometer_Standard_Deviation_Z_axis", "body_Accelerometer_MeanFrequency_X_axis",
      "body_Accelerometer_MeanFrequency_Y_axis", "body_Accelerometer_MeanFrequency_Z_axis",
      "body_Accelerometer_Jerk_Mean_X_axis", "body_Accelerometer_Jerk_Mean_Y_axis",
      "body_Accelerometer_Jerk_Mean_Z_axis", "body_Accelerometer_Jerk_Standard_Deviation_X_axis",
      "body_Accelerometer_Jerk_Standard_Deviation_Y_axis", "body_Accelerometer_Jerk_Standard_Deviation_Z_axis",
      "body_Accelerometer_Jerk_MeanFrequency_X_axis", "body_Accelerometer_Jerk_MeanFrequency_Y_axis", 
      "body_Accelerometer_Jerk_MeanFrequency_Z_axis", "body_Gyro_Mean_X_axis", "body_Gyro_Mean_Y_axis", 
      "body_Gyro_Mean_Z_axis", "body_Gyro_Standard_Deviation_X_axis", "body_Gyro_Standard_Deviation_Y_axis",
      "body_Gyro_Standard_Deviation_z_axis", "body_Gyro_MeanFrequency_X_axis", "body_Gyro_MeanFrequency_Y_axis",
      "body_Gyro_MeanFrequency_Z_axis", "body_Accelerometer_Mangitude_Mean",
      "body_Accelerometer_Magnitude_Standard_Deviation", "body_Accelerometer_Magntude_MeanFrequency", 
      "body_body_Accelerometer_Jerk_Magntitude_Mean", "body_body_Accelerometer_Jerk_Magntitude_Standard_Deviation",
      "body_body_Accelerometer_Jerk_Magntitude_MeanFrequency", "body_body_Gyro_Magnitude_Mean",
      "body_body_Gyro_Magnitude_Standard_Deviation", "body_body_Gyro_Magnitude_MeanFrequency",
      "body_body_Gyro_Jerk_Magnitude_Mean", "body_body_Gyro_Jerk_Magnitude_Standard_Devation",
      "body_body_Gyro_Jerk_Magnitude_MeanFrequency")
  
  # Step 5 preparation: name the merged_subject column name
  # now column bind merged_X, merged_subject and merged_Y_labeled
  # also using make.names() to clean up the column names to make easier search
  # also remove the description and id columns for now, I'll add back description in the end
  
  names(merged_subject) <- c("subject")
  all_data <- cbind (merged_subject, merged_Y_labeled, merged_X)
  valid_column_names <- make.names(names=names(all_data), unique=TRUE, allow_ = TRUE)
  names(all_data) <- valid_column_names
  all_data <- select (all_data, -(description))
  all_data <- select (all_data, -(id))
  
  # Step 5: find averages per subject per description 
  # there are 30 subjects, subject = 1, 2, .. 30
  # there are 5 num, num = 1, 2, 3, 4, 5
  
  merged_data <- data.frame()
  # there are 30 subjects, so i runs the loop 1:30
  # and there are 6 outcomes, so j runs the loop 1:6
  for (i in 1:30){
    for (j in 1:6){
      one_set <- filter(all_data, subject==i & num==j)
      one_set_avg <- summarize_each(one_set, funs(mean))
      merged_data <- rbind(merged_data, one_set_avg)
    }}
  
  # now merge back description
  
  merged_data$id <- 1:nrow(merged_data)
  merged_data_labeled <- merge(merged_data,activity_labels)
  merged_data_labeled <- merged_data_labeled[order(merged_data_labeled$id), ]
  
  # remove the id and num columns
  merged_data_labeled <- select (merged_data_labeled, -(num))
  merged_data_labeled <- select (merged_data_labeled, -(id))
  
  write.table(merged_data_labeled, file="step5_out.txt", row.names = FALSE)
}