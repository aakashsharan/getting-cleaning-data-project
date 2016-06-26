require(plyr)

#files
dir_uci <- "UCI\ HAR\ Dataset"
acitivity_labels_file <- paste(dir_uci, "/activity_labels.txt", sep="")
feature_file <- paste(dir_uci, "/features.txt", sep="")
x_train_file <- paste(dir_uci, "/train/X_train.txt", sep="")
y_train_file <- paste(dir_uci, "/train/y_train.txt", sep="")
subject_train_file <- paste(dir_uci, "/train/subject_train.txt", sep = "")
x_test_file  <- paste(dir_uci, "/test/X_test.txt", sep = "")
y_test_file  <- paste(dir_uci, "/test/y_test.txt", sep = "")
subject_test_file <- paste(dir_uci, "/test/subject_test.txt", sep = "")

# Load data
features <- read.table(feature_file, colClasses = c("character"))
activity_labels <- read.table(acitivity_labels_file, col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_file)
subject_train <- read.table(subject_train_file)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)
subject_test <- read.table(subject_test_file)

# 1. Merges the training and the test sets to create one data set.#

# Binding sensor data
training_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y_test)
sensor_data <- rbind(training_sensor_data, test_sensor_data)

# Label columns
sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

sensor_data_mean_std <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]

# 3. Uses descriptive activity names to name the activities in the data set

sensor_data_mean_std <- join(sensor_data_mean_std, activity_labels, by = "ActivityId", match = "first")
sensor_data_mean_std <- sensor_data_mean_std[,-1]

# 4. Appropriately labels the data set with descriptive names.

#names(sensor_data_mean_std)
# Remove parentheses
names(sensor_data_mean_std) <- gsub('\\(|\\)',"",names(sensor_data_mean_std), perl = TRUE)
# Make syntactically valid names
names(sensor_data_mean_std) <- make.names(names(sensor_data_mean_std))
# Make clearer names
names(sensor_data_mean_std) <- gsub('Acc',"Acceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('GyroJerk',"AngularAcceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Gyro',"AngularSpeed",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Mag',"Magnitude",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^t',"TimeDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^f',"FrequencyDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('\\-mean',".Mean",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('\\-std',".StandardDeviation",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq\\.',"Frequency.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq$',"Frequency",names(sensor_data_mean_std))

# 4. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

sensor_avg_by_act_sub = ddply(sensor_data_mean_std, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_avg_by_act_sub, file = "sensor_avg_by_act_sub.txt")
