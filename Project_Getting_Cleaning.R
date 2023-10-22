# -------------------------------------------------------------------------

library(tidyverse)
library(here)

# -------------------------------------------------------------------------

# get test data

xtest <- read.table( file = here::here("Getdata_projectfile","UCI_HAR_Dataset","test","X_test.txt")
                     ,header = FALSE)

ytest <- read.table( file = here::here("Getdata_projectfile","UCI_HAR_Dataset","test","y_test.txt")
                      ,header = FALSE)

# -------------------------------------------------------------------------

# get train data

xtrain <- read.table( file = here::here("Getdata_projectfile","UCI_HAR_Dataset","train","X_train.txt")
                     ,header = FALSE)

ytrain <- read.table( file = here::here("Getdata_projectfile","UCI_HAR_Dataset","train","y_train.txt")
                      ,header = FALSE)

# -------------------------------------------------------------------------

# get features data

features <- read.table( file = here::here("Getdata_projectfile","UCI_HAR_Dataset","features.txt")
                       ,header = FALSE)

# -------------------------------------------------------------------------

# get activity data

activity <- read.table( file = here::here("Getdata_projectfile","UCI_HAR_Dataset","activity_labels.txt")
                       ,header = FALSE)

# -------------------------------------------------------------------------

# get subject data

## substest

subtest <- read.table( file = here::here("Getdata_projectfile","UCI_HAR_Dataset","test","subject_test.txt")
                      ,header = FALSE)

subtest <- subtest %>%
  dplyr::rename(subjectID = V1)

## substrain

subtrain <- read.table( file = here::here("Getdata_projectfile","UCI_HAR_Dataset","train","subject_train.txt")
                     ,header = FALSE)

subtrain <- subtrain %>%
  dplyr::rename(subjectID = V1)

# -------------------------------------------------------------------------

# add column names to both train and test data

features <- features[,2]

featrasp <- t(features)

colnames(xtrain) <- featrasp

colnames(xtest) <- featrasp

# -------------------------------------------------------------------------

# rename activity columns to id and actions(walk,lay,etc.)

colnames(activity) <- c('id','actions')

# -------------------------------------------------------------------------

# row bind xtrain and xtest 

combineX <- rbind(xtrain, xtest) 

# -------------------------------------------------------------------------

# row bind ytrain and ytest

combineY <- rbind(ytrain, ytest)

# -------------------------------------------------------------------------

# row bind subject train and subject test

combineSubj <- rbind(subtrain,subtest)

# -------------------------------------------------------------------------

# column bind Y and X (the two data frames created above). We then have everything except for activity 

YXdf <- cbind(combineY,combineX, combineSubj)

# -------------------------------------------------------------------------

# merge the above data frame with the activity

db <- merge(YXdf, activity, by.x = 'V1', by.y = 'id')

# -------------------------------------------------------------------------

# getting the mean and standard deviation

colNames <- colnames(db)

db.final <- db %>%
  dplyr::select(actions, subjectID, stringr::str_subset(colNames,"\\bmean\\b|\\bstd\\b"))

# -------------------------------------------------------------------------

# transform activity to a factor variable 

db.final$actions <- as.factor(db.final$actions)

# -------------------------------------------------------------------------

# use descriptive activity names to name the activities in the data set

colnames(db.final) <- gsub("^t", "time", colnames(db.final))
colnames(db.final) <- gsub("^f", "frequency", colnames(db.final))
colnames(db.final) <- gsub("Acc", "Accelerometer", colnames(db.final))
colnames(db.final) <- gsub("Gyro", "Gyroscope", colnames(db.final))
colnames(db.final) <- gsub("Mag", "Magnitude", colnames(db.final))
colnames(db.final) <- gsub("BodyBody", "Body", colnames(db.final))

# -------------------------------------------------------------------------

# creates a second data set with the average of each variable for activity and subject.

db.final <- db.final %>% 
  dplyr::group_by(actions,subjectID) %>% 
  dplyr::arrange(desc(actions)) %>% 
  dplyr::summarise_all(mean) %>% 
  dplyr::select(subjectID,everything())

# -------------------------------------------------------------------------
# Save binary data --------------------------------------------------------

save(db.final, file = here::here("Getdata_projectfile","UCI_HAR_Dataset","tidydata.rda"))

# text file for final output

write.table(db.final, file = here::here("Getdata_projectfile","UCI_HAR_Dataset","tidydata.txt")
                                     ,row.name = FALSE)

# -------------------------------------------------------------------------
