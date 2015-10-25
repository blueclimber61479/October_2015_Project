
library(dplyr)
library(reshape2)

features <- read.table("features.txt")
featvect <- features[,2]
subject_test <- read.table("subject_test.txt")
subject_train <- read.table("subject_train.txt")
xtest <- read.table("X_test.txt", col.names = featvect)
xtrain <- read.table("X_train.txt", col.names = featvect)
ytest <- read.table("y_test.txt")
ytrain <- read.table("y_train.txt")
ytest <- rename(ytest, activitycodes = V1)
ytrain <- rename(ytrain, activitycodes = V1)

combo1 <- cbind(subject_train, xtrain)  # adds subject IDs to rows
train <- cbind(combo1, ytrain)  # adds y column to set
combo2 <- cbind(subject_test, xtest)   # adds subject IDs to rows
test <- cbind(combo2, ytest)    # adds y column to set

mergedSet <- rbind(train, test)   # creates merged set of train and test sets

mergedSet <- rename(mergedSet, subject_ID = V1)  # renames subject ID column to make train and test sets match
# replace numbers for activities in y column with activity names

activitycodes <- mergedSet[,563]
activitynames <- c()
for (i in 1:length(activitycodes)){
    if (activitycodes[i] == 1){
    name <- "Walking"
            }

    if (activitycodes[i] == 2){
    name <- "Walking upstairs"
            }

    if (activitycodes[i] == 3){
    name <- "Walking downstairs"
            }

    if (activitycodes[i] == 4){
    name <- "Sitting"
            }

    if (activitycodes[i] == 5){
    name <- "Standing"
            }

    if (activitycodes[i] == 6){
    name <- "Laying"
            }
    
   activitynames[i] <- name      # adds name to vector
}

newmergedSet <- mutate(mergedSet, activitynames)   # add column of named activities to mergedSet

selectedSet <- select(newmergedSet, subject_ID, contains("mean"), contains("std"), activitynames)  
# selects subject IDs, measurements on mean and standard deviation, and activity names

TidySet <- selectedSet %>%
    group_by(activitynames, subject_ID) %>%
    summarize_each(funs(mean))   # average of each variable by activity and subject

