# Load feature names
features <- read.table('UCI HAR Dataset/features.txt', header=FALSE, col.names=c('id', 'featureName'), colClasses = c('numeric', 'character'))

loadData <- function(datatype) {
	data_file <- paste('UCI HAR Dataset/', datatype, '/X_', datatype, '.txt', sep='')
	label_file <- paste('UCI HAR Dataset/', datatype, '/Y_', datatype, '.txt', sep='')  
	subject_file <- paste('UCI HAR Dataset/', datatype, '/subject_', datatype, '.txt', sep='')
	result <- read.table(data_file, header=FALSE, col.names=features$featureName, colClasses = rep("numeric", nrow(features)))
	result_label <- read.table(label_file, header=FALSE, col.names=c('label'), colClasses = c('numeric'))
	result_subject <- read.table(subject_file, header=FALSE, col.names=c('subject'), colClasses = c('numeric') )
	result$label <- result_label$label
	result$subject <- result_subject$subject
	result  
}

train <- loadData('train')
test <- loadData('test')

alldata <- rbind(train, test)

requiredFeatures <- grepl("mean\\(\\)",features$featureName) | grepl("std\\(\\)",features$featureName )
requiredCols <- features[requiredFeatures,]$id
requiredData <- alldata[, requiredCols]

requiredData$label <- alldata$label
requiredData$subject <- alldata$subject

activity_labels <- read.table('UCI HAR Dataset/activity_labels.txt', header=FALSE, col.names=c('id', 'activity_label'), colClasses = c('numeric', 'character'))
requiredData <- merge(requiredData, activity_labels, by.x = 'label', by.y = 'id')
requiredData <- requiredData[, !(names(requiredData) %in% c('label'))]

library(reshape2)

meltData <- melt(requiredData, id = c('subject', 'activity_label'))
result <- dcast(meltData, subject + activity_label ~ variable, mean)

addPrefix <- function(x, prefix) {
	paste(prefix, x, sep="")
}

headerNames <- gsub("\\.+", ".", names(result))
headerNames <- gsub("\\.$", "", headerNames)
headerNames <- sapply(headerNames, addPrefix, "mean.of.")
headerNames[1] <- 'subject'
headerNames[2] <- 'activity'

names(result) <- headerNames
write.table(result, "tidy-data-set.txt", row.names=FALSE)
